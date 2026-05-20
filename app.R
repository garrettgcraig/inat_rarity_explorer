# ============================================================================ #
#  iNat Rarity Explorer                                                        #
#  Discover your rarest iNaturalist observations by global observation count   #
#  Author: Garrett Craig  |  github.com/garrettgcraig                          #
# ============================================================================ #

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(httr)
library(jsonlite)
library(plotly)
library(DT)
library(dplyr)
library(later)

# ── Constants ──────────────────────────────────────────────────────────────────
BASE_URL      <- "https://api.inaturalist.org/v1"
COL_GREEN     <- "#74ac00"
REQUEST_DELAY <- 0.08   # seconds between observation page fetches — be polite
INAT_UA       <- "iNat Rarity Explorer (https://garrettgcraig.com)"

# Concurrency knobs. iNat allows up to 60 req/min — stay well below.
CONCURRENCY_PAGES   <- 4L    # pages of /observations fired in parallel
CONCURRENCY_TAXA    <- 3L    # /taxa batches fired in parallel
TAXA_BATCH_SIZE     <- 200L  # taxon IDs per /taxa call (was 30)

# Fields filter: ask iNat to send only what we use. Smaller responses → faster
# transfer AND less JSON to parse on the (single) R thread.
OBS_FIELDS  <- "id,observed_on,place_guess,photos,taxon"
TAXA_FIELDS <- "id,observations_count"

# Process-wide cache of global taxon observation counts. Survives across
# sessions in the same R worker — repeat lookups for popular taxa are free.
.global_count_cache <- new.env(parent = emptyenv())

# ── NULL-coalescing helper ─────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

chr_val <- function(x) {
  v <- tryCatch(as.character(x), error = function(e) NA_character_)
  if (length(v) == 0 || is.na(v)) NA_character_ else v
}

# ── API helpers ────────────────────────────────────────────────────────────────

#' GET with retries
safe_get <- function(url, query = list(), retries = 3, timeout_secs = 30) {
  for (i in seq_len(retries)) {
    resp <- tryCatch(
      GET(url, query = query, timeout(timeout_secs)),
      error = function(e) NULL
    )
    if (!is.null(resp) && status_code(resp) == 200) return(resp)
    if (i < retries) Sys.sleep(min(i * 0.5, 2))
  }
  NULL
}

#' Fire N HTTP GETs in parallel using libcurl's multi interface and return a
#' list of parsed JSON bodies (NULL on error). Blocks until all complete or
#' timeout. Connections are pooled, so TLS handshake is amortized across the
#' batch. This is what makes the fetch and lookup phases fast.
inat_multi_get_json <- function(urls, max_concurrent = 4L, timeout = 60L) {
  if (length(urls) == 0L) return(list())
  results <- vector("list", length(urls))
  pool <- curl::new_pool(total_con = max_concurrent,
                         host_con  = max_concurrent)
  for (i in seq_along(urls)) {
    local({
      idx <- i
      h   <- curl::new_handle()
      curl::handle_setheaders(h,
        "User-Agent" = INAT_UA,
        "Accept"     = "application/json"
      )
      curl::handle_setopt(h,
        connecttimeout = 10L,
        timeout        = as.integer(timeout),
        accept_encoding = "gzip"
      )
      curl::curl_fetch_multi(
        urls[[idx]], handle = h, pool = pool,
        done = function(res) {
          if (res$status_code >= 200 && res$status_code < 300) {
            results[[idx]] <<- tryCatch(
              jsonlite::fromJSON(rawToChar(res$content),
                                 simplifyVector = FALSE),
              error = function(e) NULL
            )
          }
        },
        fail = function(err) NULL
      )
    })
  }
  curl::multi_run(pool = pool, timeout = timeout)
  results
}

#' Build an /observations URL with the fields filter and optional date window.
build_obs_url <- function(username, page, per_page = 200L,
                          d1 = NULL, d2 = NULL,
                          quality_grade = "research",
                          id_above = NULL) {
  qs <- list(
    user_login = username,
    per_page   = per_page,
    order      = "asc",
    order_by   = "id",
    fields     = OBS_FIELDS
  )
  # Prefer id_above cursor when supplied (works past the page=1..50 cap);
  # otherwise fall back to page-based pagination.
  if (!is.null(id_above)) qs$id_above <- id_above
  else                    qs$page     <- page
  if (!is.null(quality_grade) && quality_grade != "any")
    qs$quality_grade <- quality_grade
  if (!is.null(d1)) qs$d1 <- d1
  if (!is.null(d2)) qs$d2 <- d2
  modify_url(paste0(BASE_URL, "/observations"), query = qs)
}

#' Build a /taxa URL for a batch of taxon IDs.
build_taxa_url <- function(ids, per_page = TAXA_BATCH_SIZE) {
  modify_url(paste0(BASE_URL, "/taxa"), query = list(
    id       = paste(ids, collapse = ","),
    per_page = per_page,
    fields   = TAXA_FIELDS
  ))
}

#' Parse JSON response body
parse_resp <- function(resp) {
  if (is.null(resp)) return(NULL)
  tryCatch(
    fromJSON(rawToChar(resp$content), simplifyVector = FALSE),
    error = function(e) NULL
  )
}

#' Verify that a user login exists on iNaturalist
check_user_exists <- function(username) {
  resp <- safe_get(
    paste0(BASE_URL, "/users/autocomplete"),
    list(q = username)
  )
  if (is.null(resp)) return(FALSE)
  parsed <- parse_resp(resp)
  if (is.null(parsed) || length(parsed$results) == 0) return(FALSE)
  any(sapply(parsed$results, function(u) {
    tolower(chr_val(u$login)) == tolower(username)
  }))
}

# (Legacy synchronous fetchers removed — the state machine in server() now
# uses inat_multi_get_json + build_obs_url / build_taxa_url directly.)

#' Slim a raw observation object down to only the fields we use. Called as
#' each page lands so we never hold the fat JSON blobs (comments, IDs, sounds,
#' geojson, faves, etc.) — keeps peak memory under shinyapps.io's 1 GB cap.
slim_obs <- function(o) {
  taxon <- o$taxon
  if (is.null(taxon) || is.null(taxon$id)) return(NULL)
  list(
    id          = o$id,
    observed_on = o$observed_on,
    place_guess = o$place_guess,
    photo_url   = if (!is.null(o$photos) && length(o$photos) > 0)
                    o$photos[[1]]$url else NULL,
    taxon = list(
      id                    = taxon$id,
      name                  = taxon$name,
      preferred_common_name = taxon$preferred_common_name,
      iconic_taxon_name     = taxon$iconic_taxon_name
    )
  )
}

#' Convert the raw API observation list to a tidy data frame.
#' Vectorized — builds each column in one vapply pass instead of constructing
#' N one-row data.frames and rbind-ing them. ~50x faster on a few thousand rows,
#' which keeps the main R thread from blocking long enough for shinyapps.io
#' to drop the WebSocket.
obs_list_to_df <- function(obs_list) {
  if (length(obs_list) == 0) return(data.frame())
  obs_list <- Filter(Negate(is.null), obs_list)
  if (length(obs_list) == 0) return(data.frame())

  pull_chr <- function(f) vapply(obs_list, f, character(1))

  data.frame(
    obs_id      = pull_chr(function(o) chr_val(o$id)),
    taxon_id    = vapply(obs_list, function(o) as.integer(o$taxon$id), integer(1)),
    common_name = pull_chr(function(o) chr_val(o$taxon$preferred_common_name)),
    sci_name    = pull_chr(function(o) chr_val(o$taxon$name)),
    iconic      = pull_chr(function(o) chr_val(o$taxon$iconic_taxon_name)),
    obs_date    = pull_chr(function(o) chr_val(o$observed_on)),
    place       = pull_chr(function(o) chr_val(o$place_guess)),
    thumb_url   = pull_chr(function(o) chr_val(o$photo_url)),
    inat_url    = pull_chr(function(o)
                    paste0("https://www.inaturalist.org/observations/", o$id)),
    stringsAsFactors = FALSE
  )
}

# ── CSS ────────────────────────────────────────────────────────────────────────
dark_css <- sprintf('
  body, .content-wrapper, .right-side {
    background-color: #111922 !important; color: #dde0e4;
  }
  .skin-black .main-sidebar { background-color: #0b1017 !important; }
  .skin-black .main-header .logo,
  .skin-black .main-header .navbar {
    background-color: #0b1017 !important;
    border-bottom: 2px solid %1$s !important;
    min-height: 50px !important;
    height: 50px !important;
    box-sizing: border-box !important;
  }
  .skin-black .main-header .logo { color: #dde0e4 !important; font-weight: 700; }
  /* Kill the boxed look on the hamburger so its borders do not fight
     the header underline. */
  .skin-black .main-header .navbar .sidebar-toggle,
  .skin-black .main-header .navbar .sidebar-toggle:hover,
  .skin-black .main-header .navbar .sidebar-toggle:focus {
    background: transparent !important;
    border: none !important;
    color: %1$s !important;
  }
  .skin-black .sidebar a { color: #b0c8a0 !important; }

  /* Boxes */
  .box {
    background-color: #172030 !important;
    border-radius: 8px !important;
    border: 1px solid #233040 !important;
    box-shadow: 0 3px 12px rgba(0,0,0,.45) !important;
    margin-bottom: 18px;
  }
  .box-header {
    color: #c8d8b0 !important;
    border-bottom: 1px solid #233040 !important;
    padding: 12px 16px !important;
  }
  .box-title { font-size: 15px !important; font-weight: 600 !important; }

  /* Inputs */
  .form-control {
    background: #0b1017 !important; border: 1px solid #2a3a4a !important;
    color: #dde0e4 !important; border-radius: 5px !important;
  }
  .form-control:focus {
    border-color: %1$s !important;
    box-shadow: 0 0 0 2px rgba(116,172,0,.3) !important;
  }
  .form-control::placeholder { color: #455060 !important; }

  /* Slider */
  .irs-bar, .irs-bar-edge { background: %1$s !important; border-color: %1$s !important; }
  .irs-from, .irs-to, .irs-single { background: %1$s !important; }
  .irs-line { background: #233040 !important; }
  .irs-handle > i:first-child { background: #dde0e4 !important; }

  input[type="checkbox"] { accent-color: %1$s; }
  a, a:visited { color: %1$s !important; }
  a:hover     { color: #a0e040 !important; }

  /* DataTables */
  .dataTables_wrapper { color: #8090a0 !important; }
  table.dataTable thead th {
    background: #0b1017 !important; color: %1$s !important;
    border-bottom: 2px solid #233040 !important;
  }
  table.dataTable tbody tr  { background: #172030 !important; }
  table.dataTable tbody tr:hover { background: #1e2d40 !important; }
  table.dataTable tbody td {
    border-top: 1px solid #1e2a38 !important;
    vertical-align: middle !important;
  }
  .dataTables_info, .dataTables_length label,
  .dataTables_filter label { color: #6080a0 !important; }
  .paginate_button.current, .paginate_button.current:hover {
    background: %1$s !important; color: #fff !important;
    border-color: %1$s !important;
  }
  .paginate_button:hover { background: #1e2d40 !important; color: #dde0e4 !important; }

  /* Messages */
  .msg-success {
    background: #0d1e0d; border-left: 4px solid %1$s; color: #90c070;
    padding: 11px 16px; border-radius: 6px; margin: 8px 0; font-size: 14px;
  }
  .msg-error {
    background: #1e0d0d; border-left: 4px solid #d05050; color: #d08080;
    padding: 11px 16px; border-radius: 6px; margin: 8px 0; font-size: 14px;
  }
  .msg-info {
    background: #0d1520; border-left: 4px solid #4090c0; color: #70a0c0;
    padding: 11px 16px; border-radius: 6px; margin: 8px 0; font-size: 14px;
  }

  /* Modal */
  .modal-content {
    background-color: #172030 !important;
    border: 1px solid #233040 !important;
    color: #dde0e4 !important;
  }
  .modal-header, .modal-footer {
    border-color: #233040 !important;
  }
  .modal-title { color: %1$s !important; }
  .modal-body { color: #dde0e4 !important; }
  .modal-backdrop.in { opacity: 0.7 !important; }

  /* Welcome */
  .welcome-box {
    text-align: center; padding: 60px 30px; color: #6a8090;
  }
  .welcome-box h2 { color: %1$s; font-size: 30px; margin-bottom: 14px; }
  .welcome-box p  { font-size: 15px; max-width: 560px; margin: 0 auto 10px; line-height: 1.7; }

  /* Sidebar stats */
  .stat-row {
    display: flex; justify-content: space-between; align-items: center;
    padding: 5px 0; border-bottom: 1px solid #1e2e3e; font-size: 13px;
  }
  .stat-lbl { color: #6a8090; }
  .stat-val { color: #a0c878; font-weight: bold; }
', COL_GREEN)

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "black",

  dashboardHeader(
    title     = "🌿 iNat Rarity Explorer",
    titleWidth = 275
  ),

  dashboardSidebar(
    width = 275,

    div(style = "padding: 18px 16px;",

      tags$label(
        style = "color:#8fb070; font-size:13px; font-weight:600;",
        "iNaturalist Username"
      ),
      div(style = "margin-top:5px; margin-bottom:14px;",
        textInput("username", NULL,
                  placeholder = "e.g. hommedeterre", width = "100%")
      ),

      div(
        style = "margin-bottom:12px;",
        tags$label(style = "color:#8fb070; font-size:12px; font-weight:600;",
                   "Quality grade"),
        radioButtons(
          "quality_grade", NULL,
          choices  = c("Research Grade only" = "research",
                       "All quality grades"  = "any"),
          selected = "research",
          inline   = FALSE
        )
      ),

      actionButton(
        "fetch_btn",
        label = tags$span(
          style = paste0("display:inline-flex; align-items:center;",
                         " justify-content:center; gap:8px; width:100%;"),
          icon("seedling"), "Find My Rarest"
        ),
        width = "100%",
        style = paste0(
          "background:", COL_GREEN, "; border-color:#5a8800; color:#fff;",
          "font-weight:700; font-size:15px; padding:10px 0;",
          "border-radius:6px; letter-spacing:.3px; width:100%;",
          "text-align:center;"
        )
      ),

      tags$hr(style = "border-color:#233040; margin:18px 0;"),

      tags$label(style = "color:#8fb070; font-size:12px;",
                 "Show top N in chart:"),
      sliderInput("top_n", NULL,
                  min = 5, max = 50, value = 20, step = 5, width = "100%"),

      div(style = "margin-top:4px;",
        checkboxInput("log_scale",    "Log scale on chart",              value = TRUE),
        checkboxInput("hide_unnamed", "Only taxa with common names",     value = FALSE)
      ),

      tags$hr(style = "border-color:#233040; margin:15px 0;"),

      uiOutput("sidebar_stats")
    )
  ),

  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML(dark_css))),

    fluidRow(column(12, uiOutput("status_panel"))),

    conditionalPanel(
      condition = "output.has_data",

      fluidRow(
        box(
          title  = tags$span(icon("chart-bar"), "  Rarity Chart — Top N Rarest of Your Taxa"),
          width  = 12,
          withSpinner(
            plotlyOutput("rarity_plot", height = "530px"),
            color = COL_GREEN, type = 6
          )
        )
      ),

      fluidRow(
        box(
          title = tags$span(icon("table"), "  Full Rarity Table — All Unique Taxa"),
          width = 12,
          div(
            style = "margin-bottom:10px; font-size:13px; color:#6a8090;",
            "Click any thumbnail to view the observation on iNaturalist. ",
            "Table includes all ranked taxa; chart shows the top-N slider selection."
          ),
          withSpinner(
            DTOutput("rarity_table"),
            color = COL_GREEN, type = 6
          )
        )
      )
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(
    rarity_df     = NULL,    # final ranked data frame — cached for session
    username      = NULL,
    quality_grade = "research",  # grade used for the current results
    n_total_obs   = 0,
    n_taxa        = 0,
    status        = "idle",  # idle | fetching | done | error
    status_msg    = "",
    error_msg     = ""
  )

  # ── Fetch state machine ───────────────────────────────────────────────────
  #
  # Why a state machine: shinyapps.io's WebSocket disconnects if the R thread
  # is blocked for too long. Multi-process async (future) blows past the
  # 1 GB free-tier memory limit. Solution: keep everything in one R process,
  # but break the long fetch into tiny steps scheduled with later::later() —
  # control returns to Shiny's event loop between steps, so heartbeats flow
  # and memory stays modest.
  #
  # fs is a plain environment (not reactiveValues) — its fields are mutated
  # from later() callbacks where reactive contexts aren't available. Only
  # rv$* writes drive UI updates.
  fs <- new.env()
  fs$generation <- 0L  # incremented each fetch; stale callbacks check & exit

  # Schedule the next step on the event loop. later() callbacks run outside
  # any reactive context, so wrap in withReactiveDomain(session) — otherwise
  # shinyjs::enable/disable can't find the session and the app dies.
  schedule <- function(fn, delay = 0.05, gen = fs$generation) {
    later::later(function() {
      if (gen != fs$generation) return()  # superseded — abort
      shiny::withReactiveDomain(session, {
        tryCatch(fn(), error = function(e) finish_error(conditionMessage(e)))
      })
    }, delay)
  }

  finish_error <- function(msg) {
    rv$status    <- "error"
    rv$error_msg <- msg
    fs$pages     <- NULL
    fs$obs_df    <- NULL
    fs$taxa_df   <- NULL
    fs$counts    <- NULL
    fs$batches   <- NULL
    enable("fetch_btn"); enable("username")
  }

  # ── Stage handlers ────────────────────────────────────────────────────────
  step_verify <- function() {
    ok <- tryCatch(check_user_exists(fs$username), error = function(e) FALSE)
    if (!ok) {
      finish_error(sprintf(
        "User '%s' was not found on iNaturalist. Check the spelling and try again.",
        fs$username))
      return()
    }
    rv$status_msg <- "Counting observations..."
    schedule(step_count)
  }

  step_count <- function() {
    q <- list(user_login = fs$username, per_page = 0)
    if (fs$qg != "any") q$quality_grade <- fs$qg
    resp <- safe_get(paste0(BASE_URL, "/observations"), q)
    if (is.null(resp)) {
      finish_error("Could not connect to the iNaturalist API.")
      return()
    }
    total <- as.integer(parse_resp(resp)$total_results %||% 0)
    if (total == 0) {
      finish_error(sprintf(
        "No %sobservations found for '%s'.",
        if (fs$qg == "research") "research-grade " else "", fs$username))
      return()
    }
    fs$total_obs   <- total
    fs$per_page    <- 200L
    fs$pages       <- list()
    fs$n_so_far    <- 0L
    fs$d1          <- NULL
    fs$d2          <- NULL

    # iNat caps page-based pagination at page*per_page <= 10 000. For ≤10k
    # accounts we use page-based pagination AND fire pages in parallel — fast.
    # For >10k accounts we cursor with id_above (sequential, but unbounded).
    if (total <= 10000) {
      fs$cur_page  <- 1L
      fs$n_pages   <- ceiling(total / fs$per_page)
      fs$use_cursor <- FALSE
      rv$status_msg <- sprintf("Fetching %s observations...",
                               format(total, big.mark = ","))
    } else {
      fs$id_above   <- 0L
      fs$use_cursor <- TRUE
      rv$status_msg <- sprintf(
        "Fetching %s observations (cursor mode for large account)...",
        format(total, big.mark = ","))
    }
    schedule(step_fetch_page)
  }

  # Handle a parallel batch of observation pages (cursor or page-based).
  # For page-based: fires CONCURRENCY_PAGES requests at once.
  # For cursor: must run sequentially since each id_above depends on the prior
  # response's max id.
  step_fetch_page <- function() {
    if (fs$use_cursor) {
      # Sequential id_above cursor: one page per tick. Still benefits from
      # `fields=` and keep-alive within inat_multi_get_json's pool.
      url <- build_obs_url(fs$username, page = NULL, per_page = fs$per_page,
                           quality_grade = fs$qg,
                           id_above = fs$id_above)
      resp <- inat_multi_get_json(url, max_concurrent = 1L)[[1]]
      results <- if (!is.null(resp)) resp$results else list()
      if (length(results) == 0L) {
        schedule(step_parse)
        return()
      }
      slimmed <- Filter(Negate(is.null), lapply(results, slim_obs))
      if (length(slimmed) > 0L) {
        fs$pages[[length(fs$pages) + 1L]] <- slimmed
        fs$n_so_far <- fs$n_so_far + length(slimmed)
      }
      # Next cursor = highest id seen this page (iNat returns ascending).
      ids_this <- vapply(results, function(o) as.integer(o$id %||% 0L),
                         integer(1))
      fs$id_above <- max(ids_this, na.rm = TRUE)

      rv$status_msg <- sprintf("Fetched %s / %s observations...",
                               format(fs$n_so_far, big.mark = ","),
                               format(fs$total_obs, big.mark = ","))

      # Done when iNat returns fewer than per_page (last page).
      if (length(results) < fs$per_page) schedule(step_parse)
      else                                schedule(step_fetch_page, delay = 0.05)
      return()
    }

    # Page-based parallel branch: fire CONCURRENCY_PAGES pages at once.
    if (fs$cur_page > fs$n_pages) {
      schedule(step_parse)
      return()
    }
    this_batch <- seq.int(
      fs$cur_page,
      min(fs$cur_page + CONCURRENCY_PAGES - 1L, fs$n_pages)
    )
    urls <- vapply(this_batch, function(p) {
      build_obs_url(fs$username, page = p, per_page = fs$per_page,
                    d1 = fs$d1, d2 = fs$d2, quality_grade = fs$qg)
    }, character(1))

    responses <- inat_multi_get_json(urls, max_concurrent = CONCURRENCY_PAGES)
    for (resp in responses) {
      if (is.null(resp) || length(resp$results) == 0L) next
      slimmed <- Filter(Negate(is.null), lapply(resp$results, slim_obs))
      if (length(slimmed) > 0L) {
        fs$pages[[length(fs$pages) + 1L]] <- slimmed
        fs$n_so_far <- fs$n_so_far + length(slimmed)
      }
    }

    rv$status_msg <- sprintf("Fetched %s / %s observations...",
                             format(fs$n_so_far, big.mark = ","),
                             format(fs$total_obs, big.mark = ","))

    fs$cur_page <- fs$cur_page + length(this_batch)
    if (fs$cur_page > fs$n_pages) schedule(step_parse)
    else                          schedule(step_fetch_page, delay = 0.05)
  }

  step_parse <- function() {
    rv$status_msg <- "Parsing observations..."
    obs_list <- if (length(fs$pages) > 0) do.call(c, fs$pages) else list()
    fs$pages <- NULL  # free memory
    if (length(obs_list) == 0) {
      finish_error(sprintf("No observations found for '%s'.", fs$username))
      return()
    }
    fs$obs_df <- obs_list_to_df(obs_list)
    rm(obs_list)
    if (nrow(fs$obs_df) == 0) {
      finish_error("Could not parse any observations from the API response.")
      return()
    }
    fs$n_total_obs <- nrow(fs$obs_df)
    rv$n_total_obs <- fs$n_total_obs
    schedule(step_summarise)
  }

  step_summarise <- function() {
    rv$status_msg <- "Summarising unique taxa..."
    oc <- fs$obs_df %>% count(taxon_id, name = "n_user_obs")
    fs$taxa_df <- fs$obs_df %>%
      group_by(taxon_id) %>% slice(1) %>% ungroup() %>%
      left_join(oc, by = "taxon_id")
    fs$obs_df <- NULL  # free memory
    fs$n_taxa <- nrow(fs$taxa_df)
    rv$n_taxa <- fs$n_taxa
    ids <- as.character(unique(fs$taxa_df$taxon_id))
    fs$counts <- setNames(rep(NA_integer_, length(ids)), ids)

    # Fill from the process-wide cache; only the unseen IDs need an API call.
    cached_ids <- intersect(ids, ls(.global_count_cache))
    for (id in cached_ids) {
      fs$counts[id] <- get(id, envir = .global_count_cache)
    }
    uncached <- setdiff(ids, cached_ids)

    if (length(uncached) == 0L) {
      schedule(step_rank)
      return()
    }
    fs$batches   <- split(uncached,
                          ceiling(seq_along(uncached) / TAXA_BATCH_SIZE))
    fs$n_batches <- length(fs$batches)
    fs$cur_batch <- 1L
    rv$status_msg <- sprintf(
      "Looking up global counts for %s taxa (%d cached, %d to fetch in %d batches)...",
      format(fs$n_taxa, big.mark = ","),
      length(cached_ids), length(uncached), fs$n_batches)
    schedule(step_lookup)
  }

  # Fire CONCURRENCY_TAXA /taxa batches in parallel each tick. Each batch
  # carries up to TAXA_BATCH_SIZE (200) IDs — vs. the old 30 per call.
  # Combined: ~6× fewer requests AND each round serves ~3 in parallel.
  step_lookup <- function() {
    if (fs$cur_batch > fs$n_batches) {
      schedule(step_rank)
      return()
    }
    end_batch <- min(fs$cur_batch + CONCURRENCY_TAXA - 1L, fs$n_batches)
    these     <- seq.int(fs$cur_batch, end_batch)
    urls      <- vapply(these, function(i) build_taxa_url(fs$batches[[i]]),
                        character(1))

    responses <- inat_multi_get_json(urls, max_concurrent = CONCURRENCY_TAXA)
    for (resp in responses) {
      if (is.null(resp) || length(resp$results) == 0L) next
      for (taxon in resp$results) {
        tid <- as.character(taxon$id %||% NA)
        if (is.na(tid) || !nzchar(tid)) next
        cnt <- as.integer(taxon$observations_count %||% NA_integer_)
        if (tid %in% names(fs$counts)) fs$counts[tid] <- cnt
        if (!is.na(cnt)) assign(tid, cnt, envir = .global_count_cache)
      }
    }

    rv$status_msg <- sprintf("Looking up global counts — batch %d / %d",
                             end_batch, fs$n_batches)
    fs$cur_batch <- end_batch + 1L
    schedule(step_lookup, delay = 0.05)
  }

  step_rank <- function() {
    rv$status_msg <- "Ranking by rarity..."
    fs$taxa_df$global_count <- fs$counts[as.character(fs$taxa_df$taxon_id)]
    rarity_df <- fs$taxa_df %>%
      filter(!is.na(global_count)) %>%
      arrange(global_count) %>%
      mutate(rank = row_number())

    rv$rarity_df  <- rarity_df
    rv$username   <- fs$username
    rv$status     <- "done"
    r1 <- rarity_df
    rv$status_msg <- sprintf(
      "✅ Done! %s observations · %s unique taxa ranked · rarest: %s (%s global obs)",
      format(fs$n_total_obs, big.mark = ","),
      format(nrow(r1), big.mark = ","),
      coalesce(r1$common_name[1], r1$sci_name[1], "Unknown"),
      format(r1$global_count[1], big.mark = ","))

    fs$taxa_df <- NULL; fs$counts <- NULL; fs$batches <- NULL
    enable("fetch_btn"); enable("username")
  }

  # ── Kick off ─────────────────────────────────────────────────────────────
  observeEvent(input$fetch_btn, {

    username <- trimws(input$username)
    if (!nzchar(username)) {
      showNotification("Please enter an iNaturalist username.", type = "warning")
      return()
    }

    qg <- input$quality_grade

    # Reset cached data
    rv$rarity_df     <- NULL
    rv$quality_grade <- qg
    rv$status        <- "fetching"
    rv$status_msg    <- paste0("Connecting to iNaturalist for: ", username, " ...")

    fs$generation <- fs$generation + 1L
    fs$username   <- username
    fs$qg         <- qg
    fs$pages      <- NULL
    fs$obs_df     <- NULL
    fs$taxa_df    <- NULL
    fs$counts     <- NULL
    fs$batches    <- NULL

    disable("fetch_btn"); disable("username")

    schedule(step_verify)
  })


  # ── Reactive: filtered chart data ──────────────────────────────────────────
  chart_data <- reactive({
    req(rv$rarity_df)
    df <- rv$rarity_df
    if (input$hide_unnamed) df <- df %>% filter(!is.na(common_name) & nzchar(common_name))
    head(df, input$top_n)
  })

  # ── Signal for conditionalPanel ────────────────────────────────────────────
  output$has_data <- reactive({
    !is.null(rv$rarity_df) && nrow(rv$rarity_df) > 0
  })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)

  # ── Status panel ───────────────────────────────────────────────────────────
  output$status_panel <- renderUI({
    switch(rv$status,
      idle = div(
        class = "welcome-box",
        tags$h2("🌿 iNat Rarity Explorer"),
        tags$p(
          "Find out which of your iNaturalist observations are the rarest on Earth, ",
          "ranked by how few people have ever recorded that species globally."
        ),
        tags$p(tags$small(
          style = "color:#455060;",
          "Enter your iNaturalist username in the sidebar and click ",
          tags$b("Find My Rarest"), " to begin.",
          tags$br(),
          "Only research-grade observations are included. ",
          "Large accounts (1000+ taxa) may take a couple of minutes."
        ))
      ),
      fetching = div(
        class = "msg-info",
        icon("spinner", class = "fa-spin"), " ", rv$status_msg
      ),
      done  = div(class = "msg-success", rv$status_msg),
      error = div(
        class = "msg-error",
        icon("circle-xmark"), " ", rv$error_msg
      )
    )
  })

  # ── Sidebar stats ──────────────────────────────────────────────────────────
  output$sidebar_stats <- renderUI({
    req(rv$status == "done", !is.null(rv$rarity_df))
    df <- rv$rarity_df
    r1 <- df[1, ]

    tagList(
      tags$h5(style = "color:#74ac00; margin-bottom:8px;", "📊 Session Summary"),
      div(class = "stat-row",
          span(class = "stat-lbl", "User"),
          span(class = "stat-val", rv$username)),
      div(class = "stat-row",
          span(class = "stat-lbl", "Total obs"),
          span(class = "stat-val", format(rv$n_total_obs, big.mark = ","))),
      div(class = "stat-row",
          span(class = "stat-lbl", "Unique taxa"),
          span(class = "stat-val", format(rv$n_taxa, big.mark = ","))),
      div(class = "stat-row",
          span(class = "stat-lbl", "Ranked"),
          span(class = "stat-val", format(nrow(df), big.mark = ","))),
      br(),
      tags$h6(style = "color:#74ac00;", "🏆 Rarest Find:"),
      div(
        style = "font-size:12px; color:#a0c878; line-height:1.6; padding:4px 2px;",
        tags$em(coalesce(r1$common_name, r1$sci_name, "Unknown")),
        tags$br(),
        tags$small(
          style = "color:#506070;",
          format(r1$global_count, big.mark = ","), " global observations"
        )
      )
    )
  })

  # ── Plotly lollipop chart ──────────────────────────────────────────────────
  output$rarity_plot <- renderPlotly({
    req(chart_data())

    df <- chart_data() %>%
      mutate(
        label = case_when(
          !is.na(common_name) & nzchar(coalesce(common_name, "")) ~
            paste0(tools::toTitleCase(tolower(coalesce(common_name, ""))),
                   "  (", sci_name, ")"),
          TRUE ~ coalesce(sci_name, paste0("taxon:", taxon_id))
        ),
        label     = factor(label, levels = rev(label)),
        x_start   = 1,   # lollipop segment base (safe for log scale too)
        taxon_url   = paste0("https://www.inaturalist.org/taxa/", taxon_id),
        your_obs_url = paste0(
          "https://www.inaturalist.org/observations?taxon_id=", taxon_id,
          "&user_id=", rv$username,
          if (rv$quality_grade != "any") paste0("&quality_grade=", rv$quality_grade) else ""
        ),
        hover_txt = paste0(
          "<b>", coalesce(common_name, sci_name, "Unknown"), "</b><br>",
          "<i>", coalesce(sci_name, ""), "</i><br>",
          "🌍 Global obs: <b>", format(global_count, big.mark = ","), "</b><br>",
          "📅 Your date: ", coalesce(obs_date, "unknown"), "<br>",
          "📍 Place: ", coalesce(place, "unknown"), "<br>",
          "🔢 Your obs (this taxon): ", n_user_obs, "<br>",
          "<i style='color:#8fb070;'>Click dot for links →</i>"
        )
      )

    use_log <- isTRUE(input$log_scale)

    plot_ly(source = "rarity_plot") %>%
      event_register("plotly_click") %>%
      # ── Lollipop sticks ──
      add_segments(
        data      = df,
        y         = ~label, yend = ~label,
        x         = ~x_start, xend = ~global_count,
        line      = list(color = "#2a4535", width = 1.5),
        hoverinfo = "skip",
        showlegend = FALSE
      ) %>%
      # ── Lollipop heads ──
      add_markers(
        data       = df,
        y          = ~label,
        x          = ~global_count,
        customdata = ~paste(taxon_id, taxon_url, your_obs_url,
                            coalesce(common_name, sci_name, "Unknown"),
                            sep = "|"),
        hoverinfo  = "text",
        text       = ~hover_txt,
        marker     = list(
          size       = 14,
          color      = ~log10(pmax(global_count, 1)),
          colorscale = list(
            list(0,   "#74ac00"),
            list(0.4, "#2a8a50"),
            list(1,   "#106080")
          ),
          showscale  = TRUE,
          colorbar   = list(
            title       = list(text  = "log₁₀(obs)",
                               font  = list(color = "#b0c0a0", size = 12)),
            tickfont    = list(color = "#7090a0"),
            bgcolor     = "#172030",
            bordercolor = "#233040",
            len         = 0.55,
            x           = 1.01
          ),
          line = list(color = "#a0e050", width = 1)
        ),
        showlegend = FALSE
      ) %>%
      layout(
        paper_bgcolor = "#172030",
        plot_bgcolor  = "#172030",
        font          = list(color = "#dde0e4",
                             family = "'Segoe UI', Arial, sans-serif"),
        xaxis = list(
          title    = if (use_log) "Global Observations (log₁₀ scale)"
                     else         "Global Observations",
          type     = if (use_log) "log" else "linear",
          gridcolor = "#1e2d3d",
          zeroline  = FALSE,
          tickfont  = list(color = "#7090a0")
        ),
        yaxis = list(
          title     = "",
          gridcolor = "#1e2d3d",
          tickfont  = list(color = "#b0c090", size = 11)
        ),
        margin  = list(l = 310, r = 90, t = 20, b = 60),
        hoverlabel = list(
          bgcolor     = "#0b1017",
          bordercolor = COL_GREEN,
          font        = list(color = "#dde0e4", size = 13)
        )
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d")
      )
  })

  # ── Click-a-dot: open modal with links (hover tooltips disappear too fast) ─
  observeEvent(event_data("plotly_click", source = "rarity_plot"), {
    ev <- event_data("plotly_click", source = "rarity_plot")
    if (is.null(ev) || is.null(ev$customdata)) return()
    parts <- strsplit(as.character(ev$customdata), "|", fixed = TRUE)[[1]]
    if (length(parts) < 4) return()

    showModal(modalDialog(
      title = tags$span(style = paste0("color:", COL_GREEN, ";"), parts[4]),
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$p(style = "color:#dde0e4!important; margin-bottom:14px;",
             "Open on iNaturalist:"),
      tags$div(
        style = "display:flex; flex-direction:column; gap:10px;",
        tags$a(
          href = parts[2], target = "_blank",
          style = paste0("background:", COL_GREEN, "; color:#fff!important; padding:10px 14px;",
                         "border-radius:6px; text-decoration:none; font-weight:600;"),
          "🔗 Taxon page"
        ),
        tags$a(
          href = parts[3], target = "_blank",
          style = paste0("background:", COL_GREEN, "; color:#fff!important; padding:10px 14px;",
                         "border-radius:6px; text-decoration:none; font-weight:600;"),
          "📷 Your observations of this taxon"
        )
      )
    ))
  })

  # ── DT rarity table ────────────────────────────────────────────────────────
  output$rarity_table <- renderDT({
    req(rv$rarity_df)

    df <- rv$rarity_df
    if (input$hide_unnamed)
      df <- df %>% filter(!is.na(common_name) & nzchar(coalesce(common_name, "")))

    tbl <- df %>%
      transmute(
        `#`    = rank,
        Photo  = sprintf(
          '<a href="%s" target="_blank">%s</a>',
          inat_url,
          ifelse(
            !is.na(thumb_url) & nzchar(coalesce(thumb_url, "")),
            sprintf(
              paste0(
                '<img src="%s" height="55" style="border-radius:5px;',
                ' border:1.5px solid #74ac00;"',
                ' onerror="this.outerHTML=\'<span style=font-size:22px>📷</span>\'">'
              ),
              thumb_url
            ),
            "<span style='font-size:22px;'>🔗</span>"
          )
        ),
        `Common Name`     = coalesce(common_name, "—"),
        `Scientific Name` = sprintf(
          '<a href="https://www.inaturalist.org/taxa/%s" target="_blank" title="View taxon page"><i style="color:#90b090;">%s</i></a>',
          taxon_id, coalesce(sci_name, "Unknown")
        ),
        Date              = coalesce(obs_date, "—"),
        Place             = coalesce(place, "—"),
        `Global Obs`      = format(global_count, big.mark = ","),
        `Your Obs`        = sprintf(
          '<a href="https://www.inaturalist.org/observations?taxon_id=%s&user_id=%s%s" target="_blank" title="View your observations of this taxon" style="color:#74ac00; font-weight:bold;">%s</a>',
          taxon_id, rv$username,
          if (rv$quality_grade != "any") paste0("&quality_grade=", rv$quality_grade) else "",
          n_user_obs
        )
      )

    datatable(
      tbl,
      escape    = FALSE,
      rownames  = FALSE,
      selection = "none",
      options   = list(
        pageLength = 25,
        dom        = "lftip",
        scrollX    = TRUE,
        columnDefs = list(
          list(orderable     = FALSE,      targets = 1),
          list(className     = "dt-center", targets = c(0, 1, 4, 5, 6, 7)),
          list(width         = "45px",     targets = 0),
          list(width         = "70px",     targets = 1),
          list(width         = "130px",    targets = 4)
        )
      ),
      class = "display compact"
    ) %>%
      formatStyle(
        "Global Obs",
        color      = COL_GREEN,
        fontWeight = "bold",
        fontSize   = "15px"
      ) %>%
      formatStyle(
        "#",
        color    = "#4a6070",
        fontSize = "12px"
      )
  })
}

# ── Launch ─────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
