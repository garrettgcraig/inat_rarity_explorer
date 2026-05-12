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

#' Fetch a single page of observations.
#' quality_grade: "research", "needs_id", "casual", or NULL for all grades.
fetch_obs_page <- function(username, page = 1, per_page = 200,
                           d1 = NULL, d2 = NULL, quality_grade = "research") {
  q <- list(
    user_login = username,
    per_page   = per_page,
    page       = page,
    order      = "asc",
    order_by   = "observed_on"
  )
  if (!is.null(quality_grade) && quality_grade != "any") q$quality_grade <- quality_grade
  if (!is.null(d1)) q$d1 <- d1
  if (!is.null(d2)) q$d2 <- d2
  parse_resp(safe_get(paste0(BASE_URL, "/observations"), q))
}

#' Fetch all observations within a date range (handles the 10 k API cap)
fetch_range <- function(username, d1 = NULL, d2 = NULL,
                        quality_grade = "research", progress_cb = NULL) {
  first <- fetch_obs_page(username, page = 1, d1 = d1, d2 = d2,
                          quality_grade = quality_grade)
  if (is.null(first)) return(list(ok = FALSE, data = NULL,
                                  err = "API request failed"))

  total   <- as.integer(first$total_results %||% 0)
  if (total == 0) return(list(ok = TRUE, data = list(), actual_total = 0))

  n_fetch  <- min(total, 10000)
  per_page <- 200
  n_pages  <- ceiling(n_fetch / per_page)
  all_obs  <- first$results

  if (n_pages >= 2) {
    for (pg in 2:n_pages) {
      pg_data <- fetch_obs_page(username, page = pg, d1 = d1, d2 = d2,
                                quality_grade = quality_grade)
      if (!is.null(pg_data) && length(pg_data$results) > 0)
        all_obs <- c(all_obs, pg_data$results)
      if (!is.null(progress_cb)) progress_cb(length(all_obs))
      Sys.sleep(REQUEST_DELAY)
    }
  }

  list(ok = TRUE, data = all_obs, actual_total = total)
}

#' Fetch ALL observations, using year-by-year chunking when total > 10 k
fetch_all_observations <- function(username, total_obs,
                                   quality_grade = "research",
                                   progress_cb = NULL) {
  if (total_obs <= 10000) {
    return(fetch_range(username, quality_grade = quality_grade,
                       progress_cb = progress_cb))
  }

  all_obs    <- list()
  start_year <- 2007
  end_year   <- as.integer(format(Sys.Date(), "%Y"))

  for (yr in start_year:end_year) {
    d1 <- sprintf("%d-01-01", yr)
    d2 <- sprintf("%d-12-31", yr)

    chunk <- fetch_range(username, d1 = d1, d2 = d2,
                         quality_grade = quality_grade,
                         progress_cb = function(n) {
      if (!is.null(progress_cb)) progress_cb(length(all_obs) + n)
    })

    if (isTRUE(chunk$ok) && length(chunk$data) > 0)
      all_obs <- c(all_obs, chunk$data)

    if (!is.null(progress_cb)) progress_cb(length(all_obs))
    Sys.sleep(0.25)
  }

  list(ok = TRUE, data = all_obs)
}

#' Fetch global observation counts for many taxa in batched /taxa calls.
#' The /taxa endpoint returns observations_count on each taxon object,
#' letting us resolve ~30 taxa per request instead of 1 — a ~30x speedup.
get_global_counts_batch <- function(taxon_ids, batch_size = 30,
                                    progress_cb = NULL) {
  ids       <- as.character(unique(taxon_ids))
  counts    <- setNames(rep(NA_integer_, length(ids)), ids)
  batches   <- split(ids, ceiling(seq_along(ids) / batch_size))
  n_batches <- length(batches)

  for (i in seq_along(batches)) {
    ids_str <- paste(batches[[i]], collapse = ",")
    resp    <- safe_get(
      paste0(BASE_URL, "/taxa"),
      list(id = ids_str, per_page = batch_size)
    )
    parsed <- parse_resp(resp)

    if (!is.null(parsed) && length(parsed$results) > 0) {
      for (taxon in parsed$results) {
        tid <- as.character(taxon$id)
        if (tid %in% names(counts)) {
          counts[tid] <- as.integer(taxon$observations_count %||% NA_integer_)
        }
      }
    }

    if (!is.null(progress_cb)) progress_cb(i, n_batches)
    Sys.sleep(0.1)
  }

  counts  # named integer vector: names = taxon_id (character), values = count
}

#' Convert the raw API observation list to a tidy data frame.
#' Vectorized — builds each column in one vapply pass instead of constructing
#' N one-row data.frames and rbind-ing them. ~50x faster on a few thousand rows,
#' which keeps the main R thread from blocking long enough for shinyapps.io
#' to drop the WebSocket.
obs_list_to_df <- function(obs_list) {
  if (length(obs_list) == 0) return(data.frame())
  obs_list <- Filter(function(o) !is.null(o$taxon) && !is.null(o$taxon$id),
                     obs_list)
  if (length(obs_list) == 0) return(data.frame())

  pull_chr <- function(f) vapply(obs_list, f, character(1))
  thumb_of <- function(o) {
    if (!is.null(o$photos) && length(o$photos) > 0)
      chr_val(o$photos[[1]]$url) else NA_character_
  }

  data.frame(
    obs_id      = pull_chr(function(o) chr_val(o$id)),
    taxon_id    = vapply(obs_list, function(o) as.integer(o$taxon$id), integer(1)),
    common_name = pull_chr(function(o) chr_val(o$taxon$preferred_common_name)),
    sci_name    = pull_chr(function(o) chr_val(o$taxon$name)),
    iconic      = pull_chr(function(o) chr_val(o$taxon$iconic_taxon_name)),
    obs_date    = pull_chr(function(o) chr_val(o$observed_on)),
    place       = pull_chr(function(o) chr_val(o$place_guess)),
    thumb_url   = pull_chr(thumb_of),
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
  }
  .skin-black .main-header .logo { color: #dde0e4 !important; font-weight: 700; }
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
    title     = tags$div(
      style = "display:flex; align-items:center; gap:8px; font-weight:700; font-size:17px;",
      "🌿 iNat Rarity Explorer"
    ),
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
        label = tags$span(icon("seedling"), "  Find My Rarest"),
        width = "100%",
        style = paste0(
          "background:", COL_GREEN, "; border-color:#5a8800; color:#fff;",
          "font-weight:700; font-size:15px; padding:10px 0;",
          "border-radius:6px; letter-spacing:.3px; width:100%;"
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
    fs$obs_list  <- NULL
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
    fs$needs_chunk <- total > 10000
    fs$per_page    <- 200
    fs$obs_list    <- list()

    if (fs$needs_chunk) {
      fs$cur_year <- 2007L
      fs$end_year <- as.integer(format(Sys.Date(), "%Y"))
      rv$status_msg <- sprintf(
        "Fetching %s observations year by year (account is prolific)...",
        format(total, big.mark = ","))
      schedule(step_fetch_year_start)
    } else {
      fs$d1 <- NULL; fs$d2 <- NULL
      fs$cur_page <- 1L
      fs$n_pages  <- ceiling(min(total, 10000) / fs$per_page)
      rv$status_msg <- sprintf("Fetching %s observations...",
                               format(total, big.mark = ","))
      schedule(step_fetch_page)
    }
  }

  step_fetch_year_start <- function() {
    if (fs$cur_year > fs$end_year) {
      schedule(step_parse)
      return()
    }
    fs$d1 <- sprintf("%d-01-01", fs$cur_year)
    fs$d2 <- sprintf("%d-12-31", fs$cur_year)
    q <- list(user_login = fs$username, per_page = 0,
              d1 = fs$d1, d2 = fs$d2)
    if (fs$qg != "any") q$quality_grade <- fs$qg
    resp <- safe_get(paste0(BASE_URL, "/observations"), q)
    yr_total <- if (!is.null(resp))
      as.integer(parse_resp(resp)$total_results %||% 0) else 0L
    if (yr_total == 0) {
      fs$cur_year <- fs$cur_year + 1L
      schedule(step_fetch_year_start)
      return()
    }
    fs$cur_page <- 1L
    fs$n_pages  <- ceiling(min(yr_total, 10000) / fs$per_page)
    schedule(step_fetch_page)
  }

  step_fetch_page <- function() {
    page <- fetch_obs_page(fs$username, page = fs$cur_page,
                           per_page = fs$per_page,
                           d1 = fs$d1, d2 = fs$d2,
                           quality_grade = fs$qg)
    if (!is.null(page) && length(page$results) > 0)
      fs$obs_list <- c(fs$obs_list, page$results)

    rv$status_msg <- sprintf("Fetched %s / %s observations...",
                             format(length(fs$obs_list), big.mark = ","),
                             format(fs$total_obs, big.mark = ","))

    if (fs$cur_page < fs$n_pages) {
      fs$cur_page <- fs$cur_page + 1L
      schedule(step_fetch_page, delay = 0.08)
    } else if (fs$needs_chunk) {
      fs$cur_year <- fs$cur_year + 1L
      schedule(step_fetch_year_start, delay = 0.2)
    } else {
      schedule(step_parse)
    }
  }

  step_parse <- function() {
    rv$status_msg <- "Parsing observations..."
    if (length(fs$obs_list) == 0) {
      finish_error(sprintf("No observations found for '%s'.", fs$username))
      return()
    }
    fs$obs_df   <- obs_list_to_df(fs$obs_list)
    fs$obs_list <- NULL  # free memory
    if (nrow(fs$obs_df) == 0) {
      finish_error("Could not parse any observations from the API response.")
      return()
    }
    rv$n_total_obs <- nrow(fs$obs_df)
    schedule(step_summarise)
  }

  step_summarise <- function() {
    rv$status_msg <- "Summarising unique taxa..."
    oc <- fs$obs_df %>% count(taxon_id, name = "n_user_obs")
    fs$taxa_df <- fs$obs_df %>%
      group_by(taxon_id) %>% slice(1) %>% ungroup() %>%
      left_join(oc, by = "taxon_id")
    fs$obs_df <- NULL  # free memory
    rv$n_taxa <- nrow(fs$taxa_df)
    ids <- as.character(unique(fs$taxa_df$taxon_id))
    fs$batches   <- split(ids, ceiling(seq_along(ids) / 30))
    fs$n_batches <- length(fs$batches)
    fs$cur_batch <- 1L
    fs$counts    <- setNames(rep(NA_integer_, length(ids)), ids)
    rv$status_msg <- sprintf(
      "Looking up global counts for %s taxa in %d batches...",
      format(rv$n_taxa, big.mark = ","), fs$n_batches)
    schedule(step_lookup)
  }

  step_lookup <- function() {
    if (fs$cur_batch > fs$n_batches) {
      schedule(step_rank)
      return()
    }
    batch <- fs$batches[[fs$cur_batch]]
    resp <- safe_get(paste0(BASE_URL, "/taxa"),
                     list(id = paste(batch, collapse = ","), per_page = 30))
    parsed <- parse_resp(resp)
    if (!is.null(parsed) && length(parsed$results) > 0) {
      for (taxon in parsed$results) {
        tid <- as.character(taxon$id)
        if (tid %in% names(fs$counts))
          fs$counts[tid] <- as.integer(taxon$observations_count %||% NA_integer_)
      }
    }
    rv$status_msg <- sprintf("Looking up global counts — batch %d / %d",
                             fs$cur_batch, fs$n_batches)
    fs$cur_batch <- fs$cur_batch + 1L
    schedule(step_lookup, delay = 0.1)
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
      format(rv$n_total_obs, big.mark = ","),
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
    fs$obs_list   <- NULL
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
              '<img src="%s" height="55" style="border-radius:5px;',
              ' border:1.5px solid #74ac00;"',
              ' onerror="this.outerHTML=\'<span style=font-size:22px>📷</span>\'">',
              thumb_url
            ) |> paste(collapse = ""),
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
