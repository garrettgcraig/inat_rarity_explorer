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

#' Convert the raw API observation list to a tidy data frame
obs_list_to_df <- function(obs_list) {
  if (length(obs_list) == 0) return(data.frame())

  rows <- lapply(obs_list, function(obs) {
    taxon <- obs$taxon
    if (is.null(taxon) || is.null(taxon$id)) return(NULL)

    thumb <- NA_character_
    if (!is.null(obs$photos) && length(obs$photos) > 0)
      thumb <- chr_val(obs$photos[[1]]$url)

    data.frame(
      obs_id      = chr_val(obs$id),
      taxon_id    = as.integer(taxon$id),
      common_name = chr_val(taxon$preferred_common_name),
      sci_name    = chr_val(taxon$name),
      iconic      = chr_val(taxon$iconic_taxon_name),
      obs_date    = chr_val(obs$observed_on),
      place       = chr_val(obs$place_guess),
      thumb_url   = thumb,
      inat_url    = paste0("https://www.inaturalist.org/observations/", obs$id),
      stringsAsFactors = FALSE
    )
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
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

  # ── Fetch ──────────────────────────────────────────────────────────────────
  observeEvent(input$fetch_btn, {

    username <- trimws(input$username)
    if (!nzchar(username)) {
      showNotification("Please enter an iNaturalist username.", type = "warning")
      return()
    }

    qg <- input$quality_grade  # snapshot at click time

    # Reset cached data
    rv$rarity_df     <- NULL
    rv$quality_grade <- qg
    rv$status        <- "fetching"
    rv$status_msg    <- paste0("Connecting to iNaturalist for: ", username, " ...")

    disable("fetch_btn")
    disable("username")

    withProgress(message = "Working...", value = 0, {

      # ── 1. Verify user ──────────────────────────────────────────────────────
      setProgress(0.02, detail = "Checking username...")

      user_ok <- tryCatch(check_user_exists(username), error = function(e) FALSE)

      if (!user_ok) {
        rv$status    <- "error"
        rv$error_msg <- sprintf(
          "User '%s' was not found on iNaturalist. Check the spelling and try again.",
          username
        )
        enable("fetch_btn"); enable("username")
        return()
      }

      # ── 2. Total count ──────────────────────────────────────────────────────
      setProgress(0.04, detail = "Counting observations...")

      count_q <- list(user_login = username, per_page = 0)
      if (qg != "any") count_q$quality_grade <- qg

      total_resp <- safe_get(paste0(BASE_URL, "/observations"), count_q)

      if (is.null(total_resp)) {
        rv$status    <- "error"
        rv$error_msg <- "Could not connect to the iNaturalist API. Check your internet connection."
        enable("fetch_btn"); enable("username")
        return()
      }

      total_parsed <- parse_resp(total_resp)
      total_obs    <- as.integer(total_parsed$total_results %||% 0)

      if (total_obs == 0) {
        rv$status    <- "error"
        rv$error_msg <- sprintf(
          "No %sobservations found for '%s'.",
          if (qg == "research") "research-grade " else "",
          username
        )
        enable("fetch_btn"); enable("username")
        return()
      }

      needs_chunk   <- total_obs > 10000
      grade_label   <- if (qg == "research") "research-grade " else ""

      rv$status_msg <- sprintf(
        "Fetching %s %sobservations%s...",
        format(total_obs, big.mark = ","),
        grade_label,
        if (needs_chunk) " (chunking by year — this account is prolific!)" else ""
      )

      # ── 3. Fetch all observations ───────────────────────────────────────────
      setProgress(0.07, detail = "Fetching observations...")

      obs_result <- tryCatch(
        fetch_all_observations(
          username, total_obs,
          quality_grade = qg,
          progress_cb = function(n) {
            pct <- 0.07 + (n / max(total_obs, 1)) * 0.38
            setProgress(
              min(pct, 0.45),
              detail = sprintf("Fetched %s / %s observations",
                               format(n, big.mark = ","),
                               format(total_obs, big.mark = ","))
            )
          }
        ),
        error = function(e) list(ok = FALSE, err = conditionMessage(e))
      )

      if (!isTRUE(obs_result$ok) || is.null(obs_result$data)) {
        rv$status    <- "error"
        rv$error_msg <- paste0(
          "Error fetching observations: ",
          obs_result$err %||% "Unknown error"
        )
        enable("fetch_btn"); enable("username")
        return()
      }

      if (length(obs_result$data) == 0) {
        rv$status    <- "error"
        rv$error_msg <- sprintf(
          "No research-grade observations found for '%s'.", username
        )
        enable("fetch_btn"); enable("username")
        return()
      }

      # ── 4. Parse observations ───────────────────────────────────────────────
      setProgress(0.47, detail = "Parsing observations...")

      obs_df <- obs_list_to_df(obs_result$data)

      if (nrow(obs_df) == 0) {
        rv$status    <- "error"
        rv$error_msg <- "Could not parse any observations from the API response."
        enable("fetch_btn"); enable("username")
        return()
      }

      rv$n_total_obs <- nrow(obs_df)

      # ── 5. Summarise unique taxa ────────────────────────────────────────────
      setProgress(0.50, detail = "Summarising unique taxa...")

      obs_counts <- obs_df %>% count(taxon_id, name = "n_user_obs")

      taxa_df <- obs_df %>%
        group_by(taxon_id) %>%
        slice(1) %>%           # keep earliest observation's metadata
        ungroup() %>%
        left_join(obs_counts, by = "taxon_id")

      n_taxa    <- nrow(taxa_df)
      rv$n_taxa <- n_taxa

      n_batches <- ceiling(n_taxa / 30)

      rv$status_msg <- sprintf(
        "Looking up global counts for %s taxa in %s batched requests (~%s faster)...",
        format(n_taxa, big.mark = ","),
        n_batches,
        paste0(min(n_taxa, 30), "x")
      )

      # ── 6. Global observation counts (batched /taxa calls) ─────────────────
      count_vec <- tryCatch(
        get_global_counts_batch(
          taxa_df$taxon_id,
          progress_cb = function(i, total) {
            pct <- 0.50 + (i / total) * 0.46
            setProgress(
              min(pct, 0.97),
              detail = sprintf("Batch %d / %d  (%d taxa each)",
                               i, total, 30L)
            )
          }
        ),
        error = function(e) {
          warning("Batch count lookup failed: ", conditionMessage(e))
          setNames(rep(NA_integer_, n_taxa), as.character(taxa_df$taxon_id))
        }
      )

      taxa_df$global_count <- count_vec[as.character(taxa_df$taxon_id)]

      # ── 7. Rank and finalise ────────────────────────────────────────────────
      setProgress(0.99, detail = "Ranking by rarity...")

      rarity_df <- taxa_df %>%
        filter(!is.na(global_count)) %>%
        arrange(global_count) %>%
        mutate(rank = row_number())

      rv$rarity_df  <- rarity_df
      rv$username   <- username
      rv$status     <- "done"
      rv$status_msg <- sprintf(
        "✅ Done! %s research-grade observations · %s unique taxa ranked · rarest: %s (%s global obs)",
        format(nrow(obs_df), big.mark = ","),
        format(nrow(rarity_df), big.mark = ","),
        coalesce(rarity_df$common_name[1], rarity_df$sci_name[1], "Unknown"),
        format(rarity_df$global_count[1], big.mark = ",")
      )

      setProgress(1)
    })

    enable("fetch_btn")
    enable("username")
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
          "&user_login=", rv$username,
          if (rv$quality_grade != "any") paste0("&quality_grade=", rv$quality_grade) else ""
        ),
        hover_txt = paste0(
          "<b>", coalesce(common_name, sci_name, "Unknown"), "</b><br>",
          "<i>", coalesce(sci_name, ""), "</i><br>",
          "🌍 Global obs: <b>", format(global_count, big.mark = ","), "</b><br>",
          "📅 Your date: ", coalesce(obs_date, "unknown"), "<br>",
          "📍 Place: ", coalesce(place, "unknown"), "<br>",
          "🔢 Your obs (this taxon): ", n_user_obs, "<br>",
          "<a href='", taxon_url, "' target='_blank' style='color:#74ac00;'>",
          "🔗 Taxon page</a>   ",
          "<a href='", your_obs_url, "' target='_blank' style='color:#74ac00;'>",
          "📷 Your observations</a>"
        )
      )

    use_log <- isTRUE(input$log_scale)

    plot_ly() %>%
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
          '<a href="https://www.inaturalist.org/observations?taxon_id=%s&user_login=%s%s" target="_blank" title="View your observations of this taxon" style="color:#74ac00; font-weight:bold;">%s</a>',
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
