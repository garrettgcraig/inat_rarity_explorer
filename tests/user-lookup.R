# Run from the repository root: Rscript tests/user-lookup.R
# Load helpers and server without launching the app.
exprs <- parse("app.R")
invisible(lapply(exprs[-length(exprs)], eval, envir = globalenv()))

stopifnot(
  identical(normalize_user_query(" @Kueda "), "Kueda"),
  identical(normalize_user_query("https://www.inaturalist.org/people/kueda/?tab=about"), "kueda"),
  identical(normalize_user_query("https://inaturalist.org/users/kueda#about"), "kueda"),
  identical(normalize_user_query("https://inaturalist.org.evil.test/people/kueda"),
            "https://inaturalist.org.evil.test/people/kueda"),
  identical(normalize_user_query("Ken-ichi Ueda"), "Ken-ichi Ueda")
)

response <- function(code, body) structure(list(
  status_code = code, content = charToRaw(body)
), class = "response")
calls <- list()
safe_get <- function(url, query, ...) {
  calls[[length(calls) + 1L]] <<- list(url = url, query = query)
  response(200L, '{"results":[{"login":"kueda-test","name":"Test"},{"login":"kueda","name":"Ken-ichi Ueda"}]}')
}
stopifnot(length(search_users("k")) == 0L, length(calls) == 0L)
matches <- search_users("@KUEDA")
stopifnot(matches[[1]]$login == "kueda",
          calls[[1]]$url == paste0(BASE_URL_V2, "/users/autocomplete"),
          calls[[1]]$query$q == "kueda", calls[[1]]$query$per_page == 8L,
          calls[[1]]$query$fields == USER_FIELDS)
safe_get <- function(...) NULL
stopifnot(is.null(search_users("kueda")))
safe_get <- function(...) response(200L, '{"results":[]}')
stopifnot(identical(search_users("nobody"), list()))

GET <- function(url, query, ...) {
  stopifnot(url == paste0(BASE_URL_V2, "/users/kueda"), query$fields == USER_FIELDS)
  response(200L, '{"results":[{"login":"kueda"}]}')
}
stopifnot(isTRUE(check_user_exists("KUEDA")))
GET <- function(...) response(404L, '{}')
stopifnot(identical(check_user_exists("kueda"), FALSE))
GET <- function(...) response(429L, '{}')
stopifnot(is.na(check_user_exists("kueda")))
GET <- function(...) response(200L, 'not json')
stopifnot(is.na(check_user_exists("kueda")))
GET <- function(...) stop("offline")
stopifnot(is.na(check_user_exists("kueda")))

safe_get <- function(url, query, ...) {
  stopifnot(url == paste0(BASE_URL_V2, "/search"), query$sources == "projects")
  response(200L, '{"results":[{"type":"project","project":{"id":42,"title":"Nature"}}]}')
}
stopifnot(identical(resolve_project("Nature"), list(id = "42", title = "Nature")))

# Exercise the real Shiny observers, rendered suggestions, and stale-click guard.
search_users <- function(query) if (nchar(query) < 2L) list() else list(
  list(login = "kueda", name = "Ken-ichi Ueda"),
  list(login = "kueda-test", name = "Test account")
)
shiny::testServer(server, {
  session$setInputs(search_mode = "user", username = "Ken-ichi", quality_grade = "research")
  session$elapse(500)
  stopifnot(user_matches$query == "Ken-ichi", length(user_matches$users) == 2L,
            grepl("@kueda", output$user_suggestions$html, fixed = TRUE))
  session$setInputs(user_pick_1 = 0)
  session$setInputs(user_pick_1 = 1)
  stopifnot(user_matches$selected == "kueda", rv$status == "idle")
  session$setInputs(username = "other", user_pick_2 = 0)
  session$setInputs(user_pick_2 = 1)
  stopifnot(user_matches$selected == "kueda") # old suggestions cannot select
  start_fetch("Ken-ichi Ueda", "research")
  stopifnot(rv$status == "idle") # name requires explicit account selection
  # Keep scheduled network work out of this deterministic test.
  assign("schedule", function(...) invisible(NULL), envir = environment(start_fetch))
  start_fetch("https://www.inaturalist.org/people/KUEDA/", "research")
  stopifnot(fs$subject == "kueda", fs$mode == "user", rv$status == "fetching")
  assign("check_user_exists", function(...) NA, envir = globalenv())
  assign("inat_multi_get_json", function(...) list(list(total_results = 0L)), envir = globalenv())
  step_count()
  stopifnot(rv$status == "error", grepl("Could not verify", rv$error_msg))
})
cat("User lookup tests passed.\n")
