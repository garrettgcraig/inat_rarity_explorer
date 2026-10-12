# 🌿 iNat Rarity Explorer

A Shiny dashboard that lets you enter any iNaturalist username and discover which of their research-grade observations are the **rarest in the world** — ranked by global observation count across all of iNaturalist.

![Dark theme dashboard with lollipop chart and data table](https://www.inaturalist.org/assets/inat-social-media-default.png)

---

## Features

- **Smart user lookup** — search usernames or display names with debounced, selectable suggestions; paste an `@username` or iNaturalist profile link, then fetch observations
- **10 k cap bypass** — users with more than 10,000 observations are handled automatically via year-by-year date-range chunking
- **Global rarity ranking** — each unique taxon's total worldwide observation count is fetched from the iNaturalist API
- **Interactive lollipop chart** — plotly chart of the top-N rarest taxa, with optional log scale
- **Sortable data table** — full ranked table with photo thumbnails, common & scientific names, date, place, and global count
- **Session caching** — re-adjusting the top-N slider or log-scale toggle never re-fetches data
- **Progress feedback** — step-by-step progress bar during both the observation fetch and global-count lookup phases
- **Graceful error handling** — user-facing messages for bad usernames, network errors, and empty results

---

## Quick Start

### 1. Install dependencies

```r
source("install_packages.R")
```

Or manually:

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyjs", "shinycssloaders",
  "httr", "jsonlite", "plotly", "DT", "dplyr"
))
```

### 2. Run the app

```r
shiny::runApp("app.R")
# or, from within the folder:
shiny::runApp()
```

---

## Usage

1. Open the app in your browser.
2. Type an iNaturalist username or display name into the sidebar text box. Suggestions appear after at least two characters; select the intended account. A full username, `@username`, or `https://www.inaturalist.org/people/username` profile link also works directly. Names and partial matches are never automatically assigned to an account.
3. Click **Find My Rarest**.
4. Wait while the app fetches observations and looks up global counts — a progress bar tracks each step.
5. Explore the lollipop chart and sortable table. Use the slider to change how many species appear in the chart.

> **Note:** The global-count lookup makes one API call per unique taxon. At ~12 calls/second this is polite to the iNaturalist API. For an account with 500 unique taxa, expect ~45 seconds for the lookup phase.

---

## API Details

All requests use the public [iNaturalist API v2](https://api.inaturalist.org/v2/docs/), with explicit field projections:

| Purpose | Endpoint |
|---|---|
| Count and fetch user observations | `GET /observations?user_login=X&quality_grade=research&per_page=200&page=1` → `total_results` and first page |
| Fetch subsequent observation pages | Same endpoint with `page=N` or `id_above=ID` |
| Global taxon counts | `GET /taxa?id=ID1,ID2,...` → each taxon's `observations_count` |
| Suggest users by login or display name | `GET /users/autocomplete?q=X&per_page=8&fields=(id:!t,login:!t,name:!t)` |
| Verify exact username when no observations match | `GET /users/X?fields=(id:!t,login:!t,name:!t)` |
| Resolve project name | `GET /search?q=X&sources=projects&per_page=1&fields=(type:!t,project:(id:!t,title:!t))` |

No API key is required.

User suggestions are kept only in the current Shiny session, use a 450 ms typing debounce, and request only account ID, login, and display name. Failed lookups are reported separately from nonexistent users or users with no matching observations.

## Validation

Run `Rscript tests/user-lookup.R` from the repository root to exercise input normalization, v2 request construction, exact-login ordering, unavailable/empty lookups, suggestion selection, stale clicks, and fetch error handling without network calls.

---

## File Structure

```
inat-rarest/
├── app.R               # Single-file Shiny application
├── install_packages.R  # One-time package installer
├── DESCRIPTION         # Dependency manifest
└── README.md           # This file
```

---

## Dependencies

| Package | Role |
|---|---|
| `shiny` | Web application framework |
| `shinydashboard` | Dashboard layout |
| `shinyjs` | Enable/disable UI elements |
| `shinycssloaders` | Spinner overlays |
| `httr` | HTTP requests to iNaturalist API |
| `jsonlite` | JSON parsing |
| `plotly` | Interactive lollipop chart |
| `DT` | Interactive data table |
| `dplyr` | Data manipulation |

Requires **R ≥ 4.2** and internet access to `api.inaturalist.org`.

---

## License

MIT © Garrett Craig
