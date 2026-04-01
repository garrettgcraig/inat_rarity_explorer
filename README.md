# 🌿 iNat Rarity Explorer

A Shiny dashboard that lets you enter any iNaturalist username and discover which of their research-grade observations are the **rarest in the world** — ranked by global observation count across all of iNaturalist.

![Dark theme dashboard with lollipop chart and data table](https://www.inaturalist.org/assets/inat-social-media-default.png)

---

## Features

- **Username lookup** — enter any iNaturalist login and fetch all research-grade observations
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
2. Type an iNaturalist username (e.g. `hommedeterre`) into the sidebar text box.
3. Click **Find My Rarest**.
4. Wait while the app fetches observations and looks up global counts — a progress bar tracks each step.
5. Explore the lollipop chart and sortable table. Use the slider to change how many species appear in the chart.

> **Note:** The global-count lookup makes one API call per unique taxon. At ~12 calls/second this is polite to the iNaturalist API. For an account with 500 unique taxa, expect ~45 seconds for the lookup phase.

---

## API Details

All data comes from the public [iNaturalist API v1](https://api.inaturalist.org/v1/docs/):

| Purpose | Endpoint |
|---|---|
| Count user observations | `GET /observations?user_login=X&quality_grade=research&per_page=0` |
| Fetch observation pages | `GET /observations?user_login=X&quality_grade=research&per_page=200&page=N` |
| Date-range chunk | Add `d1=YYYY-01-01&d2=YYYY-12-31` to above |
| Global taxon count | `GET /observations?taxon_id=X&per_page=0` → `total_results` |
| Verify username | `GET /users/autocomplete?q=X` |

No API key is required.

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
