# iNaturalist Rarity Explorer — Find Your Rarest Observations

Discover your rarest iNaturalist observations with a free, open-source R Shiny dashboard. Search an iNaturalist user or project, then rank observed taxa by their **worldwide iNaturalist observation count**.

**[Launch iNaturalist Rarity Explorer](https://garrettgcraig.shinyapps.io/inat-rarest/)** · [Report an issue](https://github.com/garrettgcraig/inat_rarity_explorer/issues)

## What does “rare” mean here?

A taxon with fewer observations on iNaturalist ranks above a taxon with more observations. This measures how frequently a taxon has been recorded on the platform; it does **not** measure population size, conservation status, or the number of distinct observers. Coverage varies by location, season, identification difficulty, and observer activity.

## Features

- **Smart user search:** find accounts by username or display name, select a suggestion, or paste an `@username` or iNaturalist profile link.
- **User and project exploration:** analyze an account's observations or search an iNaturalist project by name.
- **Quality-grade selection:** research-grade observations by default, with an option to include all quality grades.
- **Global observation-count rankings:** a trophy case, interactive charts, and a sortable table with species photos, scientific/common names, observation dates, and places.
- **Filters and CSV export:** filter by kingdom, animal class, common-name availability, and observation place; export the filtered table.
- **Large-account support:** switch to observation-ID cursor pagination beyond iNaturalist's 10,000-result page limit.
- **Session caching and progress:** changing chart controls reuses fetched data; progress messages track loading.
- **iNaturalist API v2:** explicit field projections and batched taxon-count requests reduce transferred data.

## How to use it

1. [Open the explorer](https://garrettgcraig.shinyapps.io/inat-rarest/).
2. Choose a user's observations or a project's observations.
3. For users, type at least two characters of a username or display name and select the intended account. A full login, `@login`, or `https://www.inaturalist.org/people/login` also works directly. Display names and partial matches are never automatically assigned to an account.
4. Choose the quality grade and click **Find My Rarest**.
5. Explore the ranked results, adjust filters, and download a CSV if needed.

No iNaturalist sign-in or API key is required. The app reads public observations; it does not post or modify observations. Large accounts and projects can take longer to load.

## Run locally

Requires R ≥ 4.2 and internet access to `api.inaturalist.org`.

```r
source("install_packages.R")
shiny::runApp()
```

Dependencies: `shiny`, `shinydashboard`, `shinyjs`, `shinycssloaders`, `httr`, `jsonlite`, `plotly`, `DT`, `dplyr`, and `later`. The installer and `DESCRIPTION` list the runtime requirements.

## API requests

All application requests use the public [iNaturalist API v2](https://api.inaturalist.org/v2/docs/) with explicit field projections.

| Purpose | Endpoint |
| --- | --- |
| Count and fetch observations | `/observations` with `user_login` or `project_id`, quality grade, and projected observation fields |
| Fetch beyond the page limit | `/observations` with an `id_above` cursor |
| Batch worldwide taxon counts | `/taxa?id=ID1,ID2,...` with `id` and `observations_count` |
| Suggest users by login or display name | `/users/autocomplete?q=X&per_page=8` with `id`, `login`, and `name` |
| Verify exact username when no observations match | `/users/X` with the same minimal user fields |
| Resolve a project name | `/search?q=X&sources=projects&per_page=1` with projected project `id` and `title` |

User suggestions stay in the current Shiny session, use a 450 ms typing debounce, and request only account ID, login, and display name. API failures are reported separately from nonexistent users and users with no matching observations.

## Validation

```sh
Rscript tests/user-lookup.R
```

Offline helper and Shiny interaction checks cover input normalization, v2 requests, exact-login ordering, empty/unavailable lookups, suggestion selection, stale clicks, and fetch error handling. Static HTML checks verify search metadata and introductory content before a WebSocket session starts.

## Deployment and search metadata

The existing production app is `inat-rarest` on the `garrettgcraig` shinyapps.io account. Deploy only the runtime files; keep hosting records and credentials out of Git.

```r
rsconnect::deployApp(
  appFiles = c("app.R", "DESCRIPTION"),
  appId = "17348267",
  appName = "inat-rarest",
  account = "garrettgcraig",
  server = "shinyapps.io",
  launch.browser = FALSE
)
```

The app's initial HTML contains a descriptive page title, meta description, canonical production URL, Open Graph/Twitter metadata, WebApplication JSON-LD, and introductory text. Shared `?user=` and `?project=` links remain functional, while canonical metadata points to the main app URL. Metadata improves discoverability and link descriptions; indexing and rankings depend on search engines.

## Repository contents

| Path | Purpose |
| --- | --- |
| `app.R` | Shiny UI, API helpers, and server |
| `DESCRIPTION` | Runtime dependency manifest |
| `install_packages.R` | Local package installer |
| `tests/user-lookup.R` | Offline regression checks |
| `CHANGELOG.md` | Change and release status |

## License

MIT © Garrett Craig. Observation data and photos retain their source licenses; the repository's code license does not relicense iNaturalist content.
