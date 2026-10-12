# Changelog

## Deployed — 2026-10-12

- Add debounced iNaturalist username/display-name suggestions with explicit account selection.
- Accept `@username` and iNaturalist `/people/` or `/users/` profile links, including mixed-case usernames.
- Use v2 for user search and exact-user verification with minimal field projections; report API failures separately from nonexistent users.
- Move project-name resolution to v2 search, so all application API requests use v2.
- Add offline helper and Shiny interaction regression checks. Public v2 user autocomplete and project search projections were also verified against the live API.
- Rewrite the README around the current user/project workflow, observation-count definition, cursor pagination, batched counts, validation, and deployment.
- Add descriptive title, canonical URL, social metadata, WebApplication JSON-LD, and introductory content in the initial HTML for search discovery.
- Declare the existing `later` runtime dependency and correct the repository links in `DESCRIPTION`.
- PR #1 merged and deployed to the existing `inat-rarest` shinyapps.io application (bundle 12681335). Production username suggestions and initial HTML search metadata/content were verified.
