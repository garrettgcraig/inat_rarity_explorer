# Changelog

## Unreleased — 2026-10-12

- Add debounced iNaturalist username/display-name suggestions with explicit account selection.
- Accept `@username` and iNaturalist `/people/` or `/users/` profile links, including mixed-case usernames.
- Use v2 for user search and exact-user verification with minimal field projections; report API failures separately from nonexistent users.
- Move project-name resolution to v2 search, so all application API requests use v2.
- Add offline helper and Shiny interaction regression checks. Public v2 user autocomplete and project search projections were also verified against the live API. Deployment is pending.
