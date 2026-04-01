# install_packages.R
# Run this once before launching the app.
# Installs all required CRAN packages.

pkgs <- c(
  "shiny",
  "shinydashboard",
  "shinyjs",
  "shinycssloaders",
  "httr",
  "jsonlite",
  "plotly",
  "DT",
  "dplyr"
)

missing_pkgs <- pkgs[!pkgs %in% rownames(installed.packages())]

if (length(missing_pkgs) > 0) {
  message("Installing: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, dependencies = TRUE)
} else {
  message("All packages already installed.")
}

message("Done. Launch the app with: shiny::runApp('app.R')")
