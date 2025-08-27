deploy <- function(server_name, app_id) {
  rsconnect::deployApp(
    appName = "nhp_mitigator_comparisons_app",
    appTitle = "NHP Mitigator Comparisons App",
    appFiles = c(
      "R/",
      "inst/",
      "NAMESPACE",
      "DESCRIPTION",
      "app.R"
    ),
    server = server_name,
    appId = app_id,
    lint = FALSE,
    forceUpdate = TRUE
  )
}

deploy("connect.strategyunitwm.nhs.uk", 108)
