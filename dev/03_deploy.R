# Deploy to the current and new Posit Connect servers. The current server
# (strategyunitwm.nhs.uk) will be switched off in May/June 2025 and the new
# server (currently named su.mlcsu.org) will take its name.

deploy <- function(server_name, app_id) {
  rsconnect::deployApp(
    appName = "nhp_inputs_report_app",
    appTitle = "NHP Mitigator Comparison App",
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

deploy("connect.strategyunitwm.nhs.uk", 298)
deploy("connect.su.mlcsu.org", 108)
