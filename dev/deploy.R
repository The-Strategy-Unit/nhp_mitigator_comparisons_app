get_env_var <- function(env_var) {
  env_val <- Sys.getenv(env_var, unset = NA)
  if (is.na(env_val)) {
    stop("Missing required env var: ", env_var)
  }
  env_val
}

# Assumes latest Azure data-dir name is same as latest nhp_data version
get_data_version <- function(
  repo = "The-Strategy-Unit/nhp_data",
  remove_patch = TRUE # vX.Y not vX.Y.Z
) {
  url <- glue::glue("https://github.com/{repo}/releases/latest")
  req <- httr2::request(url)
  resp <- httr2::req_perform(req)
  release_url <- httr2::resp_url(resp)
  version <- release_url |> stringr::str_replace(".*/tag/", "")
  stopifnot(stringr::str_detect(version, "^v\\d{1,}\\.\\d{1,}\\.\\d{1,}$"))

  if (remove_patch) {
    version <- version |> stringr::str_remove("\\.\\d{1,}$")
  }

  version
}

deploy <- function(
  server_name = "connect.strategyunitwm.nhs.uk",
  type = c("prod", "dev")
) {
  type <- match.arg(type)

  # Default prod values
  withr::local_envvar(
    AZ_STORAGE_EP = get_env_var("AZ_STORAGE_EP"),
    AZ_STORAGE_CONTAINER_INPUTS = get_env_var("AZ_STORAGE_CONTAINER_INPUTS"),
    FEEDBACK_FORM_URL = get_env_var("FEEDBACK_FORM_URL"),
    NHP_INPUTS_DATA_VERSION = get_data_version()
  )
  app_id <- 108
  app_name <- "nhp_compare_mitigation_prediction_app"
  app_title <- "Compare NHP Activity Mitigation Predictions"

  if (type == "dev") {
    withr::local_envvar(NHP_INPUTS_DATA_VERSION = "dev")
    app_id <- 193
    app_name <- paste0(app_name, "_dev")
    app_title <- paste(app_title, "(dev)")
  }

  rsconnect::deployApp(
    appName = app_name,
    appTitle = app_title,
    server = server_name,
    appId = app_id,
    appFiles = c(
      "R",
      "inst",
      "NAMESPACE",
      "DESCRIPTION",
      "app.R"
    ),
    envVars = c(
      "AZ_STORAGE_EP",
      "AZ_STORAGE_CONTAINER_INPUTS",
      "FEEDBACK_FORM_URL",
      "NHP_INPUTS_DATA_VERSION"
    ),
    lint = FALSE,
    forceUpdate = TRUE
  )
}

# Deploy development version between releases
deploy(type = "dev")

# Deploy on release
deploy(type = "prod")
