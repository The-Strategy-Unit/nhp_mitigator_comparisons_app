# Get container details
get_container <- function(
    tenant = Sys.getenv("AZ_TENANT_ID"),
    app_id = Sys.getenv("AZ_APP_ID"),
    ep_uri = Sys.getenv("AZ_STORAGE_EP"),
    container_name  # env var "AZ_STORAGE_CONTAINER_RESULTS" or "*_SUPPORT"
) {

  # if the app_id variable is empty, we assume that this is running on an Azure
  # VM, and then we will use Managed Identities for authentication.
  token <- if (app_id != "") {
    AzureAuth::get_azure_token(
      resource = "https://storage.azure.com",
      tenant = tenant,
      app = app_id,
      auth_type = "device_code",
      use_cache = TRUE  # avoid browser-authorisation prompt
    )
  } else {
    AzureAuth::get_managed_token("https://storage.azure.com/")
  }

  ep_uri |>
    AzureStor::blob_endpoint(token = token) |>
    AzureStor::storage_container(container_name)

}

# Fetch result-file information
get_nhp_result_sets <- function(container_results, container_support) {

  providers <- get_nhp_providers(container_support)
  allowed_datasets = get_nhp_user_allowed_datasets(NULL, providers)
  allowed <- tibble::tibble(dataset = allowed_datasets)

  container_results |>
    AzureStor::list_blobs("prod", info = "all", recursive = TRUE) |>
    dplyr::filter(!.data[["isdir"]]) |>
    purrr::pluck("name") |>
    purrr::set_names() |>
    purrr::map(
      \(name, ...) AzureStor::get_storage_metadata(container_results, name)
    ) |>
    dplyr::bind_rows(.id = "file") |>
    dplyr::semi_join(allowed, by = dplyr::join_by("dataset")) |>
    dplyr::mutate(dplyr::across("viewable", as.logical))

}

# Identify scheme codes for which data can be read
get_nhp_user_allowed_datasets <- function(groups = NULL, providers) {

  if (!(is.null(groups) || any(c("nhp_devs", "nhp_power_users") %in% groups))) {
    a <- groups |>
      stringr::str_subset("^nhp_provider_") |>
      stringr::str_remove("^nhp_provider_")
    providers <- intersect(providers, a)
  }

  c("synthetic", providers)

}

# Read the providers file
get_nhp_providers <- function(container_support) {

  raw_json <- AzureStor::storage_download(
    container_support,
    src = "providers.json",
    dest = NULL
  )

  raw_json |>
    rawToChar() |>
    jsonlite::fromJSON(simplifyVector = TRUE)

}

# Read the results jsons
get_nhp_results <- function(container_results, file) {

  temp_file <- withr::local_tempfile()
  AzureStor::download_blob(container_results, file, temp_file)

  readBin(temp_file, raw(), n = file.size(temp_file)) |>
    jsonlite::parse_gzjson_raw(simplifyVector = FALSE)  # no need to parse

}

# Isolate metadata for model-runs that have a run_stage metadata label on Azure
fetch_tagged_runs_meta <- function(container_results, container_support) {

  result_sets <- get_nhp_result_sets(container_results, container_support)

  # Factor levels to order run_stage by
  run_stages <- c(
    "final_report_ndg2",  # first level because it's preferred
    "final_report_ndg1",
    "intermediate_ndg2",
    "intermediate_ndg1",
    "initial_ndg2",
    "initial_ndg1"
  )

  latest_tagged_runs <- result_sets |>
    dplyr::filter(!is.na(.data$run_stage)) |>
    dplyr::select(.data$dataset, .data$scenario, .data$run_stage, file) |>
    dplyr::mutate(run_stage = forcats::fct(.data$run_stage, levels = run_stages)) |>
    dplyr::arrange(.data$dataset, .data$run_stage) |>  # run_stage will be ordered by level
    dplyr::slice(1, .by = .data$dataset) |>  # isolates the 'top' level within a scheme
    dplyr::mutate(
      run_stage = .data$run_stage |>
        as.character() |>  # convert from factor to allow string handling
        stringr::str_remove("(_report)?_ndg\\d") |>
        stringr::str_to_sentence()  # 'Initial', 'Intermediate', 'Final'
    )

}

# Read json files given Azure paths
fetch_tagged_runs_params <- function(runs_meta, container_results) {
  runs_meta |>
    dplyr::pull(file) |>  # paths to jsons
    purrr::map(\(file) get_nhp_results(container_results, file)) |>
    purrr::map(purrr::pluck("params")) |>
    purrr::set_names(runs_meta$dataset)  # name with scheme code
}

#' Read Rates (Trend) Data
#' Fetch and read parquet files that store contextual data used in the NHP
#' inputs app.
#' @param file Character. Name of parquet file to be read (without the
#'     extension).
#' @param inputs_data_version Character. The app version for which you want to
#'     return data (there's a separate versioned folder of parquet files for
#'     each release).
#' @param container_inputs A storage container object.
#' @return A tibble.
read_provider_data <- function(
    container_inputs,
    file = "rates",
    inputs_data_version = Sys.getenv("NHP_INPUTS_DATA_VERSION", "dev")
) {

  parquet_in <- AzureStor::storage_download(
    container_inputs,
    src = glue::glue("{inputs_data_version}/{file}.parquet"),
    dest = NULL
  )

  arrow::read_parquet(parquet_in) |> tibble::as_tibble()

}
