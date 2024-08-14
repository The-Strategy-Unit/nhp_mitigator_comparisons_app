get_nhp_result_sets <- function(
    container,
    allowed_datasets = get_nhp_user_allowed_datasets(NULL),
    folder = "prod"
) {

  allowed <- tibble::tibble(dataset = allowed_datasets)

  container |>
    AzureStor::list_blobs(folder, info = "all", recursive = TRUE) |>
    dplyr::filter(!.data[["isdir"]]) |>
    purrr::pluck("name") |>
    purrr::set_names() |>
    purrr::map(\(name, ...) AzureStor::get_storage_metadata(container, name)) |>
    dplyr::bind_rows(.id = "file") |>
    dplyr::semi_join(allowed, by = dplyr::join_by("dataset")) |>
    dplyr::mutate(dplyr::across("viewable", as.logical))

}

get_container <- function(
    tenant = Sys.getenv("AZ_TENANT_ID"),
    app_id = Sys.getenv("AZ_APP_ID"),
    ep_uri = Sys.getenv("AZ_STORAGE_EP"),
    container_name = Sys.getenv("AZ_STORAGE_CONTAINER")
) {

  # if the app_id variable is empty, we assume that this is running on an Azure VM,
  # and then we will use Managed Identities for authentication.
  token <- if (app_id != "") {
    AzureAuth::get_azure_token(
      resource = "https://storage.azure.com",
      tenant = tenant,
      app = app_id,
      auth_type = "device_code"
    )
  } else {
    AzureAuth::get_managed_token("https://storage.azure.com/")
  }

  ep_uri |>
    AzureStor::blob_endpoint(token = token) |>
    AzureStor::storage_container(container_name)

}

get_nhp_user_allowed_datasets <- function(
    groups = NULL,
    providers_path = app_sys("app", "data", "providers.json")
) {

  p <- jsonlite::read_json(providers_path, simplifyVector = TRUE)

  if (!(is.null(groups) || any(c("nhp_devs", "nhp_power_users") %in% groups))) {
    a <- groups |>
      stringr::str_subset("^nhp_provider_") |>
      stringr::str_remove("^nhp_provider_")
    p <- intersect(p, a)
  }

  c("synthetic", p)

}

get_nhp_results <- function(container, file) {

  temp_file <- withr::local_tempfile()
  AzureStor::download_blob(container, file, temp_file)

  readBin(temp_file, raw(), n = file.size(temp_file)) |>
    jsonlite::parse_gzjson_raw(simplifyVector = FALSE)
  # ignore parse_nhp_results() step; we only care about params in this repo

}

# Isolate metadata for model-runs that have a run_stage metadata label on Azure
fetch_labelled_runs_meta <- function(container) {

  result_sets <- get_nhp_result_sets(container)

  # Factor levels to order run_stage by
  run_stages <- c(
    "final_report_ndg2",  # first level because it's preferred
    "final_report_ndg1",
    "intermediate_ndg2",
    "intermediate_ndg1",
    "initial_ndg2",
    "initial_ndg1"
  )

  latest_labelled_runs <- result_sets |>
    dplyr::filter(!is.na(run_stage)) |>
    dplyr::select(dataset, scenario, run_stage, file) |>
    dplyr::mutate(run_stage = forcats::fct(run_stage, levels = run_stages)) |>
    dplyr::arrange(dataset, run_stage) |>  # run_stage will be ordered by level
    dplyr::slice(1, .by = dataset) |>  # isolates the 'top' level within a scheme
    dplyr::mutate(
      run_stage = run_stage |>
        as.character() |>  # convert from factor
        stringr::str_remove("(_report)?_ndg\\d") |>
        stringr::str_to_sentence()  # 'Initial', 'Intermediate', 'Final'
    )

}

# Read json files given Azure paths
fetch_labelled_runs_params <- function(runs_meta, container) {
  runs_meta |>
    dplyr::pull(file) |>  # paths to jsons
    purrr::map(\(file) get_nhp_results(container, file)) |>
    purrr::map(purrr::pluck("params")) |>
    purrr::set_names(runs_meta$dataset)  # name with scheme code
}
