# ==============================================================================
# DATA AND LIBRARIES REQUIRED THROUGHOUT THE BOOK
# ==============================================================================

# ========== Load required package ===============
required_packages <- c(
  "tidyverse", "janitor", "here", "igraph", "ggraph", "ggrepel",
  "scales", "glue", "knitr", "kableExtra", "sf", "patchwork",
  "dplyr", "ggalluvial", "DT"
)

invisible(lapply(required_packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}))

# Summarize package versions
pkg_versions_df <- data.frame(
  Package = required_packages,
  Version = sapply(required_packages, function(x) as.character(packageVersion(x)))
) |> 
  dplyr::arrange(Package)

# =================================================
# =========== Read Data ===========================
data <- read_csv(here("data", "data.csv")) |> janitor::clean_names()
desag <- read_csv(here("data", "desag_data.csv")) |> janitor::clean_names()
investors <- read_csv(here("data", "investors.csv")) |> janitor::clean_names()
tax_haven <- read_csv(here("data", "tax_haven_list.csv")) |> janitor::clean_names()

# link tax_haven_list.csv to desag_data.csv
desag <- desag |>
  rename(country_iso2 = country) |>
  left_join(
    tax_haven |>
      select(iso2) |>
      mutate(is_tax_haven = TRUE),
    by = c("country_iso2" = "iso2")
  ) |>
  mutate(is_tax_haven = replace_na(is_tax_haven, FALSE))

# link tax_haven_list.csv to investors.csv
investors <- investors |>
  rename(country_iso2 = country) |>
  left_join(
    tax_haven |>
      select(iso2) |>
      mutate(from_tax_haven = TRUE),
    by = c("country_iso2" = "iso2")
  ) |>
  mutate(from_tax_haven = replace_na(from_tax_haven, FALSE))

# !!! in coutries.json, for one iso2 there are more than one country or region matched
# so here I use countries_sf_all to keep all the region
# and use countries_sf_main to store the mainland
countries_sf_all <- sf::st_read(here("data", "countries.json"), quiet = TRUE)
countries_sf_main <- countries_sf_all |>
  group_by(ISO) |>
  slice_max(SHAPE_Area, n = 1, with_ties = FALSE) |>              
  ungroup() |>
  select(COUNTRY, ISO, COUNTRYAFF, AFF_ISO, geometry)

#=========================================================
# ============ Custom Functions ==========================
# Function "availability_table": check missing data with sorting
data_availability <- function(data_name, fields, caption = NULL) {
  df <- get(data_name, inherits = TRUE)
  n_total <- nrow(df)
  
  # Check if the field exists
  missing_fields <- setdiff(fields, names(df))
  if (length(missing_fields) > 0) {
    msg <- paste0(
      "These fields are missing from dataset '", data_name, "':\n",
      paste("  -", missing_fields, collapse = "\n")
    )
    stop(msg)
  }
  
  # Definition of missing value detection
  is_missing_vec <- function(x) {
    if (is.list(x)) {
      vapply(x, function(el) {
        if (is.null(el) || length(el) == 0) return(TRUE)
        if (all(is.na(el))) return(TRUE)
        if (is.character(el))
          all(is.na(el) | trimws(el) == "" | el %in% c("NA","N/A","NULL","null"))
        else FALSE
      }, logical(1))
    } else if (is.character(x)) {
      is.na(x) | trimws(x) == "" | x %in% c("NA","N/A","NULL","null")
    } else {
      is.na(x)
    }
  }
  
  # Compute missing rate per field
  res <- lapply(fields, function(fld) {
    miss <- is_missing_vec(df[[fld]])
    n_miss <- sum(miss, na.rm = TRUE)
    tibble::tibble(
      Field         = fld,
      Rows          = n_total,
      NonMissing    = n_total - n_miss,
      Missing_Count = n_miss,
      Missing_Rate  = n_miss / n_total
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::arrange(dplyr::desc(Missing_Rate))
  
  # Caption
  dataset_label <- paste0("`", data_name, "`")
  cap <- if (is.null(caption)) paste0("Data availability in ", dataset_label) else caption
  
  # table output
  res |>
    dplyr::mutate(`No data (%)` = scales::percent(Missing_Rate, accuracy = 0.1)) |>
    dplyr::select(Field, Rows, NonMissing, Missing_Count, `No data (%)`) |>
    knitr::kable(caption = cap, align = c("l","r","r","r","c")) |>
    kableExtra::kable_styling(
      full_width = FALSE,
      position   = "center",
      bootstrap_options = c("striped","hover")
    )
}

# ============================
# Function "value_summary" to check the distribution of a column in a dataset
value_summary <- function(data, column) {
  
  # Automatically capture the data object's name (e.g. "data", "desag", etc.)
  data_name <- deparse(substitute(data))
  
  col_data <- data[[column]]
  
  # Count occurrences of each unique value
  result <- tibble::tibble(Value = col_data) |>
    dplyr::count(Value, name = "Count") |>
    dplyr::mutate(
      Total = sum(Count),
      Share = scales::percent(Count / Total, accuracy = 0.1)
    ) |>
    dplyr::arrange(desc(Count))
  
  # Output table with improved caption
  knitr::kable(
    result,
    align = c("l", "r", "r"),
    caption = paste0("Value distribution of `", column, "` in dataset `", data_name, "`")
  ) |>
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "center",
      bootstrap_options = c("striped", "hover")
    )
}