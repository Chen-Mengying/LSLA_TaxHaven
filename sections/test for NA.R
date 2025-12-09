# read "data" in a normal way, all the NA will be viewed as missing value
data_read_NA_as_missing <- read_csv(here("data/raw", "data.csv")) |> janitor::clean_names()

# read "data", but all the NA will keep as character "NA"
data_read_NA_as_chr <- read_csv(here("data", "data.csv"), na = c("")) |> janitor::clean_names() 


missing_country <- data_read_NA_as_missing %>% 
  filter(is.na(country_iso_code))

chr_country <- data_read_NA_as_chr %>% 
  filter(country=="NA")

data_check <- data_read_NA_as_chr |>
  mutate(bvd_2 = str_sub(bvd_id_number, 1, 2)) |>
  select(bvd_id_number, bvd_2, country, country_iso_code)


desag <- read_csv(here("data/raw", "desag_data.csv"), na = c("")) |> janitor::clean_names()