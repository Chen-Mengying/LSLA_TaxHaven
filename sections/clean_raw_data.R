library(readxl)
library(readr)
library(dplyr)
library(writexl)
library(stringr)

# =================================================
# =========== Clean desag =========================
### Step 1: choose NA according to the first two letter

# Read ORBIS's Sheet1
orbis <- read_excel(here("data/raw", "ORBIS - AllEntitiesTable.xlsx"), sheet = 1)|> janitor::clean_names() 

# Extract the first two characters to generate FIRST2
orbis <- orbis |>
  mutate(FIRST2 = str_sub(bvd_id_number, 1, 2))

# Filter FIRST2 == "NA"
namibia <- orbis |> filter(FIRST2 == "NA")

# Output the corresponding bvd_id_number (save the file separately)
write_xlsx(namibia, "data/Namibia.xlsx")


### Step 2: Change the real NA to NULL(3267), so the rest of NA is real Namibia(15)

# Read desag_data
desag <- read_csv(here("data/raw","desag_data.csv"))|> janitor::clean_names() 

# Read the Namibia 
namibia_ids <- namibia$bvd_id_number

# Find rows where country is NA and bvd_id_number is not in Namibia
rows_to_modify <- is.na(desag$country) & !(desag$bvd_id_number %in% namibia_ids)

# Modify country column 
desag$country[rows_to_modify] <- "NULL"

# Number of modified lines output 
modified_count <- sum(rows_to_modify)
cat("Number of lines modified：", modified_count, "\n")


### Step 3: change all iso2 to iso3
# Read ISO lookup table
iso_map <- read_csv(here("data", "iso_country_code.csv")) |> janitor::clean_names() |>
  rename(iso2 = alpha_2_code, iso3 = alpha_3_code) 

# replace country → country_iso3
desag <- desag |>
  left_join(iso_map |> select(iso2, iso3),
            by = c("country" = "iso2")) |>
  mutate(country = iso3) |>
  select(-iso3)|>
  rename(country_iso3 = country)

# Check the missing value right now (3284 = 3267 + 17(II))
sum(is.na(desag$country_iso3))

# replace target_country_code_alpha2 → target_country_iso3
desag <- desag |>
  left_join(iso_map |> select(iso2, iso3),
            by = c("target_country_code_alpha2" = "iso2")) |>
  mutate(target_country_code_alpha2 = iso3)|>
  select(-iso3)|>
  rename(target_country_iso3 = target_country_code_alpha2)

# Check the missing value right now (0)
sum(is.na(desag$target_country_iso3))


## Step 4: output the cleaned desag data
write_csv(desag, "data/desag_cleaned.csv")

# =================================================
# =========== Clean investors =====================
# step1 
investor <- read_csv(here("data/raw", "investors.csv")) |>
  janitor::clean_names()

# step2
rows_to_modify_inv <- is.na(investor$country) & !(investor$bvd_id_number %in% namibia_ids)

investor$country[rows_to_modify_inv] <- "NULL"

cat("Investor rows modified:", sum(rows_to_modify_inv), "\n")

#Step3
investor <- investor |>
  left_join(iso_map |> select(iso2, iso3),
            by = c("country" = "iso2")) |>
  mutate(country = iso3) |>
  select(-iso3) |>
  rename(country_iso3 = country)

# If want to keep the missing value as NULL
# investor <- investor |>
#   left_join(iso_map |> select(iso2, iso3),
#             by = c("country" = "iso2")) |>
#   mutate(country_iso3 = if_else(is.na(iso3), country, iso3)) |>
#   select(-iso3)

write_csv(investor, "data/investors_cleaned.csv")

