# Ingets raw data
assessments <- read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/uzyt-m557.json"
) %>%
  write_parquet(here::here("cc_appeals", "big data", "assessments.parquet"))

read.socrata(
  "https://datacatalog.cookcountyil.gov/resource/wvhk-k5uv.json"
)  %>%
  write_parquet(here::here("cc_appeals", "big data", "sales.parquet"))

read.socrata(
    "https://datacatalog.cookcountyil.gov/resource/y282-6ig3.json"
) %>%
  write_parquet(here::here("cc_appeals", "big data", "appeals.parquet"))

read.socrata(
  paste0(
  "https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json?$"
  , "select=pin, triad_name where year >2004")
) %>% 
  dplyr::distinct(pin, triad_name) %>%
  write_parquet(here::here("cc_appeals", "big data", "parcel_triads.parquet"))

