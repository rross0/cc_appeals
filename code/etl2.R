# This file combines sales and assessment data

sales <- read_parquet(here::here("cc_appeals", "big data", "sales.parquet")) %>%
  dplyr::mutate(
    dplyr::across(sale_filter_same_sale_within_365:sale_filter_deed_type, ~ as.logical(.x))  # Proper encoding
    , pin = ccao::pin_format_pretty(pin, full_length = T)
    , sale_date = as.Date(sale_date)
  ) %>%
  # according to ccao, this recreates the filters for sales ratio studies
  # https://github.com/ccao-data/wiki/blob/master/SOPs/Sales-Ratio-Studies.md#exclusion-criteria
  dplyr::filter(
    !sale_filter_same_sale_within_365
    & !sale_filter_less_than_10k
    & !sale_filter_deed_type 
  ) %>%
  dplyr::select(pin, year, sale_date, sale_price)


residential_assessments <- read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  dplyr::select(pin, year)


# Want to restrict our sales sample 