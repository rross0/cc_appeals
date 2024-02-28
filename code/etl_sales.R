# This file determines which sales to use in the analysis

# Include sales data
sales <- read_parquet(here::here("cc_appeals", "big data", "sales.parquet")) %>%
  dplyr::mutate(
    dplyr::across(sale_filter_same_sale_within_365:sale_filter_deed_type, ~ as.logical(.x))  # Proper encoding
    , is_multisale = as.logical(is_multisale)
    , pin = ccao::pin_format_pretty(pin, full_length = T)
    , sale_date = as.Date(sale_date)
    , sale_price = as.numeric(sale_price)
    , year = as.numeric(year)
  ) %>%
  # according to ccao, this recreates the filters for sales ratio studies
  # https://github.com/ccao-data/wiki/blob/master/SOPs/Sales-Ratio-Studies.md#exclusion-criteria
  dplyr::filter(
    !sale_filter_same_sale_within_365
    & !sale_filter_less_than_10k
    & !sale_filter_deed_type 
    & !is_multisale
    & sale_price >= 10000
    & !is.na(deed_type)
    & deed_type != "Other"
  ) %>%
  # Do we have a way to make sure we have perfectly re-created the sales criteria the office uses?
  dplyr::select(pin, year, sale_date, sale_price)

write_parquet(sales, here::here("cc_appeals", "big data", "sales_sample.parquet")) 

rm(sales)