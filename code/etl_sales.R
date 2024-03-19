# This file determines which sales to use in the analysis

#---- Sales Sample ----
# https://github.com/rross0/cc_appeals/issues/1
sales <- read_parquet(here::here("cc_appeals", "big data", "sales.parquet")) %>%
  dplyr::mutate(
    dplyr::across(sale_filter_same_sale_within_365:sale_filter_deed_type, ~ as.logical(.x))  # Proper encoding
    , is_multisale = as.logical(is_multisale)
    , pin = ccao::pin_format_pretty(pin, full_length = T)
    , sale_date = as.Date(sale_date)
    , sale_price = as.numeric(sale_price)
    , year = as.numeric(year)
  ) %>%
  dplyr::group_by(pin) %>%
  dplyr::arrange(sale_date) %>%
  dplyr::mutate(
    sale_no_in = row_number() #We will need to identify properties with multiple sales
  )  %>%
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
  dplyr::select(pin, year, sale_date, sale_price) %>%
  ungroup()


# ---- Time adjustment ----
# See https://github.com/rross0/cc_appeals/issues/6 

locations <- read_parquet(here::here("cc_appeals", "big data", "parcel_triads.parquet")) %>%
  dplyr::filter(pin != "TEST000000001") %>%
  dplyr::mutate(pin = ccao::pin_format_pretty(pin, full_length = T))

sales <- inner_join(sales, locations, relationship = "many-to-one") %>%
  dplyr::mutate(
    sale_date = as.Date(sale_date)
  )

rm(locations)

# Model
lm <- lm(
  formula = sale_price ~ poly(sale_date, degree = 5, raw = TRUE ):township_code + township_code - 1
  , data = sales
  )

# Prediction at the sale date
sales <- sales %>%
  dplyr::mutate(
      sale_date_original = sale_date # Save the original sale date
    , yhat = predict(lm, newdata = sales) # Predict mean sale price for the township at each sale date
    , adjustment_ratio = sale_price / yhat # Calculate the adjustment factor
    , sale_date_2 = #Choose a sale date at the lien date
      as.Date(
        paste0(as.character(lubridate::year(sale_date)), "/01", "/01")  #lien date same year
    )
    , sale_date = sale_date_2 #Replace sale date
  )
# Prediction at lien date, same year
sales <- sales %>%
  dplyr::mutate(
      yhat2 = predict(lm, newdata = sales) #Predict the man sale price at the first of the year
    , adjusted_sale_price_2 = yhat2 * adjustment_ratio #Apply the adjustment factor
    , time_adjustment_2 = adjusted_sale_price_2 / sale_price #Included for later illustration
    , time_difference_2 = difftime(sale_date_2, sale_date_original, units = "days")#Included for later illustration
    , sale_date_3 = #Choose a sale date at the lien date
      as.Date(
        paste0(as.character(lubridate::year(sale_date) - 1), "/01", "/01")#lien date prior year
      ) 
    , sale_date = sale_date_3 #Replace sale date
  )

# Prediction at lien date, prior year
sales <- sales %>%
  dplyr::mutate(
    yhat3 = predict(lm, newdata = sales) #Predict the man sale price at the first of the prior year
    , adjusted_sale_price_3 = yhat3 * adjustment_ratio #Apply the adjustment factor
    , time_adjustment_3 = adjusted_sale_price_3 / sale_price #Included for later illustration
    , time_difference_3 = difftime(sale_date_3, sale_date_original, units = "days")#Included for later illustration
    , sale_date_4 = #Choose a sale date at the lien date
      as.Date(
        paste0(as.character(lubridate::year(sale_date) + 2), "/01", "/01") #lien date succeeding year
      )
    , sale_date = sale_date_4 #Replace sale date
  )

# Prediction at lien date, succeeding year
sales <- sales %>%
  dplyr::mutate(
    yhat4 = predict(lm, newdata = sales) #Predict the man sale price at the first of the succeeding year
    , adjusted_sale_price_4 = yhat4 * adjustment_ratio #Apply the adjustment factor
    , time_adjustment_4 = adjusted_sale_price_4 / sale_price #Included for later illustration
    , time_difference_4 = difftime(sale_date_4, sale_date_original, units = "days")#Included for later illustration
  )

# Re-set the sale_date to original value
sales <- sales %>%
  dplyr::mutate(
    sale_date = sale_date_original #Replace sale date with the original data
    , township = ccao::town_convert(township_code)
  ) %>%
  dplyr::select(-sale_date_original) 

# ---- Save -----
# Save the sales data
write_parquet(sales, here::here("cc_appeals", "big data", "sales_sample.parquet")) 
save(sales, file = here::here("cc_appeals", "big data", "sales_sample.rda"))

# Save the model
saveRDS(lm, file = here::here("cc_appeals", "big data", "sales_adjustment_model.rda"))

rm(sales, lm)