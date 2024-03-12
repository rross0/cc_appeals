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

#---- Visualize ----
a <- sales %>%
  dplyr::filter(township == "Rogers Park" & year >= 2015 & sale_price <= 2*10^6) %>%
  dplyr::mutate(
    sale_type = "Real Sale Data"
    , sale_week = round(as.numeric(difftime(sale_date, as.Date("2015/01/01"), units = "weeks")),0)
  )

b <- a %>%
  dplyr::select(sale_price, sale_week, township_code) %>%
  dplyr::mutate(
    sale_type = "Real Sale Data"
  ) %>%
  dplyr::group_by(sale_week, sale_type) %>%
  dplyr::summarise(
    mean_sale_price = mean(sale_price)
    , sales = n()
  )

c <- a %>%
  dplyr::select(yhat, sale_week) %>%
  dplyr::mutate(
    sale_type = "Yhat"
  ) %>%
  dplyr::rename(sale_price = yhat)  %>%
  dplyr::group_by(sale_week, sale_type) %>%
  dplyr::summarise(
    mean_sale_price = mean(sale_price)
  )

ggplot() + 
  geom_point(data = b
              , aes(x = sale_week, y = mean_sale_price, color = sale_type)) +
  geom_smooth(data = b
             , aes(x = sale_week, y = mean_sale_price, color = sale_type, weight = sales)) +
  geom_point(data = c
              , aes(x = sale_week, y = mean_sale_price, color = sale_type))

rm(a,b,c)

# Lets look at the differences between sale
a <- sales %>%
  dplyr::select(contains("time_difference"), pin, sale_date) %>%
  pivot_longer(
    cols = time_difference_2:time_difference_4
    , values_to = "time_difference"
    , names_to = "time") %>%
  dplyr::mutate(
    time = gsub("[^0-9.-]", "", time)
  )

b <- sales %>%
  dplyr::select(contains("time_adjustment"), pin, sale_date) %>%
  pivot_longer( 
    cols = time_adjustment_2:time_adjustment_4
    , values_to = "time_adjustment"
    , names_to = "time") %>%
  dplyr::mutate(
    time = gsub("[^0-9.-]", "", time)
  )

g1 <- inner_join(a, b) %>%
  dplyr::group_by(time_difference, time) %>%
  dplyr::summarise(
    mean_adjustment = mean(time_adjustment)
  ) %>%
  dplyr::mutate(
    Adjustment = case_when(
      time == 2 ~ "To lien date of reassessment year"
      , time == 3 ~ "To Jan 1 prior year"
      , time == 4 ~ "To Jan 1 following year"
    )
  ) %>%
  ggplot(aes(x = time_difference, y = mean_adjustment, colour = time)) +
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c(MyColours)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Mean adjustment factor for sales by time difference") +
  ylab("Adjustment ratio") +
  xlab("Time difference in days from sale date to adjustment date")

jpeg(file=here::here("cc_appeals", "outputs", "plot1.sales.jpeg"), width = 980, height = 760)
g1
dev.off()

rm(a, b)
#---- Single Property Example ----
table1_sales <- sales %>%
  dplyr::filter(pin =="01-01-108-016-0000") %>%
  dplyr::select(sale_price, sale_date, adjusted_sale_price_2, sale_date_2
                , adjusted_sale_price_3, sale_date_3
                ,adjusted_sale_price_4, sale_date_4)

save(table1_sales, file = here::here("cc_appeals", "outputs", "table1-sales.Rda"))

load(here::here("cc_appeals", "outputs", "table1-sales.Rda"))

# ---- Save -----
# Save the sales data
write_parquet(sales, here::here("cc_appeals", "big data", "sales_sample.parquet")) 
# Save the model
saveRDS(lm, file = here::here("cc_appeals", "big data", "sales_adjustment_model.rda"))

rm(sales)