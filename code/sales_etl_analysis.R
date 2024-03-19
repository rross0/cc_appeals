# This file analyses the sales adjustment process

sales <- read_parquet(here::here("cc_appeals", "big data", "sales_sample.parquet")) 

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