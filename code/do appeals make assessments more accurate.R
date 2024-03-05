# This file looks at whether appeals move assesments closer to market values
# See issue 2: https://github.com/rross0/cc_appeals/issues/2

a <- read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  dplyr::filter( #get appeals with a sale in the year following appeal
    appeal & !is.na(next_year_sale) & year < 2023
  ) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(
    mailed_error_pct = round((mailed_tot * 10 - next_year_sale) / next_year_sale, 1)
    , mailed_error_pct_tile = ntile(mailed_error_pct, n = 20)
    , year = factor(year)
  ) %>%
  dplyr::filter(between(mailed_error_pct, -.75, .75))

b <- read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  dplyr::filter( #get appeals with a sale in the year following appeal
    appeal & !is.na(prior_year_sale) 
  ) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(
    mailed_error_pct = round((mailed_tot * 10 - next_year_sale) / next_year_sale, 1)
    , mailed_error_pct_tile = ntile(mailed_error_pct, n = 20)
    , year = factor(year)
  ) %>%
  dplyr::filter(between(mailed_error_pct, -.75, .75))

a %>%
  ggplot(aes(x = mailed_error_pct)) +
geom_density()

# Do over-assessed homes win?
gd1 <- a %>%
  dplyr::filter(year %in% c(seq(2018, 2022, 1))) %>%
  dplyr::group_by(mailed_error_pct_tile, year) %>%
  dplyr::summarise(
    win_rate = mean(ccao_reduction_flag)
    , mean_reduction = mean()
  ) 

gd1 %>%
  ggplot(aes(x = mailed_error_pct_tile, y = win_rate, colour  = year)) +
     geom_point() + geom_line()

a %>%
  dplyr::filter(year %in% c(seq(2018, 2022, 1))) %>%
  dplyr::group_by(mailed_error_pct, year) %>%
  dplyr::summarise(
    win_rate = mean(ccao_reduction_flag)
  ) %>%
  ggplot(aes(x = mailed_error_pct, y = win_rate, colour  = year)) +
  geom_point() + geom_line()

# Do over-assessed homes win more?

a %>%
  dplyr::filter(year == 2022) %>%
  dplyr::group_by(mailed_error_pct_tile) %>%
  dplyr::summarise(
    win_rate = mean(ccao_reduction_flag)
    , win_amount = mean(ccao_reduction_pct)
  ) %>%
  ggplot(aes(x = mailed_error_pct_tile, y = win_rate)) +
  geom_point() + geom_smooth()