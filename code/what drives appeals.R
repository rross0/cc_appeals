# This file looks at some factors that may influence appeals:
# What influences people to appeal?
# What influences whether people win their appeal?
# What influences how much people win on appeal?

# ---- Property Value ----
# Property's initial assessed value
a <- read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(
    av_ntile = ntile(mailed_tot, 10)
  ) %>%
  dplyr::group_by(av_ntile, year) %>%
  dplyr::summarise(
    appeals = sum(appeal)
    , appeal_rate = mean(appeal)
    , ccao_reductions = sum(ccao_reduction_flag)
    , board_reductions = sum(board_reduction_flag)
  ) %>%
  dplyr::mutate(
    year = factor(year)
  ) 

# Do more expensive homes appeal more?
a %>%
  ggplot(aes(x = av_ntile, y = appeal_rate, color = year)) +
  geom_line() + geom_point()

# Yes

# Do more expensive homes win more often?
b <- read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(
    av_ntile = ntile(mailed_tot, 10)
  ) %>%
  dplyr::group_by(av_ntile, year) %>%
  dplyr::filter(appeal) %>% # Conditional on appealing 
  dplyr::summarise(
    ccao_win_rate = mean(ccao_reduction_flag)
    , board_win_rate = mean(board_reduction_flag)
  ) %>%
  dplyr::mutate(
    year = factor(year)
  ) 

b %>%
  ggplot(aes(x = av_ntile, y = ccao_win_rate, color = year)) +
  geom_line() + geom_point()

b %>%
  ggplot(aes(x = av_ntile, y = board_win_rate, color = year)) +
  geom_line() + geom_point()


# Yes

# Do more expensive homes win larger CCAO reductions?
c <- read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(
    av_ntile = ntile(mailed_tot, 10)
  ) %>%
  dplyr::group_by(av_ntile, year) %>%
  dplyr::filter(ccao_reduction_flag) %>% # Conditional on appealing 
  dplyr::summarise(
    median_ccao_reduction_pct = median(ccao_reduction_pct)
    , median_ccao_reduction_amount = median(ccao_reduction_amount)
  ) %>%
  dplyr::mutate(
    year = factor(year)
  ) 

c %>%
  ggplot(aes(x = av_ntile, y = median_ccao_reduction_pct, color = year)) +
  geom_line() + geom_point()

c %>%
  ggplot(aes(x = av_ntile, y = median_ccao_reduction_amount, color = year)) +
  geom_line() + geom_point()

# higher valued homes win nominally larger reductions, 
# but lower percentages of their overall housing value

# Do more expensive homes win larger CCAO reductions?
d <- read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(
    av_ntile = ntile(mailed_tot, 10)
  ) %>%
  dplyr::group_by(av_ntile, year) %>%
  dplyr::filter(board_reduction_flag) %>% # Conditional on appealing 
  dplyr::summarise(
    median_board_reduction_pct = median(board_reduction_pct)
    , median_board_reduction_amount = median(board_reduction_amount)
  ) %>%
  dplyr::mutate(
    year = factor(year)
  ) 

d %>%
  ggplot(aes(x = av_ntile, y = median_board_reduction_pct, color = year)) +
  geom_line() + geom_point()

d %>%
  ggplot(aes(x = av_ntile, y = median_board_reduction_amount, color = year)) +
  geom_line() + geom_point()

rm(a, b, c, d)

# ---- Value Growth ----
# Increase in a property's assessed value on reassessment
a <- read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(
    yoy_change_av_pct_ntile = ntile(yoy_change_av_pct, 10)
  ) %>%
  dplyr::group_by(yoy_change_av_pct_ntile, year) %>%
  dplyr::summarise(
    appeals = sum(appeal)
    , appeal_rate = mean(appeal)
    , ccao_reductions = sum(ccao_reduction_flag)
    , board_reductions = sum(board_reduction_flag)
  ) %>%
  dplyr::mutate(
    year = factor(year)
  ) 

# Do homes with larger av growth appeal more?
a %>%
  ggplot(aes(x = yoy_change_av_pct_ntile, y = appeal_rate, color = year)) +
  geom_line() + geom_point()
# The bigger the change in your assessment, the more likely you are to appeal

# Do homes with larger av growth win their appeals more often?
b <- read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(
    yoy_change_av_pct_ntile = ntile(yoy_change_av_pct, 10)
  ) %>%
  dplyr::group_by(yoy_change_av_pct_ntile, year) %>%
  dplyr::filter(appeal) %>% # Conditional on appealing 
  dplyr::summarise(
    ccao_win_rate = mean(ccao_reduction_flag)
  ) %>%
  dplyr::mutate(
    year = factor(year)
  ) 

b %>%
  ggplot(aes(x = yoy_change_av_pct_ntile, y = ccao_win_rate, color = year)) +
  geom_line() + geom_point()
# Yes, homes win more often if they have larger increases in AV

