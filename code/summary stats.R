# This file reports summary and trend statistics for appeals
# Just counts and rates and so on

a <- read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    appeals = sum(appeal)
    , appeal_rate = mean(appeal)
    , ccao_reductions = sum(ccao_reduction_flag)
    , board_reductions = sum(board_reduction_flag)
  )
# Stats for appellants
b <- read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  dplyr::group_by(year) %>%
  dplyr::filter(appeal) %>%
  dplyr::summarise(
    ccao_win_rate = mean(ccao_reduction_pct)
    , board_win_rate = mean(board_reduction_amount)
  )

# Stats for CCAO winners
c <- read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  dplyr::group_by(year) %>%
  dplyr::filter(ccao_reduction_flag) %>%
  dplyr::summarise(
    median_ccao_reduction_pct = median(ccao_reduction_pct)
    , median_ccao_reduction = median(ccao_reduction_amount)
)
# Stats for board winners
d <- read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  dplyr::group_by(year) %>%
  dplyr::filter(ccao_reduction_flag) %>%
  dplyr::summarise(
    median_board_reduction_pct = median(ccao_reduction_pct)
    , median_board_reduction = median(ccao_reduction_amount)
  )

# Appeal quantity and rate
a %>%
  ggplot(aes(x = year, y = appeal_rate)) +
  geom_bar(stat = 'identity')
a %>%
  ggplot(aes(x = year, y = appeals)) +
  geom_bar(stat = 'identity')

# CCAO win rate
b %>%
  ggplot(aes(x = year, y = ccao_rate)) +
  geom_bar(stat = 'identity')

#Board win rate
a %>%
  ggplot(aes(x = year, y = board_rate)) +
  geom_bar(stat = 'identity')

# CCAO win amount
b %>%
  ggplot(aes(x = year, y = median_ccao_reduction_pct)) +
  geom_bar(stat = 'identity')

rm(a, b)