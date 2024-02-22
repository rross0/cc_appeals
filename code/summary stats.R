# This file reports summary and trend statistics for appeals

a <- read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    appeals = sum(appeal)
    , appeal_rate = mean(appeal)
    , ccao_reductions = sum(ccao_reduction_flag)
    , ccao_rate = mean(ccao_reduction_flag)
    , board_reductions = sum(board_reduction_flag)
    , board_rate = mean(board_reduction_flag)
  )

a %>%
  ggplot(aes(x = year, y = ccao_rate)) +
  geom_bar(stat = 'identity')

a %>%
  ggplot(aes(x = year, y = appeal_rate)) +
  geom_bar(stat = 'identity')