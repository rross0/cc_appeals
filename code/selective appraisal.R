# Selective appraisal, sometimes called sales chasing, is the practice
# of setting assessed values at a recent sale price
# This practice runs against IAAO guidelines for mass appraisal
# See page 11 https://www.iaao.org/media/standards/Standard_on_Ratio_Studies.pdf 
# also see the CCAO's vignette
# https://ccao-data.github.io/assessr/articles/example-ratio-study.html#detecting-selective-appraisals

# Visual detection of sales chasing
read_parquet(here::here("cc_appeals", "big data", "ratios.parquet")) %>%
  dplyr::filter(
    year == 2022 
    & stage ==  "Mailed by Assessor") %>%
ggplot(aes(x = ratio, color = sale)) +
  stat_ecdf(color = MyColours) +
  stat_ecdf(aes(certified_ratio), ) +
  stat_ecdf(aes(board_ratio), color = as.character(ccao::ccao_colors[3])) +
  geom_vline(xintercept = 0.98) +
  geom_vline(xintercept = 1.02) +
  xlim(0.8, 1.2) +
  labs(x = "Ratio", y = "F(x)") +
  theme_minimal()

# Find some chased examples 
chased_ratios <- ratios %>%
  dplyr::filter(year == 2022 & (mailed_ratio == 1 | board_ratio == 1)) %>%
  dplyr::select(pin, mailed_ratio, board_ratio)

chased_sales <-read_parquet(here::here("cc_appeals", "big data", "sales_sample.parquet")) %>%
  inner_join(chased_ratios)

