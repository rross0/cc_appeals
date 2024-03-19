# Selective appraisal, sometimes called sales chasing, is the practice
# of setting assessed values at a recent sale price
# This practice runs against IAAO guidelines for mass appraisal
# See page 11 https://www.iaao.org/media/standards/Standard_on_Ratio_Studies.pdf 
# also see the CCAO's vignette
# https://ccao-data.github.io/assessr/articles/example-ratio-study.html#detecting-selective-appraisals

# Visual detection of sales chasing
read_parquet(here::here("cc_appeals", "big data", "ratios.parquet")) %>%
  dplyr::filter(
    year == 2017
    & stage == "Certified by Board of Reivew") %>%
ggplot(aes(x = ratio, color = sale)) +
  stat_ecdf() +
  geom_vline(xintercept = 0.98) +
  geom_vline(xintercept = 1.02) +
  xlim(0.95, 1.1) +
  labs(x = "Ratio", y = "F(x)") +
  theme_minimal() +
  theme(legend.position = "bottom") 

read_parquet(here::here("cc_appeals", "big data", "ratios.parquet")) %>%
  dplyr::filter(
    year == 2022
    & between(ratio, .5, 1.5)
  ) %>%
  ggplot(aes(x = ratio, color = sale)) +
  geom_histogram() +
  facet_grid(
    rows = vars(stage)
    , cols = vars(sale))

# Find some chased examples 
chased_ratios <- read_parquet(here::here("cc_appeals", "big data", "ratios.parquet")) %>%
  dplyr::filter(year == 2022 & ratio == 1 ) 

