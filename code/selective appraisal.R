# Selective appraisal, sometimes called sales chasing, is the practice
# of setting assessed values at a recent sale price
# This practice runs against IAAO guidelines for mass appraisal
# See page 11 https://www.iaao.org/media/standards/Standard_on_Ratio_Studies.pdf 
# also see the CCAO's vignette
# https://ccao-data.github.io/assessr/articles/example-ratio-study.html#detecting-selective-appraisals

ratios <- read_parquet(here::here("cc_appeals", "big data", "ratios.parquet"))  %>%
  dplyr::filter(year == 2022 & between(mailed_ratio, 0, 3) & is.finite(mailed_ratio)) 


# Visual detection of sales chasing
ratios %>%
ggplot(aes(mailed_ratio)) +
  stat_ecdf(color = ccao::ccao_colors[1]) +
  geom_vline(xintercept = 0.98) +
  geom_vline(xintercept = 1.02) +
  xlim(0.7, 1.3) +
  labs(x = "Ratio", y = "F(x)") +
  theme_minimal()