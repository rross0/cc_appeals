# This file looks at whether appeals makes the system better

# Do appeals decisions make assessments more accurate?
# See issue 2: https://github.com/rross0/cc_appeals/issues/2

a <- read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  dplyr::filter(
    (appeal & !is.na(prior_year_sale))
    | (appeal & !is.na(next_year_sale))
  )