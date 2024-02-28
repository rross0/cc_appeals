# This file looks at whether appeals makes the system better

# Do appeals decisions make assessments more accurate?

a <- read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  dplyr::filter(
    (appeal & !is.na(prior_year_sale))
    | (appeal & !is.na(next_year_sale))
  )