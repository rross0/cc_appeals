# This file creates a data set of sales ratios for assessment quality analysis
# See the CCAO's vignette on ratio studies
# https://github.com/ccao-data/assessr/blob/master/vignettes/example-ratio-study.Rmd

ratios <-
  read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  # Here I am making an impactful choice to include sales in the year of 
  # re-assessment as well as the year before the reassessment
  # Remove properties without sales
  dplyr::filter(
    !is.na(sale_price) | 
    !is.na(prior_year_sale) |
    !is.na(next_year_sale)
    ) %>% 
  dplyr::select(pin, year, triad_name, sale_price, prior_year_sale, next_year_sale
                , mailed_tot, certified_tot, board_tot) %>%
  dplyr::mutate(
    sale_used = case_when(
      !is.na(sale_price) ~ "current_year"
      , is.na(sale_price) & !is.na(prior_year_sale) ~  "prior_year"
      , is.na(sale_price) & is.na(prior_year_sale) ~ "next_year"
    )
    , sale_price = case_when(
      sale_used == "current_year" ~ sale_price
      , sale_used == "prior_year" ~ prior_year_sale
      , sale_used == "next_year"  ~ next_year_sale
    )
   , mailed_ratio = mailed_tot * 10 / sale_price
   , certified_ratio = certified_tot * 10 / sale_price
   , board_ratio =  board_tot * 10 / sale_price
) %>%
  dplyr::select(pin, year, triad_name, sale_price, contains("ratio"), contains("_tot"))
  
write_parquet(ratios, here::here("cc_appeals", "big data", "ratios.parquet"))

rm(ratios)

