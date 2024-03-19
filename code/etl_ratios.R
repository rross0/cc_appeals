# This file creates a data set of sales ratios for assessment quality analysis
# See the CCAO's vignette on ratio studies
# https://github.com/ccao-data/assessr/blob/master/vignettes/example-ratio-study.Rmd

ratios <-
  read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  dplyr::filter(!is.na(adjusted_sale_price_2)) %>%
  dplyr::group_by(year, triad_name, township) %>%
  dplyr::select(
    pin, mailed_tot, certified_tot, board_tot # Assessments at each stage
    , contains("adjusted_sale_price_")) %>%
  dplyr::mutate(
    mailed_ratio_currentyr = mailed_tot / adjusted_sale_price_2
    , certified_ratio_currentyr = certified_tot / adjusted_sale_price_2
    , board_ratio_currentyr = board_tot / adjusted_sale_price_2
    # Careful here. adjusted 4 estimated sale price in the future of the sale
    # So matching to assessment gives you a sale price from the past, 
    # adjusted forward
    , mailed_ratio_prioryr = mailed_tot / adjusted_sale_price_4 
    , certified_ratio_prioryr = certified_tot / adjusted_sale_price_4
    , board_ratio_prioryr = board_tot / adjusted_sale_price_4
    # adjustment three predicted a sale price backwards. 
    # so matching gives you a future year's sale
    , mailed_ratio_nextyr = mailed_tot / adjusted_sale_price_3
    , certified_ratio_nextyr = certified_tot / adjusted_sale_price_3
    , board_ratio_nextyr = board_tot / adjusted_sale_price_3
  ) %>%
  dplyr::select(pin, year, triad_name, township, contains("ratio"), contains("adjusted_sale_price_")) 

a <- ratios %>%
  dplyr::select(pin, year, triad_name, township, contains("ratio")) %>%
  pivot_longer(
    cols = mailed_ratio_currentyr:board_ratio_nextyr
    , values_to = "ratio"
    , names_to = "name"
  ) %>%
  dplyr::mutate(
    stage = case_when(
      grepl("mailed", name) ~ "Mailed by Assessor"
      , grepl("certified", name) ~ "Certified by Assessor"
      , grepl("board", name) ~ "Certified by Board of Reivew"
    )
    , sale = case_when(
      grepl("currentyr", name)  ~ "Reassessment year"
      , grepl("prioryr", name) ~ "Year prior to reassessment year"
      , grepl("nextyr", name) ~ "Year following reassessment year"
    )
  ) %>%
  dplyr::select(-name) %>%
  dplyr::ungroup()

b <- ratios %>%
  dplyr::select(pin, year, triad_name, township, contains("adjusted_sale_price")) %>%
  pivot_longer(
    cols = adjusted_sale_price_2:adjusted_sale_price_4
    , values_to = "price"
    , names_to = "name"
  ) %>%
  dplyr::mutate(
    sale = case_when(
      grepl("_2", name)  ~ "Reassessment year"
      , grepl("_4", name) ~ "Year prior to reassessment year"
      , grepl("_3", name) ~ "Year following reassessment year"
    )
  ) %>%
  dplyr::select(-name) %>%
  dplyr::ungroup() 

ratios <- inner_join(a, b, relationship = "many-to-one")

rm(a, b)

write_parquet(ratios, here::here("cc_appeals", "big data", "ratios.parquet"))

rm(ratios)

