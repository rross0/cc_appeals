# This file transforms the downloaded data from the CCAO
# and don't give me guff about the filename

# Define the classes we want
# https://github.com/ccao-data/model-res-avm#primary-data
classes <- c("202", "203", "204", "205", "206", "207", "208", "209", "210", "234", "278", "295")
# Define the years and geographies we want. 2005 was a south triad year
years <- data.frame(
  year = seq(2005, 2024, 1)
  , triad_name = c("South", "City", "North", "South", "City", "North", "South", "City", "North", 
                   "South", "City", "North", "South", "City", "North", "South", "City", "North",
                   "South", "City")
)

# Select desired fields from both data
assessments <- read_parquet(here::here("cc_appeals", "big data", "assessments.parquet")) %>%
  dplyr::select(pin, tax_year, class, mailed_tot, certified_tot, board_tot) %>%
  # dplyr::filter(between(as.numeric(class), 200, 299)) %>% # restrict analysis to residential classes
  dplyr::mutate(year = as.numeric(tax_year)) %>% # Rename so matches other data name
  dplyr::filter(class %in% classes) # Keep only homes

triads <- read_parquet(here::here("cc_appeals", "big data", "parcel_triads.parquet")) %>%
  filter(!duplicated(pin)) # There are a couple of errors in the triad_name field in CCAO's data

appeals <- read_parquet(here::here("cc_appeals", "big data", "appeals.parquet")) %>%
  dplyr::select(pin, year, case_no, class) %>%
  dplyr::mutate(year = as.numeric(year))

# Check the appeals data 
appeals %>%
  inner_join(triads) %>%
  inner_join(years) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    n = n()
  ) %>%
  print(n = 25)

# Restrict the analysis to 'homes' - residential properties with a structure
# Some dockets included both res and non res PINs. Want to ID 'mixed' dockets
appeals <- appeals %>%
  dplyr::filter(!(class %in% classes) & !is.na(case_no)) %>% # Identify the cases that involve non-res
  dplyr::distinct(case_no) %>%
  dplyr::mutate(docket_contains_nonres = 1) %>% # Indicate them
  right_join(appeals) %>%
  dplyr::select(-class)

# Check the appeals data again
appeals %>%
  inner_join(triads) %>%
  inner_join(years) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    n = n()
  ) %>%
  print(n = 25)

nrow(subset(appeals, is.na(case_no))) # No null values of case number
a <- nrow(subset(appeals, !is.na(case_no))) # Number of appeals for checking

residential_assessments <- assessments %>%
  left_join(appeals) %>% # Combine appeal and assessment data
  inner_join(triads) %>% # ID triad for each PIN
  dplyr::group_by(pin, year) %>% # Want to calculate year of year changes in AV
  dplyr::arrange(year) %>% # Want to calculate year of year changes in AV
  dplyr::mutate( 
    # Encode variables properly
      pin = ccao::pin_format_pretty(pin, full_length=T)
    , year = as.numeric(year)
    , class= as.character(class)
    , mailed_tot = as.numeric(mailed_tot)
    , certified_tot = as.numeric(certified_tot)
    , board_tot = as.numeric(board_tot)
    , prev_av = lag(board_tot, n = 1L) # Want to calculate year of year changes in AV
    , yoy_change_av = mailed_tot - prev_av
    , yoy_change_av_pct = (mailed_tot - prev_av)/ prev_av
  ) %>%
  # Taxpayers can only appeal once every three years. We want to isolate the data
  # to those years for those taxpayers
  inner_join(years)  # Only include assessments in each triad during a re-assessment year

b <- nrow(subset(appeals, !is.na(case_no)))
c <- nrow(subset(residential_assessments, !is.na(case_no)))

# Want to make sure all appeals match to an assessment
if(a == b){ print("good join")}else{print("Bad join")}; rm(appeals)
(a - b)/a # I'm going to ignore this small number of missing records

rm(assessments, classes, years, triads, a, b, c)

# Create some helpful flags
residential_assessments <- residential_assessments %>%
  dplyr::mutate(
    docket_contains_nonres = (docket_contains_nonres == 1)
    , ccao_reduction_flag = (certified_tot < mailed_tot) & !is.na(case_no) 
    , ccao_reduction_amount = (mailed_tot - certified_tot)
    , ccao_reduction_pct = (mailed_tot - certified_tot)/mailed_tot
    , board_reduction_flag = (board_tot <- certified_tot)
    , board_reduction_amount = (certified_tot - board_tot)
    , board_reduction_pct = (certified_tot - board_tot)/certified_tot
    , appeal = (!is.na(case_no))
  )

# Save the data
write_parquet(
  residential_assessments
  , here::here("cc_appeals", "big data", "residential_assessments.parquet"))

rm(residential_assessments)