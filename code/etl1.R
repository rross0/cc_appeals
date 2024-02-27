# This file transforms the downloaded data from the CCAO
# and don't give me guff about the filename

# Define the classes we want
# https://github.com/ccao-data/model-res-avm#primary-data
classes <- c("202", "203", "204", "205", "206", "207", "208", "209", "210", "234", "278", "295")

# Select desired fields from both data
assessments <- read_parquet(here::here("cc_appeals", "big data", "assessments.parquet")) %>%
  dplyr::select(pin, tax_year, class, mailed_tot, certified_tot, board_tot) %>%
  # dplyr::filter(between(as.numeric(class), 200, 299)) %>% # restrict analysis to residential classes
  dplyr::mutate(year = as.numeric(tax_year)) %>% # Rename so matches other data name
  dplyr::filter(class %in% classes) # Keep only homes

triads <- read_parquet(here::here("cc_appeals", "big data", "parcel_triads.parquet")) %>%
  filter(!is.na(triad_name)) %>%# There are a couple of errors in the triad_name field in CCAO's data
  dplyr::group_by(pin) %>%
  dplyr::summarise(
    triad_name = first(triad_name)
  )

appeals <- read_parquet(here::here("cc_appeals", "big data", "appeals.parquet")) %>%
  dplyr::select(pin, year, case_no, class) %>%
  dplyr::mutate(year = as.numeric(year)) 

# For some reason, some PINs are associated with multiple docket numbers in the same year
# Example:
dplyr::filter(appeals, pin %in% c('01011080300000', '03294110580000'))  #¯\_(ツ)_/¯

appeals <- appeals %>%
  dplyr::group_by(pin, year) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(
    appeal_number = order(order(case_no))
  ) %>%
  dplyr::filter(appeal_number == 1) %>% # Remove all but one appeal docket in each year
  dplyr::select(-appeal_number)

# Restrict the analysis to 'homes' - residential properties with a structure
# Some dockets included both res and non res PINs. Want to ID 'mixed' dockets
appeals <- appeals %>%
  dplyr::filter(!(class %in% classes) & !is.na(case_no)) %>% # Identify the cases that involve non-res
  dplyr::distinct(case_no) %>%
  dplyr::mutate(docket_contains_nonres = 1) %>% # Indicate them
  right_join(appeals, relationship = "one-to-many") %>% #Join indicator back into the data
  dplyr::select(-class)

residential_assessments <- assessments %>%
  left_join(appeals, relationship = "one-to-one") %>% # Combine appeal and assessment data
  inner_join(triads, relationship = "many-to-one") %>% # ID triad for each PIN
  dplyr::group_by(pin) %>% # Want to calculate year of year changes in AV
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
  ) 
b <- nrow(subset(appeals, !is.na(case_no)))
c <- nrow(subset(residential_assessments, !is.na(case_no)))

# Want to make sure all appeals match to an assessment
if(a == b){ print("good join")}else{print("Bad join")}; rm(appeals)
(a - b)/a 

rm(assessments, classes, triads, a, b, c)

# Include sales data
sales <- read_parquet(here::here("cc_appeals", "big data", "sales.parquet")) %>%
  dplyr::mutate(
    dplyr::across(sale_filter_same_sale_within_365:sale_filter_deed_type, ~ as.logical(.x))  # Proper encoding
    , is_multisale = as.logical(is_multisale)
    , pin = ccao::pin_format_pretty(pin, full_length = T)
    , sale_date = as.Date(sale_date)
    , sale_price = as.numeric(sale_price)
    , year = as.numeric(year)
  ) %>%
  # according to ccao, this recreates the filters for sales ratio studies
  # https://github.com/ccao-data/wiki/blob/master/SOPs/Sales-Ratio-Studies.md#exclusion-criteria
  dplyr::filter(
    !sale_filter_same_sale_within_365
    & !sale_filter_less_than_10k
    & !sale_filter_deed_type 
    & !is_multisale
    & sale_price >= 10000
    & !is.na(deed_type)
    & deed_type != "Other"
  ) %>%
  # Do we have a way to make sure we have perfectly re-created the sales criteria the office uses?
  dplyr::select(pin, year, sale_date, sale_price)

# We want to look at properties that have a sale in the year prior to appeal
# and properties that have a sale in the year after the appeal
# to simplify matters, I omit properties with multiple sales in any given year
# from the sales sample
sales <- sales %>%
  group_by(pin, year) %>%
  dplyr::summarise(
    sales = n()
  ) %>%
  dplyr::filter(sales == 1) %>%
  inner_join(sales, relationship = "one-to-one") 

residential_assessments <- residential_assessments %>%
   left_join(sales, relationship = "one-to-one") %>%
  dplyr::group_by(pin) %>%
  dplyr::arrange(year) %>%
  # Calculate lagged and leading sales
  dplyr::mutate(
    prior_year_sale = lag(sale_price, n = 1L)
    , next_year_sale = lead(sale_price, n = 1)
  )
  
rm(sales)

# Taxpayers can only appeal once every three years. We want to isolate the data
# to those years for those taxpayers
# Define the years and geographies we want. 2005 was a south triad year
years <- data.frame(
  year = seq(2005, 2024, 1)
  , triad_name = c("South", "City", "North", "South", "City", "North", "South", "City", "North", 
                   "South", "City", "North", "South", "City", "North", "South", "City", "North",
                   "South", "City")
)  

# Create some helpful flags for reductions
residential_assessments <- residential_assessments %>%
  # Taxpayers can only appeal once every three years. We want to isolate the data
  # to those years for those taxpayers
  inner_join(years, relationship = "many-to-one") %>% # Only include assessments in each triad during a re-assessment year
  dplyr::mutate(
    docket_contains_nonres = (docket_contains_nonres == 1)
    , ccao_reduction_flag = (certified_tot < mailed_tot) & !is.na(case_no) 
    , ccao_reduction_amount = (mailed_tot - certified_tot)
    , ccao_reduction_pct = (mailed_tot - certified_tot)/mailed_tot
    , board_reduction_flag = (board_tot < certified_tot)
    , board_reduction_amount = (certified_tot - board_tot)
    , board_reduction_pct = (certified_tot - board_tot)/certified_tot
    , appeal = (!is.na(case_no))
 ) 
  

# Save the data
write_parquet(
  residential_assessments
  , here::here("cc_appeals", "big data", "residential_assessments.parquet"))

rm(residential_assessments, years)