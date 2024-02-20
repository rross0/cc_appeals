# This file transforms the downloaded data from the CCAO
# and don't give me guff about the filename

# Select desired fields from both data
assessments <- read_parquet(here::here("cc_appeals", "big data", "assessments.parquet")) %>%
  dplyr::select(pin, tax_year, class, mailed_tot, certified_tot, board_tot) %>%
  dplyr::filter(between(as.numeric(class), 200, 299)) %>% # restrict analysis to residential classes
  dplyr::rename(year = tax_year) # Rename so matches other data name

appeals <- read_parquet(here::here("cc_appeals", "big data", "appeals.parquet")) %>%
  dplyr::select(pin, year, case_no) 

nrow(subset(appeals, is.na(case_no))) # No null values of case number
a <- nrow(subset(appeals, !is.na(case_no))) 

appeals <- left_join(assessments, appeals) %>%
  dplyr::mutate(
    pin = ccao::pin_format_pretty(pin,full_length=T)
    , year = as.numeric(year)
    , class= factor(class)
    , mailed_tot = as.numeric(mailed_tot)
    , certified_tot = as.numeric(certified_tot)
    , board_tot = as.numeric(board_tot)
  )

b <- nrow(subset(appeals, !is.na(case_no)))

if(a == b){rm(a,b, assessments)}else{print("Bad join")}

