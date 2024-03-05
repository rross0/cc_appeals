appeals <- read_parquet(here::here("cc_appeals", "big data", "appeals.parquet"))

appeal_codes <- readxl::read_xlsx(here::here("cc_appeals", "small data", "appeal_codes.xlsx")) 

appeals <- appeals %>%
  inner_join( 
    dplyr::rename(appeal_codes, reason_code1 = VAL2 )
  ) %>%
  dplyr::select(
    pin, year, reason_code1, LONGDESC
  )