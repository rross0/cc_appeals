appeals <- read_parquet(here::here("cc_appeals", "big data", "appeals.parquet"))

appeal_codes <- readxl::read_xlsx(here::here("cc_appeals", "small data", "appeal_codes.xlsx")) 

appeals <- appeals %>%
  inner_join( 
    dplyr::rename(appeal_codes, reason_code1 = VAL2 )
  ) %>%
  dplyr::select(
    pin, year, reason_code1, LONGDESC
  )

# What are the appeal reasons?

reasons <- appeals %>%
  group_by(reason_code1, LONGDESC, SHORTDESC, year) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(grepl("characteristics", LONGDESC))

# 2A, 2

appeals %>%
  dplyr::filter(
    reason_code1 %in% c('2A', '2') & year >= 2010
  ) %>%
  group_by(year) %>%
  dplyr::summarise(reductions = n()) %>%
  ggplot(aes(x = year, y = reductions)) +
  geom_bar(stat = 'identity') +
  labs(title = "Successful Residential Appeals", subtitle = "Resulting from Characteristic Errors")