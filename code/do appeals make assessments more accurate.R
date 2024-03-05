# This file looks at whether appeals move assesments closer to market values
# See issue 2: https://github.com/rross0/cc_appeals/issues/2

a <- read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  dplyr::filter( #get appeals with a sale in the year following appeal
    appeal & !is.na(next_year_sale) & between(year, 2018, 2023)
  ) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(
    mailed_pct = mailed_tot * 10 / next_year_sale
    , mailed_pct_tile = ntile(mailed_pct, n = 10)
    , year = factor(year)
    , assessment_accuracy = case_when(
        mailed_pct < 0.9 ~ "Under-assessed"
      , mailed_pct >= 0.9 & mailed_pct <= 1.1 ~ "Assessed accurately" 
      , mailed_pct > 1.1 ~ "Over-assessed" 
    )
  ) %>%
  dplyr::filter(between(mailed_pct, -1, 2))

b <- read_parquet(here::here("cc_appeals", "big data", "residential_assessments.parquet")) %>%
  dplyr::filter( #get appeals with a sale in the year following appeal
    appeal & !is.na(prior_year_sale) & between(year, 2018, 2023)
  ) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(
    mailed_pct = round((mailed_tot * 10) / prior_year_sale, 1)
    , mailed_pct_tile = ntile(mailed_pct, n = 10)
    , year = factor(year)
    , assessment_accuracy = case_when(
      mailed_pct < 0.9 ~ "Under-assessed"
      , mailed_pct >= 0.9 & mailed_pct <= 1.1 ~ "Assessed accurately" 
      , mailed_pct > 1.1 ~ "Over-assessed" 
    )
  ) %>%
  dplyr::filter(between(mailed_pct, -1, 2))

# 2022 is really weird because the COVID adjustment meant that basically nobody was 
# over-assessed

a %>%
  ggplot(aes(x = mailed_pct)) + geom_density()

# Graph data using sales in following years
gd1.1 <- a %>% 
  dplyr::group_by(year, assessment_accuracy) %>%
  dplyr::summarise(
    properties = n()
    , ccao_win_rate = mean(ccao_reduction_flag)
  ) 
gd1.2 <- a %>% 
  dplyr::group_by(year, assessment_accuracy, ccao_reduction_flag) %>%
  dplyr::summarise(
    properties = n()
    , mean_reduction_pct = mean(ccao_reduction_pct)
  ) 
# Create graph data using sales in preceding years
gd1.3 <- b %>% 
  dplyr::group_by(year, assessment_accuracy) %>%
  dplyr::summarise(
    properties = n()
    , ccao_win_rate = mean(ccao_reduction_flag)
  ) 
gd1.4 <- b %>% 
  dplyr::group_by(year, assessment_accuracy, ccao_reduction_flag) %>%
  dplyr::summarise(
    properties = n()
    , mean_reduction_pct = mean(ccao_reduction_pct)
  ) 

g1 <- gd1.1 %>%
  ggplot(aes(y = ccao_win_rate, x = year
             , colour = assessment_accuracy , fill = assessment_accuracy )) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_minimal() +
  coord_cartesian(ylim=c(0.2,0.7)) +
  scale_fill_manual(values = c(MyColours)) +
  scale_color_manual(values = c(MyColours)) +
  labs(
    title = "Appeal Results Compared to Sales in Succeeding Year"
    , subtitle =  "Appeal Success Rate"
    , color =  "Assessment Accuracy"
    , fill =  "Assessment Accuracy"
  ) +
  ylab("Success rate") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.position = 'bottom') 

g2 <- gd1.2 %>%
  dplyr::filter(ccao_reduction_flag) %>%
  ggplot(aes(y = mean_reduction_pct, x = year
             , colour = assessment_accuracy , fill = assessment_accuracy )) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_minimal() +
  coord_cartesian(ylim=c(0.05,0.2)) +
  scale_fill_manual(values = c(MyColours)) +
  scale_color_manual(values = c(MyColours)) +
  labs(
      subtitle = "Mean Reduction on Appeal"
    , caption = "Data from Cook County Assessors' Open Data Assets."
    , color =  "Assessment Accuracy"
    , fill =  "Assessment Accuracy"
  ) +
  ylab("Reduction in AV") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.position = 'bottom') 

g3 <- ggpubr::ggarrange(g1, g2, ncol = 2, nrow = 1
                        , common.legend = TRUE
                        , legend="bottom")
                        
# Save the Plot
jpeg(file=here::here("cc_appeals", "outputs", "plot1.jpeg"), width = 980, height = 760)
g3
dev.off()


g4 <- gd1.3 %>%
  ggplot(aes(y = ccao_win_rate, x = year
             , colour = assessment_accuracy , fill = assessment_accuracy )) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_minimal() +
  coord_cartesian(ylim=c(0.2,0.7)) +
  scale_fill_manual(values = c(MyColours)) +
  scale_color_manual(values = c(MyColours)) +
  labs(
    title = "Appeal Results Compared to Sales in Preceding Year"
    , subtitle =  "Appeal Success Rate"
    , color =  "Assessment Accuracy"
    , fill =  "Assessment Accuracy"
  ) +
  ylab("Success rate") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.position = 'bottom') 

g5 <- gd1.4 %>%
  dplyr::filter(ccao_reduction_flag) %>%
  ggplot(aes(y = mean_reduction_pct, x = year
             , colour = assessment_accuracy , fill = assessment_accuracy )) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_minimal() +
  coord_cartesian(ylim=c(0.05,0.2)) +
  scale_fill_manual(values = c(MyColours)) +
  scale_color_manual(values = c(MyColours)) +
  labs(
    subtitle = "Mean Reduction on Appeal "
    , caption = "Data from Cook County Assessors' Open Data Assets."
    , color =  "Assessment Accuracy"
    , fill =  "Assessment Accuracy"
  ) +
  ylab("Reduction in AV") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.position = 'bottom') 

g6 <- ggpubr::ggarrange(g4, g5, ncol = 2, nrow = 1
                        , common.legend = TRUE
                        , legend="bottom")

# Save the Plot
jpeg(file=here::here("cc_appeals", "outputs", "plot2.jpeg"), width = 980, height = 760)
g6
dev.off()



# More detailed view
gd2 <- a %>%
  dplyr::group_by(mailed_error_pct_tile, year) %>%
  dplyr::summarise(
    win_rate = mean(ccao_reduction_flag)
    , mean_reduction_pct = mean(ccao_reduction_pct)
  ) 

gd2 %>%
  ggplot(aes(x = mailed_error_pct_tile, y = win_rate, colour  = year)) +
  geom_point() + geom_line()

gd3 <- a %>%
  dplyr::filter(year %in% c(seq(2018, 2022, 1))) %>%
  dplyr::group_by(mailed_error_pct, year) %>%
  dplyr::summarise(
    win_rate = mean(ccao_reduction_flag)
    , mean_reduction_pct = mean(ccao_reduction_pct)
  ) 

gd3 %>%
  ggplot(aes(x = mailed_error_pct, y = win_rate, colour  = year)) +
  geom_point() + geom_line()


# Do over-assessed homes win? Using prior year sales as the rubric for over-assessed
gd2 <- b %>% 
  dplyr::group_by(year, group) %>%
  dplyr::summarise(
    properties = n()
    , ccao_win_rate = mean(ccao_reduction_flag)
    , mean_reduction_pct = mean(ccao_reduction_pct)
  ) 

gd2 %>%
  ggplot(aes(y = ccao_win_rate, x = year, colour = group, fill = group)) +
  geom_bar(stat = 'identity', position = 'dodge')


# Looking at win rates by over, under assessment more granularly   
gd3 <- a %>%
  dplyr::filter(year %in% c(seq(2018, 2022, 1))) %>%
  dplyr::group_by(mailed_error_pct_tile, year) %>%
  dplyr::summarise(
    win_rate = mean(ccao_reduction_flag)
    , mean_reduction = mean()
  ) 

gd3 %>%
  ggplot(aes(x = mailed_error_pct_tile, y = win_rate, colour  = year)) +
     geom_point() + geom_line()

a %>%
  dplyr::filter(year %in% c(seq(2018, 2022, 1))) %>%
  dplyr::group_by(mailed_error_pct, year) %>%
  dplyr::summarise(
    win_rate = mean(ccao_reduction_flag)
  ) %>%
  ggplot(aes(x = mailed_error_pct, y = win_rate, colour  = year)) +
  geom_point() + geom_line()

# Do over-assessed homes win more?

a %>%
  dplyr::filter(year == 2022) %>%
  dplyr::group_by(mailed_error_pct_tile) %>%
  dplyr::summarise(
    win_rate = mean(ccao_reduction_flag)
    , win_amount = mean(ccao_reduction_pct)
  ) %>%
  ggplot(aes(x = mailed_error_pct_tile, y = win_rate)) +
  geom_point() + geom_smooth()