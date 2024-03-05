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

#---- Categorical -----
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

# Combine
g6 <- ggpubr::ggarrange(g4, g5, ncol = 2, nrow = 1
                        , common.legend = TRUE
                        , legend="bottom")

# Save the Plot
jpeg(file=here::here("cc_appeals", "outputs", "plot2.jpeg"), width = 980, height = 760)
g6
dev.off()

rm(g1, g2,g3,g4,g5,g6,gd1.1, gd1.2,gd1.3,gd1.4)

#---- Detailed-----
# More detailed view: plot win rates and amounts of pct deviation
gd1.1 <- a %>%
  dplyr::group_by(mailed_pct, year) %>%
  dplyr::mutate(
    mailed_pct = round(mailed_pct, 1)
    , mailed_pct = case_when( # Top code for visiual
        mailed_pct <= .5 ~ .5
        , mailed_pct >= 1.5 ~ 1.5
        , TRUE ~ mailed_pct 
    )
  ) %>%
  dplyr::summarise(
    win_rate = mean(ccao_reduction_flag)
  ) %>%
  dplyr::ungroup()

gd1.2 <- a %>%
  dplyr::group_by(mailed_pct, year) %>%
  dplyr::filter(ccao_reduction_flag) %>%
  dplyr::mutate(
    mailed_pct = round(mailed_pct, 1)
    , mailed_pct = case_when( # Top code for visiual
      mailed_pct <= .5 ~ .5
      , mailed_pct >= 1.5 ~ 1.5
      , TRUE ~ mailed_pct 
    )
  ) %>%
  dplyr::summarise(
    properties = n()
    , mean_reduction_pct = mean(ccao_reduction_pct)
  ) %>% 
  dplyr::ungroup()

  gd2.1 <- b %>%
    dplyr::group_by(mailed_pct, year) %>%
    dplyr::mutate(
      mailed_pct = round(mailed_pct, 1)
      , mailed_pct = case_when( # Top code for visiual
        mailed_pct <= .5 ~ .5
        , mailed_pct >= 1.5 ~ 1.5
        , TRUE ~ mailed_pct 
      )
    ) %>%
    dplyr::summarise(
      win_rate = mean(ccao_reduction_flag)
    ) %>%
    dplyr::ungroup()
  
  gd2.2 <- b %>%
    dplyr::group_by(mailed_pct, year) %>%
    dplyr::filter(ccao_reduction_flag) %>%
    dplyr::mutate(
      mailed_pct = round(mailed_pct, 1)
      , mailed_pct = case_when( # Top code for visiual
        mailed_pct <= .5 ~ .5
        , mailed_pct >= 1.5 ~ 1.5
        , TRUE ~ mailed_pct 
      )
    ) %>%
    dplyr::summarise(
      properties = n()
      , mean_reduction_pct = mean(ccao_reduction_pct)
    ) %>%
  dplyr::ungroup()
  
g1 <- gd1.1 %>%
  ggplot(aes(x = mailed_pct, y = win_rate, colour  = year)) +
  geom_point(size = 5) + geom_line(linewidth = 2) +
  geom_vline(xintercept = 1, linetype = 'dashed') +
  geom_vline(xintercept = 1.1, linetype = 'solid') +
  geom_vline(xintercept = 0.9, linetype = 'solid') +
  theme_minimal() +
  scale_color_manual(values = c(MyColours)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Appeal Results Compared to Sales in Succeeding Year"
    , subtitle = "Appeal Success Rate"
  ) +
  ylab("Reduction in AV") +
  xlab("AV percentage of sale price") 

g2 <- gd1.2 %>%
  dplyr::filter(mean_reduction_pct > -.1) %>%
  ggplot(aes(x = mailed_pct, y = mean_reduction_pct, colour  = year)) +
  geom_point(size = 5) + geom_line(linewidth = 2) +
  geom_vline(xintercept = 1, linetype = 'dashed') +
  geom_vline(xintercept = 1.1, linetype = 'solid') +
  geom_vline(xintercept = 0.9, linetype = 'solid') +
  theme_minimal() +
  scale_color_manual(values = c(MyColours)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
   subtitle = "Appeal Reduction Percent"
  ) +
  ylab("Reduction in AV") +
  xlab("AV percentage of sale price") 

# Combine
g3 <- ggpubr::ggarrange(g1, g2, ncol = 2, nrow = 1
                        , common.legend = TRUE
                        , legend="bottom")

# Save the Plot
jpeg(file=here::here("cc_appeals", "outputs", "plot3.jpeg"), width = 980, height = 760)
g3
dev.off()

g4 <- gd2.1 %>%
  ggplot(aes(x = mailed_pct, y = win_rate, colour  = year)) +
  geom_point(size = 5) + geom_line(linewidth = 2) +
  geom_vline(xintercept = 1, linetype = 'dashed') +
  geom_vline(xintercept = 1.1, linetype = 'solid') +
  geom_vline(xintercept = 0.9, linetype = 'solid') +
  theme_minimal() +
  scale_color_manual(values = c(MyColours)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Appeal Results Compared to Sales in Preceeding Year"
    , subtitle = "Appeal Success Rate"
  ) +
  ylab("Reduction in AV") +
  xlab("AV percentage of sale price") 

g5 <- gd2.2 %>%
  dplyr::filter(mean_reduction_pct > -.1) %>%
  ggplot(aes(x = mailed_pct, y = mean_reduction_pct, colour  = year)) +
  geom_point(size = 5) + geom_line(linewidth = 2) +
  geom_vline(xintercept = 1, linetype = 'dashed') +
  geom_vline(xintercept = 1.1, linetype = 'solid') +
  geom_vline(xintercept = 0.9, linetype = 'solid') +
  theme_minimal() +
  scale_color_manual(values = c(MyColours)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    subtitle = "Appeal Reduction Percent"
  ) +
  ylab("Reduction in AV") +
  xlab("AV percentage of sale price") 

# Combine
g6 <- ggpubr::ggarrange(g4, g5, ncol = 2, nrow = 1
                        , common.legend = TRUE
                        , legend="bottom")

# Save the Plot
jpeg(file=here::here("cc_appeals", "outputs", "plot4.jpeg"), width = 980, height = 760)
g6
dev.off()

rm(g1, g2,g3,g4,g5,g6,gd1.1, gd1.2,gd2.1,gd2.2)

#--- Specific Examples----

