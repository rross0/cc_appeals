# This 'master' file calls other files and functions in order to produce
# the desired analysis

rm(list = ls())

library(RSocrata) # For getting data
library(dplyr); library(tidyverse) #For manipulating data
library(arrow) #For compressing data
library(here) #For easy root management
library(ccao); library(assessr) #Assessment packages
library(ggplot2); library(ggpubr)
library(jsonlite); library(stringr)
library(readxl)
library(lubridate) 
library(mgcv)

MyColours <- c(as.character(ccao::ccao_colors[1:2]), as.character(ccao::ccao_colors[5:12]))

# ----- Ingest raw data-----
# This file calls data from the Cook County Open Data Portal
# and then saves the results in the big data folder
if(FALSE){ # leave this off unless you want to start from scratch
source(here::here("cc_appeals", "code", "ingest.r"))
}

# This file applies filters to determine which sales to use in the analysis
# This file also executes a time adjustment to sales
source(here::here("cc_appeals", "code", "etl_sales.r"))
source(here::here("cc_appeals", "code", "sales_etl_analysis.r"))

# This file creats a sales sample for ratio analysis
source(here::here("cc_appeals", "code", "etl_ratios.r"))

# This file reports total counts and basic rates for appeals
source(here::here("cc_appeals", "code", "summarry stats.r"))

# This file looks at factors correlated with the probability of appeal, winning, amount won
source(here::here("cc_appeals", "code", "what drives appeals.r"))

# This file examines whether appeals make assessments better or not
source(here::here("cc_appeals", "code", "do appeals make assessments more fair.r"))
