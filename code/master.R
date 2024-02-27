# This 'master' file calls other files and functions in order to produce
# the desired analysis

rm(list = ls())

library(RSocrata); library(dplyr)
library(arrow) #For compressing data
library(here) #For easy root management
library(ccao); library(assessr) #Assessment packages
library(ggplot2)
library(jsonlite); library(stringr)

# ----- Ingest raw data-----
# This file calls data from the Cook County Open Data Portal
# and then saves the results in the big data folder
if(FALSE){ # leave this off unless you want to start from scratch
source(here::here("cc_appeals", "code", "ingest.r"))
}
# This file combines appeals and assessments, applies filters, and generates 
# useful variables
source(here::here("cc_appeals", "code", "etl1.r"))

# This file reports total counts and basic rates for appeals
source(here::here("cc_appeals", "code", "summarry stats.r"))

# This file looks at factors correlated with the probability of appeal, winning, amount won
source(here::here("cc_appeals", "code", "what drives appeals.r"))
