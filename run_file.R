# load libraries
source('libraries.R')

#####################
# LSOA sites to use #
#####################

# reads in spreadsheet of base locations
locations <- read_excel("LocationsForDES.xlsx")

# additional sites can be added in here by adding to this format
# or can be added to the base excel sheet 

# site <- 'Name of Site'
# post_code <- 'EX2 8EF'
# losa_sites <- 'E01029302'
# icb_site <- 'Devon'
# 
# add <- data.frame(site, post_code, losa_sites, icb_site)
# 
# locations <- locations |>
#   bind_rows(add)

losa_sites <- toString(sprintf("'%s'", locations$losa_sites))

source('data_load.R')

source('data_wrangle.R')

source('map.R')


