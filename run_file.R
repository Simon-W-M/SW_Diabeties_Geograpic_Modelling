# load libraries
source('libraries.R')

#####################
# LSOA sites to use #
#####################

# reads in spreadsheet of base locations
locations <- read_excel("LocationsForDES.xlsx") |>
  clean_names() |>
  mutate(site = site_short_name)

# additional sites can be added in here by adding to this format
# or can be added to the base excel sheet 

# site <- 'Name of Site'
# post_code <- 'EX2 8EF'
# lsoa_sites <- 'E01029302'
# icb_site <- 'Devon'
# 
# add <- data.frame(site, post_code, lsoa_sites, icb_site)
# 
# locations <- locations |>
#   bind_rows(add)

lsoa_sites <- toString(sprintf("'%s'", locations$lsoa_sites))

source('data_load.R')

source('data_wrangle.R')

source('map.R')


