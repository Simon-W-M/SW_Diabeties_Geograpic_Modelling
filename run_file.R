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

# create a list of icbs to run reports for
list_of_icbs <- unique(locations$icb_site)

# if you wish to run reports only for cetain sites use this
#list_of_icbs <- c('Devon', 'Somerset')

for (icb_it in list_of_icbs) {
  cli_alert(paste0("Report for ", icb_it, " - started"))
  cli_alert("Building base dataset")
  
  # create base table 
  base_table_data <- create_table_dataframe(icb_it, 0, 0)
  base_table <- create_summary_table(base_table_data)

  saveRDS(base_table, "base_table.rds")

  cli_alert_success("Building base dataset - complete")

  source("map.R")

  cli_alert("Building scenario tables")

  combos <- combos_table |>
    filter(icb == icb_it)

  nosites <- if_else(max(combos$no_sites, na.rm = T) > 0, 
                     max(combos$no_sites, na.rm = T), 
                     0)

  saveRDS(icb_it, "icb_it.rds")
  saveRDS(nosites, "no_scen.rds")

  # checks number of scenarios that are being run
  if (nosites > 0) {
    for (scen_no in seq(1:nosites)) {
      scenario_data <- create_table_dataframe(icb_it, scen_no, 1)
      scenario_table <- create_summary_table(scenario_data)

      saveRDS(scenario_table, paste0("scenario_table", scen_no, ".rds"))
    }
  }

  cli_alert_success("Building scenario tables - complete")
  cli_alert("Building output")

  quarto_render(
    input = "output.qmd",
    output_file = paste0(
      snakecase::to_snake_case(icb_it),
      "_geo_analysis_",
      format(
        Sys.Date(),
        "%y%m%d"
      ),
      ".html"
    )
  )
  
  cli_alert_success("Building output - complete")
  cli_alert_success(paste0("Report for ", icb_it, " - complete"))
}

boxx("Ta da!  All done.", padding = 2, background_col = '#FFB81C')  
