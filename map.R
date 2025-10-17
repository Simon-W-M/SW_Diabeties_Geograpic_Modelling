


cli_alert('Running mapping function')

################
# Map function #
################

icb_filt <- icb_it

# This section creates the cloropleth data

# create baseline data
baseline_map <- create_data_icb(oct_or_hes = 'OCT', icb_filt, 0, 0 )
base_summary <- create_summary_site_icb(baseline_map, 'site')

# create empty lists to place iterations of scenario data into 
list_of_scenario_data <- list()
list_of_summary_data <- list()

# filter the scenarios table to the icb
# we will then use the max number of sites to feed into the loop below
# if there are no scenarios (ie Somerset - it will skip this bit)

icb_scenarios <- combos_table |>
  filter(icb == icb_filt)

# if there are scenarios then run the loop across
if (max(icb_scenarios$no_sites) > 0) {

for (i in seq(0,max(icb_scenarios$no_sites))) 
{
   map_data<- create_data_icb(oct_or_hes = 'OCT', icb_filt, number_sites=i, scenario=1)
   site_summary <- create_summary_site_icb(map_data, site_or_icb = "site")
     
   list_of_scenario_data[[paste(i)]] <- map_data
   list_of_summary_data[[paste(i)]] <- site_summary
  # 
  }
}

# This section pulls the data for each site
# and has to be run across each sceanrio
# (Probably could have made neater and done in a loop as per above
#  but this is a potential later improvement)

# baseline

sites_hospital_baseline <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 1,
         site_status =='Exisiting') |>
  inner_join(base_summary,
            by = c('site' = 'site'))

sites_non_hospital_baseline <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 0,
         site_status =='Exisiting') |>
  inner_join(base_summary,
            by = c('site' = 'site'))

# scenario_1

# For the scenarios there are 4 types of site as these need different
# colours and icons
# Hospital Existing / Hoispital Potential / Non Hospital Existing / Non Hopsital Potential
# NOTE: If mobile sites were to be added then these would need to be added to each section
#       so that they had seperate data

# Checks if there is a Scenario 1
if(length(list_of_scenario_data) > 1) {

sites_hospital_existing_scenario_1 <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 1,
         site_status =='Exisiting') |>
  inner_join(list_of_summary_data[[2]],
            by = c('site' = 'site'))

sites_non_hospital_existing_scenario_1 <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 0,
         site_status =='Exisiting') |>
  inner_join(list_of_summary_data[[2]],
            by = c('site' = 'site'))

sites_hospital_potential_scenario_1 <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 1,
         site_status =='Potential') |>
  inner_join(list_of_summary_data[[2]],
            by = c('site' = 'site'))

sites_non_hospital_potential_scenario_1 <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 0,
         site_status =='Potential') |>
  inner_join(list_of_summary_data[[2]],
            by = c('site' = 'site')) 
}

# scenario 2

# checks there is a scenario 2
if(length(list_of_scenario_data) > 2) {

sites_hospital_existing_scenario_2 <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 1,
         site_status =='Exisiting') |>
  inner_join(list_of_summary_data[[3]],
             by = c('site' = 'site'))

sites_non_hospital_existing_scenario_2 <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 0,
         site_status =='Exisiting') |>
  inner_join(list_of_summary_data[[3]],
             by = c('site' = 'site'))

sites_hospital_potential_scenario_2 <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 1,
         site_status =='Potential') |>
  inner_join(list_of_summary_data[[3]],
             by = c('site' = 'site'))

sites_non_hospital_potential_scenario_2 <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 0,
         site_status =='Potential') |>
  inner_join(list_of_summary_data[[3]],
             by = c('site' = 'site'))

}

# scenario 3

'# checks there is a scenario 3'
if(length(list_of_scenario_data) > 3) {

sites_hospital_existing_scenario_3 <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 1,
         site_status =='Exisiting') |>
  inner_join(list_of_summary_data[[4]],
             by = c('site' = 'site'))

sites_non_hospital_existing_scenario_3 <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 0,
         site_status =='Exisiting') |>
  inner_join(list_of_summary_data[[4]],
             by = c('site' = 'site'))

sites_hospital_potential_scenario_3 <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 1,
         site_status =='Potential') |>
  inner_join(list_of_summary_data[[4]],
             by = c('site' = 'site'))

sites_non_hospital_potential_scenario_3 <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 0,
         site_status =='Potential') |>
  inner_join(list_of_summary_data[[4]],
             by = c('site' = 'site'))
}

# show candidate sites

pot_sites_h <- sites |>
  filter(icb_site == icb_filt,
         hospital_flag == 1,
         site_status == 'Potential')

pot_sites_o <- sites |>
  filter(icb_site == icb_filt,
         hospital_flag == 0,
         site_status == 'Potential')
         
# create pallet

# this create the colour pallet between yellow and red
# the actual colour chosen is based on the numerical column for banding
# for travel times and is standardised
# the population it creates its own bins based on the population
# and so these are not standardised across all the maps
# and are relative to each icb.  Numbers so small, I did not consider this to be
# an issue, it still shows relative hotspots of patient locations

# travel time pallet
pal <- colorBin("YlOrRd", domain = seq(1:6), 
                bins = 6)

# population pallet
pal_pop <- colorBin("Blues", domain = baseline_map$wt_pop, 
                bins = 6)

####################
# Build the map up #
####################

# the map is built with layers added to it.
# Various if statements allow bits to be added dependant on how many 
# scenarios etc

# Create base map
cloropleth_map <- leaflet(baseline_map) |>
  addTiles() |>

  # Population layer
  addPolygons(data = baseline_map,
              fillColor = ~pal_pop(wt_pop),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              popup = paste0(baseline_map$lsoa_nm, "<br>",
                            "Diabetic Population: ", round(baseline_map$wt_pop,0), "<br>"),
              group = 'Population'
  )  |>

  # Baseline layer
  addPolygons(data = baseline_map,
              fillColor = ~pal(time_band_n_h),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              popup = paste0(baseline_map$lsoa_nm, "<br>",
                             "Diabetic Population: ", round(baseline_map$wt_pop,0), "<br>",
                             "Travel time: ",round(baseline_map$travel_time_hybrid, 0), " mins", "<br>",
                             "To site: ", baseline_map$site),
              group = 'Base'
              ) |>
  addMarkers( lng = sites_hospital_baseline$long,
              lat = sites_hospital_baseline$lat,
              icon = magicIcons(icon = 'hospital'),
              popup = paste0(sites_hospital_baseline$site, "<br>",
                             "Mean travel time: ", sites_hospital_baseline$mean_trav, " mins <br>",
                             "Population < 15 mins: ", prettyNum(sites_hospital_baseline$`tot_pop_band_0 - 15`, big.mark = ","), "<br>",
                             "Population 15 < 30 mins: ", prettyNum(sites_hospital_baseline$`tot_pop_band_15+ - 30`, big.mark = ","), "<br>",
                             "Population 30 < 45 mins: ", prettyNum(sites_hospital_baseline$`tot_pop_band_30+ - 45`, big.mark = ","), "<br>",
                             "Population 45 < 60 mins: ", prettyNum(sites_hospital_baseline$`tot_pop_band_45+ - 60`, big.mark = ","), "<br>",
                             "Population 60+ mins: ", prettyNum(sites_hospital_baseline$`tot_pop_band_60 +`, big.mark = ",")
                             ),
              group = 'Base') |>
  addMarkers( lng = sites_non_hospital_baseline$long,
              lat = sites_non_hospital_baseline$lat,
              icon = magicIcons(icon = 'house-medical'),
              popup = paste0(sites_non_hospital_baseline$site, "<br>",
                             "Mean travel time: ", sites_non_hospital_baseline$mean_trav, " mins <br>",
                             "Population < 15 mins: ", prettyNum(sites_non_hospital_baseline$`tot_pop_band_0 - 15`, big.mark = ","), "<br>",
                             "Population 15 < 30 mins: ", prettyNum(sites_non_hospital_baseline$`tot_pop_band_15+ - 30`, big.mark = ","), "<br>",
                             "Population 30 < 45 mins: ", prettyNum(sites_non_hospital_baseline$`tot_pop_band_30+ - 45`, big.mark = ","), "<br>",
                             "Population 45 < 60 mins: ", prettyNum(sites_non_hospital_baseline$`tot_pop_band_45+ - 60`, big.mark = ","), "<br>",
                             "Population 60+ mins: ", prettyNum(sites_non_hospital_baseline$`tot_pop_band_60 +`, big.mark = ",")
              ),
              group = 'Base') 

  # Scenario 1 (1 additional site)
  if(length(list_of_scenario_data) > 1) {

    cloropleth_map <- cloropleth_map |>
      
    # add cloropleth  
    addPolygons(data = list_of_scenario_data[[2]],
                fillColor = ~pal(time_band_n_h),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                  popup = paste0(list_of_scenario_data[[2]]$lsoa_nm, "<br>",
                                  "Diabetic Population: ", round(list_of_scenario_data[[2]]$wt_pop,0), "<br>",
                                  "Travel time: ",round(list_of_scenario_data[[2]]$travel_time_hybrid, 1), " mins", "<br>",
                                 "To site: ", list_of_scenario_data[[2]]$site),
                group = 'Scenario_1'
    )  |>

     # existing base markers
      addMarkers( lng = sites_hospital_existing_scenario_1$long,
                  lat = sites_hospital_existing_scenario_1$lat,
                  icon = magicIcons(icon = 'hospital'),
                  popup = paste0(sites_hospital_existing_scenario_1$site, "<br>",
                                 "Mean travel time: ", sites_hospital_existing_scenario_1$mean_trav, " mins <br>",
                                 "Population < 15 mins: ", prettyNum(sites_hospital_existing_scenario_1$`tot_pop_band_0 - 15`, big.mark = ","), "<br>",
                                 "Population 15 < 30 mins: ", prettyNum(sites_hospital_existing_scenario_1$`tot_pop_band_15+ - 30`, big.mark = ","), "<br>",
                                 "Population 30 < 45 mins: ", prettyNum(sites_hospital_existing_scenario_1$`mean_trav_band_30+ - 45`, big.mark = ","), "<br>",
                                 "Population 45 < 60 mins: ", prettyNum(sites_hospital_existing_scenario_1$`tot_pop_band_45+ - 60`, big.mark = ","), "<br>",
                                 "Population 60+ mins: ", prettyNum(sites_hospital_existing_scenario_1$`tot_pop_band_60 +`, big.mark = ",")
                                 ),
                  group = 'Scenario_1') |>
      addMarkers( lng = sites_non_hospital_existing_scenario_1$long,
                  lat = sites_non_hospital_existing_scenario_1$lat,
                  icon = magicIcons(icon = 'house-medical'),
                  popup = paste0(sites_non_hospital_existing_scenario_1$site, "<br>",
                                 "Mean travel time: ", sites_non_hospital_existing_scenario_1$mean_trav, " mins <br>",
                                 "Population < 15 mins: ", prettyNum(sites_non_hospital_existing_scenario_1$`tot_pop_band_0 - 15`, big.mark = ","), "<br>",
                                 "Population 15 < 30 mins: ", prettyNum(sites_non_hospital_existing_scenario_1$`tot_pop_band_15+ - 30`, big.mark = ","), "<br>",
                                 "Population 30 < 45 mins: ", prettyNum(sites_non_hospital_existing_scenario_1$`mean_trav_band_30+ - 45`, big.mark = ","), "<br>",
                                 "Population 45 < 60 mins: ", prettyNum(sites_non_hospital_existing_scenario_1$`tot_pop_band_45+ - 60`, big.mark = ","), "<br>",
                                 "Population 60+ mins: ", prettyNum(sites_non_hospital_existing_scenario_1$`tot_pop_band_60 +`, big.mark = ",")
                  ),
                  group = 'Scenario_1') |>

     # potential markers
      addMarkers( lng = sites_hospital_potential_scenario_1$long,
                  lat = sites_hospital_potential_scenario_1$lat,
                  icon = magicIcons(icon = 'hospital',
                                    markerColor = leaf.magic::awesomePalette$green),
                  popup = paste0(sites_hospital_potential_scenario_1$site, "<br>",
                                 "Mean travel time: ", sites_hospital_potential_scenario_1$mean_trav, " mins <br>",
                                 "Population < 15 mins: ", prettyNum(sites_hospital_potential_scenario_1$`tot_pop_band_0 - 15`, big.mark = ","), "<br>",
                                 "Population 15 < 30 mins: ", prettyNum(sites_hospital_potential_scenario_1$`tot_pop_band_15+ - 30`, big.mark = ","), "<br>",
                                 "Population 30 < 45 mins: ", prettyNum(sites_hospital_potential_scenario_1$`mean_trav_band_30+ - 45`, big.mark = ","), "<br>",
                                 "Population 45 < 60 mins: ", prettyNum(sites_hospital_potential_scenario_1$`tot_pop_band_45+ - 60`, big.mark = ","), "<br>",
                                 "Population 60+ mins: ", prettyNum(sites_hospital_potential_scenario_1$`tot_pop_band_60 +`, big.mark = ",")
                  ),
                  group = 'Scenario_1') |>
      addMarkers( lng = sites_non_hospital_potential_scenario_1$long,
                  lat = sites_non_hospital_potential_scenario_1$lat,
                  icon = magicIcons(icon = 'house-medical',
                                    markerColor = leaf.magic::awesomePalette$green),
                  popup = paste0(sites_non_hospital_potential_scenario_1$site, "<br>",
                                 "Mean travel time: ", sites_non_hospital_potential_scenario_1$mean_trav, " mins <br>",
                                 "Population < 15 mins: ", prettyNum(sites_non_hospital_potential_scenario_1$`tot_pop_band_0 - 15`, big.mark = ","), "<br>",
                                 "Population 15 < 30 mins: ", prettyNum(sites_non_hospital_potential_scenario_1$`tot_pop_band_15+ - 30`, big.mark = ","), "<br>",
                                 "Population 30 < 45 mins: ", prettyNum(sites_non_hospital_potential_scenario_1$`mean_trav_band_30+ - 45`, big.mark = ","), "<br>",
                                 "Population 45 < 60 mins: ", prettyNum(sites_non_hospital_potential_scenario_1$`tot_pop_band_45+ - 60`, big.mark = ","), "<br>",
                                 "Population 60+ mins: ", prettyNum(sites_non_hospital_potential_scenario_1$`tot_pop_band_60 +`, big.mark = ",")
                  ),
                  group = 'Scenario_1')
  }

# scenario 2
if(length(list_of_scenario_data) > 2) {

  cloropleth_map <- cloropleth_map |>

    # add cloropleth 
    addPolygons(data = list_of_scenario_data[[3]],
                fillColor = ~pal(time_band_n_h),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                popup = paste0(list_of_scenario_data[[3]]$lsoa_nm, "<br>",
                               "Diabetic Population: ", round(list_of_scenario_data[[3]]$wt_pop,0), "<br>",
                               "Travel time: ",round(list_of_scenario_data[[3]]$travel_time_hybrid, 0), " mins", "<br>",
                               "To site: ", list_of_scenario_data[[3]]$site),
                group = 'Scenario_2'
    )  |>

     # existing base markers
    addMarkers( lng = sites_hospital_existing_scenario_2$long,
                lat = sites_hospital_existing_scenario_2$lat,
                icon = magicIcons(icon = 'hospital'),
                popup = paste0(sites_hospital_existing_scenario_2$site, "<br>",
                               "Mean travel time: ", sites_hospital_existing_scenario_2$mean_trav, " mins <br>",
                               "Population < 15 mins: ", prettyNum(sites_hospital_existing_scenario_2$`tot_pop_band_0 - 15`, big.mark = ","), "<br>",
                               "Population 15 < 30 mins: ", prettyNum(sites_hospital_existing_scenario_2$`tot_pop_band_15+ - 30`, big.mark = ","), "<br>",
                               "Population 30 < 45 mins: ", prettyNum(sites_hospital_existing_scenario_2$`mean_trav_band_30+ - 45`, big.mark = ","), "<br>",
                               "Population 45 < 60 mins: ", prettyNum(sites_hospital_existing_scenario_2$`tot_pop_band_45+ - 60`, big.mark = ","), "<br>",
                               "Population 60+ mins: ", prettyNum(sites_hospital_existing_scenario_2$`tot_pop_band_60 +`, big.mark = ",")
                ),
                group = 'Scenario_2') |>
    addMarkers( lng = sites_non_hospital_existing_scenario_2$long,
                lat = sites_non_hospital_existing_scenario_2$lat,
                icon = magicIcons(icon = 'house-medical'),
                popup = paste0(sites_non_hospital_existing_scenario_2$site, "<br>",
                               "Mean travel time: ", sites_non_hospital_existing_scenario_2$mean_trav, " mins <br>",
                               "Population < 15 mins: ", prettyNum(sites_non_hospital_existing_scenario_2$`tot_pop_band_0 - 15`, big.mark = ","), "<br>",
                               "Population 15 < 30 mins: ", prettyNum(sites_non_hospital_existing_scenario_2$`tot_pop_band_15+ - 30`, big.mark = ","), "<br>",
                               "Population 30 < 45 mins: ", prettyNum(sites_non_hospital_existing_scenario_2$`mean_trav_band_30+ - 45`, big.mark = ","), "<br>",
                               "Population 45 < 60 mins: ", prettyNum(sites_non_hospital_existing_scenario_2$`tot_pop_band_45+ - 60`, big.mark = ","), "<br>",
                               "Population 60+ mins: ", prettyNum(sites_non_hospital_existing_scenario_2$`tot_pop_band_60 +`, big.mark = ",")
                ),
                group = 'Scenario_2') |>

     # potential markers
    addMarkers( lng = sites_hospital_potential_scenario_2$long,
                lat = sites_hospital_potential_scenario_2$lat,
                icon = magicIcons(icon = 'hospital',
                                  markerColor = leaf.magic::awesomePalette$green),
                popup = paste0(sites_hospital_potential_scenario_2$site, "<br>",
                               "Mean travel time: ", sites_hospital_potential_scenario_2$mean_trav, " mins <br>",
                               "Population < 15 mins: ", prettyNum(sites_hospital_potential_scenario_2$`tot_pop_band_0 - 15`, big.mark = ","), "<br>",
                               "Population 15 < 30 mins: ", prettyNum(sites_hospital_potential_scenario_2$`tot_pop_band_15+ - 30`, big.mark = ","), "<br>",
                               "Population 30 < 45 mins: ", prettyNum(sites_hospital_potential_scenario_2$`mean_trav_band_30+ - 45`, big.mark = ","), "<br>",
                               "Population 45 < 60 mins: ", prettyNum(sites_hospital_potential_scenario_2$`tot_pop_band_45+ - 60`, big.mark = ","), "<br>",
                               "Population 60+ mins: ", prettyNum(sites_hospital_potential_scenario_2$`tot_pop_band_60 +`, big.mark = ",")
                ),
                group = 'Scenario_2') |>
    addMarkers( lng = sites_non_hospital_potential_scenario_2$long,
                lat = sites_non_hospital_potential_scenario_2$lat,
                icon = magicIcons(icon = 'house-medical',
                                  markerColor = leaf.magic::awesomePalette$green),
                popup = paste0(sites_non_hospital_potential_scenario_2$site, "<br>",
                               "Mean travel time: ", sites_non_hospital_potential_scenario_2$mean_trav, " mins <br>",
                               "Population < 15 mins: ", prettyNum(sites_non_hospital_potential_scenario_2$`tot_pop_band_0 - 15`, big.mark = ","), "<br>",
                               "Population 15 < 30 mins: ", prettyNum(sites_non_hospital_potential_scenario_2$`tot_pop_band_15+ - 30`, big.mark = ","), "<br>",
                               "Population 30 < 45 mins: ", prettyNum(sites_non_hospital_potential_scenario_2$`mean_trav_band_30+ - 45`, big.mark = ","), "<br>",
                               "Population 45 < 60 mins: ", prettyNum(sites_non_hospital_potential_scenario_2$`tot_pop_band_45+ - 60`, big.mark = ","), "<br>",
                               "Population 60+ mins: ", prettyNum(sites_non_hospital_potential_scenario_2$`tot_pop_band_60 +`, big.mark = ",")
                ),
                group = 'Scenario_2')
}

# scenario 3
if(length(list_of_scenario_data) > 3) {
  
  cloropleth_map <- cloropleth_map |>

    # add cloropleth   
    addPolygons(data = list_of_scenario_data[[4]],
                fillColor = ~pal(time_band_n_h),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                popup = paste0(list_of_scenario_data[[4]]$lsoa_nm, "<br>",
                               "Diabetic Population: ", round(list_of_scenario_data[[4]]$wt_pop,0), "<br>",
                               "Travel time: ",round(list_of_scenario_data[[4]]$travel_time_hybrid, 0), " mins", "<br>",
                               "To site: ", list_of_scenario_data[[4]]$site),
                group = 'Scenario_3'
    )  |>
    
    # existing base markers
    addMarkers( lng = sites_hospital_existing_scenario_3$long,
                lat = sites_hospital_existing_scenario_3$lat,
                icon = magicIcons(icon = 'hospital'),
                popup = paste0(sites_hospital_existing_scenario_3$site, "<br>",
                               "Mean travel time: ", sites_hospital_existing_scenario_3$mean_trav, " mins <br>",
                               "Population < 15 mins: ", prettyNum(sites_hospital_existing_scenario_3$`tot_pop_band_0 - 15`, big.mark = ","), "<br>",
                               "Population 15 < 30 mins: ", prettyNum(sites_hospital_existing_scenario_3$`tot_pop_band_15+ - 30`, big.mark = ","), "<br>",
                               "Population 30 < 45 mins: ", prettyNum(sites_hospital_existing_scenario_3$`mean_trav_band_30+ - 45`, big.mark = ","), "<br>",
                               "Population 45 < 60 mins: ", prettyNum(sites_hospital_existing_scenario_3$`tot_pop_band_45+ - 60`, big.mark = ","), "<br>",
                               "Population 60+ mins: ", prettyNum(sites_hospital_existing_scenario_3$`tot_pop_band_60 +`, big.mark = ",")
                ),
                group = 'Scenario_3') |>
    addMarkers( lng = sites_non_hospital_existing_scenario_3$long,
                lat = sites_non_hospital_existing_scenario_3$lat,
                icon = magicIcons(icon = 'house-medical'),
                popup = paste0(sites_non_hospital_existing_scenario_3$site, "<br>",
                               "Mean travel time: ", sites_non_hospital_existing_scenario_3$mean_trav, " mins <br>",
                               "Population < 15 mins: ", prettyNum(sites_non_hospital_existing_scenario_3$`tot_pop_band_0 - 15`, big.mark = ","), "<br>",
                               "Population 15 < 30 mins: ", prettyNum(sites_non_hospital_existing_scenario_3$`tot_pop_band_15+ - 30`, big.mark = ","), "<br>",
                               "Population 30 < 45 mins: ", prettyNum(sites_non_hospital_existing_scenario_3$`mean_trav_band_30+ - 45`, big.mark = ","), "<br>",
                               "Population 45 < 60 mins: ", prettyNum(sites_non_hospital_existing_scenario_3$`tot_pop_band_45+ - 60`, big.mark = ","), "<br>",
                               "Population 60+ mins: ", prettyNum(sites_non_hospital_existing_scenario_3$`tot_pop_band_60 +`, big.mark = ",")
                ),
                group = 'Scenario_3') |>

    # potential markers
    
    addMarkers( lng = sites_hospital_potential_scenario_3$long,
                lat = sites_hospital_potential_scenario_3$lat,
                icon = magicIcons(icon = 'hospital',
                                  markerColor = leaf.magic::awesomePalette$green),
                popup = paste0(sites_hospital_potential_scenario_3$site, "<br>",
                               "Mean travel time: ", sites_hospital_potential_scenario_3$mean_trav, " mins <br>",
                               "Population < 15 mins: ", prettyNum(sites_hospital_potential_scenario_3$`tot_pop_band_0 - 15`, big.mark = ","), "<br>",
                               "Population 15 < 30 mins: ", prettyNum(sites_hospital_potential_scenario_3$`tot_pop_band_15+ - 30`, big.mark = ","), "<br>",
                               "Population 30 < 45 mins: ", prettyNum(sites_hospital_potential_scenario_3$`mean_trav_band_30+ - 45`, big.mark = ","), "<br>",
                               "Population 45 < 60 mins: ", prettyNum(sites_hospital_potential_scenario_3$`tot_pop_band_45+ - 60`, big.mark = ","), "<br>",
                               "Population 60+ mins: ", prettyNum(sites_hospital_potential_scenario_3$`tot_pop_band_60 +`, big.mark = ",")
                ),
                group = 'Scenario_3') |>
    addMarkers( lng = sites_non_hospital_potential_scenario_3$long,
                lat = sites_non_hospital_potential_scenario_3$lat,
                icon = magicIcons(icon = 'house-medical',
                                  markerColor = leaf.magic::awesomePalette$green),
                popup = paste0(sites_non_hospital_potential_scenario_3$site, "<br>",
                               "Mean travel time: ", sites_non_hospital_potential_scenario_3$mean_trav, " mins <br>",
                               "Population < 15 mins: ", prettyNum(sites_non_hospital_potential_scenario_3$`tot_pop_band_0 - 15`, big.mark = ","), "<br>",
                               "Population 15 < 30 mins: ", prettyNum(sites_non_hospital_potential_scenario_3$`tot_pop_band_15+ - 30`, big.mark = ","), "<br>",
                               "Population 30 < 45 mins: ", prettyNum(sites_non_hospital_potential_scenario_3$`mean_trav_band_30+ - 45`, big.mark = ","), "<br>",
                               "Population 45 < 60 mins: ", prettyNum(sites_non_hospital_potential_scenario_3$`tot_pop_band_45+ - 60`, big.mark = ","), "<br>",
                               "Population 60+ mins: ", prettyNum(sites_non_hospital_potential_scenario_3$`tot_pop_band_60 +`, big.mark = ",")
                ),
                group = 'Scenario_3')
}

  # candiate saite markers

if(length(list_of_scenario_data) > 0) {
  
  cloropleth_map <- cloropleth_map |>
  
addMarkers( lng = pot_sites_h$long,
            lat = pot_sites_h$lat,
            icon = magicIcons(icon = 'hospital',
                              markerColor = leaf.magic::awesomePalette$green),
            group = 'Candidate_sites') |>
    addMarkers( lng = pot_sites_o$long,
                lat = pot_sites_o$lat,
                icon = magicIcons(icon = 'house-medical',
                                  markerColor = leaf.magic::awesomePalette$green),
                group = 'Candidate_sites')

}

# dynamically create list of groups for buttons
# based on he number of scenarios
no_scenarios <- if_else(max(icb_scenarios$no_sites)>0 , max(icb_scenarios$no_sites), 0)

list_opt <- c('Population',
              'Base',
              'Scenario_1',
              'Scenario_2',
              'Scenario_3')

groups <- head(list_opt, 2 + no_scenarios)

if (no_scenarios > 0) {
  groups <- c(groups, 'Candidate_sites')
  
}

cloropleth_map <- cloropleth_map |>
  addLayersControl(baseGroups = groups,
                   options = layersControlOptions(collapsed = FALSE)) 
  

# cloropleth_map

saveRDS(cloropleth_map, paste0('oct_map.rds'))

cli_alert_success('Creating mapping function - complete')






