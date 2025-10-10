
cli_alert('Running geocoder on site locations')
 
# use geocoder to get long lats from postcodes
sites <- locations |> 
  mutate(geo(address = postcode, 
             method = 'osm')) |>
  mutate(hospital_flag = if_else(str_detect(site, 'Hospital'),1 ,0),
         site = site_short_name)

saveRDS(sites, 'sites.rds')
saveRDS(df, 'df.rds')

cli_alert_success('Running geocoder on site locations - complete')

cli_alert('Creating mapping function')

################
# Map function #
################

map_data_base <- create_data_icb(oct_or_hes = 'OCT', 'Cornwall', number_sites=0, scenario=0)
site_summary_base <- create_summary_site_icb(site_data, "site")
map_data_1 <-  create_data_icb(oct_or_hes = 'OCT', 'Cornwall', number_sites=3, scenario=1)
site_summary_1 <- create_summary_site_icb(map_data_1, "site")
map_data_2 <-  create_data_icb(oct_or_hes = 'OCT', 'Cornwall', number_sites=3, scenario=2)
site_summary_2 <- create_summary_site_icb(map_data_2, "site")
map_data_3 <-  create_data_icb(oct_or_hes = 'OCT', 'Cornwall', number_sites=3, scenario=3)
site_summary_3 <- create_summary_site_icb(map_data_3, "site")

icb_map <- function(icb_filt) {

#site_x <- 'Gloucestershire'

  
leg_colours <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")
`Travel Bands` <- c( '0 - 15','15+ - 30','30+ - 45','45+ - 60','60 +')  

leg_df <- data.frame(leg_colours, `Travel Bands`) 

# these are all separate in order to choose appropriate icon and colour

# baseline

sites_hospital_baseline <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 1,
         site_status =='Exisiting') |>
  inner_join(site_summary_base,
            by = c('site' = 'site'))

sites_non_hospital_baseline <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 0,
         site_status =='Exisiting') |>
  inner_join(site_summary_base,
            by = c('site' = 'site'))

# scenario_1

sites_hospital_existing_scenario_1 <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 1,
         site_status =='Exisiting') |>
  inner_join(site_summary_1,
            by = c('site' = 'site'))

sites_non_hospital_existing_scenario_1 <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 0,
         site_status =='Exisiting') |>
  inner_join(site_summary_1,
            by = c('site' = 'site'))

sites_hospital_potential_scenario_1 <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 1,
         site_status =='Potential') |>
  inner_join(site_summary_1,
            by = c('site' = 'site'))

sites_non_hospital_potential_scenario_1 <- sites |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         hospital_flag == 0,
         site_status =='Potential') |>
  inner_join(site_summary_1,
            by = c('site' = 'site'))



         
# create pallet
#pal <- colorBin("YlOrRd", domain = shapefile_pt$time_band_n_p, 
#                bins = length(unique(shapefile_pt$time_band_n_p)))

pal <- colorBin("YlOrRd", domain = seq(1:6), 
                bins = 6)

pal_pop <- colorBin("Blues", domain = seq(1:6), 
                bins = 6)

# Create a cloropleth layer
cloropleth_map <- leaflet(map_data_base) |>
  addTiles() |>
  # addPolygons(data = shapefile_c,
  #             fillColor = ~pal_pop(wt_pop),
  #             weight = 2,
  #             opacity = 1,
  #             color = "white",
  #             dashArray = "3",
  #             fillOpacity = 0.7,
  #             popup = paste0(shapefile_c$origin_lsoa, "<br>",
  #                           "Diabetic Population: ", round(shapefile_c$wt_pop,0), "<br>"),
  #             group = 'Population'
  # )  |>
  addPolygons(data = map_data_base,
              fillColor = ~pal(time_band_n_h),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
             # popup = paste0(shapefile_c$origin_lsoa, "<br>",
            #                 "Diabetic Population: ", round(shapefile_c$wt_pop,0), "<br>",
            #                 "Travel time: ",round(shapefile_c$travel_time_car, 1), " mins", "<br>",
            #                 "To site: ", shapefile_c$site),
              group = 'Base'
              )  |>
  addPolygons(data = map_data_1,
              fillColor = ~pal(time_band_n_h),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
           #   popup = paste0(shapefile_pt$origin_lsoa, "<br>",
          #                   "Diabetic Population: ", round(shapefile_pt$wt_pop,0), "<br>",
          #                   "Travel time: ",round(shapefile_pt$travel_time_pt_peak, 1), " mins", "<br>",
           #                  "To site: ", shapefile_pt$site),
              group = 'Scenario_1'
  )  |>
  # addLegend("bottomright", 
  #                          pal = pal, 
  #                          values = ~time_band_n,
  #                          #labels = c('a', 'b', 'c'),
  #                          title = "Travel time bands",
  #                          opacity = 1) |>
  
  # base markers for hospital and non hospital sites
  
  addMarkers( lng = sites_hospital_baseline$long,
              lat = sites_hospital_baseline$lat,
              icon = magicIcons(icon = 'hospital'),
              # popup = paste0(sites_h$site, "<br>",
              #                "Mean travel time: ", sites_h$mean_trav, " mins <br>",
              #                "Population < 15 mins: ", prettyNum(sites_h$`0 - 15`, big.mark = ","), "<br>",
              #                "Population 15 < 30 mins: ", prettyNum(sites_h$`15+ - 30`, big.mark = ","), "<br>",
              #                "Population 30 < 45 mins: ", prettyNum(sites_h$`30+ - 45`, big.mark = ","), "<br>",
              #                "Population 45 < 60 mins: ", prettyNum(sites_h$`45+ - 60`, big.mark = ","), "<br>",
              #                "Population 60+ mins: ", prettyNum(sites_h$`60 +`, big.mark = ",")
              #                ),
              group = 'Base') |>
  addMarkers( lng = sites_non_hospital_baseline$long,
              lat = sites_non_hospital_baseline$lat,
              icon = magicIcons(icon = 'house-medical'),
              # popup = paste0(sites_m$site, "<br>",
              #                "Mean travel time: ", sites_m$mean_trav, " mins <br>",
              #                "Population < 15 mins: ", prettyNum(round(sites_m$`0 - 15`,0), big.mark = ","), "<br>",
              #                "Population 15 < 30 mins: ", prettyNum(round(sites_m$`15+ - 30`,0), big.mark = ","), "<br>",
              #                "Population 30 < 45 mins: ", prettyNum(round(sites_m$`30+ - 45`,0), big.mark = ","), "<br>",
              #                "Population 45 < 60 mins: ", prettyNum(round(sites_m$`45+ - 60`,0), big.mark = ","), "<br>",
              #                "Population 60+ mins: ", prettyNum(round(sites_m$`60 +`,0), big.mark = ",")
              # ),
              group = 'Base') |>
  
  # sceanario 1
  
  # base markers
  
  addMarkers( lng = sites_hospital_existing_scenario_1$long,
              lat = sites_hospital_existing_scenario_1$lat,
              icon = magicIcons(icon = 'hospital'),
              # popup = paste0(sites_h$site, "<br>",
              #                "Mean travel time: ", sites_h$mean_trav, " mins <br>",
              #                "Population < 15 mins: ", prettyNum(sites_h$`0 - 15`, big.mark = ","), "<br>",
              #                "Population 15 < 30 mins: ", prettyNum(sites_h$`15+ - 30`, big.mark = ","), "<br>",
              #                "Population 30 < 45 mins: ", prettyNum(sites_h$`30+ - 45`, big.mark = ","), "<br>",
              #                "Population 45 < 60 mins: ", prettyNum(sites_h$`45+ - 60`, big.mark = ","), "<br>",
              #                "Population 60+ mins: ", prettyNum(sites_h$`60 +`, big.mark = ",")
              #                ),
              group = 'Scenario_1') |>
  addMarkers( lng = sites_non_hospital_existing_scenario_1$long,
              lat = sites_non_hospital_existing_scenario_1$lat,
              icon = magicIcons(icon = 'house-medical'),
              # popup = paste0(sites_m$site, "<br>",
              #                "Mean travel time: ", sites_m$mean_trav, " mins <br>",
              #                "Population < 15 mins: ", prettyNum(round(sites_m$`0 - 15`,0), big.mark = ","), "<br>",
              #                "Population 15 < 30 mins: ", prettyNum(round(sites_m$`15+ - 30`,0), big.mark = ","), "<br>",
              #                "Population 30 < 45 mins: ", prettyNum(round(sites_m$`30+ - 45`,0), big.mark = ","), "<br>",
              #                "Population 45 < 60 mins: ", prettyNum(round(sites_m$`45+ - 60`,0), big.mark = ","), "<br>",
              #                "Population 60+ mins: ", prettyNum(round(sites_m$`60 +`,0), big.mark = ",")
              # ),
              group = 'Scenario_1') |>
  
  # potential markers
  
  addMarkers( lng = sites_hospital_potential_scenario_1$long,
              lat = sites_hospital_potential_scenario_1$lat,
              icon = magicIcons(icon = 'hospital',
                                markerColor = leaf.magic::awesomePalette$green),
              # popup = paste0(sites_h$site, "<br>",
              #                "Mean travel time: ", sites_h$mean_trav, " mins <br>",
              #                "Population < 15 mins: ", prettyNum(sites_h$`0 - 15`, big.mark = ","), "<br>",
              #                "Population 15 < 30 mins: ", prettyNum(sites_h$`15+ - 30`, big.mark = ","), "<br>",
              #                "Population 30 < 45 mins: ", prettyNum(sites_h$`30+ - 45`, big.mark = ","), "<br>",
              #                "Population 45 < 60 mins: ", prettyNum(sites_h$`45+ - 60`, big.mark = ","), "<br>",
              #                "Population 60+ mins: ", prettyNum(sites_h$`60 +`, big.mark = ",")
              #                ),
              group = 'Scenario_1') |>
  addMarkers( lng = sites_non_hospital_potential_scenario_1$long,
              lat = sites_non_hospital_potential_scenario_1$lat,
              icon = magicIcons(icon = 'house-medical',
                                markerColor = leaf.magic::awesomePalette$green),
              # popup = paste0(sites_m$site, "<br>",
              #                "Mean travel time: ", sites_m$mean_trav, " mins <br>",
              #                "Population < 15 mins: ", prettyNum(round(sites_m$`0 - 15`,0), big.mark = ","), "<br>",
              #                "Population 15 < 30 mins: ", prettyNum(round(sites_m$`15+ - 30`,0), big.mark = ","), "<br>",
              #                "Population 30 < 45 mins: ", prettyNum(round(sites_m$`30+ - 45`,0), big.mark = ","), "<br>",
              #                "Population 45 < 60 mins: ", prettyNum(round(sites_m$`45+ - 60`,0), big.mark = ","), "<br>",
              #                "Population 60+ mins: ", prettyNum(round(sites_m$`60 +`,0), big.mark = ",")
              # ),
              group = 'Scenario_1') |>

  # addMarkers( lng = sites_m$long,
  #             lat = sites_m$lat,
  #             icon = magicIcons(icon = 'truck-medical'),
  #             popup = paste0(sites_m$site, "<br>",
  #                            "Mean travel time: ", sites_m_p$mean_trav, " mins <br>",
  #                            "Population < 15 mins: ", prettyNum(round(sites_m_p$`0 - 15`,0), big.mark = ","), "<br>",
  #                            "Population 15 < 30 mins: ", prettyNum(round(sites_m_p$`15+ - 30`,0), big.mark = ","), "<br>",
  #                            "Population 30 < 45 mins: ", prettyNum(round(sites_m_p$`30+ - 45`,0), big.mark = ","), "<br>",
  #                            "Population 45 < 60 mins: ", prettyNum(round(sites_m_p$`45+ - 60`,0), big.mark = ","), "<br>",
  #                            "Population 60+ mins: ", prettyNum(round(sites_m_p$`60 +`,0), big.mark = ",")
  #             ),
  #             group = 'Public Transport') |>
  addLayersControl(baseGroups = c('Base',
                                  'Scenario_1'),
                   options = layersControlOptions(collapsed = FALSE))

cloropleth_map
}


cli_alert_success('Creating mapping function - complete')



icb_map('Cornwall')



