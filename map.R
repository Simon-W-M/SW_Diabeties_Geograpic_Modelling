
cli_alert('Running geocoder on site locations')
 
# use geocoder to get long lats from postcodes
sites <- locations |> 
  mutate(geo(address = postcode, 
             method = 'osm')) |>
  mutate(hospital_flag = if_else(str_detect(site, 'Hospital'),1 ,0))

saveRDS(sites, 'sites.rds')
saveRDS(df, 'df.rds')

cli_alert_success('Running geocoder on site locations - complete')

cli_alert('Creating mapping function')

################
# Map function #
################

icb_map <- function(site_x, data) {

#site_x <- 'Gloucestershire'

leg_colours <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#E31A1C")
`Travel Bands` <- c( '0 - 15','15+ - 30','30+ - 45','45+ - 60','60 +')  
 
  
shapefile_c <- data |>
  filter (icb_short == site_x,
          close_site == 1) 

shapefile_pt <- data |>
  filter (icb_short == site_x,
          close_site_pub == 1)

sites_h <- sites |>
  filter(icb_site == site_x,
         hospital_flag == 1) |>
  left_join(pop,
            by = c('site' = 'site'))

sites_m <- sites |>
  filter(icb_site == site_x,
         hospital_flag == 0) |>
  left_join(pop,
            by = c('site' = 'site'))

sites_h_p <- sites |>
  filter(icb_site == site_x,
         hospital_flag == 1) |>
  left_join(pop_p,
            by = c('site' = 'site'))

sites_m_p <- sites |>
  filter(icb_site == site_x,
         hospital_flag == 0) |>
  left_join(pop_p,
            by = c('site' = 'site'))
         
# create pallet
#pal <- colorBin("YlOrRd", domain = shapefile_pt$time_band_n_p, 
#                bins = length(unique(shapefile_pt$time_band_n_p)))

pal <- colorBin("YlOrRd", domain = shapefile_pt$time_band_n_p, 
                bins = 6)

pal_pop <- colorBin("Blues", domain = shapefile_pt$wt_pop, 
                bins = 6)

# Create a cloropleth layer
cloropleth_map <- leaflet(shapefile_c) |>
  addTiles() |>
  addPolygons(data = shapefile_c,
              fillColor = ~pal_pop(wt_pop),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              popup = paste0(shapefile_c$origin_lsoa, "<br>",
                            "Diabetic Population: ", round(shapefile_c$wt_pop,0), "<br>"),
              group = 'Population'
  )  |>
  addPolygons(data = shapefile_c,
              fillColor = ~pal(time_band_n),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              popup = paste0(shapefile_c$origin_lsoa, "<br>",
                             "Diabetic Population: ", round(shapefile_c$wt_pop,0), "<br>",
                             "Travel time: ",round(shapefile_c$travel_time_car, 1), " mins", "<br>",
                             "To site: ", shapefile_c$site),
              group = 'Car'
              )  |>
  addPolygons(data = shapefile_pt,
              fillColor = ~pal(time_band_n_p),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              popup = paste0(shapefile_pt$origin_lsoa, "<br>",
                             "Diabetic Population: ", round(shapefile_pt$wt_pop,0), "<br>",
                             "Travel time: ",round(shapefile_pt$travel_time_pt_peak, 1), " mins", "<br>",
                             "To site: ", shapefile_pt$site),
              group = 'Public Transport'
  )  |>
  addLegend("bottomright", 
                           pal = pal, 
                           values = ~time_band_n,
                           #labels = c('a', 'b', 'c'),
                           title = "Travel time bands",
                           opacity = 1) |>
  addMarkers( lng = sites_h$long,
              lat = sites_h$lat,
              icon = magicIcons(icon = 'hospital'),
              popup = paste0(sites_h$site, "<br>",
                             "Mean travel time: ", sites_h$mean_trav, " mins <br>",
                             "Population < 15 mins: ", prettyNum(sites_h$`0 - 15`, big.mark = ","), "<br>",
                             "Population 15 < 30 mins: ", prettyNum(sites_h$`15+ - 30`, big.mark = ","), "<br>",
                             "Population 30 < 45 mins: ", prettyNum(sites_h$`30+ - 45`, big.mark = ","), "<br>",
                             "Population 45 < 60 mins: ", prettyNum(sites_h$`45+ - 60`, big.mark = ","), "<br>",
                             "Population 60+ mins: ", prettyNum(sites_h$`60 +`, big.mark = ",")
                             ),
              group = 'Car') |>
  addMarkers( lng = sites_m$long,
              lat = sites_m$lat,
              icon = magicIcons(icon = 'truck-medical'),
              popup = paste0(sites_m$site, "<br>",
                             "Mean travel time: ", sites_m$mean_trav, " mins <br>",
                             "Population < 15 mins: ", prettyNum(round(sites_m$`0 - 15`,0), big.mark = ","), "<br>",
                             "Population 15 < 30 mins: ", prettyNum(round(sites_m$`15+ - 30`,0), big.mark = ","), "<br>",
                             "Population 30 < 45 mins: ", prettyNum(round(sites_m$`30+ - 45`,0), big.mark = ","), "<br>",
                             "Population 45 < 60 mins: ", prettyNum(round(sites_m$`45+ - 60`,0), big.mark = ","), "<br>",
                             "Population 60+ mins: ", prettyNum(round(sites_m$`60 +`,0), big.mark = ",")
              ),
              group = 'Car') |>
  addMarkers( lng = sites_h$long,
              lat = sites_h$lat,
              icon = magicIcons(icon = 'hospital'),
              popup = paste0(sites_h$site, "<br>",
                             "Mean travel time: ", sites_m_p$mean_trav, " mins <br>",
                             "Population < 15 mins: ", prettyNum(sites_m_p$`0 - 15`, big.mark = ","), "<br>",
                             "Population 15 < 30 mins: ", prettyNum(sites_m_p$`15+ - 30`, big.mark = ","), "<br>",
                             "Population 30 < 45 mins: ", prettyNum(sites_m_p$`30+ - 45`, big.mark = ","), "<br>",
                             "Population 45 < 60 mins: ", prettyNum(sites_m_p$`45+ - 60`, big.mark = ","), "<br>",
                             "Population 60+ mins: ", prettyNum(sites_m_p$`60 +`, big.mark = ",")
              ),
              group = 'Public Transport') |>
  addMarkers( lng = sites_m$long,
              lat = sites_m$lat,
              icon = magicIcons(icon = 'truck-medical'),
              popup = paste0(sites_m$site, "<br>",
                             "Mean travel time: ", sites_m_p$mean_trav, " mins <br>",
                             "Population < 15 mins: ", prettyNum(round(sites_m_p$`0 - 15`,0), big.mark = ","), "<br>",
                             "Population 15 < 30 mins: ", prettyNum(round(sites_m_p$`15+ - 30`,0), big.mark = ","), "<br>",
                             "Population 30 < 45 mins: ", prettyNum(round(sites_m_p$`30+ - 45`,0), big.mark = ","), "<br>",
                             "Population 45 < 60 mins: ", prettyNum(round(sites_m_p$`45+ - 60`,0), big.mark = ","), "<br>",
                             "Population 60+ mins: ", prettyNum(round(sites_m_p$`60 +`,0), big.mark = ",")
              ),
              group = 'Public Transport') |>
  addLayersControl(baseGroups = c('Population',
                                  'Car',
                                     'Public Transport'),
                   options = layersControlOptions(collapsed = FALSE))

cloropleth_map
}


cli_alert_success('Creating mapping function - complete')

icb_map('Devon', df)






