

#############
# data load #
#############

cli_alert('Running data query - please input your username and credentials')
cli_alert_warning('NOTE: This may be in a seperate window')

serv <- "udalsyndataprod.sql.azuresynapse.net"
db <- "UDAL_Warehouse"

con_udal <- DBI::dbConnect(
  drv = odbc::odbc(),
  driver = "ODBC Driver 17 for SQL Server",
  server = serv,
  database = "UDAL_Warehouse",
  authentication = "ActiveDirectoryInteractive"
)

cli_alert_success('Complete')
cli_alert('Downloading population data from UDAL')

# this creates a dataframe that returns population by age for each LOSA

pop_dat <- DBI::dbGetQuery(conn = con_udal, statement = paste0("

-- 65 pop by LSOA - join to above
  SELECT [Area_Code] ,
  [age],
  [Size] 
  FROM [UKHF_Demography].[ONS_Population_Estimates_For_LSOAs_By_Year_Of_Age1] AS p
 WHERE p.[Effective_Snapshot_Date] = 
 (select max(effective_snapshot_date) as effective_snapshot_date
   FROM [UKHF_Demography].[ONS_Population_Estimates_For_LSOAs_By_Year_Of_Age1] as mp
 where p.area_code = mp.area_code
   group by mp.[Area_Code])

")  )

# code to check that site has a correct lsoa
# dat_z <- dat |>
#   filter(origin_name == destination_name)

cli_alert_success('Downloading population data from UDAL - complete')

cli_alert('Caluculating weighted population')

pop_clean <- pop_dat |>
# converts column to numeric   
  mutate(age = if_else(age == '90+', 90, as.numeric(age)), 
# creates age bins
         band = case_when(age > 75 ~ '75+',        
                          age >= 65 ~ '65-74',
                          age >= 55 ~ '55-64',
                          age >= 45 ~ '45-54',
                          age >= 35 ~ '35-44',
                          age >= 25 ~ '25-34',
                          age >= 16 ~ '16-24',
                          age >= 0 ~ '0-15',
                          .default = 'Error - please check')) |>
# sum by band  
  summarise(size = sum(Size), 
            .by = c(Area_Code, band)) |>
# multiply population by  prevalence and uplift
  mutate(size = case_when(band ==  '0-15' ~ (size * 0) * 1.157,
                          band ==  '16-24' ~ (size * 0.008) * 1.157,
                          band ==  '25-34' ~ (size * 0.012) * 1.157,
                          band ==  '35-44' ~ (size * 0.035) * 1.157,
                          band ==  '45-54' ~ (size * 0.09) * 1.157,
                          band ==  '55-64' ~ (size * 0.127) * 1.157,
                          band ==  '65-74' ~ (size * 0.169) * 1.157,
                          band ==  '75+' ~ (size * 0.238) * 1.157,
                          .default = NA
                          )) |>
# add to single total  
  summarise(wt_pop = sum(size),
            .by = Area_Code)
  
cli_alert_success('Caluculating weighted population - complete')



cli_alert('Downloading travel data from UDAL')


trav_dat <- DBI::dbGetQuery(conn = con_udal, statement = paste0("

SELECT [origin_name] ,
  [destination_name],
  [travel_time_car],
  [travel_time_pt_peak],
  [travel_time_pt_nonpeak]
    FROM [AGEM_TravelTime].[LSOA_to_LSOA] AS t
  WHERE [destination_name] IN (", losa_sites ,")
  AND travel_time_car < 250


"))

cli_alert_success('Downloading travel data from UDAL - complete')

# you can use these to save the data so that you don't have to load the data in 
# each and every time whilst testing 

#saveRDS(raw_dat, 'raw_dat.rds')
#dat <- readRDS('raw_dat.rds')

cli_alert('Loading in CSV for ICB data')

# download some regional data - links LSOA to ICB
url <- "https://open-geography-portalx-ons.hub.arcgis.com/api/download/v1/items/8905a9ad35284b78945c3f3eb30498a2/csv?layers=0"
destfile_icb <- "icb_mapping.csv"

# check if you have downloaded data already
if (!file.exists(destfile_icb)) 
{curl_download(url, destfile_icb)}

# loads in a csv that maps the lsoa to an ICB
# could not find a better table in the warehouse that did not have gaps
icb_link <- read.csv("icb_mapping.csv") |>
  clean_names() 

cli_alert_success('Data load complete')

# download the shapefile from 
# https://geoportal.statistics.gov.uk/datasets/ons::lower-layer-super-output-areas-december-2021-boundaries-ew-bsc-v4-2/about
# you want the .shp file as called below
# save it in your folder

cli_alert('Loading in CSV for shapefile data')

# read the shapefile into r
shapefile <- st_read("LSOA_2021_EW_BSC_V4.shp") |>
  clean_names()

cli_alert_success('Data load complete')

cli_alert('Transforming shapefile data')

# need to transform the data into WGS84 standard
shapefile <- shapefile |>
  st_transform('+proj=longlat +datum=WGS84')

cli_alert_success('Transform complete')
cli_alert('Joining datasets')

# now we join the shpae file to the ICB mapping
# and filter to just the SW region
shapefile <- shapefile |>
  left_join(icb_link |> select(lsoa21cd, icb23nm),
           by = c('lsoa21cd' =  'lsoa21cd')) |>
  dplyr::filter(icb23nm %in% c(
    "NHS Bath and North East Somerset, Swindon and Wiltshire Integrated Care Board",
    "NHS Bristol, North Somerset and South Gloucestershire Integrated Care Board"  ,
    "NHS Devon Integrated Care Board"   ,
    "NHS Cornwall and the Isles of Scilly Integrated Care Board"    ,
    "NHS Dorset Integrated Care Board"  ,
    "NHS Gloucestershire Integrated Care Board"    ,
    "NHS Somerset Integrated Care Board"  
  ))

# join to pop data
data <- shapefile |>
  left_join(pop_clean,
            by = c('lsoa21cd' = 'Area_Code'))

# join to travel data
data <- data |>
  left_join(trav_dat,
            by = c('lsoa21cd' = 'origin_name'))


cli_alert('Imputing missing populations')

# impute missing values of population
# take the mean of local region
# local region is derived by removing the last character from the 
# name of the lsoa
# if there are still NaNs after imputation
# a default if 160 is added
data <- data |>
  mutate(local = substr(lsoa21nm, 1, nchar(lsoa21nm) - 1)) |>
  mutate(wt_pop = if_else(is.na(wt_pop), round(mean(wt_pop, na.rm=TRUE),0), wt_pop),
         .by = local) |>
  mutate(wt_pop = if_else(is.nan(wt_pop), 160, wt_pop),
         .by = local) 

cli_alert('Imputing missing travel times')

# impute missing values of travel time
# take the mean of local region by site

# first isolate those without a site
data_t <- data |>
  filter(is.na(destination_name))

# then left join the locations to create a row per location
data_t <- data_t |>
  cross_join(locations)

# add the locations details to the main data
data <- data |>
  filter(!is.na(destination_name)) |>
  left_join(locations,
            by = c('destination_name' = 'losa_sites'))

# stitch the two datasets back together
# if we still have NaNs after imputation, we stick a default of 50 in
data <- bind_rows(data, data_t) |>
  mutate(losa_sites = if_else(is.na(destination_name), losa_sites, destination_name))

# impute a mean travel time by site and local area  
data <- data |>
  mutate(travel_time_car = if_else(is.na(travel_time_car), 
                                   mean(travel_time_car, 
                                        na.rm=TRUE), 
                                   travel_time_car),
         travel_time_pt_peak = if_else(is.na(travel_time_pt_peak), 
                                   mean(travel_time_pt_peak, 
                                        na.rm=TRUE), 
                                   travel_time_pt_peak),
         .by = c(local, losa_sites)) |>
  mutate(travel_time_car = if_else(is.nan(travel_time_car), 
                                   50, 
                                   travel_time_car),
         travel_time_pt_peak = if_else(is.nan(travel_time_pt_peak), 
                                   999, 
                                   travel_time_pt_peak)) |>
  mutate(travel_time_hybrid = ((travel_time_car * 74) + (travel_time_pt_peak * 26)) / 100)
  

cli_alert('Tidying dataframe')
 
# tidy up names of columns
data <- data |>
  select(origin_lsoa = lsoa21cd,
         site_lsoa = losa_sites ,
         wt_pop,
         travel_time_car,
         travel_time_pt_peak,
         travel_time_hybrid,
         icb = icb23nm,
         site,
         icb_site,
         geometry) |>
  mutate(icb_short = case_when(icb == "NHS Devon Integrated Care Board"  ~ 'Devon',
                               icb == "NHS Bristol, North Somerset and South Gloucestershire Integrated Care Board"    ~ 'BNSSG',
                               icb == "NHS Gloucestershire Integrated Care Board"   ~ 'Gloucestershire',
                               icb == "NHS Bath and North East Somerset, Swindon and Wiltshire Integrated Care Board"  ~ 'BSW',
                               icb == "NHS Cornwall and the Isles of Scilly Integrated Care Board"   ~ 'Cornwall',
                               icb == "NHS Somerset Integrated Care Board" ~ 'Somerset',
                               icb == "NHS Dorset Integrated Care Board"   ~ 'Dorset',
                               .default = "*PLEASE CHECK*"),
         travel_time_car = if_else(is.nan(travel_time_car), 999, travel_time_car)
  )

cli_alert_success('Data loaded and joined')

#saveRDS(data, 'data.rds')


#datal <- data |> filter(origin_lsoa == 'E01022116')


