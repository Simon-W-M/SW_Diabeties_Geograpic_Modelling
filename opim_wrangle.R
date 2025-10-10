

icb_filt <- 'BSW'
number_additional_sites <- 1

df_o <- data  |>
  filter(icb_site == icb_filt,
         analysis == 'OCT',
         site_status == 'Exisiting') 


df_p <- data |>
  filter(icb_site == icb_filt   ,
         analysis == 'OCT',
         site_status == 'Potential')

perms <- combn(unique(df_p$site_short_name), number_additional_sites)

number_perms <- ncol(perms)

#per_string <- paste(perm[,2], collapse = '-')

results <- df_p |>
  filter(site == 'Return no results') 

#cli_progress_bar('Processesing iterations', total = number_perms)

print(paste0('Number of perms:', number_perms))

for (i in 1:number_perms) {
  
  cli_progress_bar('Processesin iterations', total = number_perms)
  
  print(paste0('Perm: ', i, ' of ', number_perms))
  
  if (number_additional_sites == 1) {
    df_perm <- df_p |>
      filter(site_short_name == c(perms[1,i]))
  } 
  
  if (number_additional_sites == 2) {
    df_perm <- df_p |>
      filter(site_short_name %in% c(perms[1,i], perms[2,i]))
  } 
  
  if (number_additional_sites == 3) {
    df_perm <- df_p |>
      filter(site_short_name %in% c(perms[1,i], perms[2,i], perms[3,i]))
  }  

  
  df_comb <- df_o |>
    bind_rows(df_perm)
  
  df_comb <- df_comb |>
    filter(icb_site == icb_short) |>
  mutate(min_hybrid_trav = min(travel_time_hybrid, na.rm = T),
         .by = c(icb, origin_lsoa)) |>  
    #mutate(close_site_hybrid = if_else(min_hybrid_trav == travel_time_hybrid,       # add flag for min travel time
    #                                   1, 0)) |> 
    filter(min_hybrid_trav == travel_time_hybrid) |>
    mutate(time_band_h = case_when (min_hybrid_trav <=15 ~ '0 - 15',        # create travel time bins
                                    min_hybrid_trav <=30 ~ '15+ - 30',
                                    min_hybrid_trav <=45 ~ '30+ - 45',
                                    min_hybrid_trav <=60 ~ '45+ - 60',
                                    min_hybrid_trav > 60 ~ '60 +',
                                    .default = 'Please check'),
           time_band_n_h = case_when (min_hybrid_trav <=15 ~ 1,
                                      min_hybrid_trav <=30 ~ 2,
                                      min_hybrid_trav <=45 ~ 3,
                                      min_hybrid_trav <=60 ~ 4,
                                      min_hybrid_trav > 60 ~ 5,
                                      .default = 6),
           time_band_f_h = factor(time_band_h,
                                  levels=c('0 - 15',
                                           '15+ - 30',
                                           '30+ - 45',
                                           '45+ - 60',
                                           '60 +')))
           
  if (number_additional_sites == 1) {
    df_comb <- df_comb |>
      mutate(iteration = paste0(perms[1,i])) 
  } 
  
  if (number_additional_sites == 2) {
    df_comb <- df_comb |>
      mutate(iteration = paste0(perms[1,i], ' & ', perms[2,i])) 
  } 
  
  if (number_additional_sites == 3) {
    df_comb <- df_comb |>
      mutate(iteration = paste0(perms[1,i], ' & ', perms[2,i], ' & ', perms[3,i])) 
  } 
    
     results <- results |>
      bind_rows(df_comb)
    
    cli_progress_update()
  
}




# 
# table(results$iteration)
# 
# z <- data |> filter(icb_site == 'BNSSG') 
# 
# table(z$site_short_name)
# 
# length(unique(z$origin_lsoa))

iteration_summary <- data.frame(results) |>
  select(-geometry) |>
    #select(site, wt_pop, time_band_f) |>
  mutate(mean_trav = round(weighted.mean(travel_time_hybrid, wt_pop, na.rm = TRUE), 1),
         .by = iteration) |>
  mutate(mean_trav_band = round(weighted.mean(travel_time_hybrid, wt_pop, na.rm = TRUE), 1),
         .by = c(iteration, time_band_f_h)) |>
  summarise(tot_pop_band = round(sum(wt_pop),1),
            .by = c(iteration,mean_trav,mean_trav_band, time_band_f_h)) |>
  mutate(tot_pop_site = sum(tot_pop_band),
         .by = iteration) |>
  arrange(iteration, time_band_f_h ) |>
  
  mutate(culative_pop = cumsum(tot_pop_band),
         culm_per = round((culative_pop / tot_pop_site) * 100,1),
         .by = iteration) |>
  arrange(time_band_f_h) |>
  pivot_wider(names_from  = time_band_f_h,
              values_from  = c(mean_trav_band,
                               tot_pop_band,
                               culative_pop,
                               culm_per)) |>
  select(iteration,
         tot_pop_site,
         mean_trav,
         `culative_pop_0 - 15`,
         `culative_pop_15+ - 30`,
         `culative_pop_30+ - 45`,
         `culative_pop_45+ - 60`,
         `culative_pop_60 +`,
         `culm_per_0 - 15`,
         `culm_per_15+ - 30`,
         `culm_per_30+ - 45`,
         `culm_per_45+ - 60`,
         `culm_per_60 +`,
         `mean_trav_band_0 - 15`,
         `mean_trav_band_15+ - 30`,
         `mean_trav_band_30+ - 45`,
         `mean_trav_band_45+ - 60`,
         `mean_trav_band_60 +`,
         `tot_pop_band_0 - 15`,
         `tot_pop_band_15+ - 30`,
         `tot_pop_band_30+ - 45`,
         `tot_pop_band_45+ - 60`,
         `tot_pop_band_60 +`
         )

saveRDS(results,"results.rds")
 

# results

#it_sum_bnssg3
#Yate West Gate Centre & Cadbury Heath Healthcare & Clevedon Medical Centre
#Yate West Gate Centre & Hanham Community Centre & Clevedon Medical Centre
#Yate West Gate Centre & Willow Surgery & Clevedon Medical Centre

#it_sum_bnssg_2
#	Willow Surgery & Clevedon Medical Centre
#Willow Surgery & Parish Wharf Leisure Centre
#Yate West Gate Centre & Clevedon Medical Centre

#it_sum_bnssg_1
# Willow Surgery
# Yate West Gate Centre
# Lawrence Hill Health Centre

#it_sum_devon3
#Kingsbridge Community Hospital & Honiton Hospital & Okehampton Community Hospital
#Totnes Hospital & Honiton Hospital & Okehampton Community Hospital
#Axminster Hospital & Kingsbridge Community Hospital & Okehampton Community Hospital

#it_sum_devon2
#Okehampton Community Hospital & Honiton Hospital
#Okehampton Community Hospital & Axminster Hospital
#Crediton Hospital & Honiton Hospital

#it_sum_devon1
#Honiton Hospital
#Axminster Hospital
#Seaton Hospital Valley

#it_sum_cornwall3
#Helston Hospital & Saltash Wesley Church & Launceston Town Hall
#Saltash Wesley Church & Launceston Town Hall & Helston Medical Centre
#Saltash Wesley Church & Launceston Town Hall & Mullion Surgery

#it_sum_cornwall2
#Saltash Wesley Church & Launceston Town Hall
#Saltash Wesley Church & Bude Methodist Church
#Launceston Town Hall & Liskerrett Community Centre

#it_sum_cornwall1
#Liskerrett Community Centre
#Saltash Wesley Church
#Launceston Town Hall

#it_sum_dorset3
#NA

#it_sum_dorset2
#Blandford Day Centre & Dorset County Hospital

#it_sum_dorset1
#Blandford Day Centre
#Dorset County Hospital

#it_sum_gloucestershire3
#Tewkesbury Community Hospital & The Vale community Hospital & Cirencester Hospital

#it_sum_gloucestershire2
#The Vale community Hospital & Cirencester Hospital
#Tewkesbury Community Hospital & Cirencester Hospital
#Tewkesbury Community Hospital & The Vale community Hospital

#it_sum_gloucestershire1
#Cirencester Hospital
#Tewkesbury Community Hospital
#The Vale community Hospital

#it_sum_bsw2
#Trowbridge Health Centre  & Hathaway Medical Centre

#it_sum_bsw1
#Hathaway Medical Centre
#Trowbridge Health Centre

