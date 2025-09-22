
cli_alert('Calculating optimal site')

# assign each origin lsoa to the site that is closest with the icb
# thus
# pick minimum travel time
# where lsoa of icb = lsoa of icb of site 

df <- data  |>
  #filter(origin_lsoa == 'E01022116') |>
  filter(icb_site == icb_short) |>
  mutate(min_trav = min(travel_time_car, na.rm = T),             # calculate min travel time by site/icb
         min_pub_trav = min(travel_time_pt_peak, na.rm = T),
         min_hybrid_trav = min(travel_time_hybrid, na.rm = T),
         .by = c(icb, origin_lsoa)) |>  
  mutate(close_site = if_else(min_trav == travel_time_car,       # add flag for min travel time
                              1, 0),
         close_site_pub = if_else(min_pub_trav == travel_time_pt_peak,       # add flag for min travel time
                              1, 0),
         close_site_hybrid = if_else(min_hybrid_trav == travel_time_hybrid,       # add flag for min travel time
                                  1, 0)) |> 
  mutate(time_band = case_when (min_trav <=15 ~ '0 - 15',        # create travel time bins
                                min_trav <=30 ~ '15+ - 30',
                                min_trav <=45 ~ '30+ - 45',
                                min_trav <=60 ~ '45+ - 60',
                                min_trav > 60 ~ '60 +',
                                .default = 'Please check'),
         time_band_n = case_when (min_trav <=15 ~ 1,
                                 min_trav <=30 ~ 2,
                                 min_trav <=45 ~ 3,
                                 min_trav <=60 ~ 4,
                                 min_trav > 60 ~ 5,
                                 .default = 6),
         time_band_f = factor(time_band,
                               levels=c('0 - 15',
                                        '15+ - 30',
                                        '30+ - 45',
                                        '45+ - 60',
                                        '60 +')),
         time_band_p = case_when (min_pub_trav <=15 ~ '0 - 15',        # create travel time bins
                                  min_pub_trav <=30 ~ '15+ - 30',
                                  min_pub_trav <=45 ~ '30+ - 45',
                                  min_pub_trav <=60 ~ '45+ - 60',
                                  min_pub_trav > 60 ~ '60 +',
                                .default = 'Please check'),
         time_band_n_p = case_when (min_pub_trav <=15 ~ 1,
                                    min_pub_trav <=30 ~ 2,
                                    min_pub_trav <=45 ~ 3,
                                    min_pub_trav <=60 ~ 4,
                                    min_pub_trav > 60 ~ 5,
                                  .default = 6),
         time_band_f_p = factor(time_band_p,
                              levels=c('0 - 15',
                                       '15+ - 30',
                                       '30+ - 45',
                                       '45+ - 60',
                                       '60 +')),
         time_band_h = case_when (min_hybrid_trav <=15 ~ '0 - 15',        # create travel time bins
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
                                         '60 +')),
         hospital_flag = if_else(str_detect(site, 'Hospital'),1 ,0)) # add flags for hospitals

cli_alert_success('Calculating optimal site - complete')

# population served by travel time / site           
pop <- data.frame(df) |>
  filter(close_site == 1) |>
  select(-geometry) |>
  #select(site, wt_pop, time_band_f) |>
  mutate(mean_trav = round(weighted.mean(travel_time_car, wt_pop, na.rm = TRUE), 1),
         .by = site) |>
  summarise(tot_pop_site = round(sum(wt_pop),1),
            .by = c(icb_short, site, mean_trav,time_band_f)) |>
  arrange(time_band_f) |>
  pivot_wider(names_from  = time_band_f,
              values_from  = tot_pop_site)

pop[is.na(pop)] <- 0

# population served by travel time / site           
pop_p <- data.frame(df) |>
  filter(close_site_pub == 1) |>
  select(-geometry) |>
  #select(site, wt_pop, time_band_f) |>
  mutate(mean_trav = round(weighted.mean(travel_time_pt_peak, wt_pop, na.rm = TRUE), 1),
         .by = site) |>
  summarise(tot_pop_site = round(sum(wt_pop),1),
            .by = c(icb_short, site, mean_trav,time_band_f_p)) |>
  arrange(time_band_f_p) |>
  pivot_wider(names_from  = time_band_f_p,
              values_from  = tot_pop_site)

pop_p[is.na(pop_p)] <- 0

# population served by travel time / site           
pop_h <- data.frame(df) |>
  filter(close_site_hybrid == 1) |>
  select(-geometry) |>
  #select(site, wt_pop, time_band_f) |>
  mutate(mean_trav = round(weighted.mean(travel_time_hybrid, wt_pop, na.rm = TRUE), 1),
         .by = site) |>
  mutate(mean_trav_band = round(weighted.mean(travel_time_hybrid, wt_pop, na.rm = TRUE), 1),
         .by = c(site, time_band_f_h)) |>
  summarise(tot_pop_band = round(sum(wt_pop),1),
            .by = c(icb_short, site, mean_trav,mean_trav_band, time_band_f_h)) |>
  mutate(tot_pop_site = sum(tot_pop_band),
         .by = site) |>
  arrange(site, time_band_f_h ) |>
  mutate(culative_pop = cumsum(tot_pop_band),
         culm_per = (culative_pop / tot_pop_site),
         .by = site) |>
  arrange(time_band_f_h) |>
  pivot_wider(names_from  = time_band_f_h,
              values_from  = c(tot_pop_band,
                               mean_trav_band))

pop_h[is.na(pop_h)] <- 0



saveRDS(pop, 'pop.rds')

pop |> group_by (icb_short) |>
  gt() |>
  sub_missing(missing_text = "-")
