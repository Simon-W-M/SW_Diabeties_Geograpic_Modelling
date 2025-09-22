
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
         culm_per = round((culative_pop / tot_pop_site) * 100,1),
         .by = site) |>
  arrange(time_band_f_h) |>
  pivot_wider(names_from  = time_band_f_h,
              values_from  = c(mean_trav_band,
                               tot_pop_band,
                               culative_pop,
                               culm_per)) |>
  select(icb_short,
         site,
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
         `tot_pop_band_60 +`)

pop_h[is.na(pop_h)] <- 0

# population served by travel time / site           
pop_h_t <- data.frame(df) |>
  filter(close_site_hybrid == 1) |>
  select(-geometry) |>
  #select(site, wt_pop, time_band_f) |>
  mutate(mean_trav = round(weighted.mean(travel_time_hybrid, wt_pop, na.rm = TRUE), 1),
         .by = icb_short) |>
  mutate(mean_trav_band = round(weighted.mean(travel_time_hybrid, wt_pop, na.rm = TRUE), 1),
         .by = c(icb_short, time_band_f_h)) |>
  summarise(tot_pop_band = round(sum(wt_pop),1),
            .by = c(icb_short,mean_trav,mean_trav_band, time_band_f_h)) |>
  mutate(tot_pop_site = sum(tot_pop_band),
         .by = icb_short) |>
  arrange(icb_short, time_band_f_h ) |>
  mutate(culative_pop = cumsum(tot_pop_band),
         culm_per = round((culative_pop / tot_pop_site) * 100,1),
         .by = icb_short) |>
  arrange(time_band_f_h) |>
  pivot_wider(names_from  = time_band_f_h,
              values_from  = c(mean_trav_band,
                               tot_pop_band,
                               culative_pop,
                               culm_per)) |>
  select(icb_short,
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
         `tot_pop_band_60 +`)

pop_h[is.na(pop_h)] <- 0

wd <-80

saveRDS(pop, 'pop.rds')

z <- pop_h |>
  bind_rows(pop_h_t) |>
  filter(icb_short == 'Devon') |>
  select(-icb_short) |>
  gt() |>
  fmt_number(
    columns = everything(),
    use_seps = TRUE, decimals = 1
  ) |>
  gt_duplicate_column(`culm_per_0 - 15`,
                      after = `culm_per_0 - 15`,
                      append_text = "bar"
  ) |>
  gt_plt_bar_pct(
    column = `culm_per_0 - 15bar`,
    scaled = TRUE,
    fill = "#005EB8",
    background = "lightblue",
    width = wd
  ) |>
  gt_duplicate_column(`culm_per_15+ - 30`,
                      after = `culm_per_15+ - 30`,
                      append_text = "bar"
  ) |>
  gt_plt_bar_pct(
    column = `culm_per_15+ - 30bar`,
    scaled = TRUE,
    fill = "#005EB8",
    background = "lightblue",
    width = wd
  ) |>
  gt_duplicate_column(`culm_per_30+ - 45`,
                      after = `culm_per_30+ - 45`,
                      append_text = "bar"
  ) |>
  gt_plt_bar_pct(
    column = `culm_per_30+ - 45bar`,
    scaled = TRUE,
    fill = "#005EB8",
    background = "lightblue",
    width = wd
  ) |>
  gt_duplicate_column(`culm_per_45+ - 60`,
                      after = `culm_per_45+ - 60`,
                      append_text = "bar"
  ) |>
  gt_plt_bar_pct(
    column = `culm_per_45+ - 60bar`,
    scaled = TRUE,
    fill = "#005EB8",
    background = "lightblue",
    width = wd
  ) |>
  gt_duplicate_column(`culm_per_60 +`,
                      after = `culm_per_60 +`,
                      append_text = "bar"
  ) |>
  gt_plt_bar_pct(
    column = `culm_per_60 +bar`,
    scaled = TRUE,
    fill = "#005EB8",
    background = "lightblue",
    width = wd
  ) |>
  sub_missing(missing_text = "-") |>
  cols_merge(
    columns = c(
      `culative_pop_0 - 15`,
      `culm_per_0 - 15`
    ),
    pattern = "{1} ({2}%)"
  ) |>
  cols_merge(
    columns = c(
      `culative_pop_15+ - 30`,
      `culm_per_15+ - 30`
    ),
    pattern = "{1} ({2}%)"
  ) |>
  cols_merge(
    columns = c(
      `culative_pop_30+ - 45`,
      `culm_per_30+ - 45`
    ),
    pattern = "{1} ({2}%)"
  ) |>
  cols_merge(
    columns = c(
      `culative_pop_45+ - 60`,
      `culm_per_45+ - 60`
    ),
    pattern = "{1} ({2}%)"
  ) |>
  cols_merge(
    columns = c(
      `culative_pop_60 +`,
      `culm_per_60 +`
    ),
    pattern = "{1} ({2}%)"
  ) |>
  cols_merge(
    columns = c(
      `tot_pop_band_0 - 15`,
      `mean_trav_band_0 - 15`
    ),
    pattern = "{1} ({2} mins)") |>
  cols_merge(
    columns = c(
      `tot_pop_band_15+ - 30`,
      `mean_trav_band_15+ - 30`
    ),
    pattern = "{1} ({2} mins)")|>
  cols_merge(
    columns = c(
      `tot_pop_band_30+ - 45`,
      `mean_trav_band_30+ - 45`
    ),
    pattern = "{1} ({2} mins)")|>
  cols_merge(
    columns = c(
      `tot_pop_band_45+ - 60`,
      `mean_trav_band_45+ - 60`
    ),
    pattern = "{1} ({2} mins)")|>
  cols_merge(
    columns = c(
      `tot_pop_band_60 +`,
      `mean_trav_band_60 +`
    ),
    pattern = "{1} ({2} mins)") |>
  tab_spanner(label = "Total Site", 
              columns = c(
    mean_trav,
    tot_pop_site
  )) |>
  cols_move(columns = `culm_per_0 - 15bar`,
            after = `culative_pop_0 - 15`) |>
  cols_move(columns = `culm_per_15+ - 30bar`,
            after = `culative_pop_15+ - 30`) |>
  cols_move(columns = `culm_per_30+ - 45bar`,
            after = `culative_pop_30+ - 45`)|>
  cols_move(columns = `culm_per_45+ - 60bar`,
            after = `culative_pop_45+ - 60`)|>
  cols_move(columns = `culm_per_60 +bar`,
            after = `culative_pop_60 +`) |>
  cols_label(
    mean_trav = "Mean Travel Time (Mins)",
    tot_pop_site = "Diabetes Population",
    `culative_pop_0 - 15` = "0 - 15",
    `culm_per_0 - 15bar` = "",
    `culative_pop_15+ - 30` = "15+ - 30",
    `culm_per_15+ - 30bar` = "",
    `culative_pop_30+ - 45` = "30+ - 45",
    `culm_per_30+ - 45bar` = "",
    `culative_pop_45+ - 60` = "45+ - 60",
    `culm_per_45+ - 60bar` = "",
    `culative_pop_60 +` = "60+",
    `culm_per_60 +` = "",
    `culm_per_60 +bar` = "",
    `tot_pop_band_0 - 15` = "0 - 15",
    `tot_pop_band_15+ - 30` = "15+ - 30",
    `tot_pop_band_30+ - 45` = "30+ - 45",
    `tot_pop_band_45+ - 60` = "45+ - 60",
    `tot_pop_band_60 +` = "60+"
  )  |>
  tab_spanner(label = "Cumlative Population Served by Travel Band (%)", 
                 columns = c(
                   `culative_pop_0 - 15`,
                   `culm_per_0 - 15bar`,
                   `culative_pop_15+ - 30`,
                   `culm_per_15+ - 30bar` ,
                   `culative_pop_30+ - 45`,
                   `culm_per_30+ - 45bar`,
                   `culative_pop_45+ - 60`,
                   `culm_per_45+ - 60bar`,
                   `culative_pop_60 +`,
                   `culm_per_60 +` 
                   )) |>
  tab_spanner(label = "Population by Time Band (Mean Travel Time)", 
              columns = c(
                `tot_pop_band_0 - 15`,
                `tot_pop_band_15+ - 30`,
                `tot_pop_band_30+ - 45`,
                `tot_pop_band_45+ - 60` ,
                `tot_pop_band_60 +`
              )) 
