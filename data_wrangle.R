
cli_alert('Calculating optimal site')

# assign each origin lsoa to the site that is closest with the icb
# thus
# pick minimum travel time
# where lsoa of icb = lsoa of icb of site 

# function to create data frames per scenario

# test variables
# oct_or_hes <- 'OCT'  # or HES
# icb_filt <- 'Cornwall'
# number_sites <- 2
# scenario <- 1

#' @title Prepare Travel Time Data for ICB Site Selection Analysis
#'
#' @description
#' This function filters and processes travel time data for a specific Integrated Care Board (ICB)
#' and a defined site selection scenario. It combines all 'Existing' sites within the ICB
#' with a set of 'optimal' sites (determined by the scenario), and then calculates the minimum
#' hybrid (car/public transport) travel time from each patient origin (LSOA) to the
#' closest site in the selected network.
#'
#' @details
#' The function relies on two external data frames that must be available in the environment:
#' \itemize{
#'   \item \code{combos_table}: Used to determine the specific list of 'optimal' sites based on the
#'     \code{scenario}, \code{number_sites}, and \code{icb_filt}.
#'   \item \code{data}: Contains the raw LSOA-to-site travel time data and site metadata.
#' }
#' The function returns a tibble filtered to contain only the connection to the nearest site
#' for each unique \code{origin_lsoa} in the specified ICB, based on the hybrid travel time.
#'
#' @param oct_or_hes A character string specifying the type of analysis data to use.
#'   Must match values in the \code{analysis} column of the \code{data} frame (e.g., 'OCT'
#'   for Ophthalmology Community Testing or 'HES' for benchmark against HES sites).
#'   Defaults to \code{'OCT'}.
#' @param icb_filt A character string specifying the ICB to filter the data for (e.g., 'Cornwall').
#'   Must match values in the \code{icb}, \code{icb_short}, and \code{icb_site} columns.
#'   Defaults to \code{'Cornwall'}.
#' @param number_sites An integer specifying the number of sites in the optimisation scenario.
#'   Used to filter the \code{combos_table} on the \code{no_sites} column. This argument is
#'   mandatory. A zero can be entered for the base scenario or HES sites.
#' @param scenario A character string identifying the specific optimization scenario or combo ID.
#'   Used to filter the \code{combos_table} on the \code{combo} column. This argument is mandatory.
#'   A zero can be entered for the base scenario or HES sites.
#'
#' @return A data frame (tibble) containing the LSOA connections to their closest site in the
#'   selected network. Key columns added include:
#'   \itemize{
#'     \item \code{min_trav}, \code{min_pub_trav}, \code{min_hybrid_trav}: The minimum travel times
#'       (car, public transport, hybrid) from the LSOA to the selected site network.
#'     \item \code{close_site_hybrid}: Flag (1/0) indicating the row represents the closest site.
#'     \item \code{time_band_h}, \code{time_band_n_h}, \code{time_band_f_h}: Categorical and
#'       numeric bins for the minimum hybrid travel time (0-15, 15+-30, etc.).
#'     \item \code{hospital_flag}: Flag (1/0) indicating if the closest site is a Hospital.
#'   }
#'
#' @importFrom dplyr filter select bind_rows mutate if_else case_when
#' @importFrom stringr str_detect
#' @export
create_data_icb <- function(oct_or_hes = "OCT",
                            icb_filt = "Cornwall",
                            number_sites,
                            scenario) {
 
   # select scenario and sites to run  
   optimal_sites <- combos_table |>
    filter(
      combo == scenario,
      no_sites == number_sites,
      icb == icb_filt
    )
  # create vector of optimal sites
  optimal_sites <- optimal_sites$site

  df <- data |>
    filter(
      analysis == oct_or_hes,
      icb_short == icb_filt,
      site_status == "Exisiting",
      icb_site == icb_short
    ) |> # ensure all are seen within icb
    bind_rows(data |> filter(site_short_name %in% optimal_sites)) |>
    mutate(site = site_short_name) |>
    mutate(
      min_trav = min(travel_time_car, na.rm = T), # calculate min travel time by site/icb
      min_pub_trav = min(travel_time_pt_peak, na.rm = T),
      min_hybrid_trav = min(travel_time_hybrid, na.rm = T),
      .by = c(icb, origin_lsoa)
    ) |>
    mutate(
      close_site_hybrid = if_else(min_hybrid_trav == travel_time_hybrid, # add flag for min travel time
        1, 0
      ),
      time_band_h = case_when(min_hybrid_trav <= 15 ~ "0 - 15", # create travel time bins
        min_hybrid_trav <= 30 ~ "15+ - 30",
        min_hybrid_trav <= 45 ~ "30+ - 45",
        min_hybrid_trav <= 60 ~ "45+ - 60",
        min_hybrid_trav > 60 ~ "60 +",
        .default = "Please check"
      ),
      time_band_n_h = case_when(min_hybrid_trav <= 15 ~ 1,
        min_hybrid_trav <= 30 ~ 2,
        min_hybrid_trav <= 45 ~ 3,
        min_hybrid_trav <= 60 ~ 4,
        min_hybrid_trav > 60 ~ 5,
        .default = 6
      ),
      time_band_f_h = factor(time_band_h,
        levels = c(
          "0 - 15",
          "15+ - 30",
          "30+ - 45",
          "45+ - 60",
          "60 +"
        )
      ),
      hospital_flag = if_else(str_detect(site, "Hospital"), 1, 0)
    ) |> # add flags for hospitals
    filter(
      close_site_hybrid == 1,
      icb_site == icb_short
    )
  
  df
}



test_data_hes <- create_data_icb(oct_or_hes = 'HES', 'Cornwall', 0,0)

test_data<-  create_data_icb(oct_or_hes = 'OCT', 'Cornwall', 2, 2 )

cli_alert_success('Calculating optimal site - complete')


#' @title Summarise Population Served by Travel Time Band, Site, or ICB
#'
#' @description
#' This function calculates weighted mean travel times and cumulative population coverage
#' based on hybrid travel time bands, grouping the results either by individual site or
#' across the entire Integrated Care Board (ICB).
#'
#' @details
#' The input data frame (\code{data_sum}) is expected to be the output of a function like
#' \code{create_data_icb}, containing one row per LSOA to its closest site, including
#' \code{wt_pop} (weighted population) and \code{travel_time_hybrid}.
#'
#' The function calculates:
#' \itemize{
#'   \item Weighted mean hybrid travel time for the selected grouping level (\code{mean_trav}).
#'   \item Total population served by each travel time band (\code{tot_pop_band}).
#'   \item Cumulative population and percentage served as travel time increases.
#' }
#' The output is transformed into a wide format for presentation.
#'
#' @param data_sum A data frame (tibble) containing the processed LSOA-to-closest-site data,
#'   including columns for \code{wt_pop}, \code{travel_time_hybrid}, \code{site},
#'   \code{icb_short}, and \code{time_band_f_h}. This is typically the output of
#'   \code{create_data_icb}.
#' @param site_or_icb A character string indicating the level of aggregation. If set to
#'   \code{'site'} (default), summaries are generated for each individual site within the ICB.
#'   If set to \code{'icb'}, the summary is aggregated across the entire ICB.
#'
#' @return A data frame (tibble) in wide format, summarising population metrics. Key columns:
#'   \itemize{
#'     \item \code{icb_short}: The short ICB name.
#'     \item \code{site}: The site name (or the ICB name if \code{site_or_icb = 'icb'}).
#'     \item \code{tot_pop_site}: Total population served at the grouping level.
#'     \item \code{mean_trav}: Weighted mean hybrid travel time for the grouping level.
#'     \item Columns prefixed with \code{culative}, \code{culm}, \code{mean}, and \code{tot_pop},
#'           broken down by \code{time_band_f_h} (e.g., \code{tot_pop_band_0 - 15}, \code{culm_per_45+ - 60}).
#'   }
#'
#' @importFrom dplyr select mutate summarise all_of arrange starts_with
#' @importFrom tidyr pivot_wider
#' @export
create_summary_site_icb <- function(data_sum, site_or_icb = "site") {
  if (site_or_icb == "site") {
    group_1 <- c("site", "icb_short")
    group_2 <- c("site", "time_band_f_h", "icb_short")
    group_3 <- c("site", "icb_short", "mean_trav", 
                 "mean_trav_band", "time_band_f_h")
  } else {
    group_1 <- c("icb_short")
    group_2 <- c("icb_short", "time_band_f_h")
    group_3 <- c("icb_short", "mean_trav", 
                 "mean_trav_band", "time_band_f_h")
  }

  # population served by travel time / site / icb
  summary_site <- data.frame(data_sum) |>
    select(-geometry) |>
    # select(site, wt_pop, time_band_f) |>
    mutate(
      mean_trav = round(weighted.mean(travel_time_hybrid, 
                                      wt_pop, na.rm = TRUE), 1),
      .by = all_of(group_1)
    ) |>
    mutate(
      mean_trav_band = round(weighted.mean(travel_time_hybrid, 
                                           wt_pop, na.rm = TRUE), 1),
      .by = all_of(group_2)
    ) |>
    summarise(
      tot_pop_band = round(sum(wt_pop), 1),
      .by = all_of(group_3)
    ) |>
    mutate(
      tot_pop_site = sum(tot_pop_band),
      .by = all_of(group_1)
    )

  if (site_or_icb == "icb") {
    summary_site <- summary_site |>
      mutate(site = icb_short)
  }

  summary_site <- summary_site |>
    arrange(site, time_band_f_h) |>
    mutate(
      culative_pop = cumsum(tot_pop_band),
      culm_per = round((culative_pop / tot_pop_site) * 100, 1),
      .by = all_of(group_1)
    ) |>
    arrange(time_band_f_h) |>
    pivot_wider(
      names_from = time_band_f_h,
      values_from = c(
        mean_trav_band,
        tot_pop_band,
        culative_pop,
        culm_per
      )
    ) |>
    select(
      icb_short,
      site,
      tot_pop_site,
      mean_trav,
      starts_with("culative"),
      starts_with("culm"),
      starts_with("mean"),
      starts_with("tot_pop")
    )

  summary_site
}




#' @title Generate Combined Site, ICB & HES Summary Table for Scenario Comparison
#'
#' @description
#' This function orchestrates the data processing pipeline to create a single wide-format
#' summary table that includes site-level performance for a given scenario, the overall
#' ICB performance for that scenario (OCT data), and a baseline ICB performance (HES data).
#'
#' @details
#' It uses helper functions \code{\link{create_data_icb}} and \code{\link{create_summary_site_icb}}
#' to retrieve and summarise the data.
#'
#' Three summaries are generated and combined into the final table:
#' \itemise{
#'   \item \code{site_summary}: Site-level performance for the selected scenario (\code{oct_or_hes = 'OCT'}).
#'   \item \code{icb_summary}: ICB aggregate summary for the selected scenario (\code{oct_or_hes = 'OCT'}).
#'   \item \code{hes_summary}: ICB aggregate summary for the baseline case (\code{oct_or_hes = 'HES'}).
#' }
#' The aggregate rows are clearly identified in the \code{site} column using prefixes ('TOTAL: ' and 'HES: ').
#'
#' @param icb_filt A character string specifying the Integrated Care Board (ICB) to filter the data for (e.g., 'Cornwall').
#' @param number_sites An integer specifying the number of sites in the optimization scenario.
#' @param scenario A character string identifying the specific optimization scenario or combo ID.
#'
#' @return A data frame (tibble) in wide format, containing key metrics for each site in the
#'   scenario, the ICB total for the scenario, and the ICB total for the HES baseline.
#'   Columns include total population, overall mean travel time, and detailed metrics
#'   (cumulative population, cumulative percentage, mean travel time, and total population)
#'   broken down by the hybrid travel time bands (e.g., '0 - 15', '15+ - 30', etc.).
#'
#' @importFrom dplyr bind_rows mutate select
#' @export
create_table_dataframe <- function(icb_filt, number_sites, scenario) {
  site_data <- create_data_icb(oct_or_hes = "OCT", icb_filt, number_sites, scenario)
  hes_data <- create_data_icb(oct_or_hes = "HES", icb_filt, 0, 0)

  site_summary <- create_summary_site_icb(site_data, "site")
  icb_summary <- create_summary_site_icb(site_data, "icb") |>
    mutate(site = paste0("TOTAL: ", site))
  hes_summary <- create_summary_site_icb(hes_data, "icb") |>
    mutate(site = paste0("HES: ", site))

  combined <- bind_rows(
    site_summary,
    icb_summary,
    hes_summary
  ) |>
    select(
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
      `tot_pop_band_60 +`
    )

  combined
}


a<-create_table_dataframe('Cornwall', 2, 2) 






create_summary_table <- function(data) {

  wd <-80  
  
  
data |>
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

}

create_summary_table(a)
