library(dplyr, magrittr, tibble, tidyr)
is_iqr_outlier <- function(x) {
  q <- quantile(x, c(0.25, 0.75), na.rm = T)
  iqr <- diff(q)
  (x <= q[1] - 1.5 * iqr) | (x >= q[2] + 1.5 * iqr)
}
build_life_expectancy_data <- function(){
  files <- list.files(pattern = "IHME",
                      full.names = T,
                      recursive = T)
  df <- vroom::vroom(file = files, show_col_types = F) 
  df %>% 
    dplyr::filter(year == max(df$year)) %>%
    dplyr::filter(age_name == "<1 year") %>% 
    dplyr::filter(location_name != "United States of America") %>% 
  
  
    dplyr::filter(grepl("County|Borough|Census Area|Municipality|Parish", location_name)) %>%
    select(year, fips, location_name, race_name, sex_name, age_name, val) %>% 
    mutate(fips = stringr::str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
    mutate(year = as.character(year)) -> df_2019
  # build difference
  df %>% 
    dplyr::filter(year %in% c(min(df$year), max(df$year))) %>% 
    dplyr::filter(age_name == "<1 year") %>% 
    dplyr::filter(location_name != "United States of America") %>% 
    dplyr::filter(grepl("County|Borough|Census Area|Municipality|Parish", location_name)) %>%
    select(year, fips, location_name, race_name, sex_name, age_name, val) %>% 
    mutate(fips = stringr::str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
    pivot_wider(names_from = year, values_from = val) %>% 
    mutate(val = `2019` - `2000`) %>% 
    mutate(year = "2000-2019", .before = fips) %>% 
    select(!c(`2000`, `2019`)) -> df_2000_2019
  # combine
  bind_rows(df_2019, df_2000_2019) %>% 
    group_by(year, sex_name, race_name) %>%
    # mutate(avg = mean(val, na.rm = T),
    #        sd = sd(val, na.rm = T),
    #        sd_3x = (3* sd),
    #        lb = avg - sd_3x,
    #        ub = avg + sd_3x,
    #        ol = ifelse(val >= ub | val <= lb, TRUE, FALSE),
    #        .drop = TRUE) %>%
    mutate(ol = is_iqr_outlier(val)) %>%
    select(year:val, ol) %>%
    arrange(year, fips, sex_name, race_name) %>%
    ungroup() -> ihme
  saveRDS(ihme, file = "./app/data/ihme.rds")
}
build_life_expectancy_data()
build_county_boundary <- function(){
  cb <- sf::st_read(dsn = "./data-raw/cb_2021_us_county_20m/",
                    layer = "cb_2021_us_county_20m"
  ) %>% 
    rename_with(~tolower(.x)) %>% 
    filter(stusps %in% state.abb) %>% 
    tidyr::unite(fips, statefp:countyfp, sep = "") %>% 
    select(fips, state_name, geometry) %>% 
    rmapshaper::ms_simplify(keep = .25, method = "vis") %>% 
    arrange(state_name)
  saveRDS(cb, file = "./app/data/cb_us.rds")
}
build_county_boundary()
build_state_boundary <- function(){
  sf::st_read(dsn = "./data-raw/cb_2018_us_state_20m/",
                    layer = "cb_2018_us_state_20m"
  ) %>% 
    rename_with(~tolower(.x)) %>% 
    filter(stusps %in% state.abb) %>% 
    filter(!grepl("AK|HI", stusps)) %>%
    select(stusps) %>%
    arrange(stusps) %>% 
    rmapshaper::ms_simplify(keep = .05, method = "vis") -> sb
  saveRDS(sb, file = "./app/data/sb_us.rds")
}
build_state_boundary()
build_ts_life_expectancy_data <- function(){
  files <- list.files(pattern = "IHME",
                      full.names = T,
                      recursive = T)
  df <- vroom::vroom(file = files, show_col_types = F) 
  
  df %>% 
    dplyr::filter(age_name == "<1 year") %>% 
    dplyr::filter(location_name != "United States of America") %>% 
    dplyr::filter(grepl("County|Borough|Census Area|Municipality|Parish", location_name)) %>%
    dplyr::filter(sex_name != "Both") %>%
    select(year, fips, location_name, race_name, sex_name, age_name, val) %>% 
    mutate(fips = stringr::str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
    group_by(year, race_name, sex_name) %>% 
    summarize(avg = mean(val, na.rm = T), .groups = "drop") -> a
  saveRDS(a, file = "./data/ts_ihme_by_race.rds")
}
build_ts_life_expectancy_data()
build_method_description <- function(){
 library(tibble)
 tribble(
 ~name,        ~Description,
 "bclust",     "The \"bclust\" style uses bclust to generate the breaks using bagged clustering; it may be anchored using set.seed.",
 "box",        "The \"box\" style generates 7 breaks (therefore 6 categories) based on a box-and-whisker plot. First and last categories include the data values considered as outliers, and the four remaining categories are defined by the percentiles 25, 50 and 75 of the data distribution.",
 "dpih",       "The \"dpih\" style uses the dpih() function from KernSmooth (Wand, 1995) implementing direct plug-in methodology to select the bin width of a histogram.",
 "equal",      "The \"equal\" style divides the range of the variable into n part",
 "fisher",     "The \"fisher\" style uses the algorithm proposed by W. D. Fisher (1958) and discussed by Slocum et al. (2005) as the Fisher-Jenks algorithm; added here thanks to Hisaji Ono. This style will subsample by default for more than 3000 observations. This style should always be preferred to \"jenks\" as it uses the original Fortran code and runs nested for-loops much faster.",
 "fixed",      "The \"fixed\" style permits a \"classIntervals\" object to be specified with given breaks, set in the fixedBreaks argument; the length of fixedBreaks should be n+1; this style can be used to insert rounded break values.",
 "hclust",     "The \"hclust\" style uses hclust to generate the breaks using hierarchical clustering; the pars attribute returns the hclust object generated, and can be used to find other breaks using getHclustClassIntervals; arguments to hclust may be passed through ....",
 "headtails",  "The \"headtails\" style uses the algorithm proposed by Bin Jiang (2013), in order to find groupings or hierarchy for data with a heavy-tailed distribution.",
 "jenks",      "The \"jenks\" style has been ported from Jenks' code, and has been checked for consistency with ArcView. Use \"fisher\" instead.",
 "kmeans",     "The \"kmeans\" style uses kmeans to generate the breaks; it may be anchored using set.seed;",
 "maximum",    "The \"maximum\" style uses the Maximum Breaks method of classification finding the k - 1 largest differences in var. The mean of the values that generated the largest splits is used as the interval boundary.",
 "pretty",     "The \"pretty\" style chooses a number of breaks not necessarily equal to n using pretty, but likely to be legible.",
 "quantile",   "The \"quantile\" style provides quantile breaks; arguments to quantile may be passed through ....",
 "sd",         'The "sd" style chooses breaks based on pretty of the centred and scaled variables, and may have a number of classes different from n.'
 ) -> method_description
 
 saveRDS(method_description, file = "./app/data/method_description.rds")
}
build_method_description()
