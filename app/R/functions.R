filter_ihme <- function(input_year, input_sex, input_race, input_keep_ol,
                        input_bins, input_method, input_dataset){
  input_dataset %>%
    filter(year == input_year) %>%
    filter(sex_name == input_sex) %>%
    filter(race_name == input_race) %>%
    {if(input_keep_ol) . else filter(., ol == F)} %>% 
    mutate(val_f = cut(
      val,
      breaks = classIntervals(
        val,
        n = input_bins,
        style = input_method,
        warnLargeN = F
      )[[2]],
      include.lowest = T))
  
}

map_us_data <- function(input_year, input_sex, input_race,
                        input_type, input_palette, input_reverse,
                        input_dataset, input_title, input_poly,
                        input_overlay, input_border){
  # join ----
  left_join(input_poly, input_dataset, by = join_by(fips)) -> us_le
  # main ----
  us_le %>%
    rmapshaper::ms_simplify(keep = .1, method = "vis") %>%
    ggplot() +
    geom_sf(mapping = aes(fill = val_f), stat = "sf", show.legend = T) +
    {if(input_type == "diverging")
      scale_fill_discrete_diverging(
        palette = input_palette,
        rev = input_reverse,
        name = "Age in Years")} +
    {if(input_type == "sequential")
      scale_fill_discrete_sequential(
        palette = input_palette,
        rev = input_reverse,
        name = "Age in Years")} +
    {if(!is.character(input_overlay))
      geom_sf(data = input_overlay,
              fill = scales::alpha('#c55252', 0), 
              linewidth = .5,
              col = input_border)} +
    coord_sf(crs = 5070,
             xlim = c(-2.35e6, 2.23e6),
             ylim = c(0, 3.17e6),
             expand = TRUE) +
    #geom_sf(data = input_overlay) +
    labs(title = paste(input_year, input_sex, input_race, input_title)) +
    theme_void() +
    theme(text=element_text(size=20)) -> main
  main
  # alaska ----
  us_le %>%
    filter(state_name %in% c("Alaska")) %>%
    sf::st_transform(crs = 3338) %>%
    ggplot() +
    geom_sf(mapping = aes(fill = val_f),
            show.legend = F) +
    {if(input_type == "diverging")
      scale_fill_discrete_diverging(
        palette = input_palette,
        rev = input_reverse,
        name = "Age in Years")} +
    {if(input_type == "sequential")
      scale_fill_discrete_sequential(
        palette = input_palette,
        rev = input_reverse,
        name = "Age in Years")} +
    coord_sf(crs = 3338,
             xlim = c(-1.44e6, 1.49e6),
             ylim = c(.44e6, 2.37e6),
             expand = FALSE) +
    theme_void() -> ak
  # hawaii ----
  us_le %>%
    filter(state_name %in% c("Hawaii")) %>%
    sf::st_transform(crs = 26962) %>%
    ggplot() +
    geom_sf(mapping = aes(fill = val_f),
            show.legend = F) +
    {if(input_type == "diverging")
      scale_fill_discrete_diverging(
        palette = input_palette,
        rev = input_reverse,
        name = "Age in Years")} +
    {if(input_type == "sequential")
      scale_fill_discrete_sequential(
        palette = input_palette,
        rev = input_reverse,
        name = "Age in Years")} +
    coord_sf(crs = 26962,
             xlim = c(1.3e5, 6.9e5),
             ylim = c(-1.5e5, 2.12e5),
             expand = FALSE) +
    theme_void() -> hi
  hi
  # combine
  library(cowplot)
  (ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000)))
  (ratioHawaii  <- (23 - 18) / (-154 - (-161)))
  ggdraw(main) +
    draw_plot(ak, width = 0.26, height = 0.26 * 10/6 * ratioAlaska,
              x = 0.05, y = 0.01) +
    draw_plot(hi, width = 0.15, height = 0.15 * 10/6 * ratioHawaii,
              x = 0.3, y = 0.01)

}

map_us_data_tbl <- function(input_bins, input_method, input_dataset){
  input_dataset %>%
    select(!age_name) %>%
    mutate(year = as.integer(year)) %>%
    arrange(desc(val))
}

ts_avg_life_exp <- function(input_race, input_year, input_palette,
                            input_dataset, input_keep_ol){
  #input_year <- 2019
  #input_race <- "Total"
  #input_sex <- c("Male", "Female")
  #input_keep_ol <- TRUE
  #ihme <- readRDS("./data/ihme.rds")

  input_dataset %>%
  dplyr::filter(race_name == input_race) %>%
  ggplot() +
  # geom_rect(aes(xmin = (input_year -.75), xmax = (input_year + .75), ymin = -Inf, ymax = Inf),
  #             fill = "gray50", col = "gray50", alpha = .01) +
  aes(year, avg, group = sex_name, color = sex_name) +
  geom_line(lwd = 1.2) +
  scale_color_discrete_qualitative(name = "Gender: ", palette = "dark2") +
  cowplot::theme_cowplot() +
  labs(title = paste0(input_race, " Life Expectancy"),
       y = "Years",
       x = "")
}

pull_avg_overall_life_exp <- function(input_year, input_sex, input_race,
                                      input_keep_ol, input_dataset){
  library(magrittr)
  library(dplyr)
  # ihme <- readRDS("./app/data/ihme.rds")
  # input_year <- 2019
  # input_sex <- "Male"
  # input_race <- "Total"
  # input_keep_ol <- TRUE
  # ihme %>%
  input_dataset %>% 
    filter(year == input_year) %>%
    filter(sex_name == input_sex) %>%
    filter(race_name == input_race) %>%
    {if(input_keep_ol) . else filter(., ol == F)} %>%
    group_by(sex_name) %>%
    summarize(avg = mean(val, na.rm = T), .groups = "drop") %>%
    pull() %>%
    round(1) %>%
    as.character() %>%
    paste(collapse = " | ")
}

pull_number_of_counties <- function(input_race, input_year, input_keep_ol,
                                    input_sex, dataset){
  #ihme <- readRDS("./data/ihme.rds")
  #ihme %>%
  dataset %>%
    filter(year == input_year) %>%
    filter(race_name == input_race) %>%
    filter(sex_name == input_sex) %>%
    {if(input_keep_ol) . else filter(., ol == F)} %>%
    summarize(n = sum(!is.na(val))) %>%
    pull()
}

histogram_life_exp <-function(input_year, input_race, input_sex,
                              input_dataset, input_bins, input_method,
                              input_type, input_palette, input_reverse,
                              input_keep_ol, input_title){
  input_dataset -> a
  # bin
  breaks <- classInt::classIntervals(
      a$val,
      n = input_bins,
      style = input_method,
      warnLargeN = F
    )[[2]] %>% round(1)
  breaks
  # table
  breaks_tbl <- data.frame(
    ymin = breaks[1:length(breaks) - 1],
    ymax = breaks[2:length(breaks)],
    xmin = -Inf,
    xmax = Inf,
    fill = letters[1:length(breaks) - 1]
  )

  ggplot() +
    geom_rect(breaks_tbl, mapping = aes(xmin = xmin, xmax = xmax,
                                        ymin = ymin, ymax = ymax,
                                        fill = fill),
              show.legend = F) +
    {if(input_type == "diverging")
      scale_fill_discrete_diverging(
        palette = input_palette,
        rev = input_reverse)} +
    {if(input_type == "sequential")
      scale_fill_discrete_sequential(
        palette = input_palette,
        rev = input_reverse)} +
    geom_histogram(data = a,
                   binwidth = 1,
                   aes(y = val,
                       x = after_stat(count / sum(count))),
                   fill = "gray90", col = "black") +
    scale_y_continuous(breaks = breaks,
                       expand = expansion(mult = c(0, 0))) +
    scale_x_continuous(expand = expansion(mult = c(0, 0)),
                       labels = scales::label_percent()) +
    theme_cowplot() +
    labs(x = "Pct. of Counties",
         y = "Years",
         title = paste(
           input_year,
           input_race,
           input_sex,
           input_title)
         ) -> p1
  ggplot() +
    geom_boxplot(a, mapping = aes(y = val)) +
    stat_boxplot(a, mapping = aes(y = val), geom ='errorbar', width = .25) +
    scale_y_continuous(expand = expansion(mult = c(.01, .01))) +
    cowplot::theme_nothing() -> p2
  cowplot::plot_grid(p1, p2,
                     ncol = 2, rel_widths =  c(5, 1),
                     align = 'h', axis = 'lr')
}

show_method_description <- function(input_method, input_dataset){
    input_dataset %>% 
    filter(name == input_method) %>% 
    select(Description)
}

