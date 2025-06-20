
# clear current workspace
rm(list = ls())

library(rdhs)
library(svylme)
library(psych)
library(lavaan)
library(ggplot2)
library(readxl)
library(semPlot)
library(officer)
library(survey)
library(broom)
library(ggplot2)
library(gridExtra)
library(effects)
library(writexl)
library(ggrepel)

options(survey.lonely.psu="adjust")

## -----------------------------------------------------------------------------------------------------------------------------------------
### Required functions and settings
## -----------------------------------------------------------------------------------------------------------------------------------------

# load required packages
library(pacman)
p_load(
  readr, tidyr, plyr, dplyr, purrr, forcats, survey, haven, ggplot2, 
  stringr, sp, raster, sf, labelled, plotrix, arules, fuzzyjoin, cowplot, 
  gridExtra, lme4, patchwork, readxl, janitor, ggsci, glue, ggrepel, jtools, 
  srvyr, ggpubr, collapse, gtsummary, rstatix, ggcorrplot, viridis, effects, 
  rdhs, microbenchmark, ggfittext, broom, writexl
)

# bar plot function
bar_fun <- function(df, x, fill, title, xlab) {
  ggplot(df, aes(x = .data[[x]], fill = .data[[x]])) +
    geom_bar() +
    theme_manuscript() +
    geom_bar_text(
      stat = 'count',
      aes(label = ..count..),
      vjust = 0.5,
      size = 5 * ggplot2::.pt,
      min.size = 4 * ggplot2::.pt,
      padding.x = grid::unit(0, "pt"),
      padding.y = grid::unit(0, "pt"),
      outside = TRUE
    ) +
    theme(legend.position = "none") +
    labs(title = title, x = xlab)
}

# column plot function
col_fun <- function(df, x, y, z, ylabel, color, label) {
  ggplot(df, aes(x = .data[[x]], y = .data[[y]], fill = .data[[z]])) +
    geom_col() +
    coord_flip() +
    scale_x_discrete(limits = rev) +
    theme_manuscript() +
    theme(legend.title = element_blank()) +
    labs(x = "", y = ylabel) +
    scale_fill_manual(values = color, labels = label, guide = guide_legend(reverse = TRUE))
}

# file reading function
read.files <- function(filepat1, path, fun, encoding = "latin1") {
  filenames <- list.files(path = path, pattern = filepat1, recursive = TRUE, full.names = FALSE)
  sapply(filenames, fun, simplify = FALSE)
}

# extract legend from a ggplot object
get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  plot_table$grobs[[legend_plot]]
}

# theme for general plots
theme_manuscript <- function() {
  theme_bw() + 
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
      plot.title = element_text(hjust = 0.5),
      axis.text = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 12, colour = 'black'),
      legend.text = element_text(size = 12, colour = 'black'),
      legend.key.height = unit(1, "cm")
    )
}

# theme for correlation plots
theme_corr <- function() {
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    axis.text = element_text(size = 12, color = "black")
  )
}

# stacked bar plot function
barplot_stack.fun <- function(main_title) {
  ggplot(counts, aes(fill = Var2, y = Freq, x = Var1)) + 
    geom_bar(position = "stack", stat = "identity", alpha = 0.8) +
    scale_fill_viridis(discrete = TRUE) +
    ggtitle(main_title) +
    theme_manuscript() +
    xlab("") +
    ylab("frequency") +
    scale_fill_manual(name = "Legend", values = c("darkgrey", "brown"))
}

# proportion bar plot function
barplot_prop.fun <- function(main_title) {
  ggplot(counts, aes(fill = Var2, y = Freq, x = Var1)) + 
    geom_bar(position = "fill", stat = "identity", alpha = 0.8) +
    scale_fill_viridis(discrete = TRUE) +
    ggtitle(main_title) +
    theme_manuscript() +
    scale_fill_manual(name = "Legend", values = c("darkgrey", "brown")) +
    xlab("") +
    ylab("percent")
}

# alternative version of proportion bar plot function
barplot_prop.fun2 <- function(dataframe) {
  ggplot(dataframe, aes(fill = Var2, y = Freq, x = Var1, label = scales::percent(percent, vjust = 3))) + 
    geom_bar(position = "fill", stat = "identity", alpha = 0.8) +
    scale_fill_viridis(discrete = TRUE) +
    theme_manuscript() +
    scale_fill_manual(name = "Legend", values = c("cyan", "darkcyan")) +
    xlab("") +
    ylab("proportion")
}

# survey design creation function
svydesign_fun <- function(filename) {
  svydesign(id = ~id, strata = ~strat, nest = TRUE, weights = ~wt, data = filename)
}

# generate survey estimates
result.prop <- function(var, var1, design) {
  svyby(
    formula = make.formula(var),
    by = make.formula(var1),
    FUN = svytotal,
    design = design,
    svyciprop,
    method = 'logit',
    levels = 0.95,
    vartype = "se",
    na.rm = TRUE,
    influence = TRUE
  )
}

# wrapper for survey estimation
estim_prop <- function(df, col, by) {
  svy_mal <- svydesign_fun(df)
  result.prop(col, by, design = svy_mal)
}

# get dhs country ids
ids <- dhs_countries(returnFields = c("CountryName", "DHS_CountryCode", "SubregionName", "RegionName")) %>%
  filter(RegionName == "Sub-Saharan Africa")

# ggsave helper
ggsave_fun <- function(save_as_pdf, save_as_png, plot_name, width_size, height_sze) {
  ggsave(paste0(FigDir, "/", Sys.Date(), save_as_pdf), plot_name, width = width_size, height = height_sze)
  ggsave(paste0(FigDir, "/", Sys.Date(), save_as_png), plot_name, width = width_size, height = height_sze)
}

# generate effect estimates dataframe
effect_df_fun <- function(model_) {
  effect_list_est <- summary(Effect("agric_home", model_)) 
  effect_list_est$effect %>% as.data.frame() %>%
    bind_cols(effect_list_est$lower %>% as.data.frame()) %>%
    bind_cols(effect_list_est$upper %>% as.data.frame()) %>%
    rename(effect = ....1, lower = ....2, upper = ....3) %>%
    tibble::rownames_to_column()
}

# grouped column plot
p_fun <- function(dataframe, fill_stack, y_lab) {
  ggplot(dataframe, aes(x = reorder(country, value), y = value)) +
    geom_col(position = fill_stack, aes(fill = variable)) +
    scale_fill_manual(name = "", values = c("aquamarine3", "deepskyblue4"), labels = c("+ve", "-ve")) +
    coord_flip() +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = "country", y = y_lab) +
    theme_classic2() +
    theme(axis.text.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank()) +
    theme_manuscript()
}

# stacked plots by region
ur_rur_pfun <- function(df_r_u) {
  p2_r1 <- p5_stacked_fun(df_r_u[[1]], 22500) + ggtitle("eastern") +
    theme(plot.title = element_text(margin = margin(t = 10, b = -20))) +
    p5_prop_fun(df_r_u[[1]])
  
  p2_r2 <- p5_stacked_fun(df_r_u[[2]], 22500) + ggtitle("middle") +
    theme(plot.title = element_text(margin = margin(t = 10, b = -20))) +
    p5_prop_fun(df_r_u[[2]])
  
  p2_r3 <- p5_stacked_fun(df_r_u[[3]], 22500) + ggtitle("western") +
    theme(plot.title = element_text(margin = margin(t = 10, b = -20))) +
    p5_prop_fun(df_r_u[[3]])
  
  p2_r1 / p2_r2 / p2_r3 + plot_annotation(tag_levels = "A")
}

# housing and wealth bar proportion
bar_prop_fun <- function(df, var_string, x_lab) {
  ggplot(p_data_bar, aes_string(y = "Freq", x = "home_type_new", fill = var_string)) + 
    geom_bar(position = "fill", stat = "identity", alpha = 0.8) +
    facet_wrap(~hv025) +
    theme_manuscript() +
    scale_fill_manual(name = "", values = c("darkcyan", "brown")) +
    xlab(x_lab) +
    ylab("proportion") +
    theme(legend.position = "none", strip.background = element_blank(), strip.text.x = element_blank())
}

# reproject gps
get_crs <- function(df, raster) {
  spTransform(x = df, CRSobj = crs(raster))
}

# raster extraction (basic and monthly)
extract_fun <- function(raster, dhs, buffer) {
  raster::extract(raster, dhs, buffer = buffer, fun = mean, df = TRUE) %>%
    mutate(dhs_year = dhs$DHSYEAR, hv001 = dhs$DHSCLUST)
}

extract_fun_month <- function(raster, dhs, buffer) {
  raster::extract(raster, dhs, buffer = buffer, fun = mean, df = TRUE) %>%
    mutate(dhs_year = dhs$DHSYEAR, hv001 = dhs$DHSCLUST, month = dhs$hv006)
}

# gps-survey combiner
survey_gps_comb <- function(x, y) {
  left_join(st_as_sf(all_GPS[[x]]), dhs_all[[y]], by = c("DHSCLUST" = "hv001")) %>%
    group_split(hv007, hv006)
}

# get monthly strings for lagged climate data
get_month_str <- function(file) {
  file %>%
    select(hv006, hv007) %>%
    mutate(
      mo = hv006 - 2,
      month_lag = if_else(mo < 10, str_c(".0", mo), str_c(".", mo)),
      month_lag = case_when(
        month_lag == ".0-1" ~ ".11",
        month_lag == ".00"  ~ ".12",
        TRUE ~ month_lag
      ),
      year = if_else(month_lag %in% c(".11", ".12"), hv007 - 1, hv007)
    ) %>%
    select(month_lag, year) %>%
    group_by(month_lag, year) %>%
    slice(1)
}

pick_month <- function(file, filepath) {
  files <- list.files(path = filepath, pattern = ".tif$", full.names = TRUE)
  months <- get_month_str(file) %>% mutate(year_mo = paste0(year, month_lag))
  files[grep(paste(months$year_mo, collapse = "|"), files)]
}

get_month_str_RH <- function(file) {
  file %>%
    select(hv006, hv007) %>%
    mutate(
      month_lag = hv006 - 2,
      month_lag = case_when(month_lag == -1 ~ 11, month_lag == 0 ~ 12, TRUE ~ month_lag),
      year = if_else(month_lag %in% c(11, 12), hv007 - 1, hv007)
    ) %>%
    select(month_lag, year) %>%
    mutate(across(everything(), as.numeric)) %>%
    group_by(year, month_lag) %>%
    slice(1)
}

pick_files_RH <- function(year, month_lag) {
  list_RH[[as.character(year)]][[month_lag]]
}

pick_files_temp <- function(year, month_lag) {
  list_temp[[as.character(year)]][[month_lag]]
}

# box plot
box_plot_fun <- function(df, var1, var2) {
  ggplot(df, aes_string(y = var1, x = var2, fill = var2)) + 
    geom_boxplot() +
    labs(x = "", y = "y_lab", title = "") +
    scale_fill_manual(values = c("darkgoldenrod1", "darkviolet"), name = "home type") +
    theme_manuscript() +
    theme(legend.position = "none") +
    facet_wrap(~hv025)
}

# map theme
map_theme <- function() {
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.title = element_text(hjust = 0.5),
    legend.title.align = 0.5,
    legend.title = element_text(size = 8, colour = 'black'),
    legend.text = element_text(size = 8, colour = 'black'),
    legend.key.height = unit(0.65, "cm")
  )
}

# clean country-year names
name_clean_fun <- function(df) {
  df %>%
    mutate(country_year.x = case_when(
      country_year.x == "Congo Democratic Republic 2013 - 14" ~ "DRC 2013 - 14",
      country_year.x == "Uganda 2009" ~ "Uganda 2009 - 10",
      country_year.x == "Cameroon 2018" ~ "Cameroon 2018 - 19",
      TRUE ~ country_year.x
    ))
}