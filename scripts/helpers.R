# ==========================================================================================================================================
# Script Name: Helper Functions and Packages
# Author: Dr. Ifeoma Ozodiegwu
# Edited by: Grace Legris, Research Data Analyst (gracebea@gmail.com)
# Edited: [2025-06-20]
# Purpose: Load required functions and write functions for use in analyses and modeling.
# ==========================================================================================================================================

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
library(gridExtra)
library(effects)
library(writexl)
library(ggrepel)
library(patchwork)
library(scales)
library(sf)
library(raster)
library(dplyr)
library(exactextractr)
library(tidyr)
library(pacman)

options(survey.lonely.psu = "adjust")

## -----------------------------------------------------------------------------------------------------------------------------------------
### Required functions and settings
## -----------------------------------------------------------------------------------------------------------------------------------------

# load required packages
p_load(
  readr, tidyr, plyr, dplyr, purrr, forcats, survey, haven, ggplot2, 
  stringr, sp, raster, sf, labelled, plotrix, arules, fuzzyjoin, cowplot, 
  gridExtra, lme4, patchwork, readxl, janitor, ggsci, glue, ggrepel, jtools, 
  srvyr, ggpubr, collapse, gtsummary, rstatix, ggcorrplot, viridis, effects, 
  rdhs, microbenchmark, ggfittext, broom, writexl
)

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

# survey design creation function
svydesign_fun <- function(filename) {
  svydesign(id = ~id, strata = ~strat, nest = TRUE, weights = ~wt, data = filename)
}

# get dhs country ids
ids <- dhs_countries(returnFields = c("CountryName", "DHS_CountryCode", "SubregionName", "RegionName")) %>%
  filter(RegionName == "Sub-Saharan Africa")

# generate effect estimates dataframe
effect_df_fun <- function(model_) {
  effect_list_est <- summary(Effect("agric_home", model_)) 
  effect_list_est$effect %>% as.data.frame() %>%
    bind_cols(effect_list_est$lower %>% as.data.frame()) %>%
    bind_cols(effect_list_est$upper %>% as.data.frame()) %>%
    rename(effect = ....1, lower = ....2, upper = ....3) %>%
    tibble::rownames_to_column()
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