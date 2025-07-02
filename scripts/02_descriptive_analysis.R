# ==========================================================================================================================================
# Script Name: Descriptive Analysis
# Author: Dr. Ifeoma Ozodiegwu
# Edited by: Grace Legris, Research Data Analyst (gracebea@gmail.com)
# Edited: [2025-06-20]
# Purpose: Generate main figures for manuscript. Extract estimated population in each first-level administrative subdivision in each of the 15 countries analyzed using population
# rasters. Generate maps showing the proportion of agricultural households per first-level administrative subdivision in each country.
# ==========================================================================================================================================

source(file.path("scripts/helpers.R"))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Read in Analysis Datasets
## -----------------------------------------------------------------------------------------------------------------------------------------

# load urban and rural datasets
urban_df <- read_csv(file.path("data/250605_urban_df_for_analysis.csv"))
rural_df <- read_csv(file.path("data/250605_rural_df_for_analysis.csv"))

# add a 'type' column to indicate urban
urban_df <- urban_df %>%  
  mutate(type = "Urban") %>% 
  transmute(country_year.x, country_year.x, id, strat, wt, type) %>% # select and retain only these columns
  name_clean_fun() # clean column names using custom function (see functions_employment.R script)
rural_df <- rural_df %>%  
  mutate(type = "Rural") %>% 
  transmute(country_year.x, country_year.x, id, strat, wt, type)  %>% # select and retain only these columns
  name_clean_fun() # clean column names using custom function

# extract unique country-year values (e.g. Angola 2015 - 16) from the urban dataset
recent_to_remove <- urban_df$country_year.x %>% unique()

# load urban trend dataset, filter out recent records, and add a 'type' column for urban
urban_trend_df <- read_csv(file.path("data/251106_urban_df_for_analysis_trend.csv")) %>%  
  mutate(type = "Urban")  %>% 
  filter(!country_year.x %in% recent_to_remove) %>% # remove recent entries from urban dataset
  transmute(country_year.x, country_year.x, id, strat, wt, type)  %>% # select and retain only these columns
  name_clean_fun() # clean column names using custom function

# load rural trend dataset, filter out recent records, and add a 'type' column for rural
rural_trend_df <- read_csv(file.path("data/251106_rural_df_for_analysis_trend.csv")) %>%
  mutate(type = "Rural") %>%
  filter(!country_year.x %in% recent_to_remove) %>% # remove recent entries from rural dataset
  transmute(country_year.x, country_year.x, id, strat, wt, type)  %>% # select and retain only these columns
  name_clean_fun() # clean column names using custom function


## =========================================================================================================================================
### Create Plots
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Figure 1b - Chart: # Children tested for malaria by RDT or microscopy aggregated across urban and rural clusters
## -----------------------------------------------------------------------------------------------------------------------------------------

# figure 1 sample description 

# combine urban and rural data frames (both current and trend data) into one
all_df <- rbind(urban_df, rural_df, urban_trend_df, rural_trend_df) %>%
  
  # create a survey design object with id, strata, and weights, setting nest to true
  as_survey_design(ids= id,strata=strat,nest=T,weights= wt) %>% 
  
  # group by country and urban/rural type, then calculate total population using survey weights
  group_by(country_year.x, type) %>%
  summarize(total = round(survey_total(),0)) %>%
  ungroup() 

# calculate the percentage of the total for each group within a country/year
df <- all_df %>% 
  group_by(country_year.x) %>%  
  summarise(percent = round(total / sum(total) * 100, 0)) # calculate percentage of total population

# merge the calculated percentages with the original data frame and prepare for plotting
all <- cbind(all_df, df) %>% 
  select(-c("country_year.x")) %>% # remove duplicate country_year.x column
  mutate(plot_label = ifelse(type == "Rural", percent, NA)) %>% # create plot labels only for rural areas (to avoid label clutter)
  mutate(survey = ifelse(country_year.x %in% recent_to_remove, "Recent Survey", "Preceding Survey")) # categorize surveys into recent vs. preceding surveys based on the country year

# replace space-hyphen-space with en dash and no spaces (preferred in academic writing)
all$country_year.x <- gsub(" - ", "–", all$country_year.x)

# create the plot, excluding NA labels
generate_survey_plot <- function(data, survey_type, remove_x_axis = FALSE) {
  # filter data for the specified survey type and create the plot
  p <- ggplot(data %>% filter(survey == survey_type), aes(x = reorder(country_year.x, -total), y = total, fill = type, label = total)) +
    geom_bar(stat = "identity", alpha = 0.6) +
    scale_fill_manual(values = c("#E07A5F", "darkorchid")) +
    geom_text(aes(label = ifelse(!is.na(plot_label), paste0(plot_label, "%"), "")), 
              position = position_stack(vjust = 0.5),
              color = "black", 
              size = 3) +  # adjust the size of the percentage labels
    coord_flip() +
    theme_manuscript() +
    labs(x = "", y = "") +
    scale_y_continuous(position = "right") +
    ylim(0, 11500) +
    theme(legend.position = "none", # remove the legend
          axis.text.y = element_text(size = 9),
          axis.text.x = element_text(size = 9))  # adjust the size of the axis labels
  
  if (remove_x_axis) {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  
  return(p)
}

# generates plots for both surveys
p1bc_survey1 <- generate_survey_plot(all, "Preceding Survey", remove_x_axis = TRUE) # plots for preceding surveys (top chart)
p1bc_survey2 <- generate_survey_plot(all, "Recent Survey") + # plots for recent surveys (bottom chart)
  labs(x = "", y = str_wrap("Number of Children Aged 6-59 Months Tested for Malaria\n by RDT or Microscopy in Urban and Rural Clusters (Combined)", width = 40)) +
  theme(axis.title.x = element_text(size = 11))

# now add separate annotations after creating each plot
p1bc_survey1 <- p1bc_survey1 +
  annotate("text", x = 3.5, y = 8500, label = "Rural", color = "#E07A5F", hjust = 0, fontface = "bold") +
  annotate("text", x = 4.5, y = 8500, label = "Urban", color = "darkorchid", hjust = 0, fontface = "bold")

p1bc_survey2 <- p1bc_survey2 +
  annotate("text", x = 7.5, y = 8500, label = "Rural", color = "#E07A5F", hjust = 0, fontface = "bold") +
  annotate("text", x = 8.5, y = 8500, label = "Urban", color = "darkorchid", hjust = 0, fontface = "bold")

# reduces the space between plots
p1bc_combined_plot <- p1bc_survey1 / plot_spacer() / p1bc_survey2 +
  plot_layout(heights = c(1, -0.1, 2))

p1bc_combined_plot <- (p1bc_survey1 / plot_spacer() / p1bc_survey2) +
  plot_layout(heights = c(1, -0.1, 2)) +
  theme(plot.margin = margin(10, 10, 10, 10))

# save the combined plot as a png
ggsave(file.path("outputs/figure_1bc.pdf"), p1bc_combined_plot, width = 4.5, height = 6) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Supplement Table: # Children tested for malaria by RDT or microscopy aggregated across urban and rural clusters (show counts)
## -----------------------------------------------------------------------------------------------------------------------------------------

# create separate tables for recent and preceding surveys
recent_table_data <- all %>%
  filter(survey == "Recent Survey") %>%
  select(country_year.x, type, total) %>%
  pivot_wider(names_from = type, values_from = total) %>%
  mutate(Total = Urban + Rural) %>%
  arrange(desc(Total))
preceding_table_data <- all %>%
  filter(survey == "Preceding Survey") %>%
  select(country_year.x, type, total) %>%
  pivot_wider(names_from = type, values_from = total) %>%
  mutate(Total = Urban + Rural) %>%
  arrange(desc(Total))

# rename columns for clarity
colnames(recent_table_data) <- c("DHS", "Rural", "Urban", "Total")
colnames(preceding_table_data) <- c("DHS", "Rural", "Urban", "Total")

# export both tables to a word document
doc <- read_docx()
doc <- doc %>%
  body_add_par("Preceding Surveys", style = "heading 1") %>%
  body_add_table(value = preceding_table_data, style = "table_template")

doc <- doc %>%
  body_add_par("Recent Surveys", style = "heading 1") %>%
  body_add_table(value = recent_table_data, style = "table_template")

# save the document
file_path <- file.path("outputs/counts_children_surveyed.docx")
print(doc, target = file_path)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Figure 1a - Map of Africa showing countries whose DHS surveys were included in this analysis
## -----------------------------------------------------------------------------------------------------------------------------------------

# read the shapefile for Africa boundaries
afr.shp.base<- st_read(file.path("data/afr_g2014_2013_0.shp"))

# re-load urban dataset (we need the DHS_Country_Code and CountryName vars we previously removed)
urban_df <- read_csv(file.path("data/250605_urban_df_for_analysis.csv")) %>% 
  mutate(type = "Urban") %>% 
  name_clean_fun() # clean column names using custom function (see helpers.R script)

# extract distinct country codes and names from the urban dataframe
DHS_country_codes <- urban_df %>% select(DHS_CountryCode,CountryName) %>%  distinct(DHS_CountryCode, CountryName) %>%  mutate(data_available = 1)

# fix country codes for specific countries
DHS_country_codes$DHS_CountryCode[which(DHS_country_codes$CountryName == 'Madagascar')] <- 'MG'
DHS_country_codes$DHS_CountryCode[which(DHS_country_codes$CountryName == 'Burundi')] <- 'BI'

# join the shapefile data with country codes
afr_shape_dat <- afr.shp.base %>%
  left_join(DHS_country_codes, by = c("ISO2" = "DHS_CountryCode")) %>%  
  mutate(data_com = if_else(is.na(data_available), 0, data_available)) # create a binary indicator for data availability

# create the map using ggplot2
p1a <- ggplot() +
  geom_sf(data = afr_shape_dat , aes(geometry = geometry, fill = data_available)) +
  scale_fill_continuous(low ="#ffd5c6", high= "#d08288",  na.value = "white") +
  map_theme() +
  theme(legend.position="none") + 
  labs(title = "Urban Malaria Net Ownership Data Availability in Africa")

# save as pdf
ggsave(file.path("outputs/figure_1a.pdf"), p1a, width = 6, height = 8) 

# load and prepare data
urban_df <- read_csv(file.path("data/250605_urban_df_for_analysis.csv")) %>%
  mutate(type = "Urban")

rural_df <- read_csv(file.path("data/250605_rural_df_for_analysis.csv")) %>%
  mutate(type = "Rural")

# combine urban and rural data
df <- rbind(urban_df, rural_df)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Figure 3
## -----------------------------------------------------------------------------------------------------------------------------------------

# overall
color = c("#f2a5a1", "#c55c80")
test_result_df <- df %>% 
  dplyr::select(country_year.x, home_type2, code_year, test_result) %>% 
  group_by(home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))

p <-ggplot(test_result_df, aes(fill = test_result, x= home_type2)) +
  geom_bar(aes(y = value), position = "stack", stat = "identity")+
  theme_manuscript() +
  scale_x_discrete(labels = c("Agricultural\nWorker HH", "Non-Agricultural \nWorker HH"))+
  scale_fill_manual(name = "Malaria Test Result", labels= c("Negative", "Positive"), values= color)+
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
            position = position_stack(vjust = 0.5),
            color = "white") +
  labs(x = "", y  = "Number of children (6–59 months)
  tested positive for malaria in 15 DHS datasets") +
  theme(strip.text.x = element_text(
    size = 12, color = "black"))

ggsave(file.path("outputs/sup_figure_1.pdf"), p, width = 8.5, height = 6)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Figure 3 - Urban Plot (# children tested positive by agric/non-agric households in urban areas)
## -----------------------------------------------------------------------------------------------------------------------------------------

color = c("#f2a5a1", "#c55c80")

# create a dataframe for urban data with selected columns
plot_df_um <- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result, id, strat, wt) 

# prepare data for plotting with survey design
plot_overall <- plot_df_um %>% 
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%
  group_by(home_type2, test_result) %>% # group data by home type and test result  
  summarise(
    value = survey_mean(vartype = "se"),
    count = n()  # Count of individuals in each category
    # calculate survey mean with standard error
  ) %>%
  mutate(lower_ci = value - (1.96 * value_se),  # calculate lower bound of 95% CI
         upper_ci = value + (1.96 * value_se),  # calculate lower bound of 95% CI
         percent = round(value * 100, 0),        # convert mean to percentage
         lower_ci_perc = lower_ci * 100,
         upper_ci_perc = upper_ci * 100)       


# set title for the plot
plot_overall$title = "Urban"

# create unstacked bar plot for urban data (includes confidence intervals and counts)
p_urban <- ggplot(plot_overall, aes(fill = test_result, x = home_type2)) + 
  geom_bar(aes(y = percent), position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = lower_ci_perc, ymax = upper_ci_perc), 
                width = 0.2,
                position = position_dodge(0.9),
                color = "black") +
  theme_manuscript() +
  scale_x_discrete(labels = c("Agricultural\nWorker HH", "Non-Agricultural\nWorker HH")) +
  scale_fill_manual(name = "Malaria Test Result", labels = c("Negative", "Positive"), values = color) + 
  geom_text(aes(label = paste0(percent, "%\n(", count, ")"), y = percent),
            position = position_dodge(0.9),
            color = "black", vjust = -0.4, hjust = 0.5, size = 4) +
  labs(x = "", y = "") + 
  theme(strip.text.x = element_text(size = 12, color = "black"), 
        axis.text.x = element_text(size = 12)) +
  facet_wrap(vars(title)) +
  coord_cartesian(ylim = c(0, 100))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Figure 3 - Rural Plot (# children tested positive by agric/non-agric households in rural areas)
## -----------------------------------------------------------------------------------------------------------------------------------------

plot_df_rm <- rural_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result, id, strat, wt) 

plot_overall <- plot_df_rm %>% 
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%
  group_by(home_type2, test_result) %>%   
  summarise(
    value = survey_mean(vartype = "se"),
    count = n()  # count of individuals in each category
  ) %>%
  mutate(
    lower_ci = value - (1.96 * value_se),  # lower bound of 95% CI
    upper_ci = value + (1.96 * value_se),  # upper bound of 95% CI
    percent = round(value * 100, 0),        # convert to percentages
    lower_ci_perc = lower_ci * 100,
    upper_ci_perc = upper_ci * 100)

plot_overall$title <- "Rural"

# create unstacked bar plot for rural data (includes confidence intervals)
p_rural <- ggplot(plot_overall, aes(fill = test_result, x = home_type2)) + 
  geom_bar(aes(y = percent), position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = lower_ci_perc, ymax = upper_ci_perc), 
                width = 0.2,
                position = position_dodge(0.9),
                color = "black") +
  theme_manuscript() +
  scale_x_discrete(labels = c("Agricultural\nWorker HH", "Non-Agricultural\nWorker HH")) +
  scale_fill_manual(name = "Malaria Test Result", labels = c("Negative", "Positive"), values = color) + 
  geom_text(aes(label = paste0(percent, "%\n(", count, ")"), y = percent),
            position = position_dodge(0.9),
            color = "black", vjust = -0.6, hjust = 0.5, size = 4) +
  labs(x = "", y = "") + 
  theme(strip.text.x = element_text(size = 12, color = "black"), 
        axis.text.x = element_text(size = 12)) +
  facet_wrap(vars(title)) +
  coord_cartesian(ylim = c(0, 100))

# combine the urban and rural plots, display and save as png
# extract the legend
legend <- get_only_legend(p_urban) 

# remove individual legends as we only need one
p_urban <- p_urban + theme(legend.position = "none")
p_rural <- p_rural + theme(legend.position = "none") 

combined_plot_malaria <- grid.arrange(p_urban, p_rural, ncol = 2)

# save as .pdf
ggsave((file.path("outputs/positivity_ci_bar_plots.pdf")), combined_plot_malaria, width = 10, height = 5) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Figure 6 - Plot: Net Use vs Occupation Category and Malaria Positivity
# y-axis is number of positive malaria tests in children under 5
## -----------------------------------------------------------------------------------------------------------------------------------------

# define colors for the plot (new colors)
color = c( "#1750AC", "#73B9EE")

# create a dataframe for urban data with selected columns
plot_df_un <- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, u5_net_use, id, strat, wt)

# prepare data for plotting with survey design and calculate confidence intervals
plot_urban <- plot_df_un %>% 
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%
  group_by(home_type2, u5_net_use) %>%
  summarise(
    value = survey_mean(vartype = "se"),
    count = n()  # count of individuals in each category
    # calculate survey mean with standard error
  ) %>%
  mutate(lower_ci = value - (1.96 * value_se),  # calculate lower bound of 95% CI
         upper_ci = value + (1.96 * value_se),  # calculate lower bound of 95% CI
         percent = round(value * 100, 0),        # convert mean to percentage
         lower_ci_perc = lower_ci * 100,
         upper_ci_perc = upper_ci * 100) 
plot_urban$u5_net_use <- factor(plot_urban$u5_net_use, levels = c(0, 1))

# set title for urban plot
plot_urban$title = "Urban"

# create unstacked bar plot for urban net data (includes confidence intervals)
p_net_urban <- ggplot(plot_urban, aes(fill = u5_net_use, x = home_type2)) + 
  geom_bar(aes(y = percent), position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = lower_ci_perc, ymax = upper_ci_perc), 
                width = 0.2,  # Width of the error bars
                position = position_dodge(0.9),  # Align error bars with bars
                color = "black") +  # Color for error bars
  theme_manuscript() +
  scale_x_discrete(labels = c("Agricultural\nWorker HH", "Non-Agricultural\nWorker HH")) +
  scale_fill_manual(name = "Net Use", labels = c("Did Not Use", "Used"), values = color) + 
  geom_text(aes(label = paste0(percent, "%\n(", count, ")"), y = percent),  # Combined label for percent and count
            position = position_dodge(0.9),  # Adjust label position for dodged bars
            color = "black", vjust = -0.6, hjust = 0.5, size = 4) +  # Centered in the bar, with smaller font size
  labs(x = "", y = "") + 
  theme(strip.text.x = element_text(size = 12, color = "black"), 
        axis.text.x = element_text(size = 12)) +  # Decrease size of x-axis labels
  facet_wrap(vars(title)) +  # create facets by title
  coord_cartesian(ylim = c(0, 100))  # Adjust y-limit for stacked bars

# create a dataframe for rural data with selected columns
plot_df_rn <- rural_df %>%
  dplyr::select(country_year.x, home_type2, code_year, u5_net_use, id, strat, wt)

# prepare data for plotting with survey design and calculate confidence intervals
plot_rural <- plot_df_rn %>% 
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%
  group_by(home_type2, u5_net_use) %>%
  summarise(
    value = survey_mean(vartype = "se"),
    count = n()
  ) %>%
  mutate(lower_ci = value - (1.96 * value_se),  # calculate lower bound of 95% CI
         upper_ci = value + (1.96 * value_se),  # calculate lower bound of 95% CI
         percent = round(value * 100, 0),        # convert mean to percentage
         lower_ci_perc = lower_ci * 100,
         upper_ci_perc = upper_ci * 100) 
plot_rural$u5_net_use <- factor(plot_rural$u5_net_use, levels = c(0, 1))

# set title for rural plot
plot_rural$title = "Rural"

# create unstacked bar plot for urban net data (includes confidence intervals)
p_net_rural <- ggplot(plot_rural, aes(fill = u5_net_use, x = home_type2)) + 
  geom_bar(aes(y = percent), position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = lower_ci_perc, ymax = upper_ci_perc), 
                width = 0.2,  # Width of the error bars
                position = position_dodge(0.9),  # Align error bars with bars
                color = "black") +  # Color for error bars
  theme_manuscript() +
  scale_x_discrete(labels = c("Agricultural\nWorker HH", "Non-Agricultural\nWorker HH")) +
  scale_fill_manual(name = "Net Use", labels = c("Did Not Use", "Used"), values = color) + 
  geom_text(aes(label = paste0(percent, "%\n(", count, ")"), y = percent),  # Combined label for percent and count
            position = position_dodge(0.9),  # Adjust label position for dodged bars
            color = "black", vjust = -0.6, hjust = 0.5, size = 4) +  # Centered in the bar, with smaller font size
  labs(x = "", y = "") + 
  theme(strip.text.x = element_text(size = 12, color = "black"), 
        axis.text.x = element_text(size = 12)) +  # Decrease size of x-axis labels
  facet_wrap(vars(title)) +  # create facets by title
  coord_cartesian(ylim = c(0, 100))  # Adjust y-limit for stacked bars

# remove individual legends as we only need one
p_net_urban <- p_net_urban + theme(legend.position = "none")
p_net_rural <- p_net_rural + theme(legend.position = "none") 

# combine urban and rural plots
combined_plot_nets <- grid.arrange(p_net_urban, p_net_rural, ncol = 2)

# save net plots as .pdf
ggsave((file.path("outputs/net_ci_bar_plots.pdf")), combined_plot_nets, width = 10, height = 5) 

# combine malaria and net plots
final_fig2 <- grid.arrange(combined_plot_malaria, combined_plot_nets, nrow = 2)

ggsave((file.path("outputs/bar_charts_fig2.pdf")), final_fig2, width = 8, height = 9) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Figure 3b
## -----------------------------------------------------------------------------------------------------------------------------------------

# ## -----------------------------------------------------------------------------------------------------------------------------------------
# # calculate the difference in MALARIA POSITIVITY between agricultural and non-agricultural workers in URBAN areas
# # prepare the country-level data for plotting
plot_country = plot_df_um %>% as_survey_design(ids = id,strata = strat,nest = T,weights = wt) %>%
  group_by(country_year.x, home_type2, test_result) %>% # group by country, home type, and test result
  summarise(value = round(survey_total(), 0)) %>% # calculate total values
  mutate(percent = round(value/sum(value) * 100, 0)) %>% # calculate percentage
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))

df_u <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))

plot_country <- plot_country  %>%  left_join(df_u, by = "country_year.x")  %>% mutate(plot_label = ifelse(test_result == "-ve", percent, NA))# df_u <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))

plot_country <- plot_country  %>%  left_join(df_u, by = "country_year.x")  %>% mutate(plot_label = ifelse(test_result == "-ve", percent, NA))

# filter data for positive test results
plot_country = plot_country %>% filter(test_result == "+ve")

# calculate the difference in malaria test positivity rates
diff_d_u_malaria <- plot_country %>% group_by(country_year.x) %>%
  mutate(diff_val_urban_malaria = percent[home_type2 == "Agricultural worker Household (HH)"] - percent) %>% # calculate the difference
  filter(home_type2 == "Non-Agricultural worker HH") # filter for non-agricultural households

## -----------------------------------------------------------------------------------------------------------------------------------------
# calculate the difference in NET USE between agricultural and non-agricultural workers in RURAL areas
# prepare the country-level data for plotting
plot_country = plot_df_rn %>% as_survey_design(ids = id,strata = strat, nest = T, weights = wt) %>%
  group_by(country_year.x, home_type2, u5_net_use) %>% # group by country, home type, and net use
  summarise(value = round(survey_total(), 0)) %>% # calculate total values and round them
  mutate(percent = round(value/sum(value) * 100, 0)) %>% # calculate percentage of each group
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x), # standardize country name
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH")) # label home types

# filter for households with net use
plot_country = plot_country %>% filter( u5_net_use == "1")

# calculate difference in net use rates for rural households
diff_d_r_nets <- plot_country %>%
  group_by(country_year.x) %>% # group by country and year
  mutate(diff_val_rural_nets = percent[home_type2 == "Agricultural worker Household (HH)"] - percent) %>% # calculate the difference
  filter(home_type2 == "Non-Agricultural worker HH") # filter for non-agricultural households

## -----------------------------------------------------------------------------------------------------------------------------------------
# calculate the difference in NET USE between agricultural and non-agricultural workers in URBAN areas
# prepare the country-level data for plotting
plot_country = plot_df_un %>% as_survey_design(ids = id, strata = strat, nest = T, weights = wt) %>% 
  group_by(country_year.x, home_type2, u5_net_use) %>% # group by country, home type, and net use
  summarise(value = round(survey_total(), 0)) %>% # calculate total values
  mutate(percent = round(value/sum(value) * 100, 0)) %>% # calculate percentage
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))

# filter data for households using nets
plot_country = plot_country %>% filter(u5_net_use == "1")

# calculate the difference in net use rates
diff_d_u_nets <- plot_country %>% 
  group_by(country_year.x) %>%  mutate(diff_val_urban_nets = percent[home_type2 == "Agricultural worker Household (HH)"] - percent) %>% # calculate the difference
  filter(home_type2 == "Non-Agricultural worker HH") # filter for non-agricultural households

# visualize the differences
diff_d_u_nets$title = "Urban"
p_diff_u_nets <- ggplot(diff_d_u_nets , aes(x = reorder(country_year.x, -diff_val_urban_nets), y = diff_val_urban_nets, fill)) +
  geom_bar(stat = "identity",  width = 0.7, fill = "#c55c80") +
  geom_text(aes(label= diff_val_urban_nets), position = position_stack(vjust = 0.5),
            color = "black") +
  coord_flip() +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Percentage difference in net use rates
      between agricultural worker HH and non-agricultural worker HH")+
  facet_wrap(vars(title))+
  theme_manuscript()+
  theme(legend.position = "bottom") 

# join both dataframes (malaria and net use difference) to create a scatterplot
diff_d_u_malaria <-   diff_d_u_malaria %>%  
  select(country_year.x , diff_val_urban_malaria) # select relevant columns for malaria data

diff_d_u_nets <- diff_d_u_nets %>%  
  select(country_year.x, diff_val_urban_nets) # select relevant columns for net use data

# merge the two dataframes and categorize net use
df_m_n_country <- left_join(diff_d_u_malaria, diff_d_u_nets) %>% 
  mutate(net_category = if_else(diff_val_urban_nets<0, "lower_agric_coverage", "higher_agric_coverage")) # create a new category based on net use difference

# retrieve unique code_years from the original dataframe
code_year <- plot_df_um %>%  
  select(country_year.x, code_year) %>%  
  distinct(country_year.x,code_year) # keep distinct combinations of country and code_year

# join the code_year data and handle missing values
df_m_n_country <- df_m_n_country %>%  
  left_join(code_year) %>%  
  mutate(code_year = if_else(is.na(code_year) & country_year.x== 'DRC 2013 - 14',"CD2013", code_year )) # replace NA for DRC with a specific code

# set title for the plot
df_m_n_country$title = "Urban"

# create the scatterplot
country_m_n_urban <- ggplot(df_m_n_country, aes(x = diff_val_urban_nets, y = diff_val_urban_malaria, color = net_category)) +
  geom_point(shape = 19, size = 4, alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "red") +
  geom_text_repel(aes(label = code_year), size = 4, point.padding = 5) +
  scale_color_manual(values = c("#622c88", "#622c88")) +
  theme_manuscript() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(title)) +
  labs(
    x = "Difference in net use between agricultural worker\nHHs and non-agricultural worker HHs", 
    y = "Difference in Malaria Test Positivity Rate Between\nAgricultural Worker HHs and Non-Agricultural Worker HHs"
  ) +
  theme(legend.position = "none") +
  ylim(-3, 28) +
  xlim(-20, 20)

## -----------------------------------------------------------------------------------------------------------------------------------------
# calculate the difference in MALARIA POSITIVITY between agricultural and non-agricultural workers in RURAL areas
# prepare the country-level data for plotting
plot_country = plot_df_rm %>% as_survey_design(ids= id,strata=strat,nest=T,weights= wt) %>% 
  group_by(country_year.x,home_type2, test_result) %>% # group by country, home type, and test result
  summarise(value = round(survey_total(),0)) %>% # calculate total values and round them
  mutate(percent = round(value/sum(value) *100, 0)) %>% # calculate percentage of each group
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x), # standardize country name
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH")) # label home types

# filter for positive test results
plot_country = plot_country %>% filter(test_result == "+ve")

# calculate difference in malaria positivity rates for rural households
diff_d_r_malaria <- plot_country %>% 
  group_by(country_year.x) %>% # group by country and year
  mutate(diff_val_rural_malaria = percent[home_type2 == "Agricultural worker Household (HH)"] - percent) %>% # calculate the difference
  filter(home_type2 == "Non-Agricultural worker HH") # filter for non-agricultural households

## -----------------------------------------------------------------------------------------------------------------------------------------
# join both dataframes (malaria and net use difference) to create a scatterplot for RURAL areas

# select relevant columns from the malaria and nets dataframes
diff_d_r_malaria <- diff_d_r_malaria %>%
  select(country_year.x , diff_val_rural_malaria) # keep only country-year and malaria difference

diff_d_r_nets <- diff_d_r_nets %>%
  select(country_year.x, diff_val_rural_nets) # keep only country-year and nets difference

# perform a left join to combine the dataframes
df_m_n_country_rural <- left_join(diff_d_r_malaria, diff_d_r_nets) %>% 
  mutate(net_category = if_else(diff_val_rural_nets<0, "lower_agric_coverage", "higher_agric_coverage")) # categorize net use

# extract unique country-year and code_year combinations from the plot dataframe
code_year <- plot_df_rm %>%  
  select(country_year.x, code_year) %>%  
  distinct(country_year.x,code_year) # ensure distinct values

# join the code_year data and handle missing values
df_m_n_country_rural <- df_m_n_country_rural %>%  
  left_join(code_year) %>%  
  mutate(code_year = if_else(is.na(code_year) & country_year.x== 'DRC 2013 - 14',"CD2013", code_year )) # fill missing code_year

# add title for the rural data
df_m_n_country_rural$title = "Rural"

# create scatter plot comparing net use and malaria positivity rates
country_m_n_rural <- ggplot(df_m_n_country_rural, aes(x = diff_val_rural_nets, y = diff_val_rural_malaria, color = net_category, label = code_year)) +
  geom_point(shape = 19, size = 4, alpha = 0.7) +  
  geom_smooth(method = lm, se = FALSE, color = "red") +
  geom_text_repel(size = 4, point.padding = 5) +
  scale_color_manual(values = c("#e07a5f", "#e07a5f")) +
  theme_manuscript() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(title)) +
  labs(x = "Difference in net use between agricultural worker\n HHs and non-agricultural worker HHs", 
       y = "Difference in Malaria Test Positivity Rate Between \n Agricultural Worker HHs and Non-Agricultural Worker HHs") +
  theme(legend.position = "none") +
  ylim(-3, 28) + 
  xlim(-20, 20)

# remove individual titles and x-axis labels from the urban and rural plots
country_m_n_urban <- country_m_n_urban + 
  labs(x = NULL, y = NULL)
country_m_n_rural <- country_m_n_rural + 
  labs(x = NULL, y = NULL)

final_net_v_malaria_dots <- grid.arrange(country_m_n_urban, country_m_n_rural, ncol = 2)

# arrange the combined plot
final_net_v_malaria_dots <- grid.arrange(
  final_net_v_malaria_dots,
  nrow = 1,
  heights = c(5),
  bottom = textGrob("Difference in Net Use Between Agricultural\n Worker HHs and Non-Agricultural Worker HHs",
                    gp = gpar(fontsize = 12, hjust = 0.5))
)

final_fig3 <- grid.arrange(final_fig2, final_net_v_malaria_dots, nrow = 2, heights = c(11, 6))

# save as .pdf
ggsave((file.path("outputs/malaria_test_positivity_netuse_agric_urban_rural_by_country.pdf")), final_net_v_malaria_dots, width = 8, height = 5) 
ggsave((file.path("outputs/final_fig3.pdf")), final_fig3, width = 8, height = 11) 

## =========================================================================================================================================
### COVARIATE DISTRIBUTION FIGURES
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Household Size
## -----------------------------------------------------------------------------------------------------------------------------------------

# combine urban and rural data frames into a single data frame
all_df <- rbind(urban_df, rural_df) %>%  
  mutate(home_type3 = ifelse(home_type2 == "A", "Agricultural worker \n Household (HH)", 
                             "Non-Agricultural \n worker HH")) # categorize home type based on home_type2

# convert type variable to a factor with specified levels for plotting
all_df$type_f <- factor(all_df$type, levels=c("Urban", "Rural"))

# create a boxplot of household size by home type
p1 <- ggplot(all_df, aes(x = home_type3, y = hh_size, fill = home_type3)) +
  scale_fill_manual(name = '', values =c('#5560AB','#FAAF43'))+
  geom_boxplot(outlier.size = -1, color="black", alpha = 0.7) +
  #geom_point(aes(fill = home_type3), shape = 21,size=2, alpha=0.6, stroke=0, color = '#979797')+
  labs(x = "", y = "Household Size", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12))+
  ylim(0, 16)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Roof Type
## -----------------------------------------------------------------------------------------------------------------------------------------

# categorize roof type and prepare survey design
all_df2 <- all_df %>%
  as_survey_design(ids = id, strata = strat, nest = T, weights = wt) %>%  # create a survey design object
  mutate(roof = ifelse(roof_type == 1, "Low-risk roof", "High-risk roof")) %>%  # categorize roof based on roof_type
  drop_na(roof) %>%  # remove rows with missing roof values
  mutate(roof_f = factor(roof, levels = c("Low-risk roof", "High-risk roof"))) %>%  # convert roof to a factor
  group_by(type_f, home_type3, roof_f) %>%  # group by type, home type, and roof type
  summarise(value = round(survey_total(), 0)) %>%  # calculate total survey values, rounded to nearest integer
  mutate(percent = round(value / sum(value) * 100, 0))  # calculate percentage of total values

# create a bar plot of roof type by home type
p2 <- ggplot(all_df2, aes(fill=roof_f, x= home_type3)) + 
  geom_bar(aes(y = percent), position="stack", stat = "identity", show.legend = F)+
  scale_fill_manual(name = "", values = c("bisque", "forestgreen"))+
  theme_manuscript() +
  facet_wrap(vars(type_f)) +
  labs(x = "Roof")+
  theme(strip.text.x = element_text(size = 12))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Create Housing Quality Variable and Add Graph
## -----------------------------------------------------------------------------------------------------------------------------------------

# add housing quality indicator to the df
all_df <- all_df %>%
  mutate(
    # housing quality indicator (created by Colleen Leonard @ cleonard297@gmail.com)
    floor_type = ifelse(hv213 >= 98, NA, ifelse(hv213 %in% c(30, 31, 32, 33, 34, 35, 36, 37), 1, 0)),
    wall_type = ifelse(hv214 >= 98, NA, ifelse (hv214 %in% c(30, 31, 32, 33, 34, 35, 37, 38), 1, 0)),
    roof_type = ifelse(hv215 >= 98, NA, ifelse(hv215 %in% c(30, 31, 33, 34, 35), 1, 0)),
    
    # for guinea, if hv214 == 34 (wood planks/shingles), mark wall_type = 0
    wall_type = ifelse(CountryName == "Guinea" & hv214 == 34, 0, wall_type),
    
    # for mozambique, if hv215 == 32 (calamine/cement fiber), mark roof_type = 1
    roof_type = ifelse(CountryName == "Mozambique" & hv215 == 32, 1, roof_type),
    
    # create housing quality indicator:
    # if floor_type, wall_type, and roof_type = 1 (indicating higher quality for each component), housing_quality = 1 (good housing quality)
    # if any of these components is 0 (indicating lower quality in any area), then housing_q is set to 0
    housing_quality = ifelse(floor_type == 1 & wall_type == 1 & roof_type == 1, 1, 0),
  )

# categorize housing quality and prepare survey design
all_df_hq <- all_df %>%
  as_survey_design(ids = id, strata = strat, nest = T, weights = wt) %>%  # create a survey design object
  mutate(housing_quality = ifelse(housing_quality == 1, "Good Quality", "Poor Quality")) %>%  # categorize housing quality
  drop_na(housing_quality) %>%  # remove rows with missing hq values
  mutate(hq_f = factor(housing_quality, levels = c("Good Quality", "Poor Quality"))) %>%  # convert hq to a factor
  group_by(type_f, home_type3, hq_f) %>%  # group by type, home type, and housing quality
  summarise(value = round(survey_total(), 0)) %>%  # calculate total survey values, rounded to nearest integer
  mutate(percent = round(value / sum(value) * 100, 0))  # calculate percentage of total values

# create a bar plot of housing quality by home type
p_hq <- ggplot(all_df_hq, aes(fill = hq_f, x= home_type3)) + 
  geom_bar(aes(y = percent), position="stack", stat = "identity", show.legend = F)+
  scale_fill_manual(name = "", values = c("#C8E6C9", "#1B5E20")) +
  labs(x = "", y = "Housing Quality", fill = "home type") +
  theme_manuscript() +
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Treatment-Seeking
## -----------------------------------------------------------------------------------------------------------------------------------------

# categorize treatment-seeking and prepare survey design
all_df_ts <- all_df %>%
  as_survey_design(ids = id, strata = strat, nest = T, weights = wt) %>%  # create a survey design object
  mutate(med_treat_fever_none = ifelse(med_treat_fever_none == 1, ">= 60% Sought Treatment", "< 60% Sought Treatment")) %>%  # categorize treatment-seeking
  drop_na(med_treat_fever_none) %>%  # remove rows with missing values
  mutate(ts_f = factor(med_treat_fever_none, levels = c(">= 60% Sought Treatment", "< 60% Sought Treatment"))) %>%  # convert ts to a factor
  group_by(type_f, home_type3, ts_f) %>%  # group by type, home type, and treatment-seeking
  summarise(value = round(survey_total(), 0)) %>%  # calculate total survey values, rounded to nearest integer
  mutate(percent = round(value / sum(value) * 100, 0))  # calculate percentage of total values

# create a bar plot of treatment-seeking by home type
p_ts <- ggplot(all_df_ts, aes(fill = ts_f, x= home_type3)) + 
  geom_bar(aes(y = percent), position="stack", stat = "identity", show.legend = F)+
  scale_fill_manual(name = "", values = c("#f5bbcc", "#e75480"))+
  labs(x = "", y = "Treatment-Seeking", fill = "home type") +
  theme_manuscript() +
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Wealth
## -----------------------------------------------------------------------------------------------------------------------------------------

# convert wealth to a factor with specified levels and labels
all_df$wealth_f <- factor(all_df$wealth, levels=c("5", "4", "3", "2", "1"), 
                          labels =c("Richest", "Rich", "Middle", "Poor", "Poorest"))

# create a survey design object and summarize wealth data
all_df2 <- all_df %>%
  as_survey_design(ids= id,strata=strat,nest=T,weights= wt) %>% # create survey design
  group_by(type_f, home_type3, wealth_f) %>% # group by type, home type, and wealth category
  summarise(value = round(survey_total(),0)) %>% # calculate total values, rounded to nearest integer
  mutate(percent = round(value/sum(value) *100, 0)) # calculate percentage of total values

# create a bar plot of wealth distribution by home type  
p3 <- ggplot(all_df2, aes(fill=wealth_f, x= home_type3)) + 
  geom_bar(aes(y = percent), position="stack", stat = "identity",show.legend = F)+
  scale_fill_manual(name = "Wealth Quintiles", values= colorRampPalette(c("#bbdefb", "#0d47a1"))(5)) +
  labs(x = "", y = "Wealth Quintiles", fill = "home type") +
  theme_manuscript() +
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12), legend.title = element_blank())+
  labs(x= "")


## =========================================================================================================================================
### Reading in Environmental Data
## =========================================================================================================================================

# read in environmental data from a CSV file and create a new variable for code and year
df_env <- read_csv(file.path("data/all_geospatial_monthly_DHS.csv")) %>% 
  mutate(code_year = paste(stringr::str_extract(.id, "^.{2}"), dhs_year, sep = "")) %>% # extract the first two characters from .id and concatenate with dhs_year
  select(-dhs_year) %>% # drop the original dhs_year column
  mutate(EVI_2000m_new = if_else(EVI_2000m < 0, 0, EVI_2000m)) # replace negative EVI values with 0

# add environmental data to urban and rural datasets
urban_df2 <- urban_df  %>% 
  left_join(df_env, by = c("code_year",  "hv001")) # join urban data with environmental data based on code_year and hv001

rural_df2 <- rural_df  %>% 
  left_join(df_env, by = c("code_year",   "hv001")) # join rural data with environmental data based on code_year and hv001

# combine urban and rural datasets and create a new home type variable
all_df <- rbind(urban_df2, rural_df2) %>%  
  mutate(home_type3 = ifelse(home_type2 == "A", "Agricultural worker \n Household (HH)", 
                             "Non-Agricultural \n worker HH")) # label home types based on home_type2

all_df$type_f <- factor(all_df$type, levels=c("Urban", "Rural")) # create a factor for type with specified levels

# write the final analysis dataset to a CSV file
write.csv(all_df, file.path("data/251206_urban_rural_analysis_data_for_modeling.csv"))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Enhanced Vegetation Index (EVI)
## -----------------------------------------------------------------------------------------------------------------------------------------

# calculate mean EVI for each group (agric/non-agric, urban/rural)
mean_values <- all_df %>%
  group_by(home_type3, type_f) %>%
  summarise(mean_evi = mean(EVI_2000m_new, na.rm = TRUE))
print(mean_values)

# create box plot for Enhanced Vegetation Index (EVI)
p4 <- ggplot(all_df, aes(x = home_type3, y = EVI_2000m_new, fill = home_type3)) +
  scale_fill_manual(name = '', values =c('#5560AB','#FAAF43'))+
  geom_boxplot(outlier.size = -1, color="black", alpha = 0.7) +
  #geom_point(aes(fill = home_type3), shape = 21,size=2, alpha=0.6, stroke=0, color = '#979797')+
  labs(x = "", y = "Enhanced Vegetation Index", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Precipitation
## -----------------------------------------------------------------------------------------------------------------------------------------

# calculate mean precipitation for each group (agric/non-agric, urban/rural)
mean_values <- all_df %>%
  group_by(home_type3, type_f) %>%
  summarise(mean_preci = mean(preci_monthly_2000m, na.rm = TRUE))
print(mean_values)

# create box plot for monthly precipitation
p5 <- ggplot(all_df, aes(x = home_type3, y = preci_monthly_2000m, fill = home_type3)) +
  scale_fill_manual(name = '', values =c('#5560AB','#FAAF43'))+
  geom_boxplot(outlier.size = -1, color="black", alpha = 0.7) +
  labs(x = "", y = "Precipitation (mm)", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12))+
  ylim(0, 510)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Relative Humidity
## -----------------------------------------------------------------------------------------------------------------------------------------

# create box plot for relative humidity
p6 <- ggplot(all_df, aes(x = home_type3, y = RH_monthly_2000m, fill = home_type3)) +
  scale_fill_manual(name = '', values =c('#5560AB','#FAAF43'))+
  geom_boxplot(outlier.size = -1, color="black", alpha = 0.7) +
  #geom_point(aes(fill = home_type3), shape = 21,size=2, alpha=0.6, stroke=0, color = '#979797')+
  labs(x = "", y = "Relative Humidity (%)", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12)) +
  ylim(0, 80)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Temperature
## -----------------------------------------------------------------------------------------------------------------------------------------

# create box plot for temperature
p7 <- ggplot(all_df, aes(x = home_type3, y = temp_monthly_2000m, fill = home_type3)) +
  scale_fill_manual(name = '', values =c('#5560AB','#FAAF43'))+
  geom_boxplot(outlier.size = -1, color="black", alpha = 0.7) +
  #geom_point(aes(fill = home_type3), shape = 21,size=2, alpha=0.6, stroke=0, color = '#979797')+
  labs(x = "", y = "Temperature (\u00B0C)", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12))+
  ylim(0, 35)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Age
## -----------------------------------------------------------------------------------------------------------------------------------------

# create box plot for age
p8 <- ggplot(all_df, aes(x = home_type3, y = hc1, fill = home_type3)) +
  scale_fill_manual(name = '', values =c('#5560AB','#FAAF43'))+
  geom_boxplot(outlier.size = -1, color="black", alpha = 0.7) +
  #geom_point(aes(fill = home_type3), shape = 21,size=2, alpha=0.6, stroke=0, color = '#979797')+
  labs(x = "", y = "Age (months)", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12))+
  ylim(0, 59)

# wrap multiple plots into a single plot object and save the combined plot as a pdf
all_p1 <- wrap_plots(p1,p4, p5, p6, p7, p8) 
all_p2 <- wrap_plots(p_hq, p3) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Gender
## -----------------------------------------------------------------------------------------------------------------------------------------

# mutate data frame to create new variables for gender and stunting status
all_df <- all_df %>% 
  mutate(gender = ifelse(hc27 == 2, "Female", "Male"), # assign gender based on hc27 value
         stunting_new = ifelse(hc70 < -300, "stunted", ifelse(hc70 > 8000, NA, "Not stunted"))) # assign stunting status based on hc70 value

# create a bar plot for gender distribution
p_gender <- ggplot(all_df, aes(x = home_type3, fill = gender)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1)) +
  scale_fill_manual(values =c("#ffd7b5", "#ff6700"))+
  labs(x = "", y = "Gender", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Stunting
## -----------------------------------------------------------------------------------------------------------------------------------------

# get percentages of stunting/non-stunting in each category
stunting_percentages <- all_df %>%
  drop_na(stunting_new) %>%
  group_by(home_type3, type_f, stunting_new) %>%
  summarise(count = n()) %>%
  group_by(home_type3, type_f) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()
print(stunting_percentages)

# create a bar plot for stunting distribution, filtering out missing values in stunting_new
p_stunting <- ggplot(all_df %>% drop_na(stunting_new), aes(x = home_type3, fill = stunting_new)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1)) +
  scale_fill_manual(values =c("#D1C4E9", "#4A148C"))+
  labs(x = "", y = "Stunting", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12))

# combine plots
bottom_row <- p_gender + p_stunting + p_ts
middle_row <- p_hq + p3

categorical_vars <- middle_row / bottom_row

# create a final figure layout with all_p1 above figure4_bottom and annotate with tag levels and save final figure as pdf
p_figure_4 <- all_p1 / categorical_vars + plot_annotation(tag_levels = 'A')
ggsave((file.path("outputs/covariate_plots.pdf")), p_figure_4, width = 8.5, height = 11) 


## =========================================================================================================================================
### Supplemental Figure: Countries' percentage of positive/negative results by urban and rural residence and by home type
## =========================================================================================================================================

# load data
urban_df <- read_csv(file.path("data/250605_urban_df_for_analysis.csv")) %>%
  mutate(type = "Urban")
rural_df <- read_csv(file.path("data/250605_rural_df_for_analysis.csv")) %>%
  mutate(type = "Rural")

# select relevant columns for urban and rural data
plot_df_um <- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result, id, strat, wt, type) 
plot_df_rm <- rural_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result, id, strat, wt, type) 

# relabel home type categories
plot_df_rm <- plot_df_rm %>%
  mutate(home_type2 = ifelse(home_type2 == "A", "Agricultural Worker Household", "Non-Agricultural Worker Household"))
plot_df_um <- plot_df_um %>%
  mutate(home_type2 = ifelse(home_type2 == "A", "Agricultural Worker Household", "Non-Agricultural Worker Household"))

# update country_surveyyears to remove spaces around dashes
plot_df_rm <- plot_df_rm %>%
  mutate(country_year.x = str_replace_all(country_year.x, " - ", "–")) %>%
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013–14",
                                 ifelse(country_year.x == "Cameroon 2018", "Cameroon 2018–19",country_year.x)))
plot_df_um <- plot_df_um %>%
  mutate(country_year.x = str_replace_all(country_year.x, " - ", "–")) %>%
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013–14",
                                 ifelse(country_year.x == "Cameroon 2018", "Cameroon 2018–19",country_year.x))) 

# function to summarize data for percentage calculations
summarize_data <- function(df) {
  df %>%
    group_by(country_year.x, type, home_type2, test_result) %>%
    summarise(value = n(), .groups = "drop") %>%
    group_by(country_year.x, home_type2) %>%
    mutate(percent = value / sum(value),
           percent_label = ifelse(test_result == "-ve", 
                                  paste0(round(value / sum(value[test_result == "-ve" | test_result == "+ve"]) * 100, 0), "%"), 
                                  ""))
}

plot_country_urban <- summarize_data(plot_df_um)
plot_country_rural <- summarize_data(plot_df_rm)

# function to calculate positivity rate for agricultural households
calculate_positivity <- function(df) {
  df %>%
    filter(home_type2 == "Agricultural Worker Household", test_result == "+ve") %>%
    group_by(country_year.x) %>%
    summarise(positivity_rate = sum(percent))
}

urban_positivity <- calculate_positivity(plot_country_urban)
rural_positivity <- calculate_positivity(plot_country_rural)

# merge positivity rates with the main dataframes
plot_country_urban <- left_join(plot_country_urban, urban_positivity, by = "country_year.x")
plot_country_rural <- left_join(plot_country_rural, rural_positivity, by = "country_year.x")

# define colors for the plot
color <- c("-ve" = "#f2a5a1", "+ve" = "#c55c80")

# function to generate plots
plot_malaria <- function(df, title) {
  ggplot(df, aes(x = reorder(country_year.x, positivity_rate), y = percent, fill = test_result)) +
    geom_bar(stat = "identity", position = "fill", width = 0.7) +  
    coord_flip() +
    scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +  
    scale_fill_manual(name = "Malaria Test Result", labels = c("Negative", "Positive"), values = color) +
    facet_grid(. ~ home_type2) +  
    geom_text(aes(label = percent_label, y = 0.85),
              color = "black", hjust = 1) +
    labs(x = "", y = "Percentage of malaria test results among tested children aged 6–59 months", title = title) +
    theme_manuscript() +
    theme(legend.position = "bottom")
}

# create Urban and Rural plots
urban_plot <- plot_malaria(filter(plot_country_urban), "Malaria Test Results in Urban Areas")
rural_plot <- plot_malaria(filter(plot_country_rural), "Malaria Test Results in Rural Areas")

# combine plots
final_percentage_results_plot <- grid.arrange(urban_plot, rural_plot)

# save the combined plot
ggsave((file.path("outputs/descending_country_positivity.pdf")), final_percentage_results_plot, width = 10, height = 12)

## =========================================================================================================================================
### Supplemental Figure: Countries' percentage of net use by urban and rural residence and by home type
## =========================================================================================================================================

# load data
urban_df <- read_csv(file.path("data/250605_urban_df_for_analysis.csv")) %>%
  mutate(type = "Urban")
rural_df <- read_csv(file.path("data/250605_rural_df_for_analysis.csv")) %>%
  mutate(type = "Rural")

# select relevant columns for urban and rural data
plot_df_un <- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, u5_net_use, id, strat, wt, type) 
plot_df_rn <- rural_df %>% dplyr::select(country_year.x, home_type2, code_year, u5_net_use, id, strat, wt, type) 

# relabel home type categories
plot_df_un <- plot_df_un %>%
  mutate(home_type2 = ifelse(home_type2 == "A", "Agricultural Worker Household", "Non-Agricultural Worker Household"))
plot_df_rn <- plot_df_rn %>%
  mutate(home_type2 = ifelse(home_type2 == "A", "Agricultural Worker Household", "Non-Agricultural Worker Household"))

# update country_surveyyears to remove spaces around dashes
plot_df_rn <- plot_df_rn %>%
  mutate(country_year.x = str_replace_all(country_year.x, " - ", "–")) %>%
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013–14",
                                 ifelse(country_year.x == "Cameroon 2018", "Cameroon 2018–19",country_year.x)))
plot_df_un <- plot_df_un %>%
  mutate(country_year.x = str_replace_all(country_year.x, " - ", "–")) %>%
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013–14",
                                 ifelse(country_year.x == "Cameroon 2018", "Cameroon 2018–19",country_year.x))) 

# function to summarize data for percentage calculations
summarize_data <- function(df) {
  df %>%
    group_by(country_year.x, type, home_type2, u5_net_use) %>%
    summarise(value = n(), .groups = "drop") %>%
    group_by(country_year.x, home_type2) %>%
    mutate(percent = value / sum(value),
           percent_label = ifelse(u5_net_use == 0, 
                                  paste0(round(value / sum(value[u5_net_use == 0 | u5_net_use == 1]) * 100, 0), "%"), 
                                  ""))
}

plot_country_urban <- summarize_data(plot_df_un)
plot_country_rural <- summarize_data(plot_df_rn)

# function to calculate positivity rate for agricultural households
calculate_net_use_rate <- function(df) {
  df %>%
    filter(home_type2 == "Agricultural Worker Household", u5_net_use == 1) %>%
    group_by(country_year.x) %>%
    summarise(net_use_rate = sum(percent))
}

urban_net_use_rate <- calculate_net_use_rate(plot_country_urban)
rural_net_use_rate <- calculate_net_use_rate(plot_country_rural)

# merge positivity rates with the main dataframes
plot_country_urban <- left_join(plot_country_urban, urban_net_use_rate, by = "country_year.x")
plot_country_rural <- left_join(plot_country_rural, rural_net_use_rate, by = "country_year.x")

# define colors for the plot (new colors)
color = c( "#1750AC", "#73B9EE")

# function to generate plots
plot_net <- function(df, title) {
  ggplot(df, aes(x = reorder(country_year.x, net_use_rate), y = percent, fill = u5_net_use)) +
    geom_bar(stat = "identity", position = "fill", width = 0.7) +  
    coord_flip() +
    scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +  
    scale_fill_manual(name = "Net Use", labels = c("Did not sleep under a net", "Slept under a net"), values = color) +
    facet_grid(. ~ home_type2) +  
    geom_text(aes(label = percent_label, y = 0.95),
              color = "white", hjust = 1) +
    labs(x = "", y = "Net use among children (6–59 months) tested for malaria per country", title = title) +
    theme_manuscript() +
    theme(legend.position = "bottom")
}

# make net use variable factor
plot_country_urban$u5_net_use <- as.factor(plot_country_urban$u5_net_use)
plot_country_rural$u5_net_use <- as.factor(plot_country_rural$u5_net_use)

# create Urban and Rural plots
urban_plot <- plot_net(plot_country_urban, "Net Use Results in Urban Areas")
rural_plot <- plot_net(plot_country_rural, "Net Use Results in Rural Areas")

# combine plots
final_net_results_plot <- grid.arrange(urban_plot, rural_plot)

# save the combined plot
ggsave((file.path("outputs/descending_country_net_rates.pdf")), final_net_results_plot, width = 10, height = 12)

## =========================================================================================================================================
### Create Population/Agricultural Household Maps
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Read in Top-Down Constrained Estimates Rasters for each Country (find most populous first-level administrative subdivisions)
## -----------------------------------------------------------------------------------------------------------------------------------------
# countries <- c("angola", "burkina faso", "benin", "burundi", "drc", "cote d'ivoire", "cameroon",
#                "ghana", "guinea", "madagascar", "mali", "mozambique", "nigeria", "togo", "uganda")
# for (country in countries) {
#   assign(paste0(gsub(" ", "_", country), "_raster"), 
#          raster(file.path("data/population_rasters", paste0(country, ".tif"))))
# }

## -----------------------------------------------------------------------------------------------------------------------------------------
### Read in Subdivision Shapefiles (e.g. State, Region, etc) - First-Level Administrative Subdivision in Each Country
## -----------------------------------------------------------------------------------------------------------------------------------------

# path to the folder storing GPS data
gps_folder_path <- file.path("data", "GPS")

# read in the shapefiles with first-level administrative division geographic boundaries (state, region, etc)
ao.subd <- st_read(file.path(gps_folder_path, "subdivisions", "AO", "gadm41_AGO_1.shp"))
bf.subd <- st_read(file.path(gps_folder_path, "subdivisions", "BF", "gadm41_BFA_1.shp"))
bj.subd <- st_read(file.path(gps_folder_path, "subdivisions", "BJ", "gadm41_BEN_1.shp"))
bu.subd <- st_read(file.path(gps_folder_path, "subdivisions", "BU", "gadm41_BDI_1.shp"))
cd.subd <- st_read(file.path(gps_folder_path, "subdivisions", "CD", "gadm41_COD_1.shp"))
ci.subd <- st_read(file.path(gps_folder_path, "subdivisions", "CI", "gadm41_CIV_1.shp"))
cm.subd <- st_read(file.path(gps_folder_path, "subdivisions", "CM", "gadm41_CMR_1.shp"))
gh.subd <- st_read(file.path(gps_folder_path, "subdivisions", "GH", "gadm41_GHA_1.shp"))
gn.subd <- st_read(file.path(gps_folder_path, "subdivisions", "GN", "gadm41_GIN_1.shp"))
md.subd <- st_read(file.path(gps_folder_path, "subdivisions", "MD", "gadm41_MDG_2.shp")) # shapefile 1 has the 6 provinces that were dissolved in 2009, use # 2 which has current 23 regions
ml.subd <- st_read(file.path(gps_folder_path, "subdivisions", "ML", "gadm41_MLI_1.shp"))
mz.subd <- st_read(file.path(gps_folder_path, "subdivisions", "MZ", "gadm41_MOZ_1.shp"))
ng.subd <- st_read(file.path(gps_folder_path, "subdivisions", "NG", "gadm41_NGA_1.shp"))
tg.subd <- st_read(file.path(gps_folder_path, "subdivisions", "TG", "gadm41_TGO_1.shp"))
ug.subd <- st_read(file.path(gps_folder_path, "subdivisions", "UG", "uga_admbnda_adm1_ubos_20200824.shp")) # GADM doesn't have the 4 region boundaries, so downloaded them from https://data.humdata.org/dataset/cod-ab-uga?

subdivision_files <- list(
  "AO" = ao.subd, "BF" = bf.subd, "BJ" = bj.subd, "BU" = bu.subd, "CD" = cd.subd,
  "CI" = ci.subd, "CM" = cm.subd, "GH" = gh.subd, "GN" = gn.subd, "MD" = md.subd,
  "ML" = ml.subd, "MZ" = mz.subd, "NG" = ng.subd, "TG" = tg.subd, "UG" = ug.subd
)

country_names <- c("AO" = "angola", "BF" = "burkina Faso", "BJ" = "benin", "BU" = "burundi", "CD" = "drc", "CI" = "cote d'ivoire",
                   "CM" = "cameroon", "GH" = "ghana", "GN" = "guinea", "MD" = "madagascar", "ML" = "mali", "MZ" = "mozambique", "NG" = "nigeria", "TG" = "togo", "UG" = "uganda")

# rename region names column to NAME_1 for Madagascar to match the other countries, remove province name variable (provinces were dissolved in 2009)
subdivision_files[["MD"]] <- subdivision_files[["MD"]] %>%
  select(-NAME_1) %>%       # remove NAME_1 column (province names)
  rename(NAME_1 = NAME_2)   # rename NAME_2 to NAME_1 (region names)

# rename region name column for Uganda to match the other countries (different data source for Uganda so different var name)
subdivision_files[["UG"]] <- subdivision_files[["UG"]] %>%
  rename(NAME_1 = ADM1_EN)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Extract Population Data
## If you do not have Git LFS (large file permission) installed, please skip over lines 1207-1275 and lines 1382-1387 (manual extraction
## of most populous administrative subdivisions using large .tif population raster files) and use the hard-coded list in lines 1391-1407.
## -----------------------------------------------------------------------------------------------------------------------------------------

# create an empty list to store population counts per subdivision
# pop_counts <- list()
# 
# for (code in names(subdivision_files)) {
#   
#   # get country name and corresponding raster
#   country <- country_names[[code]]
#   subd <- subdivision_files[[code]]
#   raster_var <- get(paste0(gsub(" ", "_", tolower(country)), "_raster"))
#   
#   # ensure CRS matches
#   if (!st_crs(subd) == crs(raster_var)) {
#     subd <- st_transform(subd, crs(raster_var))
#   }
#   
#   # extract total population for each subdivision
#   pop_data <- exact_extract(raster_var, subd, fun = "sum", progress = FALSE)
#   
#   # combine with subdivision names
#   subd$pop_total <- pop_data
#   
#   # store in the list
#   pop_counts[[code]] <- subd
# }
# 
# # convert list to a single dataframe if needed
# pop_counts_df <- bind_rows(pop_counts, .id = "Country_Code")
# 
# pop_counts_final <- pop_counts_df %>%
#   select(Country_Code, COUNTRY, NAME_1, ENGTYPE_1, pop_total)
# 
# pop_top_3 <- pop_counts_final %>%
#   group_by(COUNTRY) %>%
#   top_n(3, pop_total) %>%
#   ungroup()
# 
# # fill in uganda name, set types
# pop_top_3 <- pop_top_3 %>%
#   mutate(COUNTRY = case_when(
#     Country_Code == "UG" ~ "Uganda",
#     TRUE ~ COUNTRY
#   )) %>%
#   mutate(ENGTYPE_1 = case_when(
#     Country_Code %in% c("UG", "MD") ~ "Region",
#     TRUE ~ ENGTYPE_1
#   )) %>%
#   mutate(ENGTYPE_1 = case_when(
#     Country_Code %in% c("MZ") ~ "Province",
#     TRUE ~ ENGTYPE_1
#   )) %>%
#   mutate(ENGTYPE_1 = case_when(
#     NAME_1 == "Abidjan" ~ "District",
#     TRUE ~ ENGTYPE_1
#   ))
# 
# # make word doc table with this data to put in supplement
# pop_top_3_df <- pop_top_3 %>% 
#   st_drop_geometry() %>%
#   mutate(pop_total = round(pop_total)) %>% # round population to nearest whole number
#   select(-Country_Code) %>%
#   rename("Country" = COUNTRY,
#          "Subdivision Name" = NAME_1,
#          "Subdivision Type" = ENGTYPE_1,
#          "Population Estimate" = pop_total)
# 
# doc <- read_docx()
# doc <- doc %>%
#   body_add_table(value = pop_top_3_df, style = "table_template")
# print(doc, target = file.path("outputs/pop_top_3_df.docx"))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Data Prep: Read in Country Shapefile
## -----------------------------------------------------------------------------------------------------------------------------------------

# read the shapefile for Africa country boundaries
afr.shp.base <- st_read(file.path("data/afr_g2014_2013_0.shp"))

# filter shapefile to only include countries of interest and rename Cote d'Ivoire to match other dfs (for merging)
# recode MG to MD (madagascar) and BI to BU (burundi) as these are the country codes we use
afr.shp.base <- afr.shp.base %>%
  mutate(ADM0_NAME = ifelse(ADM0_NAME == "Côte d'Ivoire", "Cote d'Ivoire", ADM0_NAME)) %>%
  mutate(ISO2 = ifelse(ISO2 == "MG", "MD", ISO2)) %>%
  mutate(ISO2 = ifelse(ISO2 == "BI", "BU", ISO2)) %>%
  filter(ADM0_NAME %in% c("Angola", "Burkina Faso", "Benin", "Burundi", "Democratic Republic of the Congo", "Cote d'Ivoire", 
                          "Cameroon", "Ghana", "Guinea", "Madagascar", "Mali", "Mozambique", "Nigeria", "Togo", "Uganda"))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Read in Survey Data and Merge with GPS Data
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in urban survey data
urban_df <- read_csv(file.path("data/250605_urban_df_for_analysis.csv"))

urban_df <- urban_df %>%
  mutate(home_type2 = ifelse(home_type2 == "A", "Agricultural", "Non-Agricultural"))

# replace space-hyphen-space with en dash and no spaces (preferred in academic writing)
urban_df$year_combo <- gsub(" - ", "–", urban_df$year_combo)

# read in all shapefiles
shapefiles <- list()
country_codes <- c(
  "AOGE71FL" = "AO",  # Angola
  "BFGE81FL" = "BF",  # Burkina Faso
  "BJGE71FL" = "BJ",  # Benin
  "BUGE71FL" = "BU",  # Burundi
  "CDGE61FL" = "CD",  # Democratic Republic of Congo
  "CIGE81FL" = "CI",  # Côte d'Ivoire
  "CMGE71FL" = "CM",  # Cameroon
  "GHGE8AFL" = "GH",  # Ghana
  "GNGE71FL" = "GN",  # Guinea
  "MDGE81FL" = "MD",  # Madagascar
  "MLGE7AFL" = "ML",  # Mali
  "MZGE81FL" = "MZ",  # Mozambique
  "NGGE7BFL" = "NG",  # Nigeria
  "TGGE62FL" = "TG",  # Togo
  "UGGE7AFL" = "UG"   # Uganda
)

# loop through each country code to read in shapefiles
for (code in names(country_codes)) {
  shapefile_path <- file.path(gps_folder_path, code, paste0(code, ".shp"))
  shapefiles[[country_codes[code]]] <- st_read(shapefile_path)
}

# initialize a list to store merged datasets for each country
gps_survey_data <- list()

# loop through each shapefile and merge with the survey data
for (country_code in names(shapefiles)) {
  # extract the shapefile for the current country
  gps_data <- shapefiles[[country_code]]
  
  # filter the survey data for the corresponding country
  country_survey_data <- urban_df[urban_df$DHS_CountryCode == toupper(country_code), ]
  
  # ensure that the types of DHSCLUST and hv001 are the same
  gps_data$DHSCLUST <- as.character(gps_data$DHSCLUST)
  country_survey_data$hv001 <- as.character(country_survey_data$hv001)
  
  # merge the shapefile with the filtered survey data
  merged_data <- merge(country_survey_data, gps_data, by.x = "hv001", by.y = "DHSCLUST", all.x = TRUE)
  
  # store the merged dataset in the list
  gps_survey_data[[country_code]] <- merged_data
  
  # select only the needed columns for simplicity
  # rename hv001 to "cluster"
  # create a variable for proportion of agricultural households within each cluster
  gps_survey_data[[country_code]] <- gps_survey_data[[country_code]] %>%
    select(hv001, dhs_year, year_combo, strat, wt, test_result, DHS_CountryCode, CountryName, home_type2, LATNUM, LONGNUM, geometry) %>%
    rename(cluster = hv001) %>%
    group_by(cluster) %>%
    mutate(agric_proportion = sum(home_type2 == "Agricultural") / n()) %>%
    ungroup()
}

## -----------------------------------------------------------------------------------------------------------------------------------------
### Read in Subdivision Shapefiles (e.g. State, Region, etc) - First-Level Administrative Subdivision in Each Country
## -----------------------------------------------------------------------------------------------------------------------------------------

country_names <- c("AO" = "Angola", "BF" = "Burkina Faso", "BJ" = "Benin", "BU" = "Burundi", "CD" = "Congo Democratic Republic", "CI" = "Côte d'Ivoire",
                   "CM" = "Cameroon", "GH" = "Ghana", "GN" = "Guinea", "MD" = "Madagascar", "ML" = "Mali", "MZ" = "Mozambique", "NG" = "Nigeria", "TG" = "Togo", "UG" = "Uganda")

## =========================================================================================================================================
### Create Maps Showing Agric HH Proportion per Subdivision (add Cluster points)
## =========================================================================================================================================

agric_palette <- c("#FAAD33", "#FF6C6B", "#E95988", "#8D58AA", "#4B59A7")

# define the top 3 populous subdivisions for each country
# all data from population raster extractions (overlaid with subdivision shapefiles)
# top_populous_subdivisions <- pop_top_3_df %>%
#   group_by(Country) %>%
#   arrange(desc(`Population Estimate`), .by_group = TRUE) %>%
#   slice_head(n = 3) %>%
#   summarise(Top_Subdivisions = list(`Subdivision Name`)) %>%
#   deframe()

# hard code the three most populous administrative subdivisions (commented out code above determines these):
top_populous_subdivisions <- list(
  "Angola" = c("Luanda", "Huíla", "Benguela"),
  "Burkina Faso" = c("Centre", "Haut-Bassins", "Est"),
  "Benin" = c("Atlantique", "Borgou", "Ouémé"),
  "Burundi" = c("Gitega", "Kirundo", "Muyinga"),
  "Congo Democratic Republic" = c("Haut-Katanga", "Nord-Kivu", "Kinshasa"),
  "Côte d'Ivoire" = c("Abidjan", "Montagnes", "Sassandra-Marahoué"),
  "Cameroon" = c("Centre", "Extrême-Nord", "Littoral"),
  "Ghana" = c("Greater Accra", "Ashanti", "Eastern"),
  "Guinea" = c("Kankan", "Conakry", "Nzérékoré"),
  "Madagascar" = c("Analamanga", "Vakinankaratra", "Vatovavy Fitovinany"),
  "Mali" = c("Sikasso", "Koulikoro", "Ségou"),
  "Mozambique" = c("Nampula", "Tete", "Zambezia"),
  "Nigeria" = c("Kano", "Lagos", "Kaduna"),
  "Togo" = c("Maritime", "Plateaux", "Savanes"),
  "Uganda" = c("Central", "Eastern", "Western")
)

interval_labels <- c("[0, 0.2]", "(0.2, 0.4]", "(0.4, 0.6]", "(0.6, 0.8]", "(0.8, 1]") # labels to go in legend

for (country_code in names(gps_survey_data)) { 
  # extract the specific country from the shapefile data
  country_shape <- afr.shp.base %>% filter(ISO2 == country_code)
  
  # extract the survey and gps cluster data for the current country
  country_data <- gps_survey_data[[country_code]]
  
  # extract subdivision data for the current country and make it a spatial object
  spat_country_subd <- st_as_sf(subdivision_files[[country_code]]) %>% st_make_valid()
  
  # extract survey data for the current country and make it a spatial object
  spat_gps_survey_data <- st_as_sf(country_data, coords = c("LONGNUM", "LATNUM"),
                                   crs = st_crs(spat_country_subd)) %>% st_make_valid()

  # perform spatial join to find which geographic subdivision each cluster point is in
  gps_survey_data_with_subdivision <- st_join(spat_gps_survey_data, 
                                              spat_country_subd %>% select(NAME_1), 
                                              join = st_within)

  # calculate agric hh proportion within each geographic subdivision
  subd_averages <- gps_survey_data_with_subdivision %>%
    st_drop_geometry() %>%
    group_by(NAME_1) %>%
    summarize(subd_proportion = mean(agric_proportion, na.rm = TRUE), .groups = "drop")
  
  # join subd_proportion back into the spatial GPS data using a normal (non-spatial) join
  gps_survey_data_with_subdivision <- gps_survey_data_with_subdivision %>%
    left_join(subd_averages, by = "NAME_1")
  
  # clean up any invalid geometries in the joined data
  gps_survey_data_with_subdivision <- gps_survey_data_with_subdivision %>% st_make_valid()
  
  # get the bounding box of the country to help scale the plots uniformly
  bbox <- st_bbox(country_shape)
  
  # define a consistent zoom level for all countries
  buffer <- 0.1
  
  # adjust the bounding box to create uniform scaling (box around each country)
  xlim_range <- c(bbox["xmin"] - buffer, bbox["xmax"] + buffer)
  ylim_range <- c(bbox["ymin"] - buffer, bbox["ymax"] + buffer)
  
  # prepare the subd_proportion data
  subd_prop_data <- gps_survey_data_with_subdivision %>% 
    st_drop_geometry() %>%
    select(NAME_1, subd_proportion) %>% 
    distinct()
  
  # join this to the spatial subdivision data
  spat_country_subd_with_prop <- spat_country_subd %>%
    left_join(subd_prop_data, by = c("NAME_1"))
  
  # remove duplicate entries in spat_country_subd_with_prop
  spat_country_subd_with_prop <- spat_country_subd_with_prop %>%
    group_by(NAME_1) %>%
    filter(!(is.na(subd_proportion) & n() > 1)) %>%
    ungroup()
  
  # add a new column to flag the top 3 most populous subdivisions
  country_name <- country_names[country_code]
  top_subs <- top_populous_subdivisions[[country_name]]
  spat_country_subd_with_prop <- spat_country_subd_with_prop %>%
    mutate(is_top_populous = ifelse(NAME_1 %in% top_subs, TRUE, FALSE))
  
  # define breaks for 20% intervals (0, 0.2, 0.4, 0.6, 0.8, 1)
  even_cuts <- seq(0, 1, by = 0.2)
  
  # categorize subd_proportion by these even 20% intervals
  spat_country_subd_with_prop$prop_cat <- cut(
    spat_country_subd_with_prop$subd_proportion,
    breaks = even_cuts,
    labels = c(1, 2, 3, 4, 5),
    include.lowest = TRUE
  )
  
  # apply the same categorization for agricultural household proportions in country_data
  country_data$prop_cat <- cut(
    country_data$agric_proportion,
    breaks = even_cuts,
    labels = c(1, 2, 3, 4, 5),
    include.lowest = TRUE
  )
  
  # create the plot
  country_agric_map <- ggplot() +
    
    # color subdivisions by proportion using the agric_palette
    geom_sf(data = spat_country_subd_with_prop, aes(fill = prop_cat, geometry = geometry), color = "white") +
    
    # apply agric_palette to the fill scale
    scale_fill_manual(values = agric_palette2, labels = interval_labels, na.value = "gray") +
    
    # cluster data points without jitter
    geom_point(data = country_data, aes(x = LONGNUM, y = LATNUM, fill = prop_cat),
               size = 1.5, shape = 21, stroke = 0.2, show.legend = FALSE) +
    scale_color_gradientn(colors = agric_palette2) +
    
    # add thick black outlines for top 3 most populous subdivisions in each country
    geom_sf(data = filter(spat_country_subd_with_prop, is_top_populous == TRUE), 
            aes(geometry = geometry), 
            fill = NA, 
            color = "black", 
            linewidth = 1,
            inherit.aes = FALSE) +
    
    geom_sf(data = country_shape, aes(geometry = geometry), fill = NA, color = "black") +  # country borders
    
    # set consistent zoom level so all countries appear the same size
    coord_sf(xlim = xlim_range, ylim = ylim_range, datum = NA) +
    theme_void() +
    
    # customize labels and title
    labs(title = paste(country_names[country_code], country_data$year_combo),
         fill = "Proportion of \nAgricultural \nHouseholds") +
    
    # no axes or labels
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
  
  # assign the map plot to a variable
  assign(paste0(country_code, "_subd_plot"), country_agric_map)
}


## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine Country Plots into Grid
## -----------------------------------------------------------------------------------------------------------------------------------------

# extract the combined malaria/net use legend
prop_legend <- get_only_legend(BU_subd_plot) 

# remove legends from all plots
AO_subd_plot <- AO_subd_plot + theme(legend.position = "none")
BF_subd_plot <- BF_subd_plot + theme(legend.position = "none")
BJ_subd_plot <- BJ_subd_plot + theme(legend.position = "none")
BU_subd_plot <- BU_subd_plot + theme(legend.position = "none")
CD_subd_plot <- CD_subd_plot + theme(legend.position = "none")
CI_subd_plot <- CI_subd_plot + theme(legend.position = "none")
CM_subd_plot <- CM_subd_plot + theme(legend.position = "none")
GH_subd_plot <- GH_subd_plot + theme(legend.position = "none")
GN_subd_plot <- GN_subd_plot + theme(legend.position = "none")
MD_subd_plot <- MD_subd_plot + theme(legend.position = "none")
ML_subd_plot <- ML_subd_plot + theme(legend.position = "none")
MZ_subd_plot <- MZ_subd_plot + theme(legend.position = "none")
NG_subd_plot <- NG_subd_plot + theme(legend.position = "none")
TG_subd_plot <- TG_subd_plot + theme(legend.position = "none")
UG_subd_plot <- UG_subd_plot + theme(legend.position = "none")

agric_subd_maps <- grid.arrange(
  AO_subd_plot, BF_subd_plot, BJ_subd_plot, BU_subd_plot, CD_subd_plot, 
  CI_subd_plot, CM_subd_plot, GH_subd_plot, GN_subd_plot, MD_subd_plot, 
  ML_subd_plot, MZ_subd_plot, NG_subd_plot, TG_subd_plot, UG_subd_plot, 
  nrow = 5, ncol = 3
)

# combine the plots and legend
agric_final_subd_maps <- grid.arrange(
  agric_subd_maps,
  prop_legend,
  ncol = 2,
  widths = c(10, 2),
  top = textGrob(
    "Proportion of Agricultural Households per Subdivision (Only Urban Data)",
    gp = gpar(fontsize = 16, fontface = "bold", hjust = 0.5)
  )
)

# save as .pdf
ggsave(("outputs/agric_maps.pdf"), agric_final_subd_maps, width = 10, height = 15)  
