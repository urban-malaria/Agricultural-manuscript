# ==========================================================================================================================================
# Author: Grace Legris (gracebea@gmail.com)
# Edited: [2025-06-20]
# Purpose: Generate Malaria TPR and Net Use Rate Line Plots
# ==========================================================================================================================================

source(file.path("scripts/helpers.R"))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Malaria Data Prep
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in the trend analysis dataset for malaria test positivity
malaria_trend_data_ci <- read_csv(file.path("data/251106_df_with_ci_for_analysis_trend_malaria_grace_created.csv"))

# select only urban data
urban_trend_malaria_ci <- malaria_trend_data_ci  %>%  
  filter(title == "Urban")

# select only rural data
rural_trend_malaria_ci <- malaria_trend_data_ci %>%  
  filter(title == "Rural")

## -----------------------------------------------------------------------------------------------------------------------------------------
### Net Use Data Prep
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in the trend analysis dataset for net use rates
net_trend_data_ci <- read_csv(file.path("data/251106_df_with_ci_for_analysis_trend_net_grace_created.csv"))

# select only urban data
urban_trend_net_ci <- net_trend_data_ci %>%  
  filter(title == "Urban")

# select only rural data
rural_trend_net_ci <- net_trend_data_ci %>%  
  filter(title == "Rural")

## =========================================================================================================================================
### LINE PLOTS (Survey Year vs. Malaria TPR/Net Use)
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Data Prep for Line Plots
# 1) Create long-format urban malaria dataframe
# 2) Create long-format rural malaria dataframe
# 3) Create long-format urban net use dataframe
# 4) Create long-format rural net use dataframe
# 5) Read in + prep net distribution dataframe
## -----------------------------------------------------------------------------------------------------------------------------------------

# ***** 1) LONG FORMAT URBAN MALARIA DF *****
# create new variables with just country name and just survey year (first year in survey range if survey spans two years)
urban_trend_malaria_ci <- urban_trend_malaria_ci %>%
  mutate(
    # extract the country name (everything before the first numeric character)
    country = str_extract(country_year, "^[^0-9]+") %>% str_trim(),
    
    # extract the year (the first numeric part found in the string)
    year = str_extract(country_year, "\\d{4}") %>% as.numeric()
  )

# reshape the data to long format (necessary to plot both agric_percent and non_agric_percent)
urban_trend_malaria_ci_long <- urban_trend_malaria_ci %>%
  pivot_longer(
    cols = c(agric_percent, non_agric_percent),
    names_to = "household_type",
    values_to = "percent"
  )

# mali agric. lower CI is negative so cap the lower CI bound at 0 and upper at 100
urban_trend_malaria_ci_long <- urban_trend_malaria_ci_long %>%
  mutate(
    agric_lower_ci = ifelse(agric_lower_ci < 0, 0, agric_lower_ci),
    agric_upper_ci = ifelse(agric_upper_ci > 100, 100, agric_upper_ci),
    non_agric_lower_ci = ifelse(non_agric_lower_ci < 0, 0, non_agric_lower_ci),
    non_agric_upper_ci = ifelse(non_agric_upper_ci > 100, 100, non_agric_upper_ci)
  )

# ***** 2) LONG FORMAT RURAL MALARIA DF *****
# create new variable with just country name and just survey year (first year in survey range if survey spans two years)
rural_trend_malaria_ci <- rural_trend_malaria_ci %>%
  mutate(
    # extract the country name (everything before the first numeric character)
    country = str_extract(country_year, "^[^0-9]+") %>% str_trim(),
    
    # extract the year (the first numeric part found in the string)
    year = str_extract(country_year, "\\d{4}") %>% as.numeric()
  )

# reshape the data to long format (necessary to plot both agric_percent and non_agric_percent)
rural_trend_malaria_ci_long <- rural_trend_malaria_ci %>%
  pivot_longer(
    cols = c(agric_percent, non_agric_percent),
    names_to = "household_type",
    values_to = "percent"
  )

# cap the lower CI bound at 0 and upper at 100
rural_trend_malaria_ci_long <- rural_trend_malaria_ci_long %>%
  mutate(
    agric_lower_ci = ifelse(agric_lower_ci < 0, 0, agric_lower_ci),
    agric_upper_ci = ifelse(agric_upper_ci > 100, 100, agric_upper_ci),
    non_agric_lower_ci = ifelse(non_agric_lower_ci < 0, 0, non_agric_lower_ci),
    non_agric_upper_ci = ifelse(non_agric_upper_ci > 100, 100, non_agric_upper_ci)
  )

# ***** 3) LONG FORMAT URBAN NET USE DF *****
# create new variable with just country name and just survey year (first year in survey range if survey spans two years)
urban_trend_net_ci <- urban_trend_net_ci %>%
  mutate(
    # extract the country name (everything before the first numeric character)
    country = str_extract(country_year, "^[^0-9]+") %>% str_trim(),
    
    # extract the year (the first numeric part found in the string)
    year = str_extract(country_year, "\\d{4}") %>% as.numeric()
  )

# reshape the data to long format (necessary to plot both agric_percent and non_agric_percent)
urban_trend_net_ci_long <- urban_trend_net_ci %>%
  pivot_longer(
    cols = c(agric_percent, non_agric_percent),
    names_to = "household_type",
    values_to = "percent"
  )

# ***** 4) LONG FORMAT RURAL NET USE DF *****
# create new variable with just country name and just survey year (first year in survey range if survey spans two years)
rural_trend_net_ci <- rural_trend_net_ci %>%
  mutate(
    # extract the country name (everything before the first numeric character)
    country = str_extract(country_year, "^[^0-9]+") %>% str_trim(),
    
    # extract the year (the first numeric part found in the string)
    year = str_extract(country_year, "\\d{4}") %>% as.numeric()
  )

# reshape the data to long format (necessary to plot both agric_percent and non_agric_percent)
rural_trend_net_ci_long <- rural_trend_net_ci %>%
  pivot_longer(
    cols = c(agric_percent, non_agric_percent),
    names_to = "household_type",
    values_to = "percent"
  )

# ***** 5) NET DISTRIBUTION DF *****
net_distribution_df <- read_csv(file.path("data/220315_ITN_distribution_data.csv"))

# select only 7 countries of interest and calculate year-to-year percent change in nets distributed (will add this to line plots)
net_distribution_df <- net_distribution_df %>%
  rename(total_net_distributed = `total net distributed`) %>%
  group_by(Country) %>%
  arrange(Country, Year) %>%
  mutate(
    # set first_year_nets to the second year's distribution for Côte d'Ivoire and Benin,
    # and to the first year's distribution for other countries
    first_year_nets = if_else(Country %in% c("Côte d'Ivoire", "Benin"),
                              nth(total_net_distributed, 2, default = first(total_net_distributed)),
                              first(total_net_distributed)),
    pct_change_nets = (total_net_distributed - first_year_nets) / first_year_nets * 100  # calculate percentage change from the selected year
  ) %>%
  filter(Country %in% c("Benin", "Burkina Faso", "Cameroon", "Côte d'Ivoire", "Mali", "Mozambique", "Ghana")) %>%
  ungroup()

# replace "inf" value with NA (Cote d'Ivoire went from 0 nets to 18509750 in 2020 -> 2021) and change spelling of CI to match other dfs
net_distribution_df <- net_distribution_df %>%
  mutate(
    Country = ifelse(Country == "Côte d'Ivoire", "Cote d'Ivoire", Country),
    pct_change_nets = ifelse(is.infinite(pct_change_nets), NA, pct_change_nets)
  )

## -----------------------------------------------------------------------------------------------------------------------------------------
### Generate a line plot for Malaria Test Positivity Rate (TPR) alone
# •	X-axis: Year of the survey.
# •	Y-axis: Malaria test positivity rate (TPR).
# •	Color the lines by country and include both agric and non-agric HHs for each survey.
## -----------------------------------------------------------------------------------------------------------------------------------------

color_list <- c("#154D42", "#6f3096", "red", "#028E41", "#ff8da1", "#4777cd", "#ab0a58", "#fa7a48")

# urban malaria line plot
urban_malaria_tpr_plot <- ggplot(urban_trend_malaria_ci_long, aes(x = year, y = percent, color = country, linetype = household_type)) +
  geom_line(size = 1) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(x = "Survey Year",
       y = "Malaria Test Positivity Rate (%)",
       color = "Country",
       title = "Malaria Test Positivity Rate by\n Year and Household Type: Urban") +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14)) +
  scale_color_manual(values = color_list) +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Agricultural  ", "Non-Agricultural   ")) +
  guides(linetype = guide_legend(title = "Household Type")) +
  scale_x_continuous(breaks = seq(min(urban_trend_malaria_ci_long$year), max(urban_trend_malaria_ci_long$year), by = 2))

# rural malaria line plot
rural_malaria_tpr_plot <- ggplot(rural_trend_malaria_ci_long, aes(x = year, y = percent, color = country, linetype = household_type)) +
  geom_line(size = 1) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(x = "Survey Year",
       y = "Malaria Test Positivity Rate (%)",
       color = "Country",
       title = "Malaria Test Positivity Rate (TPR) by\n Year and Household Type: Rural") +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14)) +
  scale_color_manual(values = color_list) +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Agricultural", "Non-Agricultural")) +
  guides(linetype = guide_legend(title = "Household Type")) +
  scale_x_continuous(breaks = seq(min(urban_trend_malaria_ci_long$year), max(urban_trend_malaria_ci_long$year), by = 2))

#### COMBINE THE PLOTS

# remove individual titles and x-axis labels from the urban and rural plots
urban_malaria_tpr_plot <- urban_malaria_tpr_plot + 
  labs(title = NULL, subtitle = "Urban", x = NULL, y = NULL) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12)) 
rural_malaria_tpr_plot <- rural_malaria_tpr_plot + 
  labs(title = NULL, subtitle = "Rural", y = NULL) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12)) 

legend <- get_only_legend(urban_malaria_tpr_plot) 

# remove legends from both plots
urban_malaria_tpr_plot <- urban_malaria_tpr_plot + theme(legend.position = "none")
rural_malaria_tpr_plot <- rural_malaria_tpr_plot + theme(legend.position = "none") 

malaria_combined_line_plot <- grid.arrange(urban_malaria_tpr_plot, rural_malaria_tpr_plot)

# arrange the combined plot and legend side by side
malaria_final_line_plots <- grid.arrange(
  malaria_combined_line_plot,
  legend,
  nrow = 1,
  ncol = 2,
  heights = c(5),
  widths = c(8, 4),
  top = textGrob("Malaria Test Positivity Rate by Survey Year and Household Type",
                 gp = gpar(fontsize = 12, fontface = "bold", hjust = 0.5)),  # Adjusted hjust for centering
  left = textGrob("Malaria Test Positivity Rate (%)",
                  rot = 90,
                  gp = gpar(fontsize = 12))
)

# save as .pdf
ggsave(file.path("outputs/malaria_line_plot_combined.png"), malaria_final_line_plots, width = 10, height = 10) 

## =========================================================================================================================================
### URBAN LINE PLOTS: a) Malaria/net use combined, b) Malaria only, c) Net Use only (FACET BY COUNTRY)
## =========================================================================================================================================

# get a list of unique countries
countries <- unique(urban_trend_malaria_ci_long$country)

# define an empty list to store the plots
country_plots <- list()

countries = c("Benin", "Burkina Faso", "Cameroon", "Cote d'Ivoire", "Mali", "Mozambique", "Ghana")
country_plots <- list()

# URBAN: loop through each country
for (country_name in countries) {
  
  # get malaria TPR data for the current country
  urban_country_data_malaria_ci <- urban_trend_malaria_ci_long %>%
    filter(country == country_name) %>%
    mutate(data_type = "Malaria TPR")
  
  # get net use data for the current country
  urban_country_data_net_ci <- urban_trend_net_ci_long %>%
    filter(country == country_name) %>%
    mutate(data_type = "Net Use Rate")
  
  # combine the two datasets
  urban_combined_data <- bind_rows(urban_country_data_malaria_ci, urban_country_data_net_ci)
  
  # create plot a: combined malaria tpr and net use
  urban_main_plot <- ggplot(urban_combined_data, aes(x = year, y = percent, linetype = data_type, color = household_type)) +
    geom_line(size = 1) +
    geom_point(size = 3, alpha = 0.7) +
    # confidence intervals for malaria tpr and net use rate (agricultural and non-agricultural)
    geom_errorbar(data = urban_combined_data %>% filter(data_type == "Malaria TPR"),
                  aes(ymin = agric_lower_ci, ymax = agric_upper_ci),
                  width = 0.3, color = "#5560AB", alpha = 0.7) +
    geom_errorbar(data = urban_combined_data %>% filter(data_type == "Malaria TPR"),
                  aes(ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
                  width = 0.3, color = "#FAAF43", alpha = 0.7) +
    geom_errorbar(data = urban_combined_data %>% filter(data_type == "Net Use Rate"),
                  aes(ymin = agric_lower_ci, ymax = agric_upper_ci),
                  width = 0.3, color = "#5560AB", alpha = 0.7) +
    geom_errorbar(data = urban_combined_data %>% filter(data_type == "Net Use Rate"),
                  aes(ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
                  width = 0.3, color = "#FAAF43", alpha = 0.7) +
    theme_manuscript() +
    facet_wrap(~ country, labeller = label_value) + 
    labs(
      x = "Survey Year", y = "Percent (%)", title = paste(country_name), color = "Household Type", linetype = "Data Type"
    ) +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14)
    ) +
    scale_color_manual(values = c("agric_percent" = "#5560AB", "non_agric_percent" = "#FAAF43"),
                       labels = c("Agricultural", "Non-Agricultural")) +
    scale_linetype_manual(values = c("Malaria TPR" = "solid", "Net Use Rate" = "dashed")) +
    guides(linetype = guide_legend(override.aes = list(color = "black"))) +
    scale_x_continuous(limits = c(2009.5, 2022.5), breaks = seq(2010, 2022, by = 3)) +
    scale_y_continuous(limits = c(0, 100))
  
  # save the combined plot with the country name
  assign(paste0(country_name, "_urban_main_plot"), urban_main_plot)
  
  # get malaria TPR and net use data for the current country
  urban_country_data_malaria_ci <- urban_trend_malaria_ci_long %>%
    filter(country == country_name) %>%
    mutate(data_type = "Malaria TPR")
  
  urban_country_data_net_ci <- urban_trend_net_ci_long %>%
    filter(country == country_name) %>%
    mutate(data_type = "Net Use Rate")
  
  # combine malaria and net use data
  urban_combined_data <- bind_rows(urban_country_data_malaria_ci, urban_country_data_net_ci)
  
  # get net distribution data and percentage change for the current country (compared to first year nets were distributed)
  net_data_country <- net_distribution_df %>%
    filter(Country == country_name) %>%
    select(Year, total_net_distributed, pct_change_nets)
  
  # create a new data frame for net distribution with a legend entry
  net_data_country_legend <- net_data_country %>%
    mutate(data_type = "Percentage Change in Nets Distributed")
  
  # PLOT: TPR AND NET USE AND PERCENT CHANGE IN NETS DISTRIBUTED FROM PRIOR YEAR
  urban_comb_plot <- ggplot() +
    
    # area plot for percent change in net distribution
    geom_area(data = net_data_country_legend, 
              aes(x = Year, y = scales::rescale(pct_change_nets, to = c(0, 100))),
              fill = "lightgrey", alpha = 0.5, position = 'identity') +
    
    # line plot for malaria TPR and net use
    geom_line(data = urban_combined_data, 
              aes(x = year, y = percent, linetype = data_type, color = household_type), size = 1) +
    
    # points for malaria TPR and net use
    geom_point(data = urban_combined_data, 
               aes(x = year, y = percent, color = household_type), size = 3, alpha = 0.7) +
    
    # add confidence intervals for malaria TPR and net use rate (agricultural and non-agricultural)
    geom_errorbar(data = urban_combined_data %>% filter(data_type == "Malaria TPR", household_type == "agric_percent"),
                  aes(x = year, ymin = agric_lower_ci, ymax = agric_upper_ci),
                  width = 0.3, color = "#5560AB", alpha = 0.7) +
    geom_errorbar(data = urban_combined_data %>% filter(data_type == "Malaria TPR", household_type == "non_agric_percent"),
                  aes(x = year, ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
                  width = 0.3, color = "#FAAF43", alpha = 0.7) +
    geom_errorbar(data = urban_combined_data %>% filter(data_type == "Net Use Rate", household_type == "agric_percent"),
                  aes(x = year, ymin = agric_lower_ci, ymax = agric_upper_ci),
                  width = 0.3, color = "#5560AB", alpha = 0.7) +
    geom_errorbar(data = urban_combined_data %>% filter(data_type == "Net Use Rate", household_type == "non_agric_percent"),
                  aes(x = year, ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
                  width = 0.3, color = "#FAAF43", alpha = 0.7) +
    
    # set up primary and secondary y-axes
    scale_y_continuous(
      name = "Malaria TPR/Net Use Rate (%)",
      limits = c(0, 100),
      sec.axis = sec_axis(
        transform = ~ scales::rescale(., from = c(0, 100), to = c(-110, 16000)),
        name = "Percentage Change in Nets Distributed (%)"
      )
    ) +
    
    # add theme, labels, and scales
    theme_manuscript() +
    facet_wrap(~ country, labeller = label_value) +
    labs(
      x = "Survey Year", y = "Percent (%)", color = "Household Type", linetype = "Data Type"
    ) +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14)
    ) +
    scale_color_manual(values = c("agric_percent" = "#5560AB", "non_agric_percent" = "#FAAF43"),
                       labels = c("Agricultural", "Non-Agricultural")) +
    scale_linetype_manual(values = c("Malaria TPR" = "solid", "Net Use Rate" = "dashed", "Year Percentage Change in Nets Distributed" = "dotted"), 
                          labels = c("Malaria TPR", "Net Use Rate", "Percentage Change\n in Nets Distributed"))
  
  # save the combined plot with the country name
  assign(paste0(country_name, "_urban_plot"), urban_comb_plot)
  
  # add the combined plot to the list
  country_plots[[country_name]] <- urban_comb_plot
}

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine Malaria + Net Use Urban Plots 
## -----------------------------------------------------------------------------------------------------------------------------------------

# extract the combined malaria/net use legend
legend <- get_only_legend(Benin_urban_main_plot) 

# remove legends from all plots
Benin_urban_main_plot <- Benin_urban_main_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
`Burkina Faso_urban_main_plot` <- `Burkina Faso_urban_main_plot` + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Cameroon_urban_main_plot <- Cameroon_urban_main_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
`Cote d'Ivoire_urban_main_plot` <- `Cote d'Ivoire_urban_main_plot` + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Mali_urban_main_plot <- Mali_urban_main_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Mozambique_urban_main_plot <- Mozambique_urban_main_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Ghana_urban_main_plot <- Ghana_urban_main_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())

urban_country_line_plots <- grid.arrange(Benin_urban_main_plot, `Burkina Faso_urban_main_plot`, Cameroon_urban_main_plot, `Cote d'Ivoire_urban_main_plot`, Mali_urban_main_plot, Mozambique_urban_main_plot, Ghana_urban_main_plot,
                                         nrow = 4, ncol = 2)

# combine the plots and legend
urban_country_final_line_plots <- grid.arrange(
  urban_country_line_plots,
  legend,
  ncol = 2,  # legend in the second column, plots in the first column
  widths = c(10, 2),  # adjust the width to give more space to the plots
  top = textGrob(
    "Urban",
    gp = gpar(fontsize = 16, fontface = "bold", hjust = 0.5)  # center the title
  ),
  bottom = textGrob(
    "Survey Year",
    gp = gpar(fontsize = 12, hjust = 0.5)  # center the title
  )
)

# save as .pdf
ggsave(file.path("outputs/urban_malaria_net_plots.pdf"), urban_country_final_line_plots, width = 10, height = 10) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine Malaria + Net Use + Percent Change in Net Distribution Urban Plots 
## -----------------------------------------------------------------------------------------------------------------------------------------

# extract the combined malaria/net use legend
legend <- get_only_legend(Benin_urban_plot) 

# remove legends from all plots
Benin_urban_plot <- Benin_urban_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
`Burkina Faso_urban_plot` <- `Burkina Faso_urban_plot` + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Cameroon_urban_plot <- Cameroon_urban_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
`Cote d'Ivoire_urban_plot` <- `Cote d'Ivoire_urban_plot` + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Mali_urban_plot <- Mali_urban_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Mozambique_urban_plot <- Mozambique_urban_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Ghana_urban_plot <- Ghana_urban_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())

urban_country_line_plots <- grid.arrange(Benin_urban_plot, `Burkina Faso_urban_plot`, Cameroon_urban_plot, `Cote d'Ivoire_urban_plot`, Mali_urban_plot, Mozambique_urban_plot, Ghana_urban_plot,
                                         nrow = 4, ncol = 2)

# combine the plots and legend
urban_country_final_line_plots <- grid.arrange(
  urban_country_line_plots,
  legend,
  ncol = 2,  # legend in the second column, plots in the first column
  widths = c(10, 2),  # adjust the width to give more space to the plots
  top = textGrob(
    "Urban",
    gp = gpar(fontsize = 16, fontface = "bold", hjust = 0.5)  # center the title
  ),
  bottom = textGrob(
    "Survey Year",
    gp = gpar(fontsize = 12, hjust = 0.5)  # center the title
  )
)

# save as .pdf
ggsave(file.path("outputs/urban_country_area_plots.pdf"), urban_country_final_line_plots, width = 10, height = 10) 


## =========================================================================================================================================
### RURAL LINE PLOTS: 1) Malaria/net use combined, 2) Malaria only, 3) Net Use only 
## =========================================================================================================================================

# RURAL: loop through each country
for (country_name in countries) {
  
  # get malaria TPR data for the current country
  rural_country_data_malaria_ci <- rural_trend_malaria_ci_long %>%
    filter(country == country_name) %>%
    mutate(data_type = "Malaria TPR")
  
  # get net use data for the current country
  rural_country_data_net_ci <- rural_trend_net_ci_long %>%
    filter(country == country_name) %>%
    mutate(data_type = "Net Use Rate")
  
  # combine the two datasets
  rural_combined_data <- bind_rows(rural_country_data_malaria_ci, rural_country_data_net_ci)
  
  # create a new data frame for net distribution with a legend entry
  net_data_country_num_legend <- net_data_country %>%
    mutate(data_type = "Nets Distributed")  # Add a column for the legend entry
  
  # create plot a: combined malaria tpr and net use ---------------------------------------------------------------------------------
  rural_comb_main_plot <- ggplot(rural_combined_data, aes(x = year, y = percent, linetype = data_type, color = household_type)) +
    geom_line(size = 1) +  # Line for each group
    geom_point(size = 3, alpha = 0.7) +  # Add points for each data point
    geom_errorbar(data = rural_combined_data %>% filter(data_type == "Malaria TPR"),
                  aes(ymin = agric_lower_ci, ymax = agric_upper_ci),
                  width = 0.3, color = "#5560AB", alpha = 0.7) +
    geom_errorbar(data = rural_combined_data %>% filter(data_type == "Malaria TPR"),
                  aes(ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
                  width = 0.3, color = "#FAAF43", alpha = 0.7) +
    geom_errorbar(data = rural_combined_data %>% filter(data_type == "Net Use Rate"),
                  aes(ymin = agric_lower_ci, ymax = agric_upper_ci),
                  width = 0.3, color = "#5560AB", alpha = 0.7) +
    geom_errorbar(data = rural_combined_data %>% filter(data_type == "Net Use Rate"),
                  aes(ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
                  width = 0.3, color = "#FAAF43", alpha = 0.7) + 
    theme_manuscript() +
    facet_wrap(~ country, labeller = label_value) + 
    labs(
      x = "Survey Year", y = "Percent (%)", title = paste(country_name), color = "Household Type", linetype = "Data Type" 
    ) +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14)
    ) +
    scale_color_manual(values = c("agric_percent" = "#5560AB", "non_agric_percent" = "#FAAF43"),
                       labels = c("Agricultural", "Non-Agricultural")) +
    scale_linetype_manual(values = c("Malaria TPR" = "solid", "Net Use Rate" = "dashed")) +
    guides(linetype = guide_legend(override.aes = list(color = "black"))) +
    scale_x_continuous(limits = c(2009.5, 2022.5), breaks = seq(2010, 2022, by = 3)) + 
    scale_y_continuous(limits = c(0, 100))
  
  # save the combined plot with the country name
  assign(paste0(country_name, "_rural_main_plot"), rural_comb_main_plot)
  
  # add the combined plot to the list
  country_plots[[country_name]] <- rural_comb_main_plot
  
  # ------------------------------------------------------------------------------------------------------------------------------
  # AREA PLOTS WITH PERCENT CHANGE IN NETS DISTRIBUTED (COMPARED TO FIRST YEAR)
  
  # PLOT: TPR AND NET USE AND PERCENT CHANGE IN NETS DISTRIBUTED FROM PRIOR YEAR
  rural_area_plot <- ggplot() +
    geom_area(data = net_data_country_legend, # area plot for percent change in net distribution
              aes(x = Year, y = scales::rescale(pct_change_nets, to = c(0, 100))),
              fill = "lightgrey", alpha = 0.5, position = 'identity') +
    geom_line(data = rural_combined_data, # line plot for malaria TPR and net use
              aes(x = year, y = percent, linetype = data_type, color = household_type), size = 1) +
    geom_point(data = rural_combined_data, # points for malaria TPR and net use
               aes(x = year, y = percent, color = household_type), size = 3, alpha = 0.7) +
    
    # add confidence intervals for malaria TPR and net use rate (agricultural and non-agricultural)
    geom_errorbar(data = rural_combined_data %>% filter(data_type == "Malaria TPR", household_type == "agric_percent"),
                  aes(x = year, ymin = agric_lower_ci, ymax = agric_upper_ci),
                  width = 0.3, color = "#5560AB", alpha = 0.7) +
    geom_errorbar(data = rural_combined_data %>% filter(data_type == "Malaria TPR", household_type == "non_agric_percent"),
                  aes(x = year, ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
                  width = 0.3, color = "#FAAF43", alpha = 0.7) +
    geom_errorbar(data = rural_combined_data %>% filter(data_type == "Net Use Rate", household_type == "agric_percent"),
                  aes(x = year, ymin = agric_lower_ci, ymax = agric_upper_ci),
                  width = 0.3, color = "#5560AB", alpha = 0.7) +
    geom_errorbar(data = rural_combined_data %>% filter(data_type == "Net Use Rate", household_type == "non_agric_percent"),
                  aes(x = year, ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
                  width = 0.3, color = "#FAAF43", alpha = 0.7) +
    
    # set up primary and secondary y-axes
    scale_y_continuous(
      name = "Malaria TPR/Net Use Rate (%)",
      limits = c(0, 100),
      sec.axis = sec_axis(
        transform = ~ scales::rescale(., from = c(0, 100), to = c(-110, 16000)),
        name = "Percentage Change in Nets Distributed (%)"
      )
    ) +
    
    # add theme, labels, and scales
    theme_manuscript() +
    facet_wrap(~ country, labeller = label_value) + 
    labs(
      x = "Survey Year", y = "Percent (%)", color = "Household Type", linetype = "Data Type"
    ) +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14)
    ) +
    scale_color_manual(values = c("agric_percent" = "#5560AB", "non_agric_percent" = "#FAAF43"),
                       labels = c("Agricultural", "Non-Agricultural")) +
    scale_linetype_manual(values = c("Malaria TPR" = "solid", "Net Use Rate" = "dashed", "Year Percentage Change in Nets Distributed" = "dotted"), 
                          labels = c("Malaria TPR", "Net Use Rate", "Percentage Change\n in Nets Distributed"))
  
  # save the combined plot with the country name
  assign(paste0(country_name, "_rural_area_plot"), rural_area_plot)
  
  # add the combined plot to the list
  country_plots[[country_name]] <- rural_area_plot
  
  # FACET: make plot b: malaria tpr by country -----------------------------------------------------------------------------------
  rural_malaria_plot <- ggplot(rural_country_data_malaria_ci, aes(x = year, y = percent, color = household_type)) +
    
    # add confidence interval error bars with color by household_type
    geom_errorbar(
      aes(ymin = agric_lower_ci, ymax = agric_upper_ci),
      data = subset(rural_country_data_malaria_ci, household_type == "agric_percent"),
      width = 0.2, color = "#FAAF43", alpha = 0.7
    ) +
    geom_errorbar(
      aes(ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
      data = subset(rural_country_data_malaria_ci, household_type == "non_agric_percent"),
      width = 0.2, color = "#5560AB", alpha = 0.7
    ) +
    geom_line(size = 1) +
    geom_point(size = 3, alpha = 0.7, show.legend = TRUE) +
    theme_manuscript() +
    labs(
      x = "Survey Year", y = "Percent (%)", title = paste(country_name), color = "Household Type"
    ) +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14)
    ) +
    scale_color_manual(
      values = c("agric_percent" = "#FAAF43", "non_agric_percent" = "#5560AB"),
      labels = c("Agricultural", "Non-Agricultural")
    ) +
    scale_x_continuous(limits = c(2010, 2022), breaks = seq(2010, 2022, by = 3)) +
    scale_y_continuous(limits = c(0, 100))
  
  # save the malaria plot with the country name
  assign(paste0(country_name, "_malaria_rural_plot"), rural_malaria_plot)
  
  # FACET: c) make plot for just net use by country -------------------------------------------------------------------------------------
  rural_net_plot <- ggplot(rural_country_data_net_ci, aes(x = year, y = percent, color = household_type)) +
    geom_errorbar( # add confidence interval error bars with color by household_type
      aes(ymin = agric_lower_ci, ymax = agric_upper_ci),
      data = subset(rural_country_data_net_ci, household_type == "agric_percent"),
      width = 0.2, color = "#FAAF43", alpha = 0.7
    ) +
    geom_errorbar(
      aes(ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
      data = subset(rural_country_data_net_ci, household_type == "non_agric_percent"),
      width = 0.2, color = "#5560AB", alpha = 0.7
    ) +
    # plot the lines and points with color by household_type
    geom_line(size = 1) +
    geom_point(size = 3, alpha = 0.7, show.legend = TRUE) +
    theme_manuscript() +
    labs(
      x = "Survey Year",
      y = "Percent (%)",
      title = paste(country_name),
      color = "Household Type"
    ) +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14)
    ) +
    scale_color_manual(
      values = c("agric_percent" = "#FAAF43", "non_agric_percent" = "#5560AB"),
      labels = c("Agricultural", "Non-Agricultural")
    ) +
    scale_x_continuous(limits = c(2010, 2022), breaks = seq(2010, 2022, by = 3)) +
    scale_y_continuous(limits = c(0, 100))
  
  # save the net plot with the country name
  assign(paste0(country_name, "_net_rural_plot"), rural_net_plot)
}

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine Malaria + Net Use Rural Plots 
## -----------------------------------------------------------------------------------------------------------------------------------------

# extract the combined malaria/net use legend
legend <- get_only_legend(Benin_rural_main_plot) 

# remove legends from all plots
Benin_rural_main_plot <- Benin_rural_main_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
`Burkina Faso_rural_main_plot` <- `Burkina Faso_rural_main_plot` + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Cameroon_rural_main_plot <- Cameroon_rural_main_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
`Cote d'Ivoire_rural_main_plot` <- `Cote d'Ivoire_rural_main_plot` + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Mali_rural_main_plot <- Mali_rural_main_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Mozambique_rural_main_plot <- Mozambique_rural_main_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Ghana_rural_main_plot <- Ghana_rural_main_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())

rural_country_line_plots <- grid.arrange(Benin_rural_main_plot, `Burkina Faso_rural_main_plot`, Cameroon_rural_main_plot, `Cote d'Ivoire_rural_main_plot`, Mali_rural_main_plot, Mozambique_rural_main_plot, Ghana_rural_main_plot,
                                         nrow = 4, ncol = 2)

# combine the plots and legend
rural_country_final_line_plots <- grid.arrange(
  rural_country_line_plots,
  legend,
  ncol = 2,  # legend in the second column, plots in the first column
  widths = c(10, 2),  # adjust the width to give more space to the plots
  top = textGrob(
    "Rural",
    gp = gpar(fontsize = 16, fontface = "bold", hjust = 0.5)  # center the title
  ),
  bottom = textGrob(
    "Survey Year",
    gp = gpar(fontsize = 12, hjust = 0.5)  # center the title
  )
)

# save as .pdf
ggsave(file.path("outputs/rural_malaria_net_plots.pdf"), rural_country_final_line_plots, width = 10, height = 10) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine Malaria + Net Use + Percent Change in Net Distribution Rural Plots 
## -----------------------------------------------------------------------------------------------------------------------------------------

# extract the combined malaria/net use legend
legend <- get_only_legend(Benin_rural_area_plot) 

# remove legends from all plots
Benin_rural_area_plot <- Benin_rural_area_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
`Burkina Faso_rural_area_plot` <- `Burkina Faso_rural_area_plot` + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Cameroon_rural_area_plot <- Cameroon_rural_area_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
`Cote d'Ivoire_rural_area_plot` <- `Cote d'Ivoire_rural_area_plot` + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Mali_rural_area_plot <- Mali_rural_area_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Mozambique_rural_area_plot <- Mozambique_rural_area_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Ghana_rural_area_plot <- Ghana_rural_area_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())

rural_country_line_plots <- grid.arrange(Benin_rural_area_plot, `Burkina Faso_rural_area_plot`, Cameroon_rural_area_plot, `Cote d'Ivoire_rural_area_plot`, Mali_rural_area_plot, Mozambique_rural_area_plot, Ghana_rural_area_plot,
                                         nrow = 4, ncol = 2)

# combine the plots and legend
rural_country_final_line_plots <- grid.arrange(
  rural_country_line_plots,
  legend,
  ncol = 2,  # legend in the second column, plots in the first column
  widths = c(10, 2),  # adjust the width to give more space to the plots
  top = textGrob(
    "Rural",
    gp = gpar(fontsize = 16, fontface = "bold", hjust = 0.5)  # center the title
  ),
  bottom = textGrob(
    "Survey Year",
    gp = gpar(fontsize = 12, hjust = 0.5)  # center the title
  )
)

# save as .pdf
ggsave(file.path("outputs/rural_country_area_plots.pdf"), rural_country_final_line_plots, width = 10, height = 10) 
