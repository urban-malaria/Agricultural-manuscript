# ==========================================================================================================================================
# Script Name: Modeling and Mediation Analysis
# Author: Dr. Ifeoma Ozodiegwu
# Edited by: Grace Legris, Research Data Analyst (gracebea@gmail.com)
# Edited: [2025-06-20]
# Purpose: Create Models that Explore the Relationship Between Home Type and Malaria Test Positivity
# ==========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Read in Analysis Datasets
## -----------------------------------------------------------------------------------------------------------------------------------------

# read data from (updated 6/12/25) CSV file into all_df and apply transformations
all_df <- read_csv(file.path(PopDir, "analysis_dat/251206_urban_rural_analysis_data_for_modeling.csv")) %>%  
  mutate(malaria_result = ifelse(test_result == "+ve", 1, 0),  # create binary malaria_result based on test_result
         EVI_2000m_new = case_when(is.na(EVI_2000m_new) ~ NA,  # handle missing values in EVI_2000m_new
                                   TRUE ~ EVI_2000m_new * 10),  # scale EVI_2000m_new by a factor of 10
         home_type_factor = ifelse(home_type2 == "A", "Z_Agric", "Non-agric"),  # categorize home type
         wealth_index = as.factor(wealth),  # convert wealth index to a factor
         dhs_year_factor = as.factor(dhs_year),  # convert DHS year to a factor
         sex = ifelse(hc27 == 2, "Female", "Male"),  # recode sex variable
         u5_net_use_factor = ifelse(u5_net_use == 1, "Z_Used", "not_used"),  # categorize net use among under-fives
         roof_type_factor = ifelse(roof_type == 1, "Z_Improved", "poor"),  # categorize roof type
         stunting = ifelse(hc70 < -300, "Stunted", ifelse(hc70 > 8000, NA, "Not stunted")))  # categorize stunting status

## -----------------------------------------------------------------------------------------------------------------------------------------
### Add Housing Quality Variable
## -----------------------------------------------------------------------------------------------------------------------------------------

#### 1) Housing Quality Indicator Variable

# factors considered:
# Improved Floor: categorized as having a finished floor (i.e., parquet or polished word, vinyl, ceramic tiles, cement or carpet)
# Improved External Wall: categorized as having a finished wall (cement, stone, bricks, covered adobe, or tile)
# Improved Roof: categorized as having a finished roof (metal, calamine(zinc)/cement fiber, ceramic tiles, cement, or decra)
# Modern House: Composite variable of having an improved floor, roof, and external walls

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
    # if any of these components is 0 (indicating lower quality in any area), then housing_quality is set to 0
    housing_quality = ifelse(floor_type == 1 & wall_type == 1 & roof_type == 1, 1, 0),
  )

## -----------------------------------------------------------------------------------------------------------------------------------------
### Prepare Data for Analysis
## -----------------------------------------------------------------------------------------------------------------------------------------

# define variables of interest for analysis
var <- list("stunting", "u5_net_use_factor", "hh_size", 
            "wealth_index", "EVI_2000m_new", "housing_quality",
            "med_treat_fever_none")

# define descriptive table names for corresponding variables
table_names <- c("Stunting: Stunted", "Home type: Non-agricultural",
                 "Net Use: Used", "Household Size", 
                 "Wealth: Poor", "Wealth: Middle", "Wealth: Rich", "Wealth: Richest",
                 "EVI", "Housing Quality: Modern House",
                 "Treatment-Seeking: Over 60% Sought")

# create a copy of all_df for further modifications if needed
all_df_renamed_vars <- all_df

# define location types for analysis
location_types <- c("Urban", "Rural")

## =========================================================================================================================================
### Single Logistic Regression Analysis (Adjusted): Covariates and Malaria Positivity
# adjusted for home type to get the direct effect of covariates on malaria positivity
# this will go in the supplement
# use this to identify what statistically significant relationships exist with the covariates and malaria positivity
# if not statistically significant, the variable cannot be a mediator
## =========================================================================================================================================

# initialize an empty list to store results for each location type
all_results <- list() 

# loop through each location type defined in location_types (urban or rural) to create a survey df with CIs
for (location in location_types) {
  # filter the data based on location type
  df <- all_df %>%
    filter(type == location)
  
  # drop NA values
  df_new <- df %>% drop_na(EVI_2000m_new)
  adj_df <- list()
  
  # loop through each variable of interest
  for (i in 1:length(var)) {
    svy_design <- svydesign_fun(df_new) # create a survey design object for the filtered data
    
    # construct the formula dynamically for the model, adjusting for X
    formula <- as.formula(paste("malaria_result ~", var[[i]], "+ home_type2 + (1|hv001)"))
    
    # fit the generalized linear model using the survey design
    result <- svyglm(formula, design = svy_design, family = binomial(link = "logit"))
    
    # summarize and tidy the model results
    df_result <- tidy(result)
    
    # filter out the intercept and calculate additional metrics
    df_result <- df_result %>% 
      filter(term != "(Intercept)") %>%  # exclude the intercept from the results
      rename_at(3, ~"SE") %>%  # rename the third column to "SE"
      mutate(odds = exp(estimate)) %>%  # calculate odds from the estimates
      mutate(lower_ci = exp(-1.96 * SE + estimate)) %>%  # calculate lower confidence interval
      mutate(upper_ci = exp(1.96 * SE + estimate)) %>%  # calculate upper confidence interval
      tibble::rownames_to_column() %>%  # convert row names to a column
      mutate(type = "adjusted", location = location)  # add type and location information
    
    # store the results for the current variable
    adj_df[[i]] <- df_result
  }
  
  # combine results for the current location type into all_results
  all_results[[location]] <- bind_rows(adj_df)
}

# combine results for all location types into a single dataframe and format the output to just include ORs and CIs
all_results_combined <- bind_rows(all_results) %>%
  transmute(
    term, # keep the term column
    estimate = sprintf("%.3f (%.3f – %.3f)", # format the estimates and confidence intervals
                       round(odds, 3), round(lower_ci, 3), round(upper_ci, 3)),
    location # keep the location column
  ) %>%
  pivot_wider( # reshape the dataframe to wide format based on location
    names_from = location,
    values_from = estimate
  )

# assign descriptive table names to the term column in the combined results
all_results_combined_labeled <- all_results_combined
all_results_combined_labeled$term <- table_names 

# remove "Home type" and keep the rest
all_results_combined_labeled <- all_results_combined_labeled %>%
  filter(!str_detect(term, "Home type: Non-agricultural"))

write_csv(all_results_combined_labeled, file.path(UpdatedFigDir, "tables", "adjusted_reg_results.csv"))

# export both tables to a word document
library(officer)
doc <- read_docx()
doc <- doc %>%
  body_add_par("Logistic Regression: Covariates and Positivity, Adjusted for Home Type", style = "heading 1") %>%
  body_add_table(value = all_results_combined_labeled, style = "table_template")

# save the document
file_path <- file.path(UpdatedFigDir, "adjusted_reg_results.docx")
print(doc, target = file_path)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Make Forest Plots
## -----------------------------------------------------------------------------------------------------------------------------------------

# reshape data to long format
plot_data_long <- all_results_combined_labeled %>%
  pivot_longer(cols = c(Urban, Rural), names_to = "area_type", values_to = "or_ci") %>%
  filter(!is.na(or_ci))  # remove empty rows if any

# extract odds ratio, lower CI, upper CI from the strings
plot_data_clean <- plot_data_long %>%
  mutate(
    or_ci = str_replace_all(or_ci, "[()]", ""),  # remove parentheses
    or_ci = str_replace_all(or_ci, "–", "-"),     # normalize dash
    odds = as.numeric(str_extract(or_ci, "^[0-9.]+")),
    lower_ci = as.numeric(str_extract(or_ci, "(?<= )[0-9.]+(?= -)")),
    upper_ci = as.numeric(str_extract(or_ci, "(?<=- )[0-9.]+")),
    variables = term  # rename for plotting
  )

# set colors and order of variables to appear on the graph
or_colors <- c("Urban" = "#1A478F", "Rural" = "#C01A81")
variable_order <- c(
  "Wealth: Poor",
  "Wealth: Middle",
  "Wealth: Rich",
  "Wealth: Richest",
  "EVI",
  "Housing Quality: Modern House",
  "Household Size",
  "Stunting: Stunted",
  "Net Use: Used",
  "Treatment-Seeking: Over 60% Sought"
)
plot_data_clean <- plot_data_clean %>%
  mutate(variables = factor(variables, levels = variable_order))

# plot
forest_plot_combined <- ggplot(plot_data_clean, 
                               aes(x = odds, 
                                   y = fct_rev(variables), 
                                   color = area_type)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray30", size = 0.25) +
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), 
                 height = 0.6, 
                 position = position_dodge(width = 0.6), 
                 size = 0.5) +
  geom_point(size = 3.5, position = position_dodge(width = 0.6)) +
  scale_color_manual(values = or_colors, name = "") +
  labs(
    title = "Adjusted Odds Ratios for Malaria Positivity",
    x = "Odds Ratio", 
    y = ""
  ) +
  scale_x_continuous(limits = c(0, 2)) +
  theme_manuscript() +
  theme(
    legend.position = "right",
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    plot.title = element_text(size = 15, hjust = 0.5),
    legend.background = element_rect(fill = "transparent")
  )

forest_plot_combined

# save as .pdf
ggsave(paste0(UpdatedFigDir, "_logistic_forest_plot.pdf"), forest_plot_combined, width = 9, height = 7) 

## =========================================================================================================================================
### Single Logistic Regression Analysis (Unadjusted): Home Type and Covariates
# this will go in the supplement
# use this to identify what statistically significant relationships exist with the home types and covariates
# if not statistically significant, the variable cannot be a mediator
## =========================================================================================================================================

# initialize an empty list to store results for each location type
all_results <- list()

# make character variables factors
all_df$home_type_factor <- as.factor(all_df$home_type_factor)
all_df$sex <- as.factor(all_df$sex)
all_df$stunting <- as.factor(all_df$stunting)
all_df$u5_net_use_factor <- as.factor(all_df$u5_net_use_factor)
all_df$roof_type_factor <- as.factor(all_df$roof_type_factor)
all_df$housing_quality <- as.factor(all_df$housing_quality)
all_df$med_treat_fever_none <- as.factor(all_df$housing_quality)

# define descriptive table names for corresponding variables
table_names <- c("EVI", "Household size", "Housing quality: modern",
                 "Treatment-Seeking: Over 60% Sought", "Stunting: stunted",
                 "Net use among children under the age of five years: use", "Wealth")

# loop through each location type defined in location_types (urban or rural) to create a survey df with CIs
for (location in location_types) {
  # filter the data based on location type
  df <- all_df %>%
    filter(type == location)
  
  # drop NA values
  df_new <- df %>% drop_na(EVI_2000m_new)
  unadj_df <- list()
  
  # loop through each variable of interest
  for (i in 1:length(var)) {
    svy_design <- svydesign_fun(df_new) # create a survey design object for the filtered data
    
    # construct the formula dynamically for the model
    formula <- as.formula(paste(var[[i]], "~ home_type_factor + (1|hv001)"))
    
    # Determine the appropriate family based on the dependent variable
    is_binary <- is.factor(df_new[[var[[i]]]])
    if (is_binary) {
      # For binary outcomes
      model_family <- binomial(link = "logit")
    } else {
      # For continuous outcomes
      model_family <- gaussian(link = "identity")
    }
    
    # fit the generalized linear mixed model using the survey design
    result <- svyglm(formula, design = svy_design, family = model_family)
    
    # summarize and tidy the model results
    df_result <- tidy(result)
    
    # filter out the intercept and calculate additional metrics
    df_result <- df_result %>% 
      filter(term != "(Intercept)") %>%  # exclude the intercept from the results
      rename_at(3, ~"SE") %>%  # rename the third column to "SE"
      mutate(
        estimate_exp = ifelse(is_binary, exp(estimate), estimate),
        lower_ci = ifelse(is_binary, exp(estimate - 1.96 * SE), estimate - 1.96 * SE),
        upper_ci = ifelse(is_binary, exp(estimate + 1.96 * SE), estimate + 1.96 * SE),
        estimate_type = ifelse(is_binary, "OR", "Coef")  # Add type directly here
      ) %>%
      tibble::rownames_to_column() %>%  # convert row names to a column
      mutate(
        type = "unadjusted", 
        location = location,
        dependent_var = var[[i]]
      )  # add type, location, and dependent variable information
    
    # store the results for the current variable
    unadj_df[[i]] <- df_result
  }
  
  # combine results for the current location type into all_results
  all_results[[location]] <- bind_rows(unadj_df)
}

# combine results for all location types into a single dataframe and format the output
all_results_combined <- bind_rows(all_results) %>%
  mutate(
    estimate_formatted = sprintf("%.3f (%.3f – %.3f)", 
                                 round(estimate_exp, 3), 
                                 round(lower_ci, 3), 
                                 round(upper_ci, 3))
  ) %>%
  select(term, estimate_formatted, location, dependent_var, estimate_type) %>%
  pivot_wider(
    names_from = location,
    values_from = estimate_formatted
  ) %>%
  arrange(dependent_var)

# assign descriptive table names to the term column in the combined results
all_results_combined_labeled <- all_results_combined
all_results_combined_labeled$dependent_var <- table_names 
all_results_combined_labeled <- all_results_combined_labeled %>%
  mutate(term = case_when(
    term == "home_type_factorZ_Agric" ~ "Home type: agricultural",
    TRUE ~ term
  ))

# write to an Excel file
write_xlsx(all_results_combined_labeled, file.path(UpdatedFigDir, "single_reg_results_hometype_covariate.xlsx"))

# export to a word document
doc <- read_docx()
doc <- doc %>%
  body_add_table(value = all_results_combined_labeled, style = "table_template")
print(doc, target = file.path(UpdatedFigDir, "single_reg_results_hometype_covariate.docx"))

# assuming your dataset is named 'all_df2', and 'typem' contains "urban" and "rural"
all_df2 <- all_df %>%
  
  # create a binary variable for malaria result (1 for positive, 0 for negative)
  mutate(malaria_result = ifelse(test_result =="+ve", 1,0),
         # multiply the 'EVI_2000m_new' variable by 10, but keep missing values as NA
         EVI_2000m_new = case_when(is.na(EVI_2000m_new) ~ NA,
                                   TRUE ~ EVI_2000m_new * 10),
         # create a binary variable for home type (1 for agricultural, 0 for non-agricultural)
         home_type_dep = ifelse(home_type2 =="A", 1, 0),
         home_type_dep = as.factor(home_type_dep), # convert to a factor
         wealth_index = as.factor(wealth), # convert to a factor
         dhs_year_factor = as.factor(dhs_year), # convert to a factor
         # create a binary variable for sex (Female or Male)
         sex = ifelse(hc27 == 2, "Female", "Male"),
         u5_net_use_dep = as.factor(u5_net_use), # convert to a factor
         roof_type_dep = as.factor(roof_type), # convert to a factor
         # create a binary variable for stunting (1 for stunted, 0 for not stunted)
         # if the value of 'hc70' is greater than 8000, mark it as NA
         stunting_dep = ifelse(hc70 < -300, 1, ifelse(hc70 > 8000, NA, 0)),
         stunting_dep = as.factor(stunting_dep), # convert to a factor
         housing_quality = as.factor(housing_quality)) # convert to a factor

# filter the data to separate urban and rural models
urban_df <- all_df2 %>% filter(type == "Urban")
rural_df <- all_df2 %>% filter(type == "Rural")


## =========================================================================================================================================
### Mediation Analysis
# https://uedufy.com/how-to-run-mediation-analysis-in-r/ (adapted code)
## =========================================================================================================================================

# function to calculate confidence interval
calculate_ci <- function(est, se) {
  lower_ci <- est - 1.96 * se
  upper_ci <- est + 1.96 * se
  return(c(lower_ci, upper_ci))
}

# bootstrapping function to calculate percent mediation and its CI
bootstrap_mediation <- function(data, mediator, n_bootstrap = 1000) {
  # initialize a vector to store percent mediation for each bootstrap sample
  bootstrap_percent_mediation <- numeric(n_bootstrap)
  
  # loop to perform bootstrapping
  for (i in 1:n_bootstrap) {
    # resample data with replacement
    boot_data <- data[sample(1:nrow(data), replace = TRUE), ]
    
    # define the mediation model using lavaan syntax
    mediation_model <- paste0('
      # direct effects
      ', mediator, ' ~ a * home_type_dep
      malaria_result ~ c * home_type_dep + b * ', mediator, '
      
      # indirect effect (a * b)
      indirect := a * b

      # total effect (c + indirect)
      total := c + indirect
    ')
    
    # fit the mediation model using the bootstrapped data
    fit <- sem(mediation_model, data = boot_data)
    
    # extract estimates for indirect and total effects
    estimates <- parameterEstimates(fit)
    indirect_effect <- estimates$est[estimates$label == "indirect"]
    total_effect <- estimates$est[estimates$label == "total"]
    
    # calculate percent mediation
    if (total_effect != 0) {
      bootstrap_percent_mediation[i] <- (indirect_effect / total_effect) * 100
    } else {
      bootstrap_percent_mediation[i] <- NA  # handle case where total effect is zero
    }
  }
  
  # calculate confidence intervals for percent mediation (95%)
  ci_lower <- quantile(bootstrap_percent_mediation, probs = 0.025, na.rm = TRUE)
  ci_upper <- quantile(bootstrap_percent_mediation, probs = 0.975, na.rm = TRUE)
  
  # return mean percent mediation and its confidence intervals
  return(list(
    percent_mediation = mean(bootstrap_percent_mediation, na.rm = TRUE),
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}

# main mediation analysis function
run_mediation_analysis <- function(data, n_bootstrap = 1000) {
  
  # retain only relevant columns
  mediation_data <- data %>%    
    select(home_type_dep, malaria_result, stunting_dep, hh_size, wealth, housing_quality, u5_net_use_dep, EVI_2000m_new, med_treat_fever_none)
  
  # convert factor variables to numeric
  mediation_data$home_type_dep <- as.numeric(as.factor(mediation_data$home_type_dep))
  mediation_data$stunting_dep <- as.numeric(as.factor(mediation_data$stunting_dep))
  mediation_data$housing_quality <- as.numeric(as.factor(mediation_data$housing_quality))
  
  # define the models as a named list of mediator variables
  models <- list(
    model_number_1 = c("stunting_dep"),
    model_number_2 = c("hh_size"),
    model_number_3 = c("wealth"),
    model_number_4 = c("housing_quality"),
    model_number_5 = c("u5_net_use_dep"),
    model_number_6 = c("EVI_2000m_new"),
    model_number_7 = c("med_treat_fever_none")
  )
  
  # initialize an empty data frame to store results
  results_df <- data.frame(
    mediator = character(),
    effect_type = character(),
    estimate = numeric(),
    se = numeric(),
    lower_ci = numeric(),
    upper_ci = numeric(),
    p_value = numeric(),
    percent_mediation = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    stringsAsFactors = FALSE
  )
  
  # loop through each model and perform mediation analysis
  for (model_name in names(models)) {  
    # extract the mediator variable for the current model
    mediator <- models[[model_name]]
    
    # define the mediation model using lavaan syntax
    mediation_model <- paste0('
      # direct effects
      ', mediator, ' ~ a * home_type_dep
      malaria_result ~ c * home_type_dep + b * ', mediator, '
      
      # indirect effect (a * b)
      indirect := a * b

      # total effect (c + indirect)
      total := c + indirect
    ') 
    
    # fit the mediation model using the lavaan package
    mediation_results <- sem(mediation_model, data = mediation_data)
    
    # extract parameter estimates
    params <- parameterEstimates(mediation_results)
    
    # obtain estimates, SEs, and p-values for indirect, direct, and total effects
    indirect_est <- params$est[params$label == "indirect"]
    indirect_se <- params$se[params$label == "indirect"]
    indirect_p_value <- params$pvalue[params$label == "indirect"]
    
    direct_est <- params$est[params$label == "c"]
    direct_se <- params$se[params$label == "c"]
    direct_p_value <- params$pvalue[params$label == "c"]
    
    total_est <- params$est[params$label == "total"]
    total_se <- params$se[params$label == "total"]
    total_p_value <- params$pvalue[params$label == "total"]
    
    # calculate confidence intervals
    indirect_ci <- calculate_ci(indirect_est, indirect_se)
    direct_ci <- calculate_ci(direct_est, direct_se)
    total_ci <- calculate_ci(total_est, total_se)
    
    # calculate percent mediation, check for non-zero total effect
    # also run bootstrapping for percent mediation and its CI
    if (total_est != 0) {
      bootstrap_results <- bootstrap_mediation(mediation_data, mediator, n_bootstrap)
    } else {
      bootstrap_results <- list(percent_mediation = NA, ci_lower = NA, ci_upper = NA)
    }
    
    # store indirect effect results
    results_df <- rbind(results_df, data.frame(
      mediator = mediator,
      effect_type = "Indirect",
      estimate = indirect_est,
      se = indirect_se,
      lower_ci = indirect_ci[1],
      upper_ci = indirect_ci[2],
      p_value = indirect_p_value,
      percent_mediation = bootstrap_results$percent_mediation,
      ci_lower = bootstrap_results$ci_lower,
      ci_upper = bootstrap_results$ci_upper,
      stringsAsFactors = FALSE
    ))
    
    # store direct effect results
    results_df <- rbind(results_df, data.frame(
      mediator = mediator,
      effect_type = "Direct",
      estimate = direct_est,
      se = direct_se,
      lower_ci = direct_ci[1],
      upper_ci = direct_ci[2],
      p_value = direct_p_value,
      percent_mediation = NA,  # no percent mediation for direct effect
      ci_lower = NA,
      ci_upper = NA,
      stringsAsFactors = FALSE
    ))
    
    # store total effect results
    results_df <- rbind(results_df, data.frame(
      mediator = mediator,
      effect_type = "Total",
      estimate = total_est,
      se = total_se,
      lower_ci = total_ci[1],
      upper_ci = total_ci[2],
      p_value = total_p_value,
      percent_mediation = NA,  # no percent mediation for total effect
      ci_lower = NA,
      ci_upper = NA,
      stringsAsFactors = FALSE
    ))
  }
  
  # clean and format the results for export
  results_df <- results_df %>%
    mutate(mediator = case_when(
      mediator == "stunting_dep" ~ "Stunting",
      mediator == "hh_size" ~ "Household Size",
      mediator == "wealth" ~ "Wealth",
      mediator == "housing_quality" ~ "Housing Quality",
      mediator == "u5_net_use_dep" ~ "Net Use",
      mediator == "EVI_2000m_new" ~ "EVI",
      mediator == "med_treat_fever_none" ~ "Treatment-Seeking",
      TRUE ~ mediator  # keep original if no match
    )) %>%
    rename(
      "Mediator" = mediator,
      "Effect Type" = effect_type,
      "Estimate" = estimate,
      "SE" = se,
      "Lower 95% CI" = lower_ci,
      "Upper 95% CI" = upper_ci,
      "P-Value" = p_value,
      "% Mediation" = percent_mediation,
      "Bootstrapped Lower CI" = ci_lower,
      "Bootstrapped Upper CI" = ci_upper
    )
  
  # create a clean table with estimates, SE, CI, p-value, and percent mediation
  results_table <- results_df %>%
    select("Mediator", "Effect Type", "Estimate", "SE", "Lower 95% CI", "Upper 95% CI", 
           "P-Value", "% Mediation", "Bootstrapped Lower CI", "Bootstrapped Upper CI") %>%
    mutate(across(c("Estimate", "SE", "Lower 95% CI", "Upper 95% CI", "% Mediation", "Bootstrapped Lower CI", "Bootstrapped Upper CI"), round, 3),
           `P-Value` = ifelse(`P-Value` < 0.0001, "p < 0.0001", round(`P-Value`, 4)))
  
  # uncomment this to get rounded results
  #return(results_table)
  
  # also need a results table that isn't rounded for use in plots
  results_unrounded <- results_df %>%
    select("Mediator", "Effect Type", "Estimate", "SE", "Lower 95% CI", "Upper 95% CI", 
           "P-Value", "% Mediation", "Bootstrapped Lower CI", "Bootstrapped Upper CI") %>%
    mutate(across(c("Estimate", "SE", "Lower 95% CI", "Upper 95% CI", "% Mediation", "Bootstrapped Lower CI", "Bootstrapped Upper CI")),
           `P-Value` = ifelse(`P-Value` < 0.0001, "p < 0.0001", round(`P-Value`, 4)))
  
  return(results_unrounded)
}

# create a Word document and add title
doc <- read_docx()

# make factor variables numeric
urban_df$housing_quality <- as.numeric(urban_df$housing_quality)
urban_df$home_type_dep <- as.numeric(urban_df$home_type_dep)
urban_df$u5_net_use_dep <- as.numeric(urban_df$u5_net_use_dep)
urban_df$roof_type_dep <- as.numeric(urban_df$roof_type_dep)
urban_df$stunting_dep <- as.numeric(urban_df$stunting_dep)
urban_df$med_treat_fever_none <- as.numeric(urban_df$med_treat_fever_none)

rural_df$housing_quality <- as.numeric(rural_df$housing_quality)
rural_df$home_type_dep <- as.numeric(rural_df$home_type_dep)
rural_df$u5_net_use_dep <- as.numeric(rural_df$u5_net_use_dep)
rural_df$roof_type_dep <- as.numeric(rural_df$roof_type_dep)
rural_df$stunting_dep <- as.numeric(rural_df$stunting_dep)
rural_df$med_treat_fever_none <- as.numeric(rural_df$med_treat_fever_none)

# run the function for urban_df and add the table to the document
urban_results <- run_mediation_analysis(urban_df)
urban_results <- urban_results  %>%
  mutate(across(where(is.numeric), round, 4))
doc <- doc %>%
  body_add_par("Mediation Analysis Results - Urban", style = "heading 1") %>%
  body_add_table(value = urban_results, style = "table_template")

# run the function for rural_df and add the table to the document
rural_results <- run_mediation_analysis(rural_df)
rural_results <- rural_results  %>%
  mutate(across(where(is.numeric), round, 4))
doc <- doc %>%
  body_add_par("Mediation Analysis Results - Rural", style = "heading 1") %>%
  body_add_table(value = rural_results, style = "table_template")

# save the document
file_path <- file.path(UpdatedFigDir, "mediation_analysis_results_bootstrapped3.docx")
print(doc, target = file_path)

# save unrounded results in separate dfs
urban_unrounded_results <- urban_results
rural_unrounded_results <- rural_results

## -----------------------------------------------------------------------------------------------------------------------------------------
### Stratify Mediation Analysis by Country
## -----------------------------------------------------------------------------------------------------------------------------------------

# get list of unique countries in the df (same for urban and rural)
countries <- unique(urban_df$CountryName)

# initialize a list to store results for each country
urban_country_results <- list()
rural_country_results <- list()

# make net use a factor variable
urban_df$u5_net_use_dep <- as.numeric(as.factor(urban_df$u5_net_use))
rural_df$u5_net_use_dep <- as.numeric(as.factor(rural_df$u5_net_use))

# run mediation analysis stratified by each country
for (country in countries) {
  
  # urban data
  print(paste("Processing urban data for", country, "..."))
  urban_country_data <- urban_df %>% filter(CountryName == country)
  urban_results <- run_mediation_analysis(urban_country_data)
  urban_country_results[[country]] <- urban_results
  print(paste("Finished urban data for", country))
  
  # rural data
  print(paste("Processing rural data for", country, "..."))
  rural_country_data <- rural_df %>% filter(CountryName == country)
  rural_results <- run_mediation_analysis(rural_country_data)
  rural_country_results[[country]] <- rural_results
  print(paste("Finished rural data for", country))
}

# remove p-value from dfs in list (not joining properly and not needed for perc. mediation plots)
urban_country_results_nop <- lapply(urban_country_results, function(df) df %>% select(-`P-Value`))
rural_country_results_nop <- lapply(rural_country_results, function(df) df %>% select(-`P-Value`))

# combine all results into single data frames
combined_urban_results <- bind_rows(urban_country_results_nop, .id = "Country")
combined_rural_results <- bind_rows(rural_country_results_nop, .id = "Country")

# save to avoid running mediation again
write.csv(combined_urban_results, file = file.path(UpdatedFigDir, "urban_country_mediation_results.csv"), row.names = FALSE, quote = TRUE)
write.csv(combined_rural_results, file = file.path(UpdatedFigDir, "rural_country_mediation_results.csv"), row.names = FALSE, quote = TRUE)


## =========================================================================================================================================
### Mediation Analysis: Visualization of Results
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Supplemental Figure: Effect Size Bar Plot for Indirect, Direct, and Total Effects with Confidence Intervals
## -----------------------------------------------------------------------------------------------------------------------------------------

# filter for effect sizes (indirect, direct, and total) - by urban and rural
urban_effect_size_df <- urban_unrounded_results %>%
  filter(!is.na(Estimate) & `Effect Type` %in% c("Indirect", "Direct", "Total")) %>%
  select(Mediator, `Effect Type`, Estimate, `Lower 95% CI`, `Upper 95% CI`)
rural_effect_size_df <- rural_unrounded_results %>%
  filter(!is.na(Estimate) & `Effect Type` %in% c("Indirect", "Direct", "Total")) %>%
  select(Mediator, `Effect Type`, Estimate, `Lower 95% CI`, `Upper 95% CI`)

# create effect size plot (urban)
urban_effect_size_plot <- ggplot(urban_effect_size_df, aes(x = Mediator, y = Estimate, fill = `Effect Type`)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = `Lower 95% CI`, ymax = `Upper 95% CI`), 
                position = position_dodge(0.9), width = 0.25) +
  scale_fill_manual(values = c("Indirect" = "#ffa630", 
                               "Direct" = "#d7e8ba", 
                               "Total" = "#4da1a9")) +
  labs(title = "Effect Sizes for Indirect, Direct, and Total Effects: Urban",
       x = "Mediator",
       y = "Effect Size") +
  scale_y_continuous(limits = c(0, 0.15)) +
  theme_manuscript() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# create effect size plot (rural)
rural_effect_size_plot <- ggplot(rural_effect_size_df, aes(x = Mediator, y = Estimate, fill = `Effect Type`)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = `Lower 95% CI`, ymax = `Upper 95% CI`), 
                position = position_dodge(0.9), width = 0.25) +
  scale_fill_manual(values = c("Indirect" = "#ffa630", 
                               "Direct" = "#d7e8ba", 
                               "Total" = "#4da1a9")) +
  labs(title = "Effect Sizes for Indirect, Direct, and Total Effects: Rural",
       x = "Mediator",
       y = "Effect Size") +
  scale_y_continuous(limits = c(0, 0.15)) +
  theme_manuscript() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# combine and save urban and rural effect size bar plots

# remove individual titles and x-axis labels from the urban and rural plots
urban_effect_size_plot <- urban_effect_size_plot + 
  labs(title = NULL, subtitle = "Urban", x = NULL) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12)) 
rural_effect_size_plot <- rural_effect_size_plot + 
  labs(title = NULL, subtitle = "Rural", x = NULL, y = NULL) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12)) 

# extract the legend
legend <- get_only_legend(urban_effect_size_plot) 

# remove individual legends as we need only one
urban_effect_size_plot <- urban_effect_size_plot + theme(legend.position = "none")
rural_effect_size_plot <- rural_effect_size_plot + theme(legend.position = "none") 

combined_effect_bar_plot <- grid.arrange(urban_effect_size_plot, rural_effect_size_plot, ncol = 2)

# arrange the combined plot and legend side by side
combined_effect_bar_plot <- grid.arrange(
  combined_effect_bar_plot,
  legend,
  nrow = 1,
  ncol = 2,
  heights = c(5),
  widths = c(10, 2),
  top = textGrob("Effect Sizes for Indirect, Direct, and Total Effects by Mediator",
                 gp = gpar(fontsize = 12, fontface = "bold", hjust = 0.5)),
  bottom = textGrob("Mediator",
                    gp = gpar(fontsize = 12))
)

# save as .pdf
ggsave(paste0(UpdatedFigDir, "_mediation_effect_bar.pdf"), combined_effect_bar_plot, width = 9, height = 7) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Forest Plot for Percent Mediation
## -----------------------------------------------------------------------------------------------------------------------------------------

# filter for percent mediation (by urban and rural)
urban_percent_mediation_df <- urban_unrounded_results %>%
  filter(!is.na(`% Mediation`)) %>%
  select(Mediator, `% Mediation`, `Bootstrapped Lower CI`, `Bootstrapped Upper CI`) %>%
  filter(Mediator != "Roof Type")
rural_percent_mediation_df <- rural_unrounded_results %>%
  filter(!is.na(`% Mediation`)) %>%
  select(Mediator, `% Mediation`, `Bootstrapped Lower CI`, `Bootstrapped Upper CI`) %>%
  filter(Mediator != "Roof Type")

# combine urban and rural data frames
combined_percent_mediation_df <- bind_rows(
  urban_percent_mediation_df %>% mutate(Location = "Urban"),
  rural_percent_mediation_df %>% mutate(Location = "Rural")
)

# order urban percent mediation in descending order
combined_percent_mediation_df <- combined_percent_mediation_df %>% 
  mutate(Mediator = factor(Mediator, 
                           levels = combined_percent_mediation_df %>% 
                             filter(Location == "Urban") %>% 
                             arrange(`% Mediation`) %>%
                             pull(Mediator)))

# create the combined forest plot
combined_perc_med_forest <- ggplot(combined_percent_mediation_df,  
                                   aes(x = `% Mediation`, y = Mediator, color = Location)) +  
  geom_point(size = 3, position = position_dodge(width = 0.6)) +  
  geom_errorbarh(aes(xmin = `Bootstrapped Lower CI`, xmax = `Bootstrapped Upper CI`),  
                 height = 0.6, position = position_dodge(width = 0.6)) +  
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  labs(title = "Percent Mediation of Malaria Positivity by Mediators",  
       x = "Percent Mediation (%)",  
       y = "Mediator") +  
  scale_x_continuous(limits = c(-20, 100)) +  
  scale_color_manual(values = c("Urban" = "#1A478F", "Rural" = "#C01A81")) +  
  theme_manuscript() +  
  theme(axis.text.y = element_text(size = 10),  
        plot.title = element_text(size = 14, face = "bold"),  
        legend.background = element_rect(fill = "transparent"))
combined_perc_med_forest

# save the combined plot as a .pdf
ggsave(paste0(UpdatedFigDir, "_combined_mediation_perc_forest2.pdf"), combined_perc_med_forest, width = 7, height = 5)


## -----------------------------------------------------------------------------------------------------------------------------------------
### Make Percent Mediation Forest Plot for Each Country
## -----------------------------------------------------------------------------------------------------------------------------------------

urban_country_results <- read.csv(file.path(UpdatedFigDir, "tables", "urban_country_mediation_results.csv"), check.names = FALSE) %>%
  filter(Mediator != "Roof Type")
rural_country_results <- read.csv(file.path(UpdatedFigDir, "tables", "rural_country_mediation_results.csv"), check.names = FALSE) %>%
  filter(Mediator != "Roof Type")

# create a list to store plots (plots will show both urban+rural mediation data)
country_forest_plots <- list()

# loop over unique countries in urban and rural mediation results
for (country in unique(urban_country_results$Country)) {
  
  # retrieve urban and rural data from the country of interest
  urban_mediation_data <- urban_country_results %>%
    filter(Country == country)
  rural_mediation_data <- rural_country_results %>%
    filter(Country == country)
  
  # combine urban and rural data for plotting
  all_country_mediation_data <- rbind(urban_mediation_data %>% mutate(Location = "Urban"),
                                      rural_mediation_data %>% mutate(Location = "Rural")
  )
  
  # get unique mediator names for Urban, ordered by descending % Mediation
  urban_order <- all_country_mediation_data %>%
    filter(Location == "Urban") %>%
    arrange((`% Mediation`)) %>%
    distinct(Mediator, .keep_all = TRUE) %>%  # ensure uniqueness
    pull(Mediator)
  
  # convert Mediator to factor with correct order
  all_country_mediation_data <- all_country_mediation_data %>%
    mutate(Mediator = factor(Mediator, levels = urban_order))
  
  # create forest plot for the current country
  country_forest_plot <- ggplot(all_country_mediation_data, aes(x = `% Mediation`, y = Mediator, color = Location)) + 
    geom_point(size = 3, position = position_dodge(width = 0.5)) + 
    geom_errorbarh(aes(xmin = `Bootstrapped Lower CI`, xmax = `Bootstrapped Upper CI`), 
                   height = 0.6, position = position_dodge(width = 0.5)) + 
    geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
    scale_x_continuous(limits = c(-31, 100)) +
    scale_color_manual(values = c("Urban" = "#1A478F", "Rural" = "#C01A81")) +  
    theme_manuscript() +
    facet_wrap(~ Country, labeller = label_value) + 
    theme(
      axis.text.y = element_text(size = 11), 
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      strip.text = element_text(size = 14)
    )
  
  # store the plot in the list with the country name as the key
  country_forest_plots[[country]] <- country_forest_plot
  
  # give each plot a name
  assign(paste0(country, "_med_plot"), country_forest_plot)
}

# arrange country % mediation forest plots in a grid
country_perc_med_plots <- do.call(grid.arrange, c(country_forest_plots, ncol = 3))

# format the grid
country_perc_med_plots_final <- grid.arrange(
  country_perc_med_plots,
  ncol = 1,
  top = textGrob(
    "Percent Mediation by Key Mediators Across Countries,
    Stratified by Urban and Rural Residence",
    gp = gpar(fontsize = 16, hjust = 0.5)  # center the title
  )
)

# display the combined plot and save as .pdf
ggsave(paste0(UpdatedFigDir, "_country_mediation_forest.pdf"), country_perc_med_plots_final, width = 12, height = 14) 

# ----- make grid with only plots that have reasonable confidence intervals ------ 
selected_countries <- c("Burundi", "Cote d'Ivoire", "Nigeria", "Togo")
selected_plots <- lapply(selected_countries, function(country) country_forest_plots[[country]])
country_subset_med_plots <- do.call(grid.arrange, c(selected_plots, nrow = 3, ncol = 2))

# arrange selected plots
country_subset_med_plots_final <- grid.arrange(
  country_subset_med_plots,
  ncol = 2,  # legend in the second column, plots in the first column
  widths = c(10, 2),
  top = textGrob(
    "Percent Mediation by Key Mediators Across Countries, Stratified by Urban and Rural Residence",
    gp = gpar(fontsize = 16, fontface = "bold", hjust = 0.5)  # center the title
  )
)
ggsave(paste0(UpdatedFigDir, "_country_subset_med_plots_final.pdf"), country_subset_med_plots, width = 10, height = 10)

## =========================================================================================================================================
### Mediation Analysis: Predicted Probabilities and OR plots
## =========================================================================================================================================

create_or_pp_plots <- function(df, area_type) {
  # create survey design
  svy_design <- svydesign_fun(df)
  
  # define formulas
  formulas <- list(
    malaria_result ~ home_type_dep,
    malaria_result ~ home_type_dep + stunting_dep,
    malaria_result ~ home_type_dep + hh_size,
    malaria_result ~ home_type_dep + EVI_2000m_new,
    malaria_result ~ home_type_dep + wealth_index,
    malaria_result ~ home_type_dep + housing_quality,
    malaria_result ~ home_type_dep + u5_net_use_dep,
    malaria_result ~ home_type_dep + med_treat_fever_none
  )
  
  # define term names
  term_names <- c(
    "home type",
    "home type + stunting",
    "home type + household size", 
    "home type + EVI",
    "home type + wealth index",
    "home type + housing quality",
    "home type + u5 net use",
    "home type + treatment-seeking"
  )
  
  # model function
  fun_model <- function(model_formula) {
    svyglm(model_formula, design = svy_design, family = binomial(link = "logit"))
  }
  
  # apply models
  model_datasets_results <- lapply(formulas, fun_model)
  
  # odds ratio function
  fun_or <- function(model_) {
    df <- tidy(model_) %>%
      filter(term != "(Intercept)") %>%
      rename(SE = std.error) %>%
      mutate(
        odds = exp(estimate),
        lower_ci = exp(estimate - 1.96 * SE),
        upper_ci = exp(estimate + 1.96 * SE)
      ) %>%
      tibble::rownames_to_column() %>%
      return(df)
  }
  
  # process OR
  df_or <- lapply(model_datasets_results, fun_or) %>% 
    bind_rows(.id = "formula_id") %>% 
    filter(term == "home_type_dep1") %>% 
    mutate(formula_name = term_names[as.numeric(formula_id)])
  
  df_or <- df_or %>% 
    rename(variables = formula_name) %>% 
    select(variables, odds, lower_ci, upper_ci, p.value)
  
  # write OR results to Excel
  write_xlsx(df_or, file.path(UpdatedFigDir, paste0(area_type, "_df_or_results.xlsx")))
  
  # filter significant results
  df_or_significant <- df_or %>% filter(p.value < 0.05)
  
  # color palette
  color_list <- c("#154D42", "#6f3096", "red", "#028E41", "#ff8da1", "#4777cd", "#ab0a58", "#fa7a48")
  
  # odds ratio plot
  forest_b <- ggplot(df_or, aes(x = odds, y = variables, color = variables)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_errorbarh(aes(xmax = lower_ci, xmin = upper_ci), size = .5, height = .3) + 
    geom_point(size = 4) +
    scale_color_manual(name = "", values = color_list) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) + 
    theme(panel.border = element_blank()) +
    ylab("") + 
    xlab("Odds Ratio") +
    theme_manuscript() +
    theme(legend.position = "none") +
    xlim(0.5, 3.7) + 
    theme(axis.text.y = element_text(colour = color_list, size = 14),
          axis.text.x = element_text(size = 14),
          axis.title.x = element_text(size = 14)) +
    ggtitle(paste(area_type, "Odds Ratios")) +
    theme(plot.title = element_text(size = 18, hjust = 0.5))
  
  # predicted probabilities function
  effect_df_fun <- function(model_) {
    effect_list_est <- summary(Effect("home_type_dep", model_))
    effect_list_est$effect %>% as.data.frame() %>%
      bind_cols(effect_list_est$lower %>% as.data.frame()) %>%
      bind_cols(effect_list_est$upper %>% as.data.frame()) %>%
      rename(effect = ....1, lower = ....2, upper = ....3) %>%
      tibble::rownames_to_column(var = "term_name")
  }
  
  # term names for predicted probabilities
  term_names_pred <- data.frame(
    model_id = c("1", "2", "3", "4", "5", "6", "7", "8"), 
    variable = term_names
  )
  
  # combine effect dataframes
  df_effect <- lapply(model_datasets_results, effect_df_fun) %>%
    bind_rows(.id = "model_id") %>% 
    left_join(term_names_pred, by = "model_id") %>%
    filter(term_name == "1")
  
  # predicted probability plot
  pred_p <- ggplot() + 
    geom_errorbarh(data = df_effect, 
                   aes(x = effect, y = variable, xmax = lower, xmin = upper, color = variable), 
                   size = .5, height = .3) + 
    geom_point(data = df_effect, 
               aes(x = effect, y = variable, color = variable), 
               size = 4) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) + 
    theme(panel.border = element_blank()) + 
    xlab("Predicted Probability") +
    scale_color_manual(name = "", values = color_list) + 
    theme_manuscript() +
    xlim(0, 0.5) + 
    theme(legend.position = "none") + 
    scale_y_discrete(labels = function(y) str_wrap(y, width = 15)) +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(colour = color_list, size = 14),
          axis.text.x = element_text(size = 14),
          axis.title.x = element_text(size = 14)) +
    ggtitle(paste(area_type, "Predicted Probabilities")) + 
    theme(plot.title = element_text(size = 18, hjust = 0.5))
  
  # remove titles and adjust plots
  forest_b <- forest_b + labs(title = NULL)
  pred_p <- pred_p + 
    labs(title = NULL) + 
    scale_y_discrete(labels = NULL)
  
  # arrange combined plot
  or_pp_plots <- grid.arrange(
    forest_b,
    pred_p, 
    nrow = 1, 
    ncol = 2,
    widths = c(5.15, 3)
  )
  
  # return results
  return(list(
    odds_ratios = df_or,
    significant_odds_ratios = df_or_significant,
    forest_plot = forest_b,
    predicted_probabilities_plot = pred_p
  ))
}

# run function for urban and rural data
urban_results <- create_or_pp_plots(urban_df, "urban")
rural_results <- create_or_pp_plots(rural_df, "rural")

# arrange the combined plot
or_pp_plots <- grid.arrange(
  urban_results$forest_plot,
  urban_results$predicted_probabilities_plot,
  rural_results$forest_plot,
  rural_results$predicted_probabilities_plot, 
  nrow = 2, 
  ncol = 2,
  widths = c(5.15, 3)
)

# save final combined OR and PP plot as .pdf
ggsave(paste0(UpdatedFigDir, "_odds_pred_prob.pdf"), or_pp_plots, width = 10, height = 6) 
