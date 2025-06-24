# Agricultural Work, Malaria Prevalence, and Mediating Factors in 15 Sub-Saharan African Countries

## Overview

This repository contains the code and data structure for a cross-country analysis examining the relationship between adult agricultural occupation and malaria infection in children under five. Using Demographic and Health Survey (DHS) data from 15 Sub-Saharan African countries, the study evaluates key mediating factors and urban–rural differences in malaria risk. The project aims to support more targeted malaria interventions and promote integrated strategies for malaria control as global funding declines.

## Background

Agricultural households in sub-Saharan Africa often live and work in ecological zones that support malaria transmission, including irrigated fields, croplands, and forested areas. These communities face heightened risk due to environmental exposure, poor housing quality, and reduced effectiveness of vector control strategies in outdoor or pesticide-impacted settings. This study investigates how agricultural work shapes malaria outcomes among children under five and identifies mediating factors like wealth, treatment-seeking behavior, and environmental characteristics.

Amid high malaria burden and shrinking global health funding, particularly from the U.S., it is crucial for national programs to identify and prioritize high-risk populations. This analysis provides country-specific and subnational insights to help tailor interventions and inform efficient, evidence-based targeting of malaria control resources.

## Methodology

This project uses DHS data and combines descriptive, spatial, and mediation analyses. The methods include:

1. **Data compilation** (`01_data_compilation.R`): Loads and prepares child-level and household-level survey data from 15 countries.
2. **Descriptive analysis** (`02_descriptive_analysis.R`): Summarizes malaria prevalence, insecticide-treated net (ITN) use, and household/environmental characteristics by agricultural status.
3. **Mediation modeling** (`03_modeling.R`): Runs logistic regression within a Structural Equation Modeling framework to assess indirect and direct effects of mediators such as vegetation index, housing quality, and wealth.
4. **Visualization** (`04_line_plots.R`): Generates line plots to explore changes in malaria test positivity rate, prior night ITN use, and ITN distribution counts between the preceding survey and most recent survey included in the analysis for each country.
5. **Helper functions** (`helpers.R`): Contains utility functions used across scripts.

## Repository Structure

- `data/` — DHS-derived data and analysis files  
  - `GPS/` — GPS data files for spatial linkage  
  - `250605_rural_df_for_analysis.csv` — Cleaned datasets for analysis
  - `250605_urban_df_for_analysis.csv`  
  - `251106_df_with_ci_for_analysis_trend_malaria_grace_created.csv`  
  - `251106_df_with_ci_for_analysis_trend_net_grace_created.csv`  
  - `251106_rural_df_for_analysis_trend.csv`  
  - `251106_urban_df_for_analysis_trend.csv`  
  - `251206_urban_rural_analysis_data_for_modeling.csv`  
  - `afr_g2014_2013_0.dbf` — Shapefile components
  - `afr_g2014_2013_0.prj`  
  - `afr_g2014_2013_0.shp`  
  - `afr_g2014_2013_0.shx`  
  - `all_geospatial_monthly_DHS.csv` — Geospatial data
  - `220315_ITN_distribution_data.csv` — ITN distribution data

- `outputs/` — Results, figures, and exported model summaries

- `scripts/` — Core scripts for data processing, modeling, and plotting  
  - `01_data_compilation.R`  
  - `02_descriptive_analysis.R`  
  - `03_modeling.R`  
  - `04_line_plots.R`  
  - `helpers.R`

- `Agric_final.Rproj` — RStudio project file  
- `README.md` — Project README

## Key Findings

- Children living in agricultural households are more likely to test positive for malaria than those in non-agricultural households, even with similar ITN use rates.
- The strongest mediators of this association are household wealth, enhanced vegetation index (EVI), treatment-seeking behavior, and housing quality.
- Urban agricultural households, although less common, tend to cluster in high-density areas, highlighting the need for spatially nuanced intervention planning.

## Impact

This project supports national malaria programs and partners in making data-driven decisions about where and how to deploy interventions, especially in urbanizing contexts. By identifying subgroups of agricultural households at greatest risk and clarifying the pathways through which risk operates, this work can improve malaria targeting efficiency and inform integrated approaches.

## Contact

For questions, please contact:

**Dr. Ifeoma Ozodiegwu**  
Assistant Professor, Loyola University Chicago  
Principal Investigator, Urban Malaria Lab  
📧 [iozodiegwu@luc.edu](mailto:iozodiegwu@luc.edu)

**Grace Legris**  
Research Data Analyst, Urban Malaria Lab  
Loyola University Chicago  
📧 [gracelegris01@gmail.com](mailto:gracelegris01@gmail.com)

## Acknowledgments

This work was conducted by the Urban Malaria Lab at Loyola University Chicago with support from the Gates Foundation. We acknowledge contributions from DHS Program staff and collaborators across participating countries whose data made this analysis possible.