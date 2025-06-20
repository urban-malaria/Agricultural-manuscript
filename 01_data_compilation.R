# ==========================================================================================================================================
# Script Name: Data Compilation
# Author: Dr. Ifeoma Ozodiegwu
# Edited by: Grace Legris, Research Data Analyst (gracebea@gmail.com)
# Edited: [2025-06-20]
# Purpose: Compile analysis and geospatial datasets
# ==========================================================================================================================================

source(load_path.R)

## --------------------------------------------------------------------------------------------------------------------------------------------------
### read in the survey ids for surveys to be analyzed and logging into the DHS account (no need to login to dhs or run commented out script unless files need to be updated)
## ---------------------------------------------------------------------------------------------------------------------------------------------------

# log in into dhs (email and password needs to be removed from this script before it becomes public)
my_config <- set_rdhs_config(email = "",
                             project = "Association of household member engagement in agricultural work and malaria",
                             config_path = "rdhs.json",
                             cache_path = "data",
                             #password_prompt = TRUE,
                             global = FALSE, timeout = 600)

get_rdhs_config()

survs <- read.csv(file.path(ManDir, "csv", "surveyids_for_analysis.csv")) #%>%
#mutate(cntryId = stringr::str_extract(SurveyId, "^.{2}")) %>% group_by(cntryId) %>% 
#slice_max(SurveyYear) %>% ungroup()

# obtaining country ids
ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode", "SubregionName"))

## =========================================================================================================================================
### COMPILE ANALYSIS DATA
## =========================================================================================================================================

## ---------------------------------------------------------------------------------------------------------------------
### Data loading  and transforming IR and MR datasets (no need to run commented out script unless dhs files need an update)
## ---------------------------------------------------------------------------------------------------------------------

# #download IR datasets 
ir_datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "DT", fileType = "IR") #%>%
#group_by(CountryName) %>% slice_max(SurveyYear)

ir_downloads <- list()

for (i in 1:nrow(ir_datasets)){
  # if (ir_datasets$FileName[i] == "MDIR81DT.ZIP"){ # for some reason, I am not able to download this file with my credentials so downloading manually
  #   df <- list("MDIR81FL"="project_one/datasets/MDIR81FL.ZIP") #manually create link for madagascar 2021
  #   ir_downloads<- append(ir_downloads, df)
  # }
  # sending ir data link to the folder "project_one
  tryCatch({
    df <- get_datasets(ir_datasets$FileName[i], output_dir_root =PopDir, download_option = "zip", verbose_argument= T)
    
    ir_downloads<- append(ir_downloads, df)
    
  }, error = function(e){cat("ERROR:", conditionMessage(e), "\n")})
  
}

## ---------------------------------------------------------------------------------------------------------------------
# save ir_downloads and mr_downloads as RDS so no need to sign in to read dhs data until files are updated 
saveRDS(ir_downloads, file.path(PopDir, "analysis_dat/ir_downloads.rds"))
ir_downloads<- readRDS(file.path(PopDir, "analysis_dat/ir_downloads.rds"))

#check datasets to manaually download
data.frame(SurveyId = setdiff(survs$SurveyId, ir_datasets$SurveyId))
# 

for (i in 1:length(ir_downloads)){
  unzip(ir_downloads[[i]],  exdir = file.path(PopDir, "data/opened/IR"))
}

# creates lists of dataframes and cleans it up, adding NA columns where mutated variables are absent 
dhs_ir_urban <- list()
dhs_ir_rural <- list()

link_ir <- list.files(path = file.path(PopDir, "data", "opened", "IR"), pattern = "DTA", full.names = TRUE, recursive = F)

for (i in 1:length(link_ir)){
  dhs_ir <- read_dta(link_ir[[i]])
  
  df <- dhs_ir %>%
    dplyr::select(matches("v000|v001|v002|v003|v005|v006|v007|v012|v021|v022|v025|v106|v157|v158|v159|v168|v190|v501|v505|v704|v704a|v705|v716|v717|v731|v732|v743a|s1108ai|s1108ba|s1108bc|s1108bd|s1108bf|b19|h11_1|h11_2|h11_3|h11_4|h11_5|h11_6")) %>%
    mutate(agri_partner=tryCatch(ifelse(v705 %in% c(4, 5), "A",ifelse(v705  %in% c(1, 2, 3, 6, 7,8, 9,96), "O", ifelse(v705 == 0, "U", NA))), error =function(e) return(NA)),
           agri_woman = tryCatch(ifelse(v717 %in% c(4, 5), "A", ifelse(v717 %in% c(1, 2, 3, 6, 7, 8, 9, 11), "O", ifelse(v717 == 0, "U", NA))), error =function(e) return(NA))) %>%
    mutate(kap_weak= tryCatch(ifelse(s1108bf == 0, 1, 0), error = function(e) return(NA))) %>%
    mutate(across(contains("s1108"), ~ifelse(.x >=8, NA, .x), .names = "new_{.col}")) %>%
    mutate(kap_index = if("new_s1108ai" %in% colnames(.)) (new_s1108ai+ new_s1108ba + new_s1108bc + new_s1108bd + kap_weak)/5 else NA) %>%
    dplyr::rename(strat=v022, id=v021, dhs_year = v007, edu_woman  = v106, age_woman = v012,wealth = v190) %>%
    dplyr::select(-c(starts_with("s1108"))) %>%
    unite(col = "HH_occ", agri_partner, agri_woman, na.rm=TRUE, sep="", remove = FALSE) %>%
    mutate(HH_occ = ifelse(HH_occ == "", "M", HH_occ)) %>%
    mutate(occ_val = ifelse(grepl("A", HH_occ), "A", HH_occ)) %>%
    mutate(occ_val = ifelse(grepl("O", occ_val), "O", occ_val)) %>%
    mutate(occ_val = ifelse(grepl("U", occ_val), "U", occ_val)) %>%
    mutate(occ_val = factor(occ_val, levels = c("A", "O", "U", "M")), wt=v005/1000000)
  
  df <- df %>%  mutate(DHS_CountryCode = str_sub(v000, 1, 2)) %>%  left_join(ids) %>%
    mutate( min_year =min(dhs_year), code_year = paste0(DHS_CountryCode, min_year))
  
  df_urban <- df %>%  filter(v025 == 1)
  
  print(paste("appending urban data from", unique(df_urban[["CountryName"]]), "to list of data frames"))
  dhs_ir_urban <- append(dhs_ir_urban, list(df_urban))
  
  df_rural <- df %>%  filter(v025 == 2)
  
  print(paste("appending rural data from", unique(df_urban[["CountryName"]]), "to list of data frames"))
  dhs_ir_rural <- append(dhs_ir_rural, list(df_rural))
}

#lapply(dhs_ir_urban, function(x) table(x$h11_6)) #had diarrhea recently 
#lapply(dhs_ir_rural, function(x) table(x$h11_1))

saveRDS(dhs_ir_urban, file.path(PopDir, "analysis_dat/dhs_ir_urban.rds"))
dhs_ir_urban<- readRDS(file.path(PopDir, "analysis_dat/dhs_ir_urban.rds"))
saveRDS(dhs_ir_rural, file.path(PopDir, "analysis_dat/dhs_ir_rural.rds"))

# filter list to keep only datasets with agriculture workers that are partners as this eliminates countries with no agricultural data 
#dhs_ir_urban <- dhs_ir_urban %>%purrr::discard(~all(is.na(.x$agri_partner)))
#dhs_ir_rural <- dhs_ir_rural %>%purrr::discard(~all(is.na(.x$agri_partner)))

## ---------------------------------------------------------------------------------------------------------------------
### Data loading and transforming MR datasets
## ---------------------------------------------------------------------------------------------------------------------

# download MR datasets 
mr_datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "DT", fileType = "MR") #%>%
#group_by(CountryName) %>%
#slice_max(SurveyYear)
# 
# 
mr_downloads <- list()
# 
for (i in 1:nrow(mr_datasets)){
  tryCatch({
    df <- get_datasets(mr_datasets$FileName[i], download_option = "zip", verbose_argument= T)
    mr_downloads<- append(mr_downloads, df)
    # # 
  }, error = function(e){cat("ERROR:", conditionMessage(e), "\n")})
  # # 
}
# 
saveRDS(mr_downloads, file.path(PopDir, "analysis_dat/mr_downloads.rds"))
mr_downloads<- readRDS(file.path(PopDir, "analysis_dat/mr_downloads.rds"))
# 
# download all 36 datasets for MR
data.frame(SurveyId = setdiff(survs$SurveyId, mr_datasets$SurveyId))


dhs_mr_urban <- list()
dhs_mr_rural <- list()
# 
for (i in 1:length(mr_downloads)){
  
  unzip(mr_downloads[[i]],  exdir = file.path(PopDir, "data/opened/MR"))
  link_mr <- list.files(path = file.path(PopDir, "data", "opened", "MR"), pattern = "DTA", full.names = TRUE, recursive = F)
  dhs_mr <- read_dta(link_mr[[i]])
  
  
  df <- dhs_mr %>%
    dplyr::select(matches("mv000|mv001|mv002|mv005|mv007|mv012|mv022|mv021|mv025|mv106|mv190|mv717")) %>%
    mutate(agric_work_man_response = tryCatch(ifelse(mv717 %in% c(4, 5), "A", ifelse(mv717 %in% c(1, 2, 3, 6, 7, 8, 9, 11), "O", ifelse(mv717 == 0, "U", NA))), error =function(e) return(NA))) %>%
    dplyr::rename(strat=mv022, id=mv021, dhs_year = mv007, edu_woman  = mv106, age_woman = mv012,wealth = mv190) %>%
    mutate(wt=mv005/1000000)
  
  
  df <- df %>%  mutate(DHS_CountryCode = str_sub(mv000, 1, 2)) %>%  left_join(ids) %>%
    mutate( min_year =min(dhs_year), code_year = paste0(DHS_CountryCode, min_year)) %>%
    mutate(agric_work_man_response2= ifelse(is.na(agric_work_man_response), "M", agric_work_man_response),
           agric_work_man_response2 = factor(agric_work_man_response2, levels = c("A", "O", "U", "M")))
  
  
  df_urban <- df %>%  filter(mv025 == 1) #filter to urban
  
  print(paste("appending urban data from", unique(df_urban[["CountryName"]]), "to list of data frames"))
  dhs_mr_urban <- append(dhs_mr_urban, list(df_urban))
  
  df_rural <- df %>%  filter(mv025 == 2)
  
  print(paste("appending rural data from", unique(df_urban[["CountryName"]]), "to list of data frames"))
  dhs_mr_rural <- append(dhs_mr_rural, list(df_rural))
}

saveRDS(dhs_mr_urban, file.path(PopDir, "analysis_dat/dhs_mr_urban.rds"))
#dhs_mr_urban <- readRDS("dhs_mr_urban.rds")

saveRDS(dhs_mr_rural, file.path(PopDir, "analysis_dat/dhs_mr_rural.rds"))
#dhs_mr_rural <- readRDS("dhs_mr_rural.rds")

## ---------------------------------------------------------------------------------------------------------------------
### Data loading and transforming PR datasets
## ---------------------------------------------------------------------------------------------------------------------

# download PR datasets 
pr_datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "DT", fileType = "PR") #%>%
#group_by(CountryName) %>%
#slice_max(SurveyYear)

pr_downloads <- list()

for (i in 1:nrow(pr_datasets)){
  # if (ir_datasets$FileName[i] == "MDIR81DT.ZIP"){ # for some reason, I am not able to download this file with my credentials so downloading manually
  #   df <- list("MDIR81FL"="project_one/datasets/MDIR81FL.ZIP") #manually create link for madagascar 2021
  #   ir_downloads<- append(ir_downloads, df)
  # }
  # sending ir data link to the folder "project_one
  tryCatch({
    df <- get_datasets(pr_datasets$FileName[i], download_option = "zip", verbose_argument= T)
    pr_downloads<- append(pr_downloads, df)
    
  }, error = function(e){cat("ERROR:", conditionMessage(e), "\n")})
  
}

saveRDS(pr_downloads, file.path(PopDir, "analysis_dat/pr_downloads.rds"))
pr_downloads<- readRDS(file.path(PopDir, "analysis_dat/pr_downloads.rds"))

# creates lists of dataframes and cleans it up, adding NA columns where mutated variables are absent 
# dhs_pr_urban <- list()
# dhs_pr_rural <- list()

# download all 36 datasets for PR
data.frame(SurveyId = setdiff(survs$SurveyId, pr_datasets$SurveyId))

dhs_pr_urban <- list()
dhs_pr_rural <- list()
# 
for (i in 1:length(pr_downloads)){
  unzip(pr_downloads[[i]],  exdir = file.path(PopDir, "data/opened/PR"))
}

link_pr <- list.files(path = file.path(PopDir, "data", "opened", "PR"), pattern = "DTA", full.names = TRUE, recursive = F)

for (i in 1:length(link_pr)){
  
  dhs_pr <- read_dta(link_pr[[i]])
  
  df <- dhs_pr %>%
    dplyr::select(matches("hc1|hc60|hml12|hml16a|hml32|hml35|hv000|hv001|hv002|hv003|hv005|hv006|hv007|hv009|hv021|hv025|hv022|hv042|hv103|hv213|hv214|hv215|hv253|hv270|hvidx|sh418|sh511"))
  
  if(paste0(unique(df[["hv000"]]), unique(df[["hv007"]]))[1] == "CM62011"){
    
    df <- df %>%  mutate(test_result = ifelse(sh418 %in% c(1, 2, 3), "+ve", ifelse(sh418 == 4, "-ve", NA)))
    
  }else if(paste0(unique(df[["hv000"]]), unique(df[["hv007"]]))[1] == "TZ52007") {
    
    df <- df %>%  mutate(test_result = ifelse(sh511 ==1, "+ve", ifelse(sh511 ==0,  "-ve", NA)))
    
  }else {
    
    df <- df %>% mutate(hml32 = tryCatch(ifelse(hml32 > 1, NA, hml32), error = function(e) return(NA)),
                        hml35 = tryCatch(ifelse(hml35 > 1, NA, hml35), error = function(e) return(NA)),
                        test_result=tryCatch(ifelse(!is.na(hml32), hml32, hml35), error =function(e) return(NA)),
                        test_result = ifelse(test_result==1, "+ve",ifelse(test_result==0, "-ve", NA)), test_result = as.character(test_result))
  }
  
  #child_age =tryCatch(ifelse(!is.na(hc1), hc1, hml16a), error = function(e) return(NA)),
  
  df <- df %>%  mutate(u5_net_use = tryCatch(ifelse(hml12 %in% c(1,2), 1,0), error = function(e) return(NA)),
                       floor_type = ifelse(hv213 == 30| hv213 == 31|hv213 == 33| hv213 == 34|hv213 == 35,1, 0),
                       wall_type = ifelse(hv214 == 30| hv214 == 31| hv214 == 33| hv214 == 34,0, 1),
                       roof_type = ifelse(hv215 == 30| hv215 == 31|hv215 == 33| hv215 == 34|hv215 == 35|hv215 == 36,1, 0),
                       wealth = hv270,
                       interview_month = hv006,
                       wt=hv005/1000000) %>%
    dplyr::rename(strat = hv022, id = hv021, dhs_year = hv007, hh_size = hv009)
  
  df <- df %>%  mutate(DHS_CountryCode = str_sub(hv000, 1, 2)) %>%left_join(ids) %>%
    mutate( min_year =min(dhs_year), code_year = paste0(DHS_CountryCode, min_year))
  
  df_urban <- df %>%  filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59), hv025 == 1) #filter to urban
  
  print(paste("appending urban data from", unique(df_urban[["CountryName"]]), "to list of data frames"))
  dhs_pr_urban <- append(dhs_pr_urban, list(df_urban))
  
  df_rural <- df %>%  filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59), hv025 == 2) #filter to rural
  
  print(paste("appending rural data from", unique(df_urban[["CountryName"]]), "to list of data frames"))
  dhs_pr_rural <- append(dhs_pr_rural, list(df_rural))
}

lapply(dhs_pr_urban, function(x) table(x$hv253))
lapply(dhs_pr_rural, function(x) table(x$hv253))


# filter list to keep only datasets with malaria test data  
dhs_pr_urban <- dhs_pr_urban %>%purrr::discard(~all(is.na(.x$test_result)))
dhs_pr_rural <- dhs_pr_rural %>%purrr::discard(~all(is.na(.x$test_result)))

saveRDS(dhs_pr_urban, file.path(PopDir, "analysis_dat/dhs_pr_urban.rds"))
dhs_pr_urban<-readRDS(file.path(PopDir, "analysis_dat/dhs_pr_urban.rds"))

saveRDS(dhs_pr_rural, file.path(PopDir, "analysis_dat/dhs_pr_rural.rds"))
dhs_pr_rural<-readRDS(file.path(PopDir, "analysis_dat/dhs_pr_rural.rds"))

## ---------------------------------------------------------------------------------------------------------------------
### Get survey IDs for datasets with >1% positive malaria tests and the most recent survey
## ---------------------------------------------------------------------------------------------------------------------
df <- plot_u_df  %>% mutate(cntryId = stringr::str_extract(code_year, "^.{2}")) %>%  
  mutate(year = parse_number(code_year)) %>% group_by(cntryId) %>% slice_max(year) %>%  
  filter(test_result == "+ve" & percent > 1) %>% ungroup()%>%  select(code_year) 


write_csv(df, file.path(PopDir, "analysis_dat/final_surveys.csv"))

## ---------------------------------------------------------------------------------------------------------------------
### Create analysis datasets
## ---------------------------------------------------------------------------------------------------------------------

ir_urban <- dhs_ir_urban %>%purrr::keep(~all(.x$code_year %in% df$code_year))
ir_rural <- dhs_ir_rural %>%purrr::keep(~all(.x$code_year %in% df$code_year))

mr_urban <- dhs_mr_urban %>%purrr::keep(~all(.x$code_year %in% df$code_year))
mr_rural <- dhs_mr_rural %>%purrr::keep(~all(.x$code_year %in% df$code_year))

pr_urban <- dhs_pr_urban %>%purrr::keep(~all(.x$code_year %in% df$code_year))
pr_rural <- dhs_pr_rural %>%purrr::keep(~all(.x$code_year %in% df$code_year))


# combine all IR datasets
ir_urban %>% map(~dim(.x)[[2]]) # list column dimensions to get smallest column length in list and position
ir_urban<- ir_urban %>%map(~dplyr::select(., colnames(ir_urban[[5]])))
ir_urban <- plyr::ldply(ir_urban)

ir_rural %>% map(~dim(.x)[[2]])
ir_rural<- ir_rural %>%map(~dplyr::select(., colnames(ir_rural[[5]])))
ir_rural <- plyr::ldply(ir_rural)

# combine all MR datasets
mr_urban %>% map(~dim(.x)[[2]])
mr_urban<- mr_urban %>%map(~dplyr::select(., colnames(mr_urban[[10]])))
mr_urban <- plyr::ldply(mr_urban)

mr_rural %>% map(~dim(.x)[[2]])
mr_rural<- mr_rural %>%map(~dplyr::select(., colnames(mr_rural[[10]])))
mr_rural<- plyr::ldply(mr_rural)

# combine all PR datasets
pr_urban %>% map(~dim(.x)[[2]])
pr_urban<- pr_urban %>%map_if(~all(c('sh511') %in% colnames(.x)), ~dplyr::select(., -sh511))
pr_urban<- pr_urban %>%map(~dplyr::select(., colnames(pr_urban[[5]])%>% discard(~ .x %in% c("hc11", "hc10", "hc12"))))
pr_urban <- plyr::ldply(pr_urban)

pr_rural %>% map(~dim(.x)[[2]])
pr_rural<- pr_rural %>%map_if(~all(c('sh511') %in% colnames(.x)), ~dplyr::select(., -sh511))
pr_rural<- pr_rural %>%map(~dplyr::select(., colnames(pr_rural[[5]])%>% discard(~ .x %in% c("hc11", "hc10", "hc12"))))
pr_rural<- plyr::ldply(pr_rural)

# save compiled datasets
saveRDS(ir_rural, file.path(PopDir, "analysis_dat/ir_rural.rds"))
ir_rural<-readRDS(file.path(PopDir,"analysis_dat/ir_rural.rds"))

saveRDS(mr_urban, file.path(PopDir, "analysis_dat/mr_urban.rds"))
mr_urban<-readRDS(file.path(PopDir,"analysis_dat/mr_urban.rds"))

saveRDS(mr_rural, file.path(PopDir, "analysis_dat/mr_rural.rds"))
mr_rural<-readRDS(file.path(PopDir,"analysis_dat/mr_rural.rds"))

saveRDS(pr_urban, file.path(PopDir, "analysis_dat/pr_urban.rds"))
pr_urban<-readRDS(file.path(PopDir,"analysis_dat/pr_urban.rds"))

saveRDS(pr_rural, file.path(PopDir, "analysis_dat/pr_rural.rds"))
pr_rural<-readRDS(file.path(PopDir,"analysis_dat/pr_rural.rds"))

## ---------------------------------------------------------------------------------------------------------------------
### Combine Urban Datasets
## ---------------------------------------------------------------------------------------------------------------------

# urban IR and MR
hh_mr_urban = mr_urban %>%  group_by(code_year, mv001, mv002) %>%  summarise(agric_man = paste0(agric_work_man_response2, collapse = ""), n_men= n())

ir_mr_urban = ir_urban %>%    left_join(hh_mr_urban, by =c("code_year", "v001" = "mv001", "v002" = "mv002")) %>% 
  mutate(agric_partner_other = ifelse(is.na(agri_partner), agric_man, agri_partner)) %>% 
  unite(col = "HH_occ2", agric_partner_other, agri_woman, na.rm=TRUE, sep="", remove = FALSE) %>%
  mutate(HH_occ3 = ifelse(HH_occ2 == "", "M", HH_occ2)) %>%
  mutate(occ_val3 = ifelse(grepl("A", HH_occ3), "A", HH_occ3)) %>%
  mutate(occ_val4 = ifelse(grepl("O", occ_val3), "O", occ_val3)) %>%
  mutate(occ_val5 = ifelse(grepl("U", occ_val4), "U", occ_val4)) 

ir_mr_urban = ir_mr_urban %>%  mutate(occ_val6 = ifelse(is.na(occ_val5), "M", occ_val5),
                                      occ_val7= ifelse(grepl("M", occ_val6), "M", occ_val6)) %>%
  mutate(occ_val7 = factor(occ_val7, levels = c("A", "O", "U", "M")), wt=v005/1000000)

# pick up diarrheal disease values 
ir_mr_urban <- ir_mr_urban %>%  dplyr::mutate(across(c(h11_1:h11_6), ~ dplyr::case_when(. %in% c("2") ~ "1",
                                                                                        . %in% c("8", "9") ~ NA,
                                                                                        TRUE ~ as.character(.)), .names = "rec_{col}")) %>%
  mutate(across(starts_with("rec"), ~as.numeric(.))) %>%
  mutate(sum_dia = rowSums(select(., contains("rec_")), na.rm =TRUE))

# plot to view urban agric data and save dataset  
dhs_ir_mr_urban <- ir_mr_urban %>% group_by(code_year) %>% mutate(max_year = max(dhs_year)) %>% ungroup() %>% 
  mutate(year_combo = ifelse(max_year == min_year, max_year, paste(min_year, "-",str_sub(max_year, -2))),
         country_year = paste0(CountryName, " ", year_combo))

#create HH exposure data  
hh_ir_urban = ir_mr_urban %>% group_by(code_year) %>% 
  mutate(max_year = max(dhs_year)) %>% ungroup() %>% 
  mutate(year_combo = ifelse(max_year == min_year, max_year, paste(min_year, "-",str_sub(max_year, -2))),
         country_year = paste0(CountryName, " ", year_combo)) %>% 
  group_by(country_year, code_year,v001,v002) %>% 
  summarise(n = n(), home_type = paste0(occ_val7, collapse = ""), total_diarrhea = sum(sum_dia, na.rm =TRUE)) %>% 
  mutate(home_type2 = ifelse(grepl("A", home_type), "A", "O")) %>%  
  drop_na(home_type2) %>% ungroup()

#now join to pr dataset  (the pr dataset is 28300 and the hh ir dataset is 68-39. joining resulted in 26153 obs after filtering missing test and home type data)
urban_df=left_join(pr_urban, hh_ir_urban, by =c("code_year", "hv001" ="v001", "hv002"="v002")) %>%
  filter(!is.na(test_result)) %>% 
  filter(!is.na(home_type2))

write_csv(urban_df, file.path(PopDir, "analysis_dat/240606_urban_df_for_analysis.csv"))

## ---------------------------------------------------------------------------------------------------------------------
### Combine Rural Datasets
## ---------------------------------------------------------------------------------------------------------------------

# rural IR and MR
hh_mr_rural = mr_rural %>%  group_by(code_year, mv001, mv002) %>%  summarise(agric_man = paste0(agric_work_man_response2, collapse = ""), n_men= n())

ir_mr_rural = ir_rural %>%  left_join(hh_mr_rural, by =c("code_year", "v001" = "mv001", "v002" = "mv002")) %>% 
  mutate(agric_partner_other = ifelse(is.na(agri_partner), agric_man, agri_partner)) %>% 
  unite(col = "HH_occ2", agric_partner_other, agri_woman, na.rm=TRUE, sep="", remove = FALSE) %>%
  mutate(HH_occ3 = ifelse(HH_occ2 == "", "M", HH_occ2)) %>%
  mutate(occ_val3 = ifelse(grepl("A", HH_occ3), "A", HH_occ3)) %>%
  mutate(occ_val4 = ifelse(grepl("O", occ_val3), "O", occ_val3)) %>%
  mutate(occ_val5 = ifelse(grepl("U", occ_val4), "U", occ_val4)) 

ir_mr_rural = ir_mr_rural %>%  mutate(occ_val6 = ifelse(is.na(occ_val5), "M", occ_val5),
                                      occ_val7= ifelse(grepl("M", occ_val6), "M", occ_val6)) %>%
  mutate(occ_val7 = factor(occ_val7, levels = c("A", "O", "U", "M")), wt=v005/1000000)

# pick up diarrheal disease values 
ir_mr_rural <- ir_mr_rural %>%  dplyr::mutate(across(c(h11_1:h11_6), ~ dplyr::case_when(. %in% c("2") ~ "1",
                                                                                        . %in% c("8", "9") ~ NA,
                                                                                        TRUE ~ as.character(.)), .names = "rec_{col}")) %>%
  mutate(across(starts_with("rec"), ~as.numeric(.))) %>%
  mutate(sum_dia = rowSums(select(., contains("rec_")), na.rm =TRUE))


# plot to view rural agric data and save dataset  
dhs_ir_mr_rural <- ir_mr_rural %>% group_by(code_year) %>% mutate(max_year = max(dhs_year)) %>% ungroup() %>% 
  mutate(year_combo = ifelse(max_year == min_year, max_year, paste(min_year, "-",str_sub(max_year, -2))),
         country_year = paste0(CountryName, " ", year_combo))

# create HH exposure data  
hh_ir_rural =  ir_mr_rural%>%  group_by(code_year) %>% 
  mutate(max_year = max(dhs_year)) %>% ungroup() %>% 
  mutate(year_combo = ifelse(max_year == min_year, max_year, paste(min_year, "-",str_sub(max_year, -2))),
         country_year = paste0(CountryName, " ", year_combo)) %>% 
  group_by(country_year, code_year,v001,v002) %>% 
  summarise(n = n(), home_type = paste0(occ_val7, collapse = ""), total_diarrhea = sum(sum_dia, na.rm =TRUE)) %>% 
  mutate(home_type2 = ifelse(grepl("A", home_type), "A", "O")) %>%  
  drop_na(home_type2) %>% ungroup()

# now join to pr dataset (the pr dataset is 57231 and the hh ir dataset is 112876. joining resulted in 53464 obs after filtering missing test and home type data)
rural_df=left_join(pr_rural, hh_ir_rural, by =c("code_year", "hv001" ="v001", "hv002"="v002")) %>%
  filter(!is.na(test_result)) %>% 
  filter(!is.na(home_type2))

write_csv(rural_df, file.path(PopDir, "analysis_dat/240606_rural_df_for_analysis.csv"))


## =========================================================================================================================================
### COMPILE GEOSPATIAL DATA
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Reading and Processing DHS GPS Coordinates
## -----------------------------------------------------------------------------------------------------------------------------------------

# list all shapefiles in the DHS_coordinate folder with the .shp extension
ge_files <-  list.files(path = file.path(PopDir, "DHS_coordinate"), 
                        pattern = "*FL.shp$", full.names = TRUE, recursive = T) 

# read the shapefiles and create a new column 'cntry_year' that combines 'DHSCC' and 'DHSYEAR'
all_GPS <- lapply(ge_files, st_read) %>% 
  purrr::map(~mutate(., cntry_year = paste0(DHSCC, "_", DHSYEAR)))

# handle the case for 'SN_2012', splitting it into 'SN_2012a' and 'SN_2012b'
# create a list that will store two data frames:
# 1. a data frame of unique 'cntry_year' values from all_GPS where "SN_2012" is replaced with "SN_2012a"
# 2. a data frame of unique 'cntry_year' values from all_GPS where "SN_2012" is replaced with "SN_2012b"
cntry_years <- list(
  # bind rows of all GPS data into a single data frame, extract 'cntry_year', get unique values
  # if 'cntry_year' is "SN_2012", rename it to "SN_2012a"
  all_GPS %>% bind_rows() %>% as.data.frame() %>% 
    select(cntry_year) %>% unique() %>% 
    mutate(cntry_year = ifelse(cntry_year == "SN_2012", "SN_2012a", cntry_year)),
  
  # rename 'SN_2012' to "SN_2012b" instead
  all_GPS %>% bind_rows() %>% as.data.frame() %>% 
    select(cntry_year) %>% unique() %>% 
    mutate(cntry_year = ifelse(cntry_year == "SN_2012", "SN_2012b", cntry_year))
) %>% 
  bind_rows() %>% # bind the two data frames in the list into a single data frame
  unique() %>% # ensure only unique 'cntry_year' values are kept
  arrange(cntry_year) # sort the data by 'cntry_year' in ascending order

# update the names of the all_GPS list based on the 'cntry_years' column
names(all_GPS) <- c(cntry_years$cntry_year)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Reading and Processing DHS Datasets to Obtain Survey Month
## -----------------------------------------------------------------------------------------------------------------------------------------

# read DHS PR (Person Records) files
# list all files in the specified directory that match the pattern "*FL.DTA"
pr_files <- list.files(path = file.path(PopDir, "data/opened/PR"), 
                       pattern = "*FL.DTA$", full.names = TRUE, recursive = F) 

# read each file listed in pr_files into R
# lapply applies the read_dta function to each file path in pr_files
pr_downloads <- lapply(pr_files, read_dta)


##processing by country _ attempt to shorten code 
# dhs_dat <- list()
# GPS_dat <- list()
# 
# for (i in 1:length(pr_downloads)){
#   dhs_all1 <- list(pr_downloads[[i]])
#   names(dhs_all1) <- paste0(str_sub(pr_downloads[[i]][1, "hv000"], 1, 2), "_", min(pr_downloads[[i]]$hv007))
#   print(names(dhs_all1))
#   names(dhs_all1) <- ifelse(names(dhs_all1) == "BJ_2011", "BJ_2012", names(dhs_all1))
#   names(dhs_all1) <- ifelse(names(dhs_all1) == "CI_2011", "CI_2012", names(dhs_all1))
#   names(dhs_all1) <- ifelse(names(dhs_all1) == "MR_2019", "MR_2020", names(dhs_all1))
#   dhs_all<- dhs_all1 %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
#     map(~distinct(.,)) #get cluster numbers by month and survey year
#   
#   survey_gps_dat <- survey_gps_comb(x= names(dhs_all1), y= names(dhs_all1))
#   for(i in seq_along(survey_gps_dat)) {names(survey_gps_dat)[[i]] <- paste0(unique(survey_gps_dat[[i]]$DHSCC), "_",
#     unique(survey_gps_dat[[i]]$hv007), '_', unique(survey_gps_dat[[i]]$hv006))}
#   
#   GPS_all <- sapply(c(survey_gps_dat), sf:::as_Spatial, simplify = F)
#   dhs_dat <- append(dhs_dat, dhs_all)
#   print(paste("appending", names(dhs_all), "to list of DHS dataframes"))
#   GPS_dat <- append(GPS_dat, GPS_all)
#   print(paste("appending", paste0(unique(GPS_all[[i]]$DHSCC),"_", unique(GPS_all[[i]]$DHSYEAR)), "to list of GPS dataframes"))
# }

## =========================================================================================================================================
### Processing DHS Data by Country
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Angola
## -----------------------------------------------------------------------------------------------------------------------------------------

# angola - need to fix manual labeling

# initialize a list with the first element of pr_downloads
dhs_all1 <- list(pr_downloads[[1]])

# create a name for the list element based on the first two characters of hv000
# and the minimum value of hv007 in the dataset
names(dhs_all1) <- paste0(str_sub(pr_downloads[[1]][1, "hv000"], 1, 2), "_", min(pr_downloads[[1]]$hv007))
print(names(dhs_all1)) # verify the labeling

# create a new list dhs_all by selecting specific columns from dhs_all1
# and removing duplicate rows to get cluster numbers by month and survey year
dhs_all <- dhs_all1 %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) # get cluster numbers by month and survey year

# combine GPS data with survey information for the year 2015
AO_2015 <- survey_gps_comb(x= names(dhs_all1), y= names(dhs_all1))

# loop through each element in AO_2015 to update the names
for(i in seq_along(AO_2015)) {
  # set the name based on unique values of hv007 and hv006
  names(AO_2015)[[i]] <- paste0(unique(AO_2015[[i]]$hv007), '_', unique(AO_2015[[i]]$hv006))
}

# convert each element of AO_2015 to a spatial object and simplify the list structure
GPS_all_OA <- sapply(c(AO_2015), sf:::as_Spatial, simplify = F)

# print the names of GPS_all_OA to verify the conversion
names(GPS_all_OA) 

# assign dhs_all to dhs_all_OA for further analysis
dhs_all_OA <- dhs_all

## -----------------------------------------------------------------------------------------------------------------------------------------
### Burkina Faso
## -----------------------------------------------------------------------------------------------------------------------------------------

# initialize a list with the second and third elements of pr_downloads
dhs_all1 <- list(pr_downloads[[2]], pr_downloads[[3]])

# create names for each list element based on the first two characters of hv000
# and the minimum value of hv007 for each respective dataset
name1 <- paste0(str_sub(pr_downloads[[2]][1, "hv000"], 1, 2), "_", min(pr_downloads[[2]]$hv007))
name2 <- paste0(str_sub(pr_downloads[[3]][1, "hv000"], 1, 2), "_", min(pr_downloads[[3]]$hv007))

# assign the created names to the list dhs_all1
names(dhs_all1) <- c(name1, name2)
print(names(dhs_all1)) # verify labeling

# create a new list dhs_all by selecting specific columns from dhs_all1
# and removing duplicate rows to get cluster numbers by month and survey year
dhs_all <- dhs_all1 %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) # get cluster numbers by month and survey year

# combine GPS data with survey information for the year 2010
BF_2010 <- survey_gps_comb(x= name1, y= name1)

# loop through each element in BF_2010 to update the names
for(i in seq_along(BF_2010)) {
  # set the name based on unique values of hv007 and hv006
  names(BF_2010)[[i]] <- paste0(unique(BF_2010[[i]]$hv007), '_', unique(BF_2010[[i]]$hv006))
}

# combine GPS data with survey information for the year 2021
BF_2021 <- survey_gps_comb(x= name2, y= name2)

# loop through each element in BF_2021 to update the names
for(i in seq_along(BF_2021)) {
  # set the name based on unique values of hv007 and hv006
  names(BF_2021)[[i]] <- paste0(unique(BF_2021[[i]]$hv007), '_', unique(BF_2021[[i]]$hv006))
}

# convert each element of BF_2010 and BF_2021 to a spatial object and simplify the list structure
GPS_all_BF <- sapply(c(BF_2010, BF_2021), sf:::as_Spatial, simplify = F)

# print the names of GPS_all_BF to verify the conversion
names(GPS_all_BF) 

# assign dhs_all to dhs_all_BF for further analysis
dhs_all_BF <- dhs_all

## -----------------------------------------------------------------------------------------------------------------------------------------
### Benin - rest of countries follow same data processing steps
## -----------------------------------------------------------------------------------------------------------------------------------------

dhs_all1 <- list(pr_downloads[[4]], pr_downloads[[5]])
name1 <- paste0(str_sub(pr_downloads[[4]][1, "hv000"], 1, 2), "_", max(pr_downloads[[4]]$hv007))
name2 <- paste0(str_sub(pr_downloads[[5]][1, "hv000"], 1, 2), "_", min(pr_downloads[[5]]$hv007))
names(dhs_all1) <- c(name1, name2)
print(names(dhs_all1))


dhs_all <- dhs_all1 %>% map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year

BJ_2012 <- survey_gps_comb(x= name1, y= name1)
for(i in seq_along(BJ_2012)) {names(BJ_2012)[[i]] <- paste0(unique(BJ_2012[[i]]$hv007), '_', unique(BJ_2012[[i]]$hv006))}

BJ_2017 <- survey_gps_comb(x= name2, y= name2)
for(i in seq_along(BJ_2017)) {names(BJ_2017)[[i]] <- paste0(unique(BJ_2017[[i]]$hv007), '_', unique(BJ_2017[[i]]$hv006))}

GPS_all_BJ <- sapply(c(BJ_2012, BJ_2017), sf:::as_Spatial, simplify = F)
names(GPS_all_BJ) 
dhs_all_BJ <- dhs_all 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Burundi
## -----------------------------------------------------------------------------------------------------------------------------------------

dhs_all1 <- list(pr_downloads[[6]])
names(dhs_all1) <- paste0(str_sub(pr_downloads[[6]][1, "hv000"], 1, 2), "_", min(pr_downloads[[6]]$hv007))
print(names(dhs_all1))

dhs_all <- dhs_all1 %>% map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year

BU_2016 <- survey_gps_comb(x= names(dhs_all1), y= names(dhs_all1))
for(i in seq_along(BU_2016)) {names(BU_2016)[[i]] <- paste0(unique(BU_2016[[i]]$hv007), '_', unique(BU_2016[[i]]$hv006))}

GPS_all_BU <- sapply(c(BU_2016), sf:::as_Spatial, simplify = F)
names(GPS_all_BU) 
dhs_all_BU <- dhs_all 

## -----------------------------------------------------------------------------------------------------------------------------------------
### DRC
## -----------------------------------------------------------------------------------------------------------------------------------------
dhs_all <- list(pr_downloads[[7]])
names(dhs_all) <- paste0(str_sub(pr_downloads[[7]][1, "hv000"], 1, 2), "_", min(pr_downloads[[7]]$hv007))
print(names(dhs_all))

dhs_all <- dhs_all %>% map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year

CD_2013 <- survey_gps_comb(x= "CD_2013", y= "CD_2013")
for(i in seq_along(CD_2013)) {names(CD_2013)[[i]] <- paste0(unique(CD_2013[[i]]$hv007), '_', unique(CD_2013[[i]]$hv006))}

GPS_all_CD <- sapply(c(CD_2013), sf:::as_Spatial, simplify = F)
names(GPS_all_CD) 
dhs_all_CD <- dhs_all 


## -----------------------------------------------------------------------------------------------------------------------------------------
### Cote d'Ivoire
## -----------------------------------------------------------------------------------------------------------------------------------------
dhs_all1 <- list(pr_downloads[[8]], pr_downloads[[9]])
name1 <- paste0(str_sub(pr_downloads[[8]][1, "hv000"], 1, 2), "_", max(pr_downloads[[8]]$hv007))
name2 <- paste0(str_sub(pr_downloads[[9]][1, "hv000"], 1, 2), "_", min(pr_downloads[[9]]$hv007))
names(dhs_all1) <- c(name1, name2)
print(names(dhs_all1))


dhs_all <- dhs_all1 %>% map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year

CI_2012 <- survey_gps_comb(x= name1, y= name1)
for(i in seq_along(CI_2012)) {names(CI_2012)[[i]] <- paste0(unique(CI_2012[[i]]$hv007), '_', unique(CI_2012[[i]]$hv006))}

CI_2021 <- survey_gps_comb(x= name2, y= name2)
for(i in seq_along(CI_2021)) {names(CI_2021)[[i]] <- paste0(unique(CI_2021[[i]]$hv007), '_', unique(CI_2021[[i]]$hv006))}

GPS_all_CI <- sapply(c(CI_2012, CI_2021), sf:::as_Spatial, simplify = F)
names(GPS_all_CI)
dhs_all_CI <- dhs_all

## -----------------------------------------------------------------------------------------------------------------------------------------
### Cameroon
## -----------------------------------------------------------------------------------------------------------------------------------------
dhs_all <- list(pr_downloads[[10]], pr_downloads[[11]])
name1 <- paste0(str_sub(pr_downloads[[10]][1, "hv000"], 1, 2), "_", max(pr_downloads[[10]]$hv007))
name2 <- paste0(str_sub(pr_downloads[[11]][1, "hv000"], 1, 2), "_", min(pr_downloads[[11]]$hv007))
names(dhs_all) <- c(name1, name2)
print(names(dhs_all))

dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year

CM_2011 <- survey_gps_comb(x= name1, y= name1)
for(i in seq_along(CM_2011)) {names(CM_2011)[[i]] <- paste0(unique(CM_2011[[i]]$hv007), '_', unique(CM_2011[[i]]$hv006))}

CM_2018 <- survey_gps_comb(x= name2, y= name2)
for(i in seq_along(CM_2018)) {names(CM_2018)[[i]] <- paste0(unique(CM_2018[[i]]$hv007), '_', unique(CM_2018[[i]]$hv006))}

GPS_all_CM <- sapply(c(CM_2011, CM_2018), sf:::as_Spatial, simplify = F)
names(GPS_all_CM) 
dhs_all_CM <- dhs_all 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Ghana
## -----------------------------------------------------------------------------------------------------------------------------------------
dhs_all <- list(pr_downloads[[12]], pr_downloads[[13]])
name1 <- paste0(str_sub(pr_downloads[[12]][1, "hv000"], 1, 2), "_", min(pr_downloads[[12]]$hv007))
name2 <- paste0(str_sub(pr_downloads[[13]][1, "hv000"], 1, 2), "_", min(pr_downloads[[13]]$hv007))
names(dhs_all) <- c(name1, name2)
print(names(dhs_all))


dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year


GH_2014 <- survey_gps_comb(x= name1, y= name1)
for(i in seq_along(GH_2014)) {names(GH_2014)[[i]] <- paste0(unique(GH_2014[[i]]$hv007), '_', unique(GH_2014[[i]]$hv006))}

GH_2022 <- survey_gps_comb(x= name2, y= name2)
for(i in seq_along(GH_2022)) {names(GH_2022)[[i]] <- paste0(unique(GH_2022[[i]]$hv007), '_', unique(GH_2022[[i]]$hv006))}

GPS_all_GH <- sapply(c(GH_2014, GH_2022), sf:::as_Spatial, simplify = F)
names(GPS_all_GH) 
dhs_all_GH <- dhs_all 


## -----------------------------------------------------------------------------------------------------------------------------------------
### Gambia
## -----------------------------------------------------------------------------------------------------------------------------------------
#2013 GPS data not collected
dhs_all <- list(pr_downloads[[15]])
names(dhs_all) <- paste0(str_sub(pr_downloads[[15]][1, "hv000"], 1, 2), "_", min(pr_downloads[[15]]$hv007))
print(names(dhs_all))

dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_GM <- dhs_all 

GM_2019 <- survey_gps_comb(x= names(dhs_all), y= names(dhs_all))
for(i in seq_along(GM_2019)) {names(GM_2019)[[i]] <- paste0(unique(GM_2019[[i]]$hv007), '_', unique(GM_2019[[i]]$hv006))}

GPS_all_GM <- sapply(c(GM_2019), sf:::as_Spatial, simplify = F)
names(GPS_all_GM) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Guinea
## -----------------------------------------------------------------------------------------------------------------------------------------
dhs_all <- list(pr_downloads[[16]])
names(dhs_all) <- paste0(str_sub(pr_downloads[[16]][1, "hv000"], 1, 2), "_", min(pr_downloads[[16]]$hv007))
print(names(dhs_all))

dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_GN <- dhs_all 

GN_2012 <- survey_gps_comb(x= names(dhs_all), y= names(dhs_all))
for(i in seq_along(GN_2012)) {names(GN_2012)[[i]] <- paste0(unique(GN_2012[[i]]$hv007), '_', unique(GN_2012[[i]]$hv006))}

GPS_all_GN <- sapply(c(GN_2012), sf:::as_Spatial, simplify = F)
names(GPS_all_GN) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Madagascar
## -----------------------------------------------------------------------------------------------------------------------------------------
dhs_all <- list(pr_downloads[[17]])
names(dhs_all) <- paste0(str_sub(pr_downloads[[17]][1, "hv000"], 1, 2), "_", min(pr_downloads[[17]]$hv007))
print(names(dhs_all))


dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_MD <- dhs_all


MD_2021 <- survey_gps_comb(x= names(dhs_all), y= names(dhs_all))
for(i in seq_along(MD_2021)) {names(MD_2021)[[i]] <- paste0(unique(MD_2021[[i]]$hv007), '_', unique(MD_2021[[i]]$hv006))}


GPS_all_MD <- sapply(c(MD_2021), sf:::as_Spatial, simplify = F)
names(GPS_all_MD) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Mali
## -----------------------------------------------------------------------------------------------------------------------------------------
dhs_all <- list(pr_downloads[[18]], pr_downloads[[19]])
name1 <- paste0(str_sub(pr_downloads[[18]][1, "hv000"], 1, 2), "_", min(pr_downloads[[18]]$hv007))
name2 <- paste0(str_sub(pr_downloads[[19]][1, "hv000"], 1, 2), "_", min(pr_downloads[[19]]$hv007))
names(dhs_all) <- c(name1, name2)


dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_ML <- dhs_all

ML_2012 <- survey_gps_comb(x= name1, y= name1)
for(i in seq_along(ML_2012)) {names(ML_2012)[[i]] <- paste0(unique(ML_2012[[i]]$hv007), '_', unique(ML_2012[[i]]$hv006))}


ML_2018 <- survey_gps_comb(x= name2, y= name2)
for(i in seq_along(ML_2018)) {names(ML_2018)[[i]] <- paste0(unique(ML_2018[[i]]$hv007), '_', unique(ML_2018[[i]]$hv006))}

GPS_all_ML <- sapply(c(ML_2012, ML_2018), sf:::as_Spatial, simplify = F)
names(GPS_all_ML) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Mauritania
## -----------------------------------------------------------------------------------------------------------------------------------------
dhs_all <- list(pr_downloads[[20]])
names(dhs_all) <- paste0(str_sub(pr_downloads[[20]][1, "hv000"], 1, 2), "_", median(pr_downloads[[20]]$hv007))


dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_MR <- dhs_all


MR_2020 <- survey_gps_comb(x= names(dhs_all), y= names(dhs_all))
for(i in seq_along(MR_2020)) {names(MR_2020)[[i]] <- paste0(unique(MR_2020[[i]]$hv007), '_', unique(MR_2020[[i]]$hv006))}


GPS_all_MR <- sapply(c(MR_2020), sf:::as_Spatial, simplify = F)
names(GPS_all_MR) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Mozambique
## -----------------------------------------------------------------------------------------------------------------------------------------
dhs_all <- list(pr_downloads[[21]], pr_downloads[[22]], pr_downloads[[23]])
name1 <- paste0(str_sub(pr_downloads[[21]][1, "hv000"], 1, 2), "_", min(pr_downloads[[21]]$hv007))
name2 <- paste0(str_sub(pr_downloads[[22]][1, "hv000"], 1, 2), "_", min(pr_downloads[[22]]$hv007))
name3 <- paste0(str_sub(pr_downloads[[23]][1, "hv000"], 1, 2), "_", min(pr_downloads[[23]]$hv007))
names(dhs_all) <- c(name1, name2, name3)

dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_MZ <- dhs_all

MZ_2011 <- survey_gps_comb(x= name1, y= name1)
for(i in seq_along(MZ_2011)) {names(MZ_2011)[[i]] <- paste0(unique(MZ_2011[[i]]$hv007), '_', unique(MZ_2011[[i]]$hv006))}

MZ_2015 <- survey_gps_comb(x= name2, y= name2)
MZ_2015 <- MZ_2015[1:5] #Removed one list which contained one cluster (93) that is'nt available in the PR dataset
for(i in seq_along(MZ_2015)) {names(MZ_2015)[[i]] <- paste0(unique(MZ_2015[[i]]$hv007), '_', unique(MZ_2015[[i]]$hv006))}

MZ_2022 <- survey_gps_comb(x= name3, y= name3)
for(i in seq_along(MZ_2022)) {names(MZ_2022)[[i]] <- paste0(unique(MZ_2022[[i]]$hv007), '_', unique(MZ_2022[[i]]$hv006))}


GPS_all_MZ <- sapply(c(MZ_2011, MZ_2015, MZ_2022), sf:::as_Spatial, simplify = F)
names(GPS_all_MZ) #stopped here. Not sure where the NA is coming from 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Nigeria
## -----------------------------------------------------------------------------------------------------------------------------------------
dhs_all <- list(pr_downloads[[24]])
names(dhs_all) <- paste0(str_sub(pr_downloads[[24]][1, "hv000"], 1, 2), "_", min(pr_downloads[[24]]$hv007))
print(names(dhs_all))

#read GPS data in: read_GPS_cluster_shapefiles.R

dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_NG <- dhs_all

NG_2018 <- survey_gps_comb(x= names(dhs_all), y= names(dhs_all))
for(i in seq_along(NG_2018)) {names(NG_2018)[[i]] <- paste0(unique(NG_2018[[i]]$hv007), '_', unique(NG_2018[[i]]$hv006))}

GPS_all_NG <- sapply(c(NG_2018), sf:::as_Spatial, simplify = F)
names(GPS_all_NG) # Clusters by survey month= GPS data points

## -----------------------------------------------------------------------------------------------------------------------------------------
### Rwanda
## -----------------------------------------------------------------------------------------------------------------------------------------
# No GPS coordinates for clusters were provided for the 2017 survey - stopped here
dhs_all <- list(pr_downloads[[25]], pr_downloads[[26]], pr_downloads[[27]])
name1 <- paste0(str_sub(pr_downloads[[25]][1, "hv000"], 1, 2), "_", min(pr_downloads[[25]]$hv007))
name2 <- paste0(str_sub(pr_downloads[[26]][1, "hv000"], 1, 2), "_", min(pr_downloads[[26]]$hv007))
name3 <- paste0(str_sub(pr_downloads[[27]][1, "hv000"], 1, 2), "_", min(pr_downloads[[27]]$hv007))
names(dhs_all) <- c(name1, name2, name3)
print(names(dhs_all))


dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_RW <- dhs_all

RW_2010 <- survey_gps_comb(x= name1, y= name1)
for(i in seq_along(RW_2010)) {names(RW_2010)[[i]] <- paste0(unique(RW_2010[[i]]$hv007), '_', unique(RW_2010[[i]]$hv006))}

RW_2014 <- survey_gps_comb(x= name2, y= name2)
for(i in seq_along(RW_2014)) {names(RW_2014)[[i]] <- paste0(unique(RW_2014[[i]]$hv007), '_', unique(RW_2014[[i]]$hv006))}

RW_2019 <- survey_gps_comb(x= name3, y= name3)
for(i in seq_along(RW_2019)) {names(RW_2019)[[i]] <- paste0(unique(RW_2019[[i]]$hv007), '_', unique(RW_2019[[i]]$hv006))}

GPS_all_RW <- sapply(c(RW_2010, RW_2014, RW_2019), sf:::as_Spatial, simplify = F)
names(GPS_all_RW)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Senegal
## -----------------------------------------------------------------------------------------------------------------------------------------
dhs_all <- list(pr_downloads[[28]], pr_downloads[[29]], pr_downloads[[30]], pr_downloads[[31]], pr_downloads[[32]], pr_downloads[[33]])
name1 <- paste0(str_sub(pr_downloads[[28]][1, "hv000"], 1, 2), "_", min(pr_downloads[[28]]$hv007))
#name2 <- paste0(str_sub(pr_downloads[[29]][1, "hv000"], 1, 2), "_", min(pr_downloads[[29]]$hv007))
#name3 <- paste0(str_sub(pr_downloads[[30]][1, "hv000"], 1, 2), "_", max(pr_downloads[[30]]$hv007))
name4 <- paste0(str_sub(pr_downloads[[31]][1, "hv000"], 1, 2), "_", min(pr_downloads[[31]]$hv007))
name5 <- paste0(str_sub(pr_downloads[[32]][1, "hv000"], 1, 2), "_", min(pr_downloads[[32]]$hv007))
name6 <- paste0(str_sub(pr_downloads[[33]][1, "hv000"], 1, 2), "_", min(pr_downloads[[33]]$hv007))


names(dhs_all) <- c(name1, name4, name5, name6)
print(names(dhs_all))

#names(dhs_all) <- c("SN_2010", "SN_2012a", "SN_2012b", "SN_2014", "SN_2015", "SN_2016", "SN_2017")

dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_SN <- dhs_all

dhs_all_SN[[6]] <- NULL
dhs_all_SN[[5]] <- NULL

SN_2010 <- survey_gps_comb(x= name1, y= name1)
for(i in seq_along(SN_2010)) {names(SN_2010)[[i]] <- paste0(unique(SN_2010[[i]]$hv007), '_', unique(SN_2010[[i]]$hv006))}

#SN_2012a <- survey_gps_comb(x= "SN_2012a", y= "SN_2012a")
#for(i in seq_along(SN_2012a)) {names(SN_2012a)[[i]] <- paste0(unique(SN_2012a[[i]]$hv007), '_', unique(SN_2012a[[i]]$hv006))}

# SN_2012 <- survey_gps_comb(x= name2, y= name2)
# for(i in seq_along(SN_2012)) {names(SN_2012)[[i]] <- paste0(unique(SN_2012[[i]]$hv007), '_', unique(SN_2012[[i]]$hv006))}

# SN_2014 <- survey_gps_comb(x= name3, y= name3)
# for(i in seq_along(SN_2014)) {names(SN_2014)[[i]] <- paste0(unique(SN_2014[[i]]$hv007), '_', unique(SN_2014[[i]]$hv006))}

##### get an error with first line here: SN_2015 does not exist
SN_2015 <- survey_gps_comb(x= name4, y= name4)
for(i in seq_along(SN_2015)) {names(SN_2015)[[i]] <- paste0(unique(SN_2015[[i]]$hv007), '_', unique(SN_2015[[i]]$hv006))}
SN_2015[[10]] <- NULL

SN_2016 <- survey_gps_comb(x=  name5, y=  name5)
for(i in seq_along(SN_2016)) {names(SN_2016)[[i]] <- paste0(unique(SN_2016[[i]]$hv007), '_', unique(SN_2016[[i]]$hv006))}
SN_2016[[11]] <- NULL

SN_2017 <- survey_gps_comb(x= name6, y= name6)
for(i in seq_along(SN_2017)) {names(SN_2017)[[i]] <- paste0(unique(SN_2017[[i]]$hv007), '_', unique(SN_2017[[i]]$hv006))}
SN_2017[[10]] <- NULL

GPS_all_SN <- sapply(c(SN_2010, SN_2015, SN_2016, SN_2017), sf:::as_Spatial, simplify = F)
names(GPS_all_SN)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Togo
## -----------------------------------------------------------------------------------------------------------------------------------------
dhs_all <- list(pr_downloads[[34]])
names(dhs_all) <- paste0(str_sub(pr_downloads[[34]][1, "hv000"], 1, 2), "_", min(pr_downloads[[34]]$hv007))
print(names(dhs_all))


dhs_all <- dhs_all %>% map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_TG <- dhs_all

TG_2013 <- survey_gps_comb(x= names(dhs_all), y= names(dhs_all))
for(i in seq_along(TG_2013)) {names(TG_2013)[[i]] <- paste0(unique(TG_2013[[i]]$hv007), '_', unique(TG_2013[[i]]$hv006))}

GPS_all_TG <- sapply(c(TG_2013), sf:::as_Spatial, simplify = F)
names(GPS_all_TG)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Tanzania
## -----------------------------------------------------------------------------------------------------------------------------------------
dhs_all <- list(pr_downloads[[36]], pr_downloads[[37]])
names1 <- paste0(str_sub(pr_downloads[[36]][1, "hv000"], 1, 2), "_", max(pr_downloads[[36]]$hv007))
names2 <- paste0(str_sub(pr_downloads[[37]][1, "hv000"], 1, 2), "_", min(pr_downloads[[37]]$hv007))
names(dhs_all) <- c(names1, names2)
print(names(dhs_all))


dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_TZ <- dhs_all

#### error on first line
TZ_2012 <- survey_gps_comb(x= names1, y= names1)
for(i in seq_along(TZ_2012)) {names(TZ_2012)[[i]] <- paste0(unique(TZ_2012[[i]]$hv007), '_', unique(TZ_2012[[i]]$hv006))}

TZ_2015 <- survey_gps_comb(x= names2, y= names2)
for(i in seq_along(TZ_2015)) {names(TZ_2015)[[i]] <- paste0(unique(TZ_2015[[i]]$hv007), '_', unique(TZ_2015[[i]]$hv006))}

GPS_all_TZ <- sapply(c(TZ_2012, TZ_2015), sf:::as_Spatial, simplify = F)
names(GPS_all_TZ)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Uganda
## -----------------------------------------------------------------------------------------------------------------------------------------
dhs_all <- list(pr_downloads[[39]], pr_downloads[[40]])
names1 <- paste0(str_sub(pr_downloads[[39]][1, "hv000"], 1, 2), "_", min(pr_downloads[[39]]$hv007))
names2 <- paste0(str_sub(pr_downloads[[40]][1, "hv000"], 1, 2), "_", min(pr_downloads[[40]]$hv007))
names(dhs_all) <- c(names1, names2)
print(names(dhs_all))


dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_UG <- dhs_all
dhs_all_UG[[1]]<- NULL

UG_2009 <- survey_gps_comb(x= names1, y= names1)
for(i in seq_along(UG_2009)) {names(UG_2009)[[i]] <- paste0(unique(UG_2009[[i]]$hv007), '_', unique(UG_2009[[i]]$hv006))}

UG_2016 <- survey_gps_comb(x=names2, y=names2)
for(i in seq_along(UG_2016)) {names(UG_2016)[[i]] <- paste0(unique(UG_2016[[i]]$hv007), '_', unique(UG_2016[[i]]$hv006))}

GPS_all_UG <- sapply(c(UG_2016), sf:::as_Spatial, simplify = F)
names(GPS_all_UG)


## =========================================================================================================================================
### OTHER
## =========================================================================================================================================

#### data extraction parameter lists 
# compile lists of GPS data and DHS data for various countries
GPS_all <- list(GPS_all_OA, GPS_all_BF, GPS_all_BJ, GPS_all_BU, GPS_all_CD, GPS_all_CI,  
                GPS_all_CM, GPS_all_GH, GPS_all_GM, GPS_all_GN, GPS_all_MD, GPS_all_ML, GPS_all_MR,
                GPS_all_MZ, GPS_all_NG, GPS_all_RW, GPS_all_SN, GPS_all_TG, GPS_all_TZ, GPS_all_UG)

dhs_all <- list(dhs_all_OA, dhs_all_BF, dhs_all_BJ, dhs_all_BU, dhs_all_CD, dhs_all_CI,  
                dhs_all_CM, dhs_all_GH, dhs_all_GM, dhs_all_GN, dhs_all_MD, dhs_all_ML, dhs_all_MR,
                dhs_all_MZ, dhs_all_NG, dhs_all_RW, dhs_all_SN, dhs_all_TG, dhs_all_TZ, dhs_all_UG)


# buffers of interest
vars <- c(2000) # define buffer size in meters

# extracting EVI using a list of survey GPS points; this may take some time to run
df_list_evi <- list()

# loop through each country in the GPS_all list
for (i in 1:length(GPS_all)) {
  
  # extract EVI rasters; this is where the lag may occur
  EVI_comb <- lapply(dhs_all[[i]], pick_month, filepath= EviDir) # pick EVI data for the specified month
  EVI_comb_vc <- unlist(EVI_comb) # flatten the list to create a vector
  EVI_raster_all <- sapply(EVI_comb_vc, raster, simplify = FALSE) # read in EVI raster files with a 2-month lag
  
  #for (i in 1:length(vars)) {
  var_name <- paste0('EVI_', as.character(vars[1]), 'm')
  # extract EVI values based on the corresponding GPS points
  df <- map2(GPS_all[[i]], EVI_raster_all, get_crs) # get coordinate reference systems for the rasters
  df <- pmap(list(EVI_raster_all, df, vars[1]), extract_fun_month) # extract EVI values for the given month
  df <- df %>%  map(~rename_with(., .fn=~paste0(var_name), .cols = contains('EVI'))) # rename the columns to include the variable name
  df <- plyr::ldply(df) %>% dplyr::select(-c(ID)) # combine the extracted data into a single data frame, removing the ID column
  df <- df %>% arrange(month) %>%  group_by(dhs_year, hv001) %>%  slice(1) # arrange by month and get data for the first month if more than one survey month
  #}
  
  # store the processed data frame in the list
  df_list_evi[[i]] <- df
  
  # combine all data frames in df_list_evi into one data frame
  df_binded_EVI <- df_list_evi %>% bind_rows()
  
  # write the combined data frame to a CSV file
  write.csv(df_binded_EVI, file = file.path(OutDir, paste0("EVI_DHS.csv")),row.names = FALSE)
}

## -----------------------------------------------------------------------------------------------------------------------------------------
### Precipitation CHIRPS (35+ year quasi-global rainfall data set)
## -----------------------------------------------------------------------------------------------------------------------------------------

# initialize an empty list to store precipitation data frames
df_list_prec <- list()

# loop through each country in the GPS_all list
for (i in 1:length(GPS_all)) {
  
  # extract precipitation rasters; this is where the lag may occur
  precip_comb <- lapply(dhs_all[[i]], pick_month, filepath= PrecDir) # pick precipitation data for the specified month
  precip_comb_vc <- unlist(precip_comb) # flatten the list to create a vector
  prec_raster_all <- sapply(precip_comb_vc, raster, simplify = F) # read in precipitation raster files with a 2-month lag
  
  #for (i in 1:length(vars)) {
  var_name <- paste0('preci_monthly_', as.character(vars[1]), 'm') # define variable name for the precipitation data
  # extract precipitation values based on the corresponding GPS points
  df <- map2(GPS_all[[i]], prec_raster_all, get_crs) # get coordinate reference systems for the rasters. list of 13 vs. list of 144 
  df <- pmap(list(prec_raster_all, df, vars[1]), extract_fun_month) # extract precipitation values for the given month
  df <- df %>%  map(~rename_with(., .fn=~paste0(var_name), .cols = contains('chirps'))) # rename the columns to include the variable name
  df <- plyr::ldply(df) %>% dplyr::select(-c(ID)) # combine the extracted data into a single data frame, removing the ID column
  df <- df %>% arrange(month) %>%  group_by(dhs_year, hv001) %>%  slice(1) # arrange data by month, grouping by year and cluster ID, keeping only the first entry if multiple months exist
  #}
  
  # store the processed data frame in the list    
  df_list_prec[[i]] <- df
  
  # combine all data frames in df_list_prec into one data frame and remove negative values
  df_binded_precip <- df_list_prec %>% bind_rows() %>% 
    mutate(preci_monthly_2000m = ifelse(preci_monthly_2000m < 0, NA, preci_monthly_2000m)) # replace negative values with NA
  
  # write the combined data frame to a CSV file
  write.csv(df_binded_precip, file = file.path(OutDir, paste0("precip_DHS.csv")),row.names = FALSE)
}

## -----------------------------------------------------------------------------------------------------------------------------------------
### Relative Humidity at 1 atm - Raster Data (2-month lag)
## -----------------------------------------------------------------------------------------------------------------------------------------

# load raster data for relative humidity from 2009 to 2023
list_RH <- list(humidity_2009 <- brick(file.path(HumDir, 'rel_humidity_2009.grib')),
                humidity_2010 <- brick(file.path(HumDir, 'rel_humidity_2010.grib')),
                humidity_2011 <- brick(file.path(HumDir, 'rel_humidity_2011.grib')),
                humidity_2012 <- brick(file.path(HumDir, 'rel_humidity_2012.grib')),
                humidity_2013 <- brick(file.path(HumDir, 'rel_humidity_2013.grib')),
                humidity_2014 <- brick(file.path(HumDir, 'rel_humidity_2014.grib')),
                humidity_2015 <- brick(file.path(HumDir, 'rel_humidity_2015.grib')),
                humidity_2016 <- brick(file.path(HumDir, 'rel_humidity_2016.grib')),
                humidity_2017 <- brick(file.path(HumDir, 'rel_humidity_2017.grib')),
                humidity_2018 <- brick(file.path(HumDir, 'rel_humidity_2018.grib')),
                humidity_2019 <- brick(file.path(HumDir, 'rel_humidity_2019.grib')),
                humidity_2020 <- brick(file.path(HumDir, 'rel_humidity_2020.grib')),
                humidity_2021 <- brick(file.path(HumDir, 'rel_humidity_2021.grib')),
                humidity_2022 <- brick(file.path(HumDir, 'rel_humidity_2022.grib')),
                humidity_2023 <- brick(file.path(HumDir, 'rel_humidity_2023.grib'))
)

# rename the layers in each year's dataset using month abbreviations (Jan, Feb, etc.)
# (makes it easier to identify the months when working with the data)
for (i in 1:length(list_RH)){
  names(list_RH[[i]]) <- paste0("RH_", month.abb) # assign month abbreviations to each layer
}

# check the number of layers for the year 2022 (14th element in list_RH)
nlayers(list_RH[[14]])

# plot the first layer of 2010 to visually inspect the relative humidity data for January
plot(list_RH[[1]], 1)

# assign year names to each element in the list for easier reference
names(list_RH) <- c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", 
                    "2017", "2018", "2019","2020", "2021", "2022", "2023")


# apply over dhs_all list --- repeat for each country

# create an empty list to store results for each country
df_list_RH <- list()

# iterate over the list of countries (dhs_all)
for (k in 1:length(dhs_all)){
  
  RH_files <- lapply(dhs_all[[k]], get_month_str_RH) # extract the month string for relative humidity for each cluster
  
  # iterate through the list of RH files for each country
  for (i in 1:length(RH_files)){ 
    RH_files[[i]] <- RH_files[[i]] %>% 
      pmap(~pick_files_RH(.y, .x)) # select the correct humidity raster files based on month_lag and year
  }
  
  # flatten the list of selected raster files into a single vector
  raster_all <- unlist(RH_files)
  
  #for (i in 1:length(vars)) {
  var_name <- paste0('RH_monthly_', as.character(vars[1]), 'm') # define variable name based on the lag (vars[1]) and create the dataframe for each country
  df <- map2(GPS_all[[k]], raster_all, get_crs) # transform gps coordinates to match the projection of the raster files
  df <- pmap(list(raster_all, df, vars[1]), extract_fun_month) # extract the relative humidity data using the gps coordinates and raster files
  df <- df %>% map(~rename_with(., .fn=~paste0(var_name), .cols = contains('RH'))) # rename the humidity data columns for clarity
  df <- plyr::ldply(df)%>% dplyr::select(-c(ID)) # combine the results into a dataframe and remove the ID column
  df <- df %>% arrange(month) %>%  group_by(dhs_year, hv001) %>%  slice(1) # ensure data is sorted by month, and keep only the first record if there are multiple survey months per cluster
  #}
  
  # store the processed dataframe in the list for this country
  df_list_RH[[k]] <- df
  
  # bind the data for all countries into a single dataframe
  df_binded_RH <- df_list_RH %>% bind_rows()
  
  # write the final dataframe to a CSV file
  write.csv(df_binded_RH, file = file.path(OutDir, paste0("RH_monthly_DHS.csv")),row.names = FALSE)
}

## -----------------------------------------------------------------------------------------------------------------------------------------
### Temperature - Raster Data (2-month lag)
## -----------------------------------------------------------------------------------------------------------------------------------------

# create a list of temperature raster files for the years 2009 to 2023
list_temp <- list(temp_2009 <- brick(file.path(TempDir, 'temp_2009.grib')),
                  temp_2010 <- brick(file.path(TempDir, 'temp_2010.grib')),
                  temp_2011 <- brick(file.path(TempDir, 'temp_2011.grib')),
                  temp_2012 <- brick(file.path(TempDir, 'temp_2012.grib')),
                  temp_2013 <- brick(file.path(TempDir, 'temp_2013.grib')),
                  temp_2014 <- brick(file.path(TempDir, 'temp_2014.grib')),
                  temp_2015 <- brick(file.path(TempDir, 'temp_2015.grib')),
                  temp_2016 <- brick(file.path(TempDir, 'temp_2016.grib')),
                  temp_2017 <- brick(file.path(TempDir, 'temp_2017.grib')),
                  temp_2018 <- brick(file.path(TempDir, 'temp_2018.grib')),
                  temp_2019 <- brick(file.path(TempDir, 'temp_2019.grib')),
                  temp_2020 <- brick(file.path(TempDir, 'temp_2020.grib')),
                  temp_2021 <- brick(file.path(TempDir, 'temp_2021.grib')),
                  temp_2022 <- brick(file.path(TempDir, 'temp_2022.grib')),
                  temp_2023 <- brick(file.path(TempDir, 'temp_2023.grib'))
)

# rename the layers in each year's dataset using month abbreviations (Jan, Feb, etc.)
# (makes it easier to identify the months when working with the data)
for (i in 1:length(list_temp)){
  names(list_temp[[i]]) <- paste0("temp_", month.abb) # assign month abbreviations to each layer
}

# check the number of layers in the first raster (should be 12 for each month)
nlayers(list_temp[[1]])

# visually inspect the first layer (january 2009) of the temperature data (list in R starts with index 1)
plot(list_temp[[1]], 1)

# assign year names to each element in the list for easier reference
names(list_temp) <- c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", 
                      "2017", "2018", "2019","2020", "2021", "2022", "2023")

# initialize an empty list to store temperature data for each country
df_list_temp <- list()

# iterate over the list of countries (dhs_all)
for (k in 1:length(dhs_all)){
  
  # apply get_month_str_RH function over each country to get corresponding month strings
  temp_files <- lapply(dhs_all[[k]], get_month_str_RH)
  
  # iterate over temp_files to pick the correct temperature raster files
  for (i in 1:length(temp_files)){
    temp_files[[i]] <- temp_files[[i]] %>% 
      pmap(~pick_files_temp(.y, .x)) # pick month_lag and year to select the correct temperature raster files
  }
  
  # flatten the list of raster files
  raster_all <- unlist(temp_files)
  
  #for (i in 1:length(vars)) {
  var_name <- paste0('temp_monthly_', as.character(vars[1]), 'm') # create a variable name for temperature with a monthly lag
  df <- map2(GPS_all[[k]], raster_all, get_crs) # transform GPS coordinates to match raster projection
  df <- pmap(list(raster_all, df, vars[1]), extract_fun_month) # extract temperature data using the extract_fun_month function
  df <- df %>% map(~rename_with(., .fn=~paste0(var_name), .cols = contains('temp'))) # rename columns to include the variable name and remove 'ID' column
  df <- plyr::ldply(df)%>% dplyr::select(-c(ID))
  df <- df %>% arrange(month) %>%  group_by(dhs_year, hv001) %>%  slice(1) # ensure data is sorted by month, and keep only the first record if there are multiple survey months per cluster
  #}
  #}
  
  # store the processed data in the list for each country
  df_list_temp[[k]] <- df
  
  # combine all country-level data into a single dataframe
  df_binded_temp <- df_list_temp %>% bind_rows()
  
  # write the combined data to a CSV file
  write.csv(df_binded_temp, file = file.path(OutDir, paste0("temp_monthly_DHS.csv")),row.names = FALSE)
}

## -----------------------------------------------------------------------------------------------------------------------------------------
### Merge Environment Variables
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in the csv files for each environmental variable
df_binded_EVI <- read.csv(file.path(OutDir, "EVI_DHS.csv"))
df_binded_precip <- read.csv(file.path(OutDir, "precip_DHS.csv"))
df_binded_RH  <- read.csv(file.path(OutDir, "RH_monthly_DHS.csv"))
df_binded_temp <- read.csv(file.path(OutDir, "temp_monthly_DHS.csv"))

# merge the datasets sequentially using left joins
merged_df <- df_binded_EVI %>% 
  left_join(df_binded_precip, by = c()) %>% 
  left_join(df_binded_RH, by = c()) %>% 
  left_join(df_binded_temp, by = c()) %>%
  
  # convert temperature from kelvin to celsius for specific records
  mutate(temp_monthly_2000m = ifelse(grepl("GH_2022|MZ_2022", .id), 
                                     temp_monthly_2000m, 
                                     temp_monthly_2000m - 273.15))

# save the final merged dataframe to a csv file
write.csv(merged_df, file = file.path(OutDir, paste0("all_geospatial_monthly_DHS.csv")),row.names = FALSE)
