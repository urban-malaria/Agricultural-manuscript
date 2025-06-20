# ==========================================================================================================================================
# Script Name: Functions
# Author: Dr. Ifeoma Ozodiegwu
# Edited by: Grace Legris, Research Data Analyst (gracebea@gmail.com)
# Edited: [2024-09-26]
# Purpose: Data processing and visualization functions for agricultural malaria analysis
# ==========================================================================================================================================

# -----------------------------------------
### Required functions and settings
## ----------------------------------------

# # Reading in the necessary packages 
library(pacman)
p_load(readr, tidyr, plyr, dplyr, purrr, forcats,survey, haven, 
       ggplot2, purrr,  stringr, sp, raster,sf,   labelled, plotrix, 
       arules, fuzzyjoin, cowplot, gridExtra, lme4, patchwork, readxl , 
       janitor, ggsci, glue, ggrepel, jtools, srvyr, ggpubr, collapse,
       gtsummary, rstatix, ggcorrplot, viridis, effects, rdhs, 
       microbenchmark,  ggfittext, forcats, broom, writexl)

#plotting functions
bar_fun <- function(df, x, fill, title, xlab){ #facet_var, 
  ggplot(df, aes(x = .data[[x]], fill=.data[[x]]))+
    geom_bar()+
    #coord_flip()+
    #facet_wrap(vars(.data[[facet_var]]), scales="free")+
    theme_manuscript()+
    geom_bar_text(stat = 'count', aes(label =..count..), vjust=0.5, size =5 * ggplot2::.pt, 
                  min.size = 4 * ggplot2::.pt,
                  padding.x = grid::unit(0, "pt"),
                  padding.y = grid::unit(0, "pt"),
                  outside = TRUE )+
    theme(legend.position = "none")+
    labs(title= title,
         x=xlab)
}


col_fun <- function(df, x, y, z, ylabel, color, label){
  ggplot(df, aes(x = .data[[x]], y =.data[[y]], fill=.data[[z]]))+
    geom_col()+
    coord_flip()+ 
    scale_x_discrete(limits = rev)+
    theme_manuscript()+
    theme(legend.title=element_blank())+
    labs(x="", y= ylabel )+
    scale_fill_manual(values=color, labels= label, guide = guide_legend(reverse = TRUE))
}



#read files function 
read.files <- function(filepat1,path,fun, encoding = "latin1") {
  filenames <- list.files(path = path, pattern = filepat1, recursive = TRUE, full.names = FALSE)
  sapply(filenames, fun, simplify = F)
  
}

# get the legend for each plot (used when making a grid and only one legend is needed)
get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}


theme_manuscript <- function(){
  theme_bw() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 12, color = "black"), 
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title=element_text(size=12, colour = 'black'),
          legend.text =element_text(size = 12, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}

theme_corr <- function(){
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        axis.text.x = element_text(size = 12, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"))
}

# Stacked bar plot function

barplot_stack.fun <- function(main_title){
  barplot_stack <- ggplot(counts, aes(fill=Var2, y=Freq, x=Var1)) + 
    geom_bar(position="stack", stat="identity", alpha = 0.8) +
    scale_fill_viridis(discrete = T) +
    ggtitle(main_title) +
    theme_manuscript() +
    xlab("")+
    ylab('frequency')+
    scale_fill_manual(name="Legend", values=c("darkgrey", "brown")) 
}

#percentage bar plot function

barplot_prop.fun <- function(main_title){
  barplot_perc <- ggplot(counts, aes(fill=Var2, y=Freq, x=Var1)) + 
    geom_bar(position="fill", stat="identity", alpha = 0.8) +
    scale_fill_viridis(discrete = T) +
    ggtitle(main_title) +
    theme_manuscript() +
    scale_fill_manual(name="Legend", values=c("darkgrey", "brown"))+
    xlab("")+
    ylab('percent')
}

#percentage bar plot function

barplot_prop.fun2 <- function(dataframe){
  barplot_perc <- ggplot(dataframe, aes(fill=Var2, y=Freq, x=Var1, label=scales::percent(percent, vjust=3))) + 
    geom_bar(position="fill", stat="identity", alpha = 0.8) +
    scale_fill_viridis(discrete = T) +
    ggtitle("") +
    theme_manuscript() +
    scale_fill_manual(name="Legend", values=c("cyan", "darkcyan"))+
    xlab("")+
    ylab('proportion')
}  

#survey estimates generating functions  
result.prop<- function(var, var1, design) {
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, svyciprop, method ='logit', levels=0.95, vartype= "se", na.rm=T, influence = TRUE)
}

#survey design function 
svydesign_fun <- function(filename){
  svydesign(id= ~id,
            strata=~strat,nest=T, 
            weights= ~wt, data=filename)
}


#survey estimates generating functions  
result.prop<- function(var, var1, design) {
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svytotal, design, svyciprop, method ='logit', levels=0.95, vartype= "se", na.rm=T, influence = TRUE)
}

#estimation functions 
estim_prop <- function(df, col, by){
  svy_mal <- svydesign.fun(df)
  clu_est <- result.prop(col, by, design=svy_mal)
}

#dhs ids

ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode", "SubregionName", "RegionName")) %>% 
  filter(RegionName == "Sub-Saharan Africa")


#ggsave function
ggsave_fun <- function(save_as_pdf, save_as_png, plot_name, width_size, height_sze){
  ggsave(paste0(FigDir,"/", Sys.Date(),save_as_pdf), plot_name, width = width_size, height =height_sze)
  ggsave(paste0(FigDir,"/", Sys.Date(), save_as_png), plot_name, width = width_size, height =height_sze)
}


#regional probabilities df generator fun
effect_df_fun <- function(model_){
  effect_list_est <- summary(Effect("agric_home", model_)) 
  effect_list_est$effect %>% as.data.frame() %>% 
    bind_cols(effect_list_est$lower %>% as.data.frame()) %>% 
    bind_cols(effect_list_est$upper %>% as.data.frame()) %>% 
    rename(effect = ....1, lower = ....2, upper = ....3) %>% 
    tibble::rownames_to_column()
}

#funtion - housing quality and wealth by agric household worker status


#geom column plots function 


p_fun <- function(dataframe, fill_stack, y_lab){
  ggplot(dataframe, aes(x=reorder(country, value),y=value))+
    geom_col(position= fill_stack, aes(fill = variable))+
    scale_fill_manual(name="", values=c("aquamarine3", "deepskyblue4"), label = c("+ve", "-ve")) +
    coord_flip()+
    scale_y_continuous(expand=c(0,0))+
    labs(x = "Country", y= y_lab) +
    theme(axis.text.y=element_blank(),axis.line.y = element_blank(),
          axis.ticks.y = element_blank())+theme_classic2() +
    theme_manuscript()
}

#rura - urban stacked hist combo

ur_rur_pfun <- function(df_r_u){
  
  p2_r1 <- p5_stacked_fun(df_r_u[[1]], 22500) + ggtitle("Eastern")+
    theme(plot.title=element_text(margin=margin(t=10,b=-20)))+ p5_prop_fun(df_r_u[[1]]) 
  
  p2_r2 <- p5_stacked_fun(df_r_u[[2]], 22500) +  ggtitle("Middle")+
    theme(plot.title=element_text(margin=margin(t=10,b=-20))) + p5_prop_fun(df_r_u[[2]]) 
  
  p2_r3 <- p5_stacked_fun(df_r_u[[3]], 22500) + 
    ggtitle("Western")+theme(plot.title=element_text(margin=margin(t=10,b=-20))) + p5_prop_fun(df_r_u[[3]])
  
  p2_r1 / p2_r2 /p2_r3 +  plot_annotation(tag_levels = "A")
  
}

#housing quality and wealth by agric household worker statu

bar_prop_fun<- function(df, var_string, x_lab){
  ggplot(p_data_bar, aes_string(y="Freq", x="home_type_new", fill= var_string)) + 
    geom_bar(position="fill", stat="identity", alpha = 0.8) +
    #scale_fill_viridis(discrete = T) +
    facet_wrap(~hv025)+
    ggtitle("") +
    theme_manuscript() +
    scale_fill_manual(name="", values=c("darkcyan", "brown"))+
    #geom_text(aes(label = Freq)) +
    xlab(x_lab)+
    ylab('Proportion') +
    theme(legend.position = "none", strip.background = element_blank(),
          strip.text.x = element_blank()) 
}

#############################################
## Environment extracting functions
############################################

get_crs <- function(df, raster){
  dhs <- spTransform(x = df, CRSobj = crs(raster))
}


extract_fun <- function(raster, dhs, buffer){
  clu_val<-raster::extract(raster,dhs, buffer = buffer, fun = mean, df =TRUE) %>%
    mutate(dhs_year = dhs$DHSYEAR)%>%
    mutate(hv001 = dhs$DHSCLUST) 
}


extract_fun_month <- function(raster, dhs, buffer){
  clu_val<-raster::extract(raster,dhs, buffer = buffer, fun = mean, df =TRUE) %>%
    mutate(dhs_year = dhs$DHSYEAR, hv001 = dhs$DHSCLUST, month = dhs$hv006)
}

survey_gps_comb <- function(x, y){
  survey1 <- left_join(st_as_sf(all_GPS[[x]]), dhs_all[[y]], by = c("DHSCLUST"="hv001")) %>% 
    group_split(hv007, hv006) 
}


# Pick correct survey year and months from EVI and Precipitation data files- with 2 month lag
get_month_str <- function(file){
  file %>% 
    dplyr::select(hv006, hv007) %>% 
    mutate(mo= hv006- 2,
           month_lag= if_else(mo< 10, str_c(".0", mo), str_c(".", mo))) %>% 
    mutate(month_lag= if_else(month_lag== ".0-1", ".11", #nov
                              if_else(month_lag== ".00", ".12", month_lag)), #dec 
           year= if_else(month_lag %in% c(".11", ".12"), hv007-1, hv007)) %>% #nov and dec (already lagged by 2mo) should be the year prior to interview year
    dplyr::select(month_lag, year) %>% 
    group_by(month_lag, year) %>% 
    slice(1)
} 


pick_month <- function(file, filepath){
  
  EVI_files <- list.files(path = filepath, pattern = ".tif$", full.names = TRUE, recursive = F) # pull in files
  month_lag <- get_month_str(file) %>%  #get months and year with 2 month lag
    mutate(year_mo= paste0(year,month_lag))
  vect <- month_lag$year_mo
  EVI_files1 <- EVI_files[(grep(paste(vect, collapse="|"), EVI_files))]
  
}

get_month_str_RH <- function(file){
  file %>% 
    dplyr::select(hv006, hv007) %>% 
    mutate(month_lag= hv006- 2) %>% 
    mutate(month_lag= if_else(month_lag== -1, 11, #nov
                              if_else(month_lag== 0, 12, month_lag)), #dec 
           year= if_else(month_lag %in% c(11, 12), hv007-1, hv007)) %>% #nov and dec (already lagged by 2mo) should be the year prior to interview year
    dplyr::select(month_lag, year) %>% 
    mutate(month_lag= as.numeric(month_lag), year= as.numeric(year)) %>% 
    group_by(year, month_lag) %>% 
    slice(1)
} 

#Select the rasterbrick files needed
pick_files_RH <- function(year, month_lag){
  x <- list_RH[[as.character(year)]]
  
  RH_file <- x[[month_lag]]
  return(RH_file)
}

#Select the temperature rasterbrick files needed
pick_files_temp <- function(year, month_lag){
  x <- list_temp[[as.character(year)]]
  
  temp_file <- x[[month_lag]]
  return(temp_file)
}


# box plot function
box_plot_fun <- function(df, var1, var2){
  ggplot(df, aes_string(y=var1,  x = var2, fill= var2)) + 
    geom_boxplot()+
    labs (x = "", y = "y_lab", title = "") +
    scale_fill_manual(values=c( "darkgoldenrod1", "darkviolet"), name="home type") +
    theme_manuscript() +
    theme(legend.position = "none") + 
    facet_wrap(~ hv025)
  
}
##########################################Plotting maps

map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA), 
        plot.title = element_text(hjust = 0.5),
        legend.title.align=0.5,
        legend.title=element_text(size=8, colour = 'black'), 
        legend.text =element_text(size = 8, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}



name_clean_fun <- function(df) {
  df %>%  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",
                                         ifelse(country_year.x == "Uganda 2009", "Uganda 2009 - 10",
                                                ifelse(country_year.x == "Cameroon 2018", "Cameroon 2018 - 19",country_year.x))))
}
