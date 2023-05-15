# === PACKAGES === -----
##required_packages -----
required_packages <- c("janitor","tidyverse","rio","ISOweek","devtools","lubridate","zoo",
                        "readxl","stringi","openxlsx","ggplot2","geosphere","magrittr","PHEindicatormethods",
                        "rvest", "jsonlite", "purrr")

##install required_packages -----
for (pkg in required_packages) {
  # install packages if not already present
  if (!pkg %in% rownames(installed.packages())) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
  }
  # load packages to this current session 
  library(pkg, character.only = TRUE)
}

library(AttMOMO)

# === PARAMETERS === -----
##general parameters -----
###path
pth <- "C:/Users/.../data_attmomo/"

###function source path
functions_source <- "C:/Users/.../script_AttMOMO_functions_source.R"

##sico_scrape parameters -----
output_file_sico <- "C:/Users/.../obitos_aces_scrape.xlsx"

##ipma_scrape parameters -----
#api link with all the stations information
api_ipma_stations_link <- "https://api.ipma.pt/open-data/observation/meteorology/stations/stations.json"

#xlsx output file location
output_file_ipma <- "C:/Users/.../ipma_scrape.xlsx"

#choose starting (d1) and ending (d2) date
starting_date <- "2019-01-01"
ending_date <- "2023-04-01"


##attmomo parameters -----
###data paths
###check file examples at input_files_example
###all files must be in root folder
path_centroide_aces <- "centroide_aces.xlsx"
path_incidencia_covid <- "incidencia_covid.xlsx"
path_incidencia_gripe <- "incidencia_gripe.xlsx"
path_linkage_geo <- "linkage_geo.xlsx"
path_obitos_aces <- "obitos_aces.xlsx"
path_obitos_covid <- "obitos_covid.xlsx"
path_temperaturas_aces <- "temperaturas_aces.xlsx"
path_stations_data <- "stations_ipma_data.xlsx"

###attmomo arguments
StartDate <- "2019-01-01"
EndDate <- "2022-12-05"
lags <- 3
ptrend <- 0.05
p26 <- 0.05
p52 <- 0.10

# === CALLS === -----
source(paste0(functions_source))

##sico_scrape call -----
sico_scrape <- get_evm_data(type = "aces")

##ipma_scrape call -----
ipma_scrape <- get_ipma_data()

##attmomo call-----
attmomo_aces_all <- attMOMO_ors(indicators = NULL, geography_level = "aces")

#Plots and visualization

#attmomo_aces_all <- AttData
###creates zscores
attmomo_aces_all$B_upper_2Z <- (2*sqrt(attmomo_aces_all$VEB))+attmomo_aces_all$EB #baseline, upper limit 2 Z-scores
attmomo_aces_all$B_lower_2Z <- (2*-(1)*sqrt(attmomo_aces_all$VEB))+attmomo_aces_all$EB #baseline, lower limit 2 Z-scores
attmomo_aces_all$AB_upper_2Z <- (2*sqrt(attmomo_aces_all$VEAB))+attmomo_aces_all$EAB #Adjusted baseline, upper limit 2 Z-scores
attmomo_aces_all$AB_lower_2Z <- (2*-(1)*sqrt(attmomo_aces_all$VEAB))+attmomo_aces_all$EAB #Adjusted baseline, lower limit 2 Z-scores 
attmomo_aces_all$B_upper_4Z <- (4*sqrt(attmomo_aces_all$VEB))+attmomo_aces_all$EB #baseline, upper limit 2 Z-scores
attmomo_aces_all$B_lower_4Z <- (4*-(1)*sqrt(attmomo_aces_all$VEB))+attmomo_aces_all$EB #baseline, lower limit 2 Z-scores
attmomo_aces_all$AB_upper_4Z <- (4*sqrt(attmomo_aces_all$VEAB))+attmomo_aces_all$EAB #Adjusted baseline, upper limit 2 Z-scores
attmomo_aces_all$AB_lower_4Z <- (4*-(1)*sqrt(attmomo_aces_all$VEAB))+attmomo_aces_all$EAB #Adjusted baseline, lower limit 2 Z-scores 


###TESTING creates dataframe and plot/images for each aces -----
#### dataframes for each aces -----
for (aces4 in unique(attmomo_aces_all$group)) {
  aces2 <- gsub('[^[:alnum:] ]','',aces4)
  aces2 <- gsub(' ','',aces2)
  aces2 <- paste0("attmomo_",aces2)
  attmomo_aces_each <- attmomo_aces_all %>% filter(., group == aces4)
  print(attmomo_aces_each)
  assign(x = aces2, value = attmomo_aces_each)
}

#### dataframes and observed+baseline plot for each aces -----
for (aces4 in unique(attmomo_aces_all$group)) {
  aces2 <- gsub('[^[:alnum:] ]','',aces4)
  aces2 <- gsub(' ','',aces2)
  aces2 <- paste0("attmomo_",aces2)
  attmomo_aces_each <- attmomo_aces_all %>% filter(., group == aces4)
  print(attmomo_aces_each)
  assign(x = aces2, value = attmomo_aces_each)
  
  #print image for each aces in root - OBSERVED AND BASELINE
  attmomo_plot <- ggplot(attmomo_aces_each,aes(x=as.numeric(as.factor(ISOweek))))+
    geom_line(aes(y=deaths,color="Observado"),size=2)+
    geom_line(aes(y=EB,color="Linha de base"))+
    geom_line(aes(y=B_upper_2Z,color="+/-2 z-scores"),linetype="dashed")+
    geom_line(aes(y=B_lower_2Z,color="+/-2 z-scores"),linetype="dashed")+
    scale_color_manual(name="Legenda",values = (c("Observado"="gray",
                                                  "Linha de base"="black",
                                                  "+/-2 z-scores"="steelblue")))+
    scale_x_continuous(name = "Ano-Semana ISO",labels = attmomo_aces_each$ISOweek[rep(c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),nrow(attmomo_aces_each)/15)], breaks=seq(1,nrow(attmomo_aces_each),15))+
    scale_y_continuous("Número de óbitos")+
    theme_classic()+
    theme(legend.position = "bottom")+
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5))+
    theme(axis.title.x = element_text(angle = 0, vjust = -3))+
    theme(axis.line = element_line(color = "black",size = 1,linetype = "solid"))
  ggsave(filename = paste0("figure",aces2,".png"), device = "png")
  
}

#### dataframes and observed+ADJUSTED baseline plot for each aces -----
for (aces4 in unique(attmomo_aces_all$group)) {
  aces2 <- gsub('[^[:alnum:] ]','',aces4)
  aces2 <- gsub(' ','',aces2)
  aces2 <- paste0("attmomo_",aces2)
  attmomo_aces_each <- attmomo_aces_all %>% filter(., group == aces4)
  print(attmomo_aces_each)
  assign(x = aces2, value = attmomo_aces_each)
  
  #print image for each aces in root - OBSERVED AND ADJUSTED BASELINE
  attmomo_plot <- ggplot(attmomo_aces_each,aes(x=as.numeric(as.factor(ISOweek))))+
    geom_line(aes(y=deaths,color="Observado"),size=2)+
    geom_line(aes(y=EAB,color="Linha de base ajustada"))+
    geom_line(aes(y=AB_upper_2Z,color="+/-2 z-scores"),linetype="dashed")+
    geom_line(aes(y=AB_lower_2Z,color="+/-2 z-scores"),linetype="dashed")+
    scale_color_manual(name="Legenda",values = (c("Observado"="gray",
                                                  "Linha de base ajustada"="black",
                                                  "+/-2 z-scores"="steelblue")))+
    scale_x_continuous(name = "Ano-Semana ISO",labels = attmomo_aces_each$ISOweek[rep(c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),nrow(attmomo_aces_each)/15)], breaks=seq(1,nrow(attmomo_aces_each),15))+
    scale_y_continuous("Número de óbitos")+
    theme_classic()+
    theme(legend.position = "bottom")+
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5))+
    theme(axis.title.x = element_text(angle = 0, vjust = -3))+
    theme(axis.line = element_line(color = "black",size = 1,linetype = "solid"))
  ggsave(filename = paste0("figure_adjusted",aces2,".png"), device = "png")
}
attmomo_plot
####testing plot for 1 aces baseline
####testing plot for 1 aces adjusted baseline
attmomo_aces_each <- attmomo_ACEST�megaIBaixoT�mega
attmomo_plot <- ggplot(attmomo_aces_each,aes(x=as.numeric(as.factor(ISOweek))))+
  geom_line(aes(y=deaths,color="Observado"),size=2)+
  geom_line(aes(y=EAB,color="Linha de base ajustada"))+
  geom_line(aes(y=AB_upper_2Z,color="+/-2 z-scores"),linetype="dashed")+
  geom_line(aes(y=AB_lower_2Z,color="+/-2 z-scores"),linetype="dashed")+
  scale_color_manual(name="Legenda",values = (c("Observado"="gray",
                                                "Linha de base ajustada"="black",
                                                "+/-2 z-scores"="steelblue")))+
  scale_x_continuous(name = "Ano-Semana ISO",labels = attmomo_aces_each$ISOweek[rep(c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),nrow(attmomo_aces_each)/15)], breaks=seq(1,nrow(attmomo_aces_each),15))+
  scale_y_continuous("Número de óbitos")+
  theme_classic()+
  theme(legend.position = "bottom")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5))+
  theme(axis.title.x = element_text(angle = 0, vjust = -3))+
  theme(axis.line = element_line(color = "black",size = 1,linetype = "solid"))
ggsave(filename = paste0("figure_adjusted_single_",attmomo_aces_each,".png"), device = "png")
attmomo_plot
  
