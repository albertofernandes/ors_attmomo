#=== AttMOMO ors === -----
#attmomo function, to be called from script_AttMOMO_function.R
attMOMO_ors <- function(indicators=NULL, geography_level = c("nacional", "regional", "aces")) { 
  functions_attmomo_source <- "C:/Users/.../script_AttMOMO_source.R"
  source(paste0(functions_attmomo_source))
  
##uploads -----
###uploads data for each indicator -----
    for (i in indicators){
    if (i == "covid"){
      inc_covid <- import(paste0(pth, path_incidencia_covid))
      obitos_covid <- import(paste0(pth, path_obitos_covid))
    }
    if (i == "gripe"){
      inc_gripe <- import(paste0(pth, path_incidencia_gripe))
    }
  }
###uploads centroid, deaths and climate based on geography level -----
  if (geography_level == "aces"){
    centroides_aces <- import(paste0(pth, path_centroide_aces))
    obitos_baseline <- import(paste0(pth,path_obitos_aces))
    climate <- import(paste0(pth, path_temperaturas_aces))
  }
###match file (with population, region, aces) -----
  match_conc_reg <- import(paste0(pth, path_linkage_geo))
  
###temperature stations -----
  stations <- import(paste0(pth, path_stations_data))
  
## PRE ATTMOMO FUNCION PROCESSING-----
  #ACES -----
  if(geography_level == "aces"){
    #population -----
    pop_aces <- match_conc_reg %>% 
      clean_names() %>% 
      group_by(aces4) %>%
      mutate(pop_aces=sum(populacao)) %>% #calculates ACES population
      mutate(prop_pop=populacao/pop_aces) %>%
      ungroup()
    
    #deaths -----
    death_data_aces <- obitos_baseline %>% 
      mutate(date = as.Date(date)) %>% #data format Y-m-d
      mutate(ISOweek=ISOweek(date)) %>% 
      group_by(ISOweek) %>% 
      summarise(across(where(is.numeric), sum))
    
    death_data_aces_long <- pivot_longer(death_data_aces,2:ncol(death_data_aces),names_to = "group") %>% 
      rename(deaths=value)
    
    #temperature -----
    climate %<>% 
      mutate_at(vars(c(lng,lat)),as.numeric) %>% #converts lng and lat to numeric variable
      mutate_at(vars(dt),as.Date) %>% #converts date to date
      mutate(coord=map(.x = row_number(),.f = ~c(lng[.x],lat[.x]))) #merge lng and lat
    
    stations2 <- stations %>% 
      mutate_at(vars(c(lat,lng)),as.numeric) %>% #converts lng and lat to numeric
      filter(country %in% c("Portugal")) %>% #filter for country portugal
      filter(!is.na(lat)|!is.na(lng)) %>%  #clears NA values
      mutate(coord=lapply(1:nrow(.), function(x){c(lng[x],lat[x])})) #merge lng and lat variables
    
    centroides_aces2 <- centroides_aces %>% 
      mutate(coord=lapply(1:nrow(.), function(x){c(lng[x],lat[x])})) #merge lng and lat variables
    
    #removes days with no values for tmin, tmed, tmax
    climate2 <- climate %>% 
      filter(!(is.na(tar_min)|is.na(tar_med)|is.na(tar_max)))
    
    #creates dataframe with combination concelho - dia, that will be used to attribute temperature values, based on the closest station with data
    #you need to choose begining and ending date
    #creates a dataframe expanding each row on the select to a row for each day from start date to end date
    aces_dia <- crossing(aces=centroides_aces$linkage_geo_aces4,
                         dt=as_date(as.Date(StartDate):as.Date(EndDate))) %>% 
      left_join(centroides_aces2 %>% select(linkage_geo_aces4,coord),by = c("aces"="linkage_geo_aces4")) %>% 
      rename(coord_conc=coord) %>% 
      mutate(station_id=NA,
             station_name=NA,
             tar_min=NA,
             tar_med=NA,
             tar_max=NA)
    
    #Check closest five stations for each aces_centroid -----
    #Join temperature values from climate2, for aces + date
    #keeps first non-empty value of closest station
    
    for (j in 1:nrow(centroides_aces2)) {
      cur_conc <- centroides_aces2$linkage_geo_aces4[j]
      cur_conc_coord <- centroides_aces2$coord[j][[1]]
      
      closest_station <- lapply(1:nrow(stations2), function(x){distGeo(centroides_aces2$coord[j][[1]],stations2$coord[x][[1]])}) %>% 
        as_tibble_col(.) %>% unlist() %>% as_tibble #weird way to avoid "new names warning"....
      
      min_index <- which(closest_station$value== min(closest_station$value,na.rm = T)) #index value of the closest station
      second_index <- which(closest_station$value==sort(closest_station$value)[2]) #2nd smallest
      third_index <- which(closest_station$value==sort(closest_station$value)[3]) #3rd smallest...
      fourth_index <- which(closest_station$value==sort(closest_station$value)[4]) #3rd smallest...
      fifth_index <- which(closest_station$value==sort(closest_station$value)[5]) #3rd smallest...
      sixth_index <- which(closest_station$value==sort(closest_station$value)[6])
      seventh_index <- which(closest_station$value==sort(closest_station$value)[7])
      
      centroides_aces2$icao1[j] <- stations2$icao[min_index]
      centroides_aces2$icao2[j] <- stations2$icao[second_index]
      centroides_aces2$icao3[j] <- stations2$icao[third_index]
      centroides_aces2$icao4[j] <- stations2$icao[fourth_index]
      centroides_aces2$icao5[j] <- stations2$icao[fifth_index]
      centroides_aces2$icao6[j] <- stations2$icao[sixth_index]
      centroides_aces2$icao7[j] <- stations2$icao[seventh_index]
    }
    
    #add data from closest station -----
    #import values
    aces_dia3 <- aces_dia %>% 
      select(-(station_id:tar_max)) %>% 
      #closest
      left_join(centroides_aces2 %>% select(linkage_geo_aces4,icao1),by=c("aces"="linkage_geo_aces4")) %>% 
      left_join(climate2 %>% select(id, dt,tar_min,tar_med,tar_max),by=c("icao1"="id","dt"="dt")) %>%
      rename(tmin1=tar_min,
             tmax1=tar_max,
             tmed1=tar_med) %>%  
      #second closest
      left_join(centroides_aces2 %>% select(linkage_geo_aces4,icao2),by=c("aces"="linkage_geo_aces4")) %>% 
      left_join(climate2 %>% select(id, dt,tar_min,tar_med,tar_max),by=c("icao2"="id","dt"="dt")) %>% 
      rename(tmin2=tar_min,
             tmax2=tar_max,
             tmed2=tar_med) %>% 
      #third closest
      left_join(centroides_aces2 %>% select(linkage_geo_aces4,icao3),by=c("aces"="linkage_geo_aces4")) %>% 
      left_join(climate2 %>% select(id, dt,tar_min,tar_med,tar_max),by=c("icao3"="id","dt"="dt")) %>% 
      rename(tmin3=tar_min,
             tmax3=tar_max,
             tmed3=tar_med) %>% 
      #fourth closest
      left_join(centroides_aces2 %>% select(linkage_geo_aces4,icao4),by=c("aces"="linkage_geo_aces4")) %>% 
      left_join(climate2 %>% select(id, dt,tar_min,tar_med,tar_max),by=c("icao4"="id","dt"="dt")) %>% 
      rename(tmin4=tar_min,
             tmax4=tar_max,
             tmed4=tar_med) %>%   
      #fifth closest
      left_join(centroides_aces2 %>% select(linkage_geo_aces4,icao5),by=c("aces"="linkage_geo_aces4")) %>% 
      left_join(climate2 %>% select(id, dt,tar_min,tar_med,tar_max),by=c("icao5"="id","dt"="dt")) %>% 
      rename(tmin5=tar_min,
             tmax5=tar_max,
             tmed5=tar_med) %>%
      #sixth closest
      left_join(centroides_aces2 %>% select(linkage_geo_aces4,icao6),by=c("aces"="linkage_geo_aces4")) %>% 
      left_join(climate2 %>% select(id, dt,tar_min,tar_med,tar_max),by=c("icao6"="id","dt"="dt")) %>% 
      rename(tmin6=tar_min,
             tmax6=tar_max,
             tmed6=tar_med) %>%
      #seventh closest
      left_join(centroides_aces2 %>% select(linkage_geo_aces4,icao7),by=c("aces"="linkage_geo_aces4")) %>% 
      left_join(climate2 %>% select(id, dt,tar_min,tar_med,tar_max),by=c("icao7"="id","dt"="dt")) %>% 
      rename(tmin7=tar_min,
             tmax7=tar_max,
             tmed7=tar_med) %>%
      
      #get first non empty value -----
    mutate(tar_min=coalesce(tmin1,tmin2,tmin3,tmin4,tmin5,tmin6,tmin7),
           tar_med=coalesce(tmed1,tmed2,tmed3,tmed4,tmed5,tmin6,tmin7),
           tar_max=coalesce(tmax1,tmax2,tmax3,tmax4,tmax5,tmin6,tmin7)) %>% 
      select(-(icao1:tmax7)) %>% 
      group_by(aces) %>% 
      mutate(tar_min=zoo::na.approx(tar_min),
             tar_med=zoo::na.approx(tar_med),
             tar_max=zoo::na.approx(tar_max))  #interpolates missing values. around 0.5% of all values!!!
    
    #temperature for aces, weighted by concelho population proportion -----
    weighted_temperature_PT <- aces_dia3 %>% 
      left_join(pop_aces %>% 
                  select(aces4,prop_pop), by=c("aces"="aces4")) %>% 
      group_by(dt, aces) %>% 
      summarise(tmin=weighted.mean(x = tar_min,w = prop_pop),
                tmed=weighted.mean(x = tar_med,w = prop_pop),
                tmax=weighted.mean(x = tar_max,w = prop_pop))
    
    
    #predicted ET for ACES -----
    #PREDICTED TEMPERATURES -----
    ET_PT <- weighted_temperature_PT %>%
      clean_names() %>%
      select(dt, aces, tmin, tmed, tmax) %>%
      
      mutate(
        date_formated = as.Date(dt, "%m-%d-%Y")
      ) %>% 
      mutate(
        ISOweek = ISOweek(date_formated)
      ) %>%
      group_by(ISOweek, aces) %>% 
      summarise(
        med_temp = mean(tmed,na.rm=T),
        min_temp = mean(tmin,na.rm=T),
        max_temp = mean(tmax,na.rm=T)
      ) %>% 
      
      ungroup() %>% 
      mutate(
        wk=as.numeric(as.factor(ISOweek))
      ) %>% 
      mutate(
        sin52 = sin((2 * pi/(365.25/7)) * wk),
        cos52 = cos((2 * pi/(365.25/7)) * wk)
      ) %>%
      drop_na(med_temp) %>% 
      
      group_by(aces) %>%
      group_modify(~ {
        .x %>%
          mutate(ptemp=predict(glm(med_temp ~ sin52 + cos52, data = .)),
                 ptmin=predict(glm(min_temp ~ sin52 + cos52, data = .)),
                 ptmax=predict(glm(max_temp ~ sin52 + cos52, data = .)))
      })%>%
      ungroup() %>% 
      #important: there is ET when med_temp < ptmin and med_temp > ptmax
      #where ptmin and ptmax are the glm using min_temp and max_temp + sin and cos adjust
      mutate(ET=case_when(
        med_temp<ptmin ~ med_temp-ptmin,
        med_temp>ptmax ~ med_temp-ptmax,
        TRUE ~ 0)) %>% 
      select(ISOweek,ET,aces,ptmin,ptmax)
    
  }
  #REGIONAL-----
  #NACIONAL-----
## ATTMOMO FUNCTION -----

     
### ATTMOMO CALL -----     
if (geography_level == "aces"){
  for (gg in unique(death_data_aces_long$group)) {       # for-loop over columns
    paste(gg)
    country <- "Portugal"
    StartWeek <- ISOweek(StartDate)
    EndWeek <- ISOweek(EndDate)
    groups <- c(gg)
    #groups = c('Norte', 'Centro', 'LVT', 'Alentejo', 'Algarve') #ARS ou concelho
    #pooled <- c('Norte', 'Centro', 'LVT', 'Alentejo', 'Algarve') #ARS ou concelho
    #pooled <- c("all")
    #indicators <- c('covid', 'gripe')
    indicators <- NULL
    death_data <- death_data_aces_long %>% filter(group==gg)
    ET_data <- ET_PT %>% filter(aces==gg)
    #lags <- 3
    #ptrend <- 0.05
    #p26 <- 0.05
    #p52 <- 0.10
    
    AttData_Regioes <- AttMOMO_estimation_Davy(
     country = country,
     #wdir = wdir,
     StartWeek = StartWeek,
     EndWeek = EndWeek,
     groups = groups,
     #pooled = pooled,
     indicators = indicators,
     death_data = death_data,
     ET_data = ET_data,
     lags = lags,
     ptrend = ptrend,
     p26 = p26,
     p52 = p52)
    
    #create temporary obj for each region
    assign(x = paste0("AttData_Regioes___",gg),
           value = AttData_Regioes)
    
    #assign(x = paste0("AttData_Regioes_m_",gg),
    #       value = AttData_Regioes$m)
  }
  dfs <- lapply(ls(pattern="AttData_Regioes___"), function(x) get(x)) #coloca os ficheiros das regions numa lista
  AttData_Regioes <- data.table::rbindlist(dfs) #binds all in the list
  
  
  }
  return(AttData_Regioes)
}
#=== SICO_SCRAPE_FUNCTION === -----
get_evm_data <- function(type = "aces",mode = "scrape") {
    #only verified for ACES
    possible_arguments_type <- c("concelho",
                                 "geral",
                                 "idades",
                                 "causas",
                                 "externas",
                                 "local",
                                 "ars",
                                 "distrito",
                                 "aces")
    possible_arguments_mode <- c("scrape",
                                 "update")
    
    #PARAMETERS: add a parameter for the type of desired output -----
    ## available are: "concelho","geral","idades","causas","externas","local","ARS","distrito","ACES"
    if (isTRUE(type %in% possible_arguments_type)==FALSE){
      paste("Parameter not available. Options: concelho,geral,idades,causas,externas,local,ars,distrito,aces")
    }
    ## available are: "scrape", "update"
    if (isTRUE(mode %in% possible_arguments_mode)==FALSE){
      paste("Parameter not available. Options: scrape from 2014 to today, or update")
    }
    
    #type="geral"
    #creates my url variable
    if (str_to_lower(type) == "aces") {
      myurl <- paste0("https://evm.min-saude.pt/table?t=", str_to_upper(type), "&s=0")
    } else if (str_to_lower(type) == "ars") {
      myurl <- paste0("https://evm.min-saude.pt/table?t=", str_to_upper(type), "&s=0")
    } else {
      myurl <- paste0("https://evm.min-saude.pt/table?t=", type, "&s=0")
    }
    
    # Common steps for each type of extraction-----
    rv <- rvest::read_html(myurl) %>%
      rvest::html_nodes("script") %>%
      rvest::html_text() %>%
      as_tibble() %>%
      filter(!value == "")
    
    # extract header values
    rv_headers <- rv$value %>%
      as.list() %>%
      map(fromJSON) %>%
      map(., ~ .x$x$container) %>%
      map(., minimal_html) %>%
      map(., html_table) %>%
      map(., ~ .x[[1]]) %>%
      map(., ~ names(.x))
    
    # get the dataset (one list for each year)
    rv2 <- rv$value %>%
      as.list() %>%
      map(fromJSON) %>%
      map(., ~ .x$x$data) %>%
      map(t) %>%
      map(., ~ as_tibble(.x))
    
    # Part that is different for each type of extraction-----
    #geral -----
    #daily deaths since 2009 in Portugal
    if (type == "geral") {
      # case GERAL
      # assuming it will always start at 2009, and keep adding one year
      rv2_named <- purrr::map2(
        .x = rv2,
        .y = rv_headers,
        .f = ~ setNames(object = .x, nm = .y)
      ) %>%
        purrr::map_dfr(., ~ as_tibble(.x)) %>%
        pivot_longer(-Data, names_to = "temp_year", values_to = "deaths")
      
      rv3 <- rv2_named %>%
        janitor::clean_names() %>%
        mutate(
          temp_month = str_sub(data, 1, 3),
          temp_day = str_sub(data, 5, 6)
        ) %>%
        mutate(temp_mon = dplyr::case_when(
          temp_month == "Jan" ~ "01",
          temp_month == "Fev" ~ "02",
          temp_month == "Mar" ~ "03",
          temp_month == "Abr" ~ "04",
          temp_month == "Mai" ~ "05",
          temp_month == "Jun" ~ "06",
          temp_month == "Jul" ~ "07",
          temp_month == "Ago" ~ "08",
          temp_month == "Set" ~ "09",
          temp_month == "Out" ~ "10",
          temp_month == "Nov" ~ "11",
          temp_month == "Dez" ~ "12"
        )) %>%
        mutate(date = as.Date(paste(temp_year, temp_mon, temp_day, sep = "-"), format = "%Y-%m-%d")) %>%
        select(!starts_with("temp"), -data) %>%
        relocate(date, .before = 1)
      
      return(rv3)
    }
    #concelho-----
    else if (str_to_lower(type) == "concelho") {
      # assuming it will always start at 2014, and keep adding one year
      names(rv2) <- 2014:(2014 + nrow(rv) - 1)
      
      rv2_named <- map2(
        .x = rv2,
        .y = rv_headers,
        .f = ~ setNames(object = .x, nm = .y)
      ) %>%
        map_dfr(., ~ as_tibble(.x)) %>%
        #janitor::clean_names() %>%
        pivot_longer(cols = -Concelho, names_to = "week", values_to = "deaths") %>%
        drop_na(deaths) %>%
        group_by(Concelho, week) %>%
        mutate(deaths = as.numeric(deaths)) %>%
        summarise(deaths = sum(deaths))
      
      rv3 <- rv2_named %>%
        mutate(week = stringr::str_replace_all(week, pattern = "Semana ", replacement = "W")) %>%
        mutate(week = paste0(stringr::str_sub(week, 5, 8), "-", stringr::str_sub(week, 1, 3))) %>% 
        janitor::clean_names()
      # mutate(week = as.aweek(week)) %>%
      # mutate(w2 = week2date(week)) %>%
      # arrange(Concelho,w2)
      
      return(rv3)
    }
    #idades-----  
    else if (str_to_lower(type) == "idades") {
      # case IDADES----
      # assuming it will always start at 2014, and keep adding one year
      names(rv2) <- 2014:(2014 + nrow(rv) - 1)
      
      rv2_named <- purrr::map2(
        .x = rv2,
        .y = rv_headers,
        .f = ~ setNames(object = .x, nm = .y)
      ) %>%
        purrr::map_dfr(., ~ as_tibble(.x), .id = "temp_year") %>%
        pivot_longer(-c(1:2), names_to = "age_group", values_to = "deaths")
      
      rv3 <- rv2_named %>%
        janitor::clean_names() %>%
        mutate(
          temp_month = str_sub(data_mm_dd, 1, 3),
          temp_day = str_sub(data_mm_dd, 5, 6)
        ) %>%
        mutate(temp_mon = dplyr::case_when(
          temp_month == "Jan" ~ "01",
          temp_month == "Fev" ~ "02",
          temp_month == "Mar" ~ "03",
          temp_month == "Abr" ~ "04",
          temp_month == "Mai" ~ "05",
          temp_month == "Jun" ~ "06",
          temp_month == "Jul" ~ "07",
          temp_month == "Ago" ~ "08",
          temp_month == "Set" ~ "09",
          temp_month == "Out" ~ "10",
          temp_month == "Nov" ~ "11",
          temp_month == "Dez" ~ "12"
        )) %>%
        mutate(date = as.Date(paste(temp_year, temp_mon, temp_day, sep = "-"), format = "%Y-%m-%d")) %>%
        select(!starts_with("temp"), -data_mm_dd) %>%
        relocate(date, .before = 1)
      
      return(rv3)
    }
    #causas -----  
    else if (str_to_lower(type) == "causas") {
      # case CAUSAS----
      # assuming it will always start at 2014, and keep adding one year
      names(rv2) <- 2014:(2014 + nrow(rv) - 1)
      
      rv2_named <- map2(
        .x = rv2,
        .y = rv_headers,
        .f = ~ setNames(object = .x, nm = .y)
      ) %>%
        map_dfr(., ~ as_tibble(.x), .id = "temp_year") %>%
        janitor::clean_names() %>%
        mutate(
          temp_month = str_sub(data_mm_dd, 1, 3),
          temp_day = str_sub(data_mm_dd, 5, 6)
        ) %>%
        mutate(temp_mon = dplyr::case_when(
          temp_month == "Jan" ~ "01",
          temp_month == "Fev" ~ "02",
          temp_month == "Mar" ~ "03",
          temp_month == "Abr" ~ "04",
          temp_month == "Mai" ~ "05",
          temp_month == "Jun" ~ "06",
          temp_month == "Jul" ~ "07",
          temp_month == "Ago" ~ "08",
          temp_month == "Set" ~ "09",
          temp_month == "Out" ~ "10",
          temp_month == "Nov" ~ "11",
          temp_month == "Dez" ~ "12"
        )) %>%
        mutate(date = as.Date(paste(temp_year, temp_mon, temp_day, sep = "-"), format = "%Y-%m-%d")) %>%
        select(!starts_with("temp"), -data_mm_dd) %>%
        relocate(date, .before = 1) %>%
        pivot_longer(-date, names_to = "causes", values_to = "deaths")
      
      return(rv2_named)
    }
    #local -----
    else if (str_to_lower(type) == "local") {
      # case LOCAL----
      # assuming it will always start at 2014, and keep adding one year
      names(rv2) <- 2014:(2014 + nrow(rv) - 1)
      
      rv2_named <- map2(
        .x = rv2,
        .y = rv_headers,
        .f = ~ setNames(object = .x, nm = .y)
      ) %>%
        map_dfr(., ~ as_tibble(.x), .id = "temp_year") %>%
        janitor::clean_names() %>%
        mutate(
          temp_month = str_sub(data_mm_dd, 1, 3),
          temp_day = str_sub(data_mm_dd, 5, 6)
        ) %>%
        mutate(temp_mon = dplyr::case_when(
          temp_month == "Jan" ~ "01",
          temp_month == "Fev" ~ "02",
          temp_month == "Mar" ~ "03",
          temp_month == "Abr" ~ "04",
          temp_month == "Mai" ~ "05",
          temp_month == "Jun" ~ "06",
          temp_month == "Jul" ~ "07",
          temp_month == "Ago" ~ "08",
          temp_month == "Set" ~ "09",
          temp_month == "Out" ~ "10",
          temp_month == "Nov" ~ "11",
          temp_month == "Dez" ~ "12"
        )) %>%
        mutate(date = as.Date(paste(temp_year, temp_mon, temp_day, sep = "-"), format = "%Y-%m-%d")) %>%
        select(!starts_with("temp"), -data_mm_dd) %>%
        relocate(date, .before = 1) %>%
        pivot_longer(-date, names_to = "place_of_death", values_to = "deaths") %>%
        mutate(place_of_death = str_replace_all(string = place_of_death, pattern = "na_instituic_o_de_saude", replacement = "na_instituicao_de_saude"))
      
      return(rv2_named)
    }
    #ars -----  
    else if (str_to_lower(type) == "ars") {
      # case ARS----
      # assuming it will always start at 2014, and keep adding one year
      names(rv2) <- c("ARS Norte", "ARS Centro", "ARS LVT", "ARS Alentejo", "ARS Algarve", "RA Açores", "RA Madeira", "Estrangeiro", "Desconhecido")
      
      rv2_named <- map2(
        .x = rv2,
        .y = rv_headers,
        .f = ~ setNames(object = .x, nm = .y)
      ) %>%
        map_dfr(., ~ as_tibble(.x), .id = "ars") %>%
        pivot_longer(-c(1:2), names_to = "temp_year", values_to = "deaths") %>%
        janitor::clean_names() %>%
        mutate(
          temp_month = str_sub(data_mm_dd, 1, 3),
          temp_day = str_sub(data_mm_dd, 5, 6)
        ) %>%
        mutate(temp_mon = dplyr::case_when(
          temp_month == "Jan" ~ "01",
          temp_month == "Fev" ~ "02",
          temp_month == "Mar" ~ "03",
          temp_month == "Abr" ~ "04",
          temp_month == "Mai" ~ "05",
          temp_month == "Jun" ~ "06",
          temp_month == "Jul" ~ "07",
          temp_month == "Ago" ~ "08",
          temp_month == "Set" ~ "09",
          temp_month == "Out" ~ "10",
          temp_month == "Nov" ~ "11",
          temp_month == "Dez" ~ "12"
        )) %>%
        mutate(date = as.Date(paste(temp_year, temp_mon, temp_day, sep = "-"), format = "%Y-%m-%d")) %>%
        select(!starts_with("temp"), -data_mm_dd) %>%
        relocate(date, .before = 1)
      
      return(rv2_named)
    }
    #distrito -----
    else if (str_to_lower(type) == "distrito") {
      # case DISTRITO----
      # assuming it will always start at 2014, and keep adding one year
      names(rv2) <- 2014:(2014 + nrow(rv) - 1)
      
      rv2_named <- map2(
        .x = rv2,
        .y = rv_headers,
        .f = ~ setNames(object = .x, nm = .y)
      ) %>%
        map_dfr(., ~ as_tibble(.x), .id = "temp_year") %>%
        janitor::clean_names() %>%
        mutate(
          temp_month = str_sub(data_mm_dd, 1, 3),
          temp_day = str_sub(data_mm_dd, 5, 6)
        ) %>%
        mutate(temp_mon = dplyr::case_when(
          temp_month == "Jan" ~ "01",
          temp_month == "Fev" ~ "02",
          temp_month == "Mar" ~ "03",
          temp_month == "Abr" ~ "04",
          temp_month == "Mai" ~ "05",
          temp_month == "Jun" ~ "06",
          temp_month == "Jul" ~ "07",
          temp_month == "Ago" ~ "08",
          temp_month == "Set" ~ "09",
          temp_month == "Out" ~ "10",
          temp_month == "Nov" ~ "11",
          temp_month == "Dez" ~ "12"
        )) %>%
        mutate(date = as.Date(paste(temp_year, temp_mon, temp_day, sep = "-"), format = "%Y-%m-%d")) %>%
        select(!starts_with("temp"), -data_mm_dd) %>%
        relocate(date, .before = 1) %>% 
        rename(deaths = obitos)
      
      return(rv2_named)
    }
    #aces -----  
    else if (str_to_lower(type) == "aces") {
      # case ACES ----
      # assuming it will always start at 2014, and keep adding one year
      names(rv2) <- 2014:(2014 + nrow(rv) - 1)
      
      rv2_named <- map2(
        .x = rv2,
        .y = rv_headers,
        .f = ~ setNames(object = .x, nm = .y)
      ) %>%
        map_dfr(., ~ as_tibble(.x), .id = "temp_year") %>%
        janitor::clean_names() %>%
        mutate(
          temp_month = str_sub(data_mm_dd, 1, 3),
          temp_day = str_sub(data_mm_dd, 5, 6)
        ) %>%
        mutate(temp_mon = dplyr::case_when(
          temp_month == "Jan" ~ "01",
          temp_month == "Fev" ~ "02",
          temp_month == "Mar" ~ "03",
          temp_month == "Abr" ~ "04",
          temp_month == "Mai" ~ "05",
          temp_month == "Jun" ~ "06",
          temp_month == "Jul" ~ "07",
          temp_month == "Ago" ~ "08",
          temp_month == "Set" ~ "09",
          temp_month == "Out" ~ "10",
          temp_month == "Nov" ~ "11",
          temp_month == "Dez" ~ "12"
        )) %>%
        mutate(date = as.Date(paste(temp_year, temp_mon, temp_day, sep = "-"), format = "%Y-%m-%d")) %>%
        select(!starts_with("temp"), -data_mm_dd) %>%
        relocate(date, .before = 1) %>% 
        rename(deaths = obitos)
      #return(rv2_named)
      
      #new changes to adapt to attmomo functions
      #consider ACES in porto and gaia espinho as in the same ACES
      zx7_merged <- rv2_named %>% 
        mutate(aces_merged = case_when(
          aces %in% c("ACES Grande Porto V - Porto Ocidental","ACES Grande Porto VI - Porto Oriental") ~ "ACES Grande Porto V+VI",
          aces %in% c("ACES Grande Porto VII - Gaia","ACES Grande Porto VIII - Espinho/Gaia") ~ "ACES Grande Porto VII+VIII",
          TRUE ~ aces
        ))
      
      #pivot_wider
      by_merged_zx7 <- zx7_merged %>% 
        mutate(deaths = as.numeric(deaths)) %>% 
        select(date,deaths,aces_merged) %>% 
        group_by(date,aces_merged) %>% 
        summarise(merged_deaths=sum(deaths)) %>% 
        pivot_wider(names_from = aces_merged, values_from = merged_deaths)
      
      #export to output_file_sico
      rio::export(by_merged_zx7, output_file_sico)
    }
  }

#=== IPMA_SCRAPE_FUNCTION === -----
get_ipma_data <- function() {

#creates a list to get the station coordinates in the json
odd_indexes<-seq(1,394,2)
even_indexes <- seq(2,394,2)

d1 <- as.Date(starting_date, "%Y-%m-%d")
d2 <- as.Date(ending_date, "%Y-%m-%d")

#import json from api
stations <- rio::import(api_ipma_stations_link)


#use to quick test with only one station
#test_station <- data.frame(id="1210863",local="madeupzone",coord_x=1,coord_y=2)
#st <- test_station

#creates a station dataframe with (station) id, local, coord_x and coord_y
#uncomment to work properly!
stations_dataframe <- data.frame(id=stations$properties$idEstacao,
                                 local=stations$properties$localEstacao,
                                 coord_x=unlist(stations$geometry$coordinates)[even_indexes],
                                 coord_y=unlist(stations$geometry$coordinates)[odd_indexes])


#blank dataframe to use with bind_rows
#lng: weather station longitude
#lat: weather station latitude
#id: weather station id
#name: weather station name
#pr_nm_med: pressão atmosférica média em hPa
#hr_med: relative air humidity (%), daily average at 1.5m height
#rrr_qt: total rainfall (mm)
#tar_min: minimum air temperature (ºC) daily minimum at 1.5m height
#tar_med: average air temperature (ºC) daily average at 1.5m height
#ff_med: daily average wind intensity (km/h)
#dt: date
#tar_max: maximum air temperature (ºC) daily maximum at 1.5m height

output_dataframe <- data.frame(lng=NA,
                               lat=NA,
                               id=NA,
                               name=NA,
                               pr_nm_med=NA,
                               hr_med=NA,
                               rrr_qt=NA,
                               tar_min=NA,
                               tar_med=NA,
                               ff_med=NA,
                               dt=NA,
                               tar_max=NA)

out2 <- output_dataframe

#tryCatch function to register errors

readUrl <- function(url) {
  out <- tryCatch(
    {
      temp <- rio::import(url)
      
      temp2 <- data.frame(lng=temp$longitude,
                          lat=temp$latitude,
                          id=temp$station_id,
                          name=temp$station_name,
                          pr_nm_med=temp$data$Pr_Nm_med,
                          hr_med=temp$data$HR_med,
                          rrr_qt=temp$data$RRR_qt,
                          tar_min=temp$data$Tar_min,
                          tar_med=temp$data$Tar_med,
                          ff_med=temp$data$ff_med,
                          dt=temp$data$datetime,
                          tar_max=temp$data$Tar_max)
      return(temp2)
      
    },
    error=function(cond) {
      #adiciona uma mensagem com o erro
      if (!exists("erros")) {
        erros <<- paste0(s,"-",yy,"-",mm)
      } else {
        erros[length(erros)+1] <<- paste0(s,"-",yy,"-",mm)
      }
      # Choose a return value in case of error
      return(NA)
    },
    warning=function(cond) {
      # Choose a return value in case of warning
      return(NA)
    },
    finally={
      message(paste0(s,"-",yy,"-",mm))
    }
  )    
  return(out)
}


#loop through all stations
#loop through dates (d1 and d2)

dat <- format(seq(d1,d2,by="month"), "%Y%m%d")

for (s in stations_dataframe$id) {
  for (d in dat) {
    yy <- format(as.Date(d, "%Y%m%d"),"%Y")
    mm <- format(as.Date(d, "%Y%m%d"),"%m")
    m <- month(as.Date(d,format = "%Y%m%d"))
    
    #creates url with specific link to date and station
    url <- as.character(str_glue("http://api.ipma.pt/public-data/observation/surface-stations/daily_stations/{yy}/{mm}/obs_dia_{yy}_{m}_{s}.json"))
    
    #reads url as table
    url_table <- readUrl(url) %>% as_tibble()
    
    #if there are rows in the table, adds new row to out2
    if (nrow(url_table)>1 & exists("out2")) {
      out2 <- rbind(out2,url_table)
    }
  }
}

#drop na stations and change dates
out3 <- out2 %>% 
  drop_na(id) %>% 
  mutate(dt=as.Date(substr(dt,1,10),"%Y-%m-%d")) %>% 
  arrange(dt)

rio::export(out3, output_file_ipma)
}
