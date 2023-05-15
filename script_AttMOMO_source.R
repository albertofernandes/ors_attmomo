AttMOMO_estimation_Davy <-  function (country, StartWeek, EndWeek, groups, pooled = NULL, 
                                      indicators, death_data, ET_data, lags = 3, ptrend = 0.05, 
                                      p26 = 0.05, p52 = 0.1) 
{
  group <- ET <- summer <- winter <- EB <- EAB <- season <- deaths <- VEB <- EET <- VEET <- VEAB <- wk <- . <- anova <- glm <- median <- residuals <- df.residual <- predict.glm <- quasipoisson <- NULL
  library(data.table)
  AttData <- data.table(death_data)
  #check if death_data has all the columns needed - group, ISOweek, deaths
  if (sum(colnames(AttData) %in% c("group", "ISOweek", 
                                   "deaths")) != 3) {
    stop("Columns group, ISOweek, deaths not in deaths_date")
  }
  #loop through function groups
  for (g in groups) {
    #check if all function groups  are in the death_data$group
    if (!(g %in% unique(AttData$group))) {
      stop(paste("group", g, "not in death_data"))
    }
    #check if each function group respects the start and end week
    if ((min(AttData[g == group, ]$ISOweek) > StartWeek) | 
        (max(AttData[g == group, ]$ISOweek) < EndWeek)) {
      stop(paste("death_data", g, "do not cover", 
                 StartWeek, "to", EndWeek))
    }
  }
  AttData <- AttData[(StartWeek <= ISOweek) & (ISOweek <= EndWeek) & 
                       (group %in% groups), ]
  AttData <- AttData[order(group, ISOweek), ]
  ET_data <- data.table(ET_data)
  #checks if columns ISOWeek and ET on ET_data
  if (sum(colnames(ET_data) %in% c("ISOweek", "ET")) != 
      2) {
    stop("Columns ISOweek, deaths not in ET_date")
  }
  #checks if ET_data respects start and end week (but not for each group?)
  if ((min(ET_data$ISOweek) > StartWeek) | (max(ET_data$ISOweek) < EndWeek)) {
    stop(paste("ET_data do not cover", StartWeek, "to", EndWeek))
  }
  AttData <- merge(AttData, ET_data[order(ISOweek), ], by = "ISOweek")
  
  for (i in indicators) {
    #checks if there is _data for each indicator
    X <- try(data.table(eval(parse(text = paste0(i, "_data")))))
    if (inherits(X, "try-error")) {
      stop(paste0("Could not read ", i, "_data"))
    }
    #checks if each indicator_data has columns group, ISOweek, indicator
    if (sum(colnames(X) %in% c("group", "ISOweek", 
                               i)) != 3) {
      stop(paste0("Columns group, ISOweek, ", i, 
                  "not in ", i, "_date"))
    }
    for (g in groups) {
      #checks if all groups are in _data
      if (!(g %in% unique(X$group))) {
        stop(paste0("group ", g, " not in ", 
                    i, "_data"))
      }
      #checks if each _data$group respects start and end week
      if ((min(X[g == group, ]$ISOweek) > StartWeek) | 
          (max(X[g == group, ]$ISOweek) < EndWeek)) {
        stop(paste0(i, "_data ", g, " do not cover ", 
                    StartWeek, " to ", EndWeek))
      }
    }
    #AttData with deaths, ET and _data
    AttData <- merge(AttData, X[order(group, ISOweek), ], 
                     by = c("group", "ISOweek"))
    rm(X)
  }
  #ends indicator loop
  
  #creates season = year - 1 if week < 27
  #summer = 1 if between W21 and W39
  #winter = 1 if before W20 and after W40
  AttData[, `:=`(season = as.numeric(substr(ISOweek,1, 4)) - (as.numeric(substr(ISOweek, 7, 8)) < 27), 
                 summer = as.numeric((21 <= as.numeric(substr(ISOweek, 7, 8))) & (as.numeric(substr(ISOweek, 7, 8)) <= 39)), 
                 winter = as.numeric((20 >= as.numeric(substr(ISOweek, 7, 8))) | (as.numeric(substr(ISOweek, 7, 8)) >= 40)))]
  
  
  AttData[, `:=`(cold_summer = -((ET < 0) * ET) * summer, #if ET < 0, in the summer, ET - protective
                 warm_summer = ((ET > 0) * ET) * summer, #if ET > 0, in the summer, ET - risk
                 cold_winter = -((ET < 0) * ET) * winter, #if ET < 0, in the winter, ET - risk
                 warm_winter = ((ET > 0) * ET) * winter)] #if ET > 0, in the winter, ET - protective
  
  AttData[, `:=`(summer = NULL, winter = NULL)]
  
  
  for (i in c(indicators, c("cold_summer", "warm_summer", "cold_winter", "warm_winter"))) {
    for (l in 0:lags) {
      expr <- parse(text = paste0(i, "_d", l, " := shift(", 
                                  i, ", ", l, ", type = 'lag')"))
      AttData[, eval(expr), by = group]
    }
  }
  for (i in indicators) {
    for (l in 0:lags) {
      for (s in unique(AttData$season)) {
        expr <- parse(text = paste0(i, "_d", l, 
                                    "_", s, " := ifelse(", s, " == season, ", 
                                    i, "_d", l, ", 0)"))
        AttData[, eval(expr), by = group]
      }
      expr <- parse(text = paste0(i, "_d", l, " := NULL"))
      AttData[, eval(expr)]
    }
  }
  AttData[, `:=`(cold_summer = NULL, warm_summer = NULL, 
                 cold_winter = NULL, warm_winter = NULL)]
  AttData[is.na(AttData)] <- 0
  AttData[, `:=`(wk, as.numeric(as.factor(ISOweek)))]
  AttData[, `:=`(const = 1, 
                 sin52 = sin((2 * pi/(365.25/7)) * wk), 
                 cos52 = cos((2 * pi/(365.25/7)) * wk), 
                 sin26 = sin((4 * pi/(365.25/7)) * wk), 
                 cos26 = cos((4 * pi/(365.25/7)) * wk))]
  
  AttData.B <- copy(AttData)
  
  for (l in 0:lags) {
    for (i in c("cold_summer", "warm_summer", 
                "cold_winter", "warm_winter")) {
      expr <- parse(text = paste0(i, "_d", l, ":= 0"))
      AttData.B[, eval(expr)]
    }
    for (i in indicators) {
      for (s in unique(AttData$season)) {
        expr <- parse(text = paste0(i, "_d", l, 
                                    "_", s, " := 0"))
        AttData.B[, eval(expr)]
      }
    }
  }
  AttData.ET <- copy(AttData)
  AttData.ET[, `:=`(const = 0, wk = 0, sin52 = 0, cos52 = 0, 
                    sin26 = 0, cos26 = 0)]
  for (i in indicators) {
    for (l in 0:lags) {
      for (s in unique(AttData$season)) {
        expr <- parse(text = paste0(i, "_d", l, 
                                    "_", s, " := 0"))
        AttData.ET[, eval(expr)]
      }
    }
  }
  for (i in indicators) {
    X <- copy(AttData)
    X[, `:=`(const = 0, wk = 0, sin52 = 0, cos52 = 0, 
             sin26 = 0, cos26 = 0)]
    for (l in 0:lags) {
      for (ir in c("cold_summer", "warm_summer", 
                   "cold_winter", "warm_winter")) {
        expr <- parse(text = paste0(ir, "_d", l, 
                                    ":= 0"))
        X[, eval(expr)]
      }
      for (ir in indicators[indicators != i]) {
        for (s in unique(AttData$season)) {
          expr <- parse(text = paste0(ir, "_d", 
                                      l, "_", s, " := 0"))
          X[, eval(expr)]
        }
      }
    }
    assign(paste0("AttData.", i), X)
  }
  parm <- paste(grep("_d[0-9]", names(AttData), value = TRUE), 
                collapse = " + ")
  
  #**********************************************************************************
  #*ALTEREI MANUALMENTE O parm PARA APAGAR OS LAGS 2 E 3 DE TEPERATURA
  #*depois cntinuar a correr
  #**********************************************************************************
  #EXEMPLO: parm <- "cold_summer_d0 + cold_summer_d1 + cold_summer_d2 + cold_summer_d3 + warm_summer_d0 + warm_summer_d1 + warm_summer_d2 + warm_summer_d3 + cold_winter_d0 + cold_winter_d1 + cold_winter_d2 + cold_winter_d3 + warm_winter_d0 + warm_winter_d1 + warm_winter_d2 + warm_winter_d3 + covid_d0_2013 + covid_d0_2014 + covid_d0_2015 + covid_d0_2016 + covid_d0_2017 + covid_d0_2018 + covid_d0_2019 + covid_d0_2020 + covid_d1_2013 + covid_d1_2014 + covid_d1_2015 + covid_d1_2016 + covid_d1_2017 + covid_d1_2018 + covid_d1_2019 + covid_d1_2020 + covid_d2_2013 + covid_d2_2014 + covid_d2_2015 + covid_d2_2016 + covid_d2_2017 + covid_d2_2018 + covid_d2_2019 + covid_d2_2020 + covid_d3_2013 + covid_d3_2014 + covid_d3_2015 + covid_d3_2016 + covid_d3_2017 + covid_d3_2018 + covid_d3_2019 + covid_d3_2020 + gripe_d0_2013 + gripe_d0_2014 + gripe_d0_2015 + gripe_d0_2016 + gripe_d0_2017 + gripe_d0_2018 + gripe_d0_2019 + gripe_d0_2020 + gripe_d1_2013 + gripe_d1_2014 + gripe_d1_2015 + gripe_d1_2016 + gripe_d1_2017 + gripe_d1_2018 + gripe_d1_2019 + gripe_d1_2020 + gripe_d2_2013 + gripe_d2_2014 + gripe_d2_2015 + gripe_d2_2016 + gripe_d2_2017 + gripe_d2_2018 + gripe_d2_2019 + gripe_d2_2020 + gripe_d3_2013 + gripe_d3_2014 + gripe_d3_2015 + gripe_d3_2016 + gripe_d3_2017 + gripe_d3_2018 + gripe_d3_2019 + gripe_d3_2020"
  #Apagar os valores não desejados
  
  #parm <-  "cold_summer_d0 + cold_summer_d1 + warm_summer_d0 + warm_summer_d1 + cold_winter_d0 + cold_winter_d1 + warm_winter_d0 + warm_winter_d1 + covid_d0_2013 + covid_d0_2014 + covid_d0_2015 + covid_d0_2016 + covid_d0_2017 + covid_d0_2018 + covid_d0_2019 + covid_d0_2020 + covid_d1_2013 + covid_d1_2014 + covid_d1_2015 + covid_d1_2016 + covid_d1_2017 + covid_d1_2018 + covid_d1_2019 + covid_d1_2020 + covid_d2_2013 + covid_d2_2014 + covid_d2_2015 + covid_d2_2016 + covid_d2_2017 + covid_d2_2018 + covid_d2_2019 + covid_d2_2020 + covid_d3_2013 + covid_d3_2014 + covid_d3_2015 + covid_d3_2016 + covid_d3_2017 + covid_d3_2018 + covid_d3_2019 + covid_d3_2020 + gripe_d0_2013 + gripe_d0_2014 + gripe_d0_2015 + gripe_d0_2016 + gripe_d0_2017 + gripe_d0_2018 + gripe_d0_2019 + gripe_d0_2020 + gripe_d1_2013 + gripe_d1_2014 + gripe_d1_2015 + gripe_d1_2016 + gripe_d1_2017 + gripe_d1_2018 + gripe_d1_2019 + gripe_d1_2020 + gripe_d2_2013 + gripe_d2_2014 + gripe_d2_2015 + gripe_d2_2016 + gripe_d2_2017 + gripe_d2_2018 + gripe_d2_2019 + gripe_d2_2020 + gripe_d3_2013 + gripe_d3_2014 + gripe_d3_2015 + gripe_d3_2016 + gripe_d3_2017 + gripe_d3_2018 + gripe_d3_2019 + gripe_d3_2020"
  
  #parm <-  "cold_summer_d0 + cold_summer_d1 + warm_summer_d0 + warm_summer_d1 + cold_winter_d0 + cold_winter_d1 + warm_winter_d0 + warm_winter_d1 + covid_d0_2019 + covid_d0_2020 + covid_d0_2021 + covid_d1_2019 + covid_d1_2020 + covid_d1_2021 + covid_d2_2019 + covid_d2_2020 + covid_d2_2021 + covid_d3_2019 + covid_d3_2020 + covid_d3_2021 + gripe_d0_2013 + gripe_d0_2014 + gripe_d0_2015 + gripe_d0_2016 + gripe_d0_2017 + gripe_d0_2018 + gripe_d0_2019 + gripe_d0_2020 + gripe_d1_2013 + gripe_d1_2014 + gripe_d1_2015 + gripe_d1_2016 + gripe_d1_2017 + gripe_d1_2018 + gripe_d1_2019 + gripe_d1_2020 + gripe_d2_2013 + gripe_d2_2014 + gripe_d2_2015 + gripe_d2_2016 + gripe_d2_2017 + gripe_d2_2018 + gripe_d2_2019 + gripe_d2_2020 + gripe_d3_2013 + gripe_d3_2014 + gripe_d3_2015 + gripe_d3_2016 + gripe_d3_2017 + gripe_d3_2018 + gripe_d3_2019 + gripe_d3_2020"
  
  #prediction add on
  first_isoweek <- ISOweek(as.Date(EndDate) + 7)
  second_isoweek <- ISOweek(as.Date(EndDate) + 14)
  
  AttData <- AttData %>% add_column(prediction_week = "No")
  
  AttData <- AttData %>%
    mutate(excess_temperature = case_when(
      ET != 0 ~ "Yes",
      ET == 0 ~ "No"))
  #prediction add on
  
  
  for (g in groups) {
    print(paste("### Group", g, "###"))
    f <- paste(c("deaths ~ -1 + const + wk", "sin52 + cos52", 
                 "sin26 + cos26", parm), collapse = " + ")
    m <- try(glm(f, quasipoisson(identity), data = AttData[group == 
                                                             g, ]), silent = TRUE)
    if ((!inherits(m, "try-error")) & (median(AttData[group == 
                                                      g, ]$deaths) > 0)) {
      if (m$converged) {
        fa <- paste(c("deaths ~ -1 + const + wk", 
                      parm), collapse = " + ")
        ma <- glm(fa, quasipoisson(identity), data = AttData[group == 
                                                               g, ])
        if (anova(m, ma, dispersion = max(1, sum(residuals(m, 
                                                           type = "deviance")^2)/df.residual(m)), 
                  test = "LRT")$`Pr(>Chi)`[2] > max(p52, 
                                                    p26)) {
          m <- ma
        }
        else {
          fa <- paste(c("deaths ~ -1 + const + wk", 
                        "sin52 + cos52", parm), collapse = " + ")
          ma <- glm(fa, quasipoisson(identity), data = AttData[group == 
                                                                 g, ])
          if (anova(m, ma, dispersion = max(1, sum(residuals(m, 
                                                             type = "deviance")^2)/df.residual(m)), 
                    test = "LRT")$`Pr(>Chi)`[2] > 
              p26) {
            m <- ma
          }
          else {
            fa <- paste(c("deaths ~ -1 + const + wk", 
                          parm), collapse = " + ")
            ma <- glm(fa, quasipoisson(identity), data = AttData[group == 
                                                                   g, ])
            if (anova(m, ma, dispersion = max(1, sum(residuals(m, 
                                                               type = "deviance")^2)/df.residual(m)), 
                      test = "LRT")$`Pr(>Chi)`[2] > 
                p52) {
              m <- ma
            }
          }
        }
      }
      else {
        f <- paste(c("deaths ~ -1 + const + wk", 
                     parm), collapse = " + ")
        m <- try(glm(f, quasipoisson(identity), data = AttData[group == 
                                                                 g, ]), silent = TRUE)
        if ((inherits(m, "try-error")) | (!m$converged)) {
          print(paste("The model did not converge. A Simple model with only trend used i.e. no effect of indicators"))
          f <- paste(c("deaths ~ -1 + const + wk"))
          m <- glm(f, quasipoisson(identity), data = AttData[group == 
                                                               g, ])
        }
      }
    }
    else {
      if (median(AttData[group == g, ]$deaths) == 0) {
        msg <- "Zero inflated."
      }
      else {
        msg <- NULL
      }
      if (inherits(m, "try-error")) 
        print(paste("Could not fit model.", msg, 
                    "Simple model with only trend used i.e. no effect of indicators"))
      f <- paste(c("deaths ~ -1 + const + wk"))
      m <- glm(f, quasipoisson(identity), data = AttData[group == 
                                                           g, ])
    }
    f <- paste("deaths ~ -1 +", paste(names(m$coefficients[!is.na(m$coefficients)]), 
                                      collapse = " + "))
    m <- glm(f, quasipoisson(identity), data = AttData[group == 
                                                         g, ])
    print(summary(m, dispersion = max(1, sum(residuals(m, 
                                                       type = "deviance")^2)/df.residual(m))))
    #berto
    AttData[group == g, `:=`(EB = predict.glm(m, newdata = AttData.B[group == 
                                                                       g, ], se.fit = TRUE)$fit)]
    AttData[group == g, `:=`(VEB = (max(1, sum(residuals(m)^2)/df.residual(m))) * 
                               EB + predict.glm(m, newdata = AttData.B[group == 
                                                                         g, ], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), 
                                                se.fit = TRUE)$se.fit^2)]
    AttData[group == g, `:=`(EET = predict.glm(m, newdata = AttData.ET[group == 
                                                                         g, ], se.fit = TRUE)$fit, 
                             VEET = predict.glm(m, newdata = AttData.ET[group == 
                                                                          g, ], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), 
                                                se.fit = TRUE)$se.fit^2)]
    for (i in indicators) {
      expr <- parse(text = paste0("`:=`(E", i, " = predict.glm(m, newdata = AttData.", 
                                  i, "[group == g,], se.fit=TRUE)$fit,\n      VE", 
                                  i, " = predict.glm(m, newdata = AttData.", 
                                  i, "[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)"))
      AttData[group == g, eval(expr)]
    }
    X <- copy(AttData)
    for (i in indicators) {
      for (l in 0:lags) {
        for (s in unique(AttData$season)) {
          expr <- parse(text = paste0(i, "_d", 
                                      l, "_", s, " := ifelse(E", i, 
                                      " >= 0, 0,", i, "_d", l, "_", 
                                      s, ")"))
          X[, eval(expr)]
        }
      }
    }
    AttData[group == g, `:=`(EAB = predict.glm(m, newdata = X[group == 
                                                                g, ], se.fit = TRUE)$fit)]
    AttData[group == g, `:=`(VEAB = (max(1, sum(residuals(m)^2)/df.residual(m))) * 
                               EAB + predict.glm(m, newdata = X[group == g, ], dispersion = max(1, 
                                                                                                sum(residuals(m)^2)/df.residual(m)), se.fit = TRUE)$se.fit^2)]
    for (i in indicators) {
      X <- copy(AttData)
      X[, `:=`(const = 0, wk = 0, sin52 = 0, cos52 = 0, 
               sin26 = 0, cos26 = 0)]
      for (l in 0:lags) {
        for (ir in c("cold_summer", "warm_summer", 
                     "cold_winter", "warm_winter")) {
          expr <- parse(text = paste0(ir, "_d", 
                                      l, ":= 0"))
          X[, eval(expr)]
        }
        for (l in 0:lags) {
          for (s in unique(AttData$season)) {
            for (ir in indicators[indicators != i]) {
              expr <- parse(text = paste0(ir, "_d", 
                                          l, "_", s, " := 0"))
              X[, eval(expr)]
            }
            expr <- parse(text = paste0(i, "_d", 
                                        l, "_", s, " := ifelse(E", 
                                        i, " < 0, 0,", i, "_d", l, 
                                        "_", s, ")"))
            X[, eval(expr)]
          }
        }
      }
      expr <- parse(text = paste0("`:=`(EA", i, " = predict.glm(m, newdata = X[group == g,], se.fit=TRUE)$fit,\n                                  VEA", 
                                  i, " = predict.glm(m, newdata = X[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)"))
      AttData[group == g, eval(expr)]
    }
    rm(X)
    
  }
  AttData[, `:=`(const = NULL, wk = NULL, sin52 = NULL, 
                 cos52 = NULL, sin26 = NULL, cos26 = NULL)]
  for (l in 0:lags) {
    for (i in c("cold_summer", "warm_summer", 
                "cold_winter", "warm_winter")) {
      expr <- parse(text = paste0(i, "_d", l, " := NULL"))
      AttData[, eval(expr)]
    }
    for (i in indicators) {
      for (s in unique(AttData$season)) {
        expr <- parse(text = paste0(i, "_d", l, 
                                    "_", s, " := NULL"))
        AttData[, eval(expr)]
      }
    }
  
    
    
  }
  ###predict add on
  for (g in groups) {
    
  country_pr <- c("Portugal","Portugal")
  ISOweek_pr <- c(first_isoweek,second_isoweek)
  group_pr <- c(g,g)
  deaths_pr <- c(NA,NA)
  ET_pr  <- c(NA,NA)
  aces_pr <- c(g,g)
  ptmin_pr  <- c(NA,NA)
  ptmax_pr  <- c(NA,NA)
  season_pr <- c(2023,2023)
  EB_pr  <- c(NA,NA)
  VEB_pr  <- c(NA,NA)
  EET_pr  <- c(NA,NA)
  VEET_pr  <- c(NA,NA)
  EAB_pr  <- c(NA,NA)
  VEAB_pr  <- c(NA,NA)
  prediction_week_pr <- c("Yes","Yes")
  excess_temperature <- c(NA,NA)
  
  append_end_weeks <- data.frame(ISOweek_pr, group_pr, deaths_pr, ET_pr, aces_pr, ptmin_pr,
                                 ptmax_pr, season_pr, EB_pr, VEB_pr, EET_pr, VEET_pr, EAB_pr, VEAB_pr, 
                                 prediction_week_pr, excess_temperature)
  
  
  #append_end_weeks
  #AttData <- rbind(AttData, append_end_weeks, use.names=FALSE)
  
  #AttData[group == g, `:=`(EB = predict.glm(m, newdata = AttData.B[group == g, ], se.fit = TRUE)$fit)]
  #AttData[group == g, `:=`(VEB = (max(1, sum(residuals(m)^2)/df.residual(m))) * 
  #                           EB + predict.glm(m, newdata = AttData.B[group == 
  #                                                                     g, ], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), 
  #                                            se.fit = TRUE)$se.fit^2)]
  #AttData[group == g, `:=`(EET = predict.glm(m, newdata = AttData.ET[group == 
  #                                                                     g, ], se.fit = TRUE)$fit, 
  #                         VEET = predict.glm(m, newdata = AttData.ET[group == 
  #                                                                      g, ], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), 
  #                                            se.fit = TRUE)$se.fit^2)]
  #rm(append_end_weeks)
  #rm(country_pr, ISOweek_pr, group_pr, deaths_pr, ET_pr, aces_pr, ptmin_pr,
  #   ptmax_pr, EB_pr, VEB_pr, EET_pr, VEET_pr, EAB_pr, VEAB_pr)
  #}
  ###predict add on
  
  
  AttData[, `:=`(season, NULL)]
  
  rm(fa, expr, parm, g, i, l, ma, AttData.B, AttData.ET)
  if (!is.null(pooled)) {
    pooledData <- AttData[group %in% pooled, .(group = "TotalPooled", 
                                               deaths = sum(deaths, na.rm = TRUE), ET = mean(ET, 
                                                                                             na.rm = TRUE), EB = sum(EB, na.rm = TRUE), VEB = sum(VEB, 
                                                                                                                                                  na.rm = TRUE), EET = sum(EET, na.rm = TRUE), 
                                               VEET = sum(VEET, na.rm = TRUE), EAB = sum(EAB, na.rm = TRUE), 
                                               VEAB = sum(VEAB, na.rm = TRUE)), keyby = ISOweek]
    for (i in indicators) {
      expr <- parse(text = paste0(".(", i, " = NA,\n                                     E", 
                                  i, " = sum(E", i, ", na.rm = TRUE),\n                                     VE", 
                                  i, " = sum(VE", i, ", na.rm = TRUE),\n                                     EA", 
                                  i, " = sum(EA", i, ", na.rm = TRUE),\n                                     VEA", 
                                  i, " = sum(VEA", i, ", na.rm = TRUE))"))
      pooledData <- merge(pooledData, AttData[group %in% 
                                                pooled, eval(expr), keyby = ISOweek], by = "ISOweek", 
                          all.x = TRUE)
    }
    AttData <- rbind(AttData, pooledData)
  }
  AttData <- cbind(country, AttData)
  AttData <- AttData[order(country, group, ISOweek)]
  return(AttData)
  #return(m)
  }
}
