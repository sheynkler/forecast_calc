library(chron)
require(lubridate)

# 1. 


calc_Year <- function(x){
  df <- data.frame(x)
  
  df$n <- yday(x)
  df$year <- year(x)
  df$rest <- df$year %% 4
   
  df$dayyear <- ifelse(df$rest == 0, 366, 365) 
  df$dn <- df$n / df$dayyear
  return(df$year + df$dn)
}

calc_Month <- function(x){
  m31 <- c(1,3,5,7,8,10,12)
  m30 <- c(2,4,6,9,11)
  df <- data.frame(x)
  df$n <- mday(x)
  df$month <- month(x)
  df$rest <- year(x) %% 4
  
  #if(rest == 0)  daymonth2 <- 29 else daymonth2 <- 28
'  if(month %in% m31) maxmonth <- 31
  if(month %in% m30) maxmonth <- 30
  if(month == 2 & rest == 0) maxmonth <- 29
  if(month == 2 & rest != 0) maxmonth <- 28'
  
  df$maxmonth <- ifelse(df$month %in% m31, 31, 30)
  df$maxmonth <- ifelse(df$month == 2 & df$rest == 0, 29, df$maxmonth)
  df$maxmonth <- ifelse(df$month == 2 & df$rest != 0, 28, df$maxmonth)
  
  df$dn <- df$n / df$maxmonth
  return(df$month + df$dn - 1)
}


calc_Week <- function(x){
  n <- hour(x)
  weekday <- wday(x)
  #print(weekday)
  dn <- n / 24
  return(weekday + dn - 2)
}


calc_Hour <- function(x){
  n <- as.numeric(substr(x, 12,13))
  #n <- minutes(x)
  hour <- hour(x)
  #print(weekday)
  dn <- n / 60
  return(hour + dn )
}

# 2.

#time_predictor <- Sys.time()
#create_predictor(Sys.time())

create_predictor <- function(data_time, duration = 0, by = "hour"){
  t <- seq(data_time, data_time + duration, by = by)
  
  data_predictor_2 <- data.frame(predictor = t)
  
  
  data_predictor_2$weekday <-  calc_Week(t)
  

  data_predictor_2$monthday <- mday(t) - 1    #  день месяца
  data_predictor_2$yearday <- yday(t) -1     #  день года
  data_predictor_2$year <- calc_Year(t)    #  год с дробью
  
  data_predictor_2$month <-  calc_Month(t)   # месяц с дробью
  data_predictor_2$hour <- calc_Hour(t)      # часы с дробью
  
  
  data_predictor_2$year <- data_predictor_2$year - min(data_predictor_2$year) 
  #table(data_predictor_2$year)
  
  param_log_nn <- F
  if(param_log_nn) data_predictor_2$nn_log10 <- log10(1:nrow(data_predictor_2))
  
  
  
  #feiertage einstellen
  
  feiertage <- read.csv("feiertage.csv", header = T, sep = ";")
  feiertage$date <- as.Date(feiertage$TIMESTMP, "%d.%m.%Y") 
  
  
  data_predictor_2$day <- as.Date(data_predictor_2$predictor)

  
  tt <- match(data_predictor_2$day, feiertage$date)
  #tt[1:100]
  data_predictor_2$feiertage <- feiertage$FEIERTAG[tt]
  data_predictor_2$feiertage <- ifelse(is.na(data_predictor_2$feiertage), 0, data_predictor_2$feiertage)
  #table(data_predictor_2$feiertage)
  data_predictor_2$feiertage[1:100]
  data_predictor_2$day <- NULL
  
  #summary(data_predictor_2$monthday)
  
  
  
  
  PARAMETER_FUER_AG <- 4
  
  # hour
  
  for(i in 1:PARAMETER_FUER_AG){
    fur <- sin(i*2*pi*data_predictor_2$hour/24)
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "h", "sin", i, sep = "_")
    fur <- cos(i*2*pi*data_predictor_2$hour/24)
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "h", "cos", i, sep = "_")
    
    
  }
  
  # week day
  
  
  for(i in 1:PARAMETER_FUER_AG){
    fur <- sin(i*2*pi*data_predictor_2$weekday/7 )
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "w", "sin", i, sep = "_")
    fur <- cos(i*2*pi*data_predictor_2$weekday/7)
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "w", "cos", i, sep = "_")
    
    
  }
  
  # monthday
  
  year_month_param <- 365/12 -1
  
  
  for(i in 1:PARAMETER_FUER_AG){
    fur <- sin(i*2*pi*data_predictor_2$monthday/year_month_param)
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "d", "sin", i, sep = "_")
    fur <- cos(i*2*pi*data_predictor_2$monthday/year_month_param)
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "d", "cos", i, sep = "_")
    
    
  }
  # month
  
  for(i in 1:PARAMETER_FUER_AG){
    fur <- sin(i*2*pi*data_predictor_2$month/12)
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "m", "sin", i, sep = "_")
    fur <- cos(i*2*pi*data_predictor_2$month/12)
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "m", "cos", i, sep = "_")
    
    
  }
  
  
  
  #table(data_predictor_2$year)
  
  
  # year
  
  year_diff <- max(data_predictor_2$year) - min(data_predictor_2$year) + 1
  
  for(i in 1:PARAMETER_FUER_AG){
    fur <- sin(i*2*pi*data_predictor_2$year/year_diff)
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "y", "sin", i, sep = "_")
    fur <- cos(i*2*pi*data_predictor_2$year/year_diff)
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "y", "cos", i, sep = "_")
    
    
  }
  


  
  data_predictor_end <- data_predictor_2[8:ncol(data_predictor_2)]
  return(data_predictor_end)
  
  
}



create_predictor_2 <- function(data_time1, data_time2, by = "hour"){
  t <- seq(data_time1, data_time2, by = by)
  
  data_predictor_2 <- data.frame(predictor = t)
  
  
  data_predictor_2$weekday <-  calc_Week(t)
  
  
  data_predictor_2$monthday <- mday(t) - 1    #  день месяца
  data_predictor_2$yearday <- yday(t) -1     #  день года
  data_predictor_2$year <- calc_Year(t)    #  год с дробью
  
  data_predictor_2$month <-  calc_Month(t)   # месяц с дробью
  data_predictor_2$hour <- calc_Hour(t)      # часы с дробью
  
  
  data_predictor_2$year <- data_predictor_2$year - min(data_predictor_2$year) 
  #table(data_predictor_2$year)
  
  param_log_nn <- F
  if(param_log_nn) data_predictor_2$nn_log10 <- log10(1:nrow(data_predictor_2))
  
  
  
  #feiertage einstellen
  
  feiertage <- read.csv("feiertage.csv", header = T, sep = ";")
  feiertage$date <- as.Date(feiertage$TIMESTMP, "%d.%m.%Y") 
  
  
  data_predictor_2$day <- as.Date(data_predictor_2$predictor)
  
  
  tt <- match(data_predictor_2$day, feiertage$date)
  #tt[1:100]
  data_predictor_2$feiertage <- feiertage$FEIERTAG[tt]
  data_predictor_2$feiertage <- ifelse(is.na(data_predictor_2$feiertage), 0, data_predictor_2$feiertage)
  #table(data_predictor_2$feiertage)
  data_predictor_2$feiertage[1:100]
  data_predictor_2$day <- NULL
  
  #summary(data_predictor_2$monthday)
  
  
  
  
  PARAMETER_FUER_AG <- 4
  
  # hour
  
  for(i in 1:PARAMETER_FUER_AG){
    fur <- sin(i*2*pi*data_predictor_2$hour/24)
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "h", "sin", i, sep = "_")
    fur <- cos(i*2*pi*data_predictor_2$hour/24)
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "h", "cos", i, sep = "_")
    
    
  }
  
  # week day
  
  
  for(i in 1:PARAMETER_FUER_AG){
    fur <- sin(i*2*pi*data_predictor_2$weekday/7 )
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "w", "sin", i, sep = "_")
    fur <- cos(i*2*pi*data_predictor_2$weekday/7)
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "w", "cos", i, sep = "_")
    
    
  }
  
  # monthday
  
  year_month_param <- 365/12 -1
  
  
  for(i in 1:PARAMETER_FUER_AG){
    fur <- sin(i*2*pi*data_predictor_2$monthday/year_month_param)
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "d", "sin", i, sep = "_")
    fur <- cos(i*2*pi*data_predictor_2$monthday/year_month_param)
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "d", "cos", i, sep = "_")
    
    
  }
  # month
  
  for(i in 1:PARAMETER_FUER_AG){
    fur <- sin(i*2*pi*data_predictor_2$month/12)
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "m", "sin", i, sep = "_")
    fur <- cos(i*2*pi*data_predictor_2$month/12)
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "m", "cos", i, sep = "_")
    
    
  }
  
  
  
  #table(data_predictor_2$year)
  
  
  # year
  
  year_diff <- max(data_predictor_2$year) - min(data_predictor_2$year) + 1
  
  for(i in 1:PARAMETER_FUER_AG){
    fur <- sin(i*2*pi*data_predictor_2$year/year_diff)
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "y", "sin", i, sep = "_")
    fur <- cos(i*2*pi*data_predictor_2$year/year_diff)
    data_predictor_2[,(ncol(data_predictor_2)+1)] <- fur
    names(data_predictor_2)[ncol(data_predictor_2)] <- paste("f", "y", "cos", i, sep = "_")
    
    
  }
  
  
  
  
  data_predictor_end <- data_predictor_2[8:ncol(data_predictor_2)]
  return(data_predictor_end)
  
  
}
