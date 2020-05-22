library(dplyr)
library(lubridate)
library(data.table)

# setwd("Documents/R_i_Python/Praca_domowa_3/RiPython_project3")

mergeCsv <- function(dirPath,outputName){
  list.files(path = dirPath, pattern = "*.csv", full.names = TRUE) %>%
    lapply(function(fileName) read.csv(fileName, stringsAsFactors = FALSE)) %>%
    rbindlist() -> merged
  write.csv(merged, file = paste(c("csv_source/csv_merged/",outputName,".csv"), collapse = ''))
}

workplaces <- function(DF){
  DF %>%
    mutate(weekday = format(as.Date(stoptime), "%u")) %>%
    filter(weekday >= 1 & weekday <= 5) %>%
    filter(hour(stoptime) < 10) %>%
    group_by(end.station.id) %>%
    summarise(end.station.name[1], 
              end.station.latitude[1], 
              end.station.longitude[1], 
              hires = n(), 
              manHires = sum(gender == 1), 
              womanHires = sum(gender == 2)) %>%
    arrange(desc(hires)) -> Res
  
  colnames(Res) <- c("stationId", "stationName", "latitude", "longitude", "totalHires", "manHires", "womanHires")
  return(Res)
}

processWorkplaces <- function(){
  DF <- read.csv("csv_source/csv_merged/merged_JC_2019.csv")
  DF <- workplaces(DF)
  write.csv(DF,file="csv_results/workplaces_JC_2019.csv")
  print("Zapisano JC")
  
  DF <- read.csv("csv_source/csv_merged/merged_NY_2019.csv")
  DF <- workplaces(DF)
  write.csv(DF,file="csv_results/workplaces_NY_2019.csv")
  print("Zapisano NY")
}

hiringByAge <- function(DF){
  currentYear <- as.integer(format(Sys.Date(), "%Y"))
  DF %>%
    filter(hour(starttime) < 10) %>%
    mutate(age = case_when(
      currentYear - birth.year < 20 ~ 20,
      currentYear - birth.year < 40 ~ 40,
      currentYear - birth.year < 60 ~ 60,
      currentYear - birth.year < 80 ~ 80,
      TRUE ~ 0
    )) %>%
    group_by(end.station.id) %>%
      summarise(end.station.name[1], 
                end.station.latitude[1], 
                end.station.longitude[1], 
                under20 = sum(age == 20),
                under40 = sum(age == 40),
                under60 = sum(age == 60),
                under80 = sum(age == 80)) -> Res
  
  colnames(Res)[1:4] <- c("stationId", "stationName", "latitude", "longitude")
  return(Res)
}

processHiringByAge <- function(){
  DF <- read.csv("csv_source/csv_merged/merged_JC_2019.csv")
  DF <- hiringByAge(DF)
  write.csv(DF,file="csv_results/hiringByAge_JC_2019.csv")
  print("Zapisano JC")
  
  DF <- read.csv("csv_source/csv_merged/merged_NY_2019.csv")
  DF <- hiringByAge(DF)
  write.csv(DF,file="csv_results/hiringByAge_NY_2019.csv")
  print("Zapisano NY")
}

spendingFreeTime <- function(DF){
  DF %>%
    filter(hour(stoptime) > 10 & hour(stoptime) < 16) %>%
    mutate(weekday = format(as.Date(stoptime), "%u")) %>%
    filter(weekday >= 6) %>%
    group_by(end.station.id) %>%
    summarise(end.station.name[1], 
              end.station.latitude[1], 
              end.station.longitude[1], 
              hires = n()) -> Res
  
  colnames(Res) <- c("stationId", "stationName", "latitude", "longitude", "hires")
  return(Res)
}

processSpendingFreeTime <- function(){
  DF <- read.csv("csv_source/csv_merged/merged_JC_2019.csv")
  DF <- spendingFreeTime(DF)
  write.csv(DF,file="csv_results/spendingFreeTime_JC_2019.csv")
  print("Zapisano JC")
  
  DF <- read.csv("csv_source/csv_merged/merged_NY_2019.csv")
  DF <- spendingFreeTime(DF)
  write.csv(DF,file="csv_results/spendingFreeTime_NY_2019.csv")
  print("Zapisano NY")
}