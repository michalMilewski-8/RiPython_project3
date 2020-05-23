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

mergeCsvFaster <- function(dirPath,outputName){
  list.files(path = dirPath, pattern = "*.csv", full.names = TRUE) %>%
    lapply(function(fileName){
      print(fileName)
      read.csv(fileName, stringsAsFactors = FALSE)}) %>%
    rbindlist(fill=TRUE) -> merged
  fwrite(merged, file = paste(c("csv_source/csv_merged/",outputName,".csv"), collapse = ''), verbose = TRUE, showProgress = TRUE)
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
    filter(hour(stoptime) < 10) %>%
    mutate(age = case_when(
      currentYear - birth.year < 20 ~ 20,
      currentYear - birth.year < 30 ~ 30,
      currentYear - birth.year < 40 ~ 40,
      currentYear - birth.year < 50 ~ 50,
      currentYear - birth.year < 60 ~ 60,
      currentYear - birth.year < 70 ~ 70,
      currentYear - birth.year < 80 ~ 80,
      TRUE ~ 0
    )) %>%
    group_by(end.station.id) %>%
      summarise(end.station.name[1], 
                end.station.latitude[1], 
                end.station.longitude[1], 
                under20 = sum(age == 20),
                under30 = sum(age == 30),
                under40 = sum(age == 40),
                under50 = sum(age == 50),
                under60 = sum(age == 60),
                under70 = sum(age == 70),
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

mostCommonStopPerStation <- function(DF){
  DF %>%
    mutate(DayTime = if_else(hour(stoptime)>0&hour(stoptime)<12,"morning","afternoon"))%>%
    mutate(weekday = as.numeric(format(as.Date(stoptime), "%u"))) %>%
    group_by(start.station.id, weekday,DayTime,end.station.id) %>%
    summarise(start.station.name = start.station.name[1],
              start.station.latitude = start.station.latitude[1],
              start.station.longitude = start.station.longitude[1],
              end.station.name = end.station.name[1],
              end.station.latitude = end.station.latitude[1],
              end.Station.Longitude = end.station.longitude[1],
              Count = n())->Res
  
  return(Res)
}

processMostCommonStopPerStation <- function(){
  DF <- as.data.table(fread("csv_source/csv_merged/NYC_2019_merged.csv",))
  DF <- mostCommonStopPerStation(DF)
  fwrite(DF,file="csv_results/MostCommonStopPerStation_NYC_2019.csv", showProgress = TRUE)
  
  DF <- as.data.table(fread("csv_source/csv_merged/JC_merged.csv"))
  DF <- mostCommonStopPerStation(DF)
  fwrite(DF,file="csv_results/MostCommonStopPerStation_JC.csv", showProgress = TRUE)
}

ridersByAge <- function(DF){
  DF %>%
    filter(usertype == "Subscriber" & !is.na(as.integer(birth.year)) & gender != 0) %>%
    mutate(AgeGroup = plyr::round_any((2019 - as.integer(birth.year)),10,f = floor)) %>%
    group_by(AgeGroup, gender)%>%
    summarise(Count = n(),Mean = mean(tripduration), Median = median(tripduration), Max = quantile(tripduration,.99), percentile90th = quantile(tripduration,.90)) -> Res
  
  return(Res)
}


processridersByAge <- function(){
  DF <- fread("csv_source/csv_merged/NYC_2019_merged.csv",stringsAsFactors = FALSE)
  DF <- ridersByAge(DF)
  fwrite(DF,file="csv_results/RidersByAgeAndGender_NYC_2019.csv", showProgress = TRUE)
  
  DF <- fread("csv_source/csv_merged/JC_merged.csv",stringsAsFactors = FALSE)
  DF <- ridersByAge(DF)
  fwrite(DF,file="csv_results/RidersByAgeAndGender_JC.csv", showProgress = TRUE)
}

bestTimeToRide <- function(DF){
  DF %>%
    mutate(DayHour = hour(starttime))%>%
    mutate(weekday = as.numeric(format(as.Date(stoptime), "%u"))) %>%
    group_by(weekday, DayHour)%>%
    summarise(Count = n(), Mean = mean(tripduration)) -> Res
  return(Res)
}

bestTimeToRideLong <- function(DF){
  highval = quantile(DF$tripduration,.98)
  DF %>%
    filter(tripduration >= highval) %>%
    mutate(DayHour = hour(starttime))%>%
    mutate(weekday = as.numeric(format(as.Date(stoptime), "%u"))) %>%
    group_by(weekday, DayHour)%>%
    summarise(Count = n(), Mean = mean(tripduration)) -> Res
  return(Res)
}


processridersbestTimeToRide <- function(){
  DF <- fread("csv_source/csv_merged/NYC_2019_merged.csv",stringsAsFactors = FALSE)
  DF <- bestTimeToRide(DF)
  fwrite(DF,file="csv_results/BestTimeToRider_NYC_2019.csv", showProgress = TRUE)
  
  DF <- fread("csv_source/csv_merged/JC_merged.csv",stringsAsFactors = FALSE)
  DF <- bestTimeToRide(DF)
  fwrite(DF,file="csv_results/BestTimeToRider_JC.csv", showProgress = TRUE)
}

processridersbestTimeToRideLong <- function(){
  DF <- fread("csv_source/csv_merged/NYC_2019_merged.csv",stringsAsFactors = FALSE)
  DF <- bestTimeToRideLong(DF)
  fwrite(DF,file="csv_results/bestTimeToRideLong_NYC_2019.csv", showProgress = TRUE)
  
  DF <- fread("csv_source/csv_merged/JC_merged.csv",stringsAsFactors = FALSE)
  DF <- bestTimeToRideLong(DF)
  fwrite(DF,file="csv_results/bestTimeToRideLong_JC.csv", showProgress = TRUE)
}

bikeRanking <- function(DF){
  DF %>%
    group_by(bikeid)%>%
    summarise(CountOfRenting = n(), SumOfRentedTime = sum(tripduration)) %>%
    mutate(AverageRentTime = SumOfRentedTime / CountOfRenting ) %>%
    arrange(desc(SumOfRentedTime)) -> Res
  return(Res)
}

processbikeRanking <- function(){
  DF <- fread("csv_source/csv_merged/NYC_2019_merged.csv",stringsAsFactors = FALSE)
  DF <- bikeRanking(DF)
  fwrite(DF,file="csv_results/bikeRanking_NYC_2019.csv", showProgress = TRUE)
  
  DF <- fread("csv_source/csv_merged/JC_merged.csv",stringsAsFactors = FALSE)
  DF <- bikeRanking(DF)
  fwrite(DF,file="csv_results/bikeRanking_JC.csv", showProgress = TRUE)
}

#processbikeRanking()
#processridersbestTimeToRide()
#processridersbestTimeToRideLong()
processridersByAge()
processMostCommonStopPerStation()