#path <- getwd()
#file <- "repdata_data_StormData.csv.bz2"
#zippath <- paste(path, file, sep = "/")
#   unzip(zippath)

library(dplyr)
library(ggplot2)
library(lubridate)
#cache = TRUE

if (!exists("repdata")) {
    repdata <- read.csv2("repdata_data_StormData.csv.bz2", header = TRUE, sep = ",")
}

repdata2 <- subset(repdata, select = c("STATE__","STATE", "EVTYPE", "BGN_DATE", "BGN_TIME", "END_DATE", "END_TIME", 
                           "TIME_ZONE", "LENGTH", "WIDTH", "F", "MAG", "FATALITIES", "INJURIES", "PROPDMG", 
                           "PROPDMGEXP", "CROPDMG", "CROPDMGEXP", "WFO" ))

#convert all this number mumbo jumbo into Names so people know what i'm doing?
#repdata2 <- repdata2[1:50000,]
#repdata3 <- repdata2
repdata2[, c(1,9, 10, 12, 13:18)] <- sapply(repdata2[c(1,9, 10, 12, 13:18)], as.character)
repdata2[, c(1,9, 10, 12, 13:18)] <- sapply(repdata2[c(1,9, 10, 12, 13:18)], as.numeric)

#converts to POSIXlt class
repdata2$NewStartDate <- strptime(repdata2[, 4], format = "%m/%d/%Y %H:%M:%S") #time parsing, not needed, not totally functional
repdata2$NewEndDate   <- strptime(repdata2[, 6], format = "%m/%d/%Y %H:%M:%S") #strptime(repdata2[1:10, 5], format = "%I%M")

eventlist <- sort(unique(repdata2$EVTYPE))
eventlist2 <- sort(unique(repdata2$EVTYPE))
eventlist2 <- recode(eventlist2, "Wintry mix" = "WINTERY MIX")


#test <- repdata3[1:10000,]

states <- repdata2 %>% 
    group_by(EVTYPE, STATE) %>% 
    summarise(instances = n(), totFat = sum(FATALITIES), totInjur = sum(INJURIES), totPropDam = sum(PROPDMG), totCropDam = sum(CROPDMG))

events <- repdata2 %>% 
    group_by(EVTYPE) %>% 
    summarise(instances = n(), totFat = sum(FATALITIES), totInjur = sum(INJURIES), totPropDam = sum(PROPDMG), totCropDam = sum(CROPDMG))

years <- repdata2 %>% 
    group_by(Year = year(NewStartDate)) %>%
    summarise(instances = n(), totFat = sum(FATALITIES), totInjur = sum(INJURIES), totPropDam = sum(PROPDMG), totCropDam = sum(CROPDMG))

yearsEvent <- repdata2 %>% 
  group_by(Year = year(NewStartDate), EVTYPE) %>%
  summarise(instances = n(), totFat = sum(FATALITIES), totInjur = sum(INJURIES), totPropDam = sum(PROPDMG), totCropDam = sum(CROPDMG))

plot(years$Year, years$totFat, main = "Fatalities by Year, all event Types")

plot(yearsEvent$Year, yearsEvent$totFat, col = yearsEvent$EVTYPE)
#legend("right", c(1, 2), legend =unique(yearsEvent$EVTYPE), col = unique(yearsEvent$EVTYPE), pch = 2,  )

#I need to clean up these event type and combine them into like categories

ggplot()

#code that also works
repdataConcise <- select(repdata, STATE__, STATE, EVTYPE, BGN_DATE, BGN_TIME, END_DATE, END_TIME, 
                         TIME_ZONE, LENGTH, WIDTH, F, MAG, FATALITIES, INJURIES, PROPDMG, 
                         PROPDMGEXP, CROPDMG, CROPDMGEXP, WFO)
