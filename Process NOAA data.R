#path <- getwd()
#file <- "repdata_data_StormData.csv.bz2"
#zippath <- paste(path, file, sep = "/")
#   unzip(zippath)

library(dplyr)
library(ggplot2)
library(lubridate)
library(stringdist)
#cache = TRUE

a <- Sys.time()
if (!exists("repdata")) {
    repdata <- read.csv2("repdata_data_StormData.csv.bz2", header = TRUE, sep = ",")
} #could use read.csv or read.table-- read.csv2 = 5.1 minutes
Sys.time()- a

#Removes uneeded columns, could probably remove Len, Wid, F, Mag.
repdata2 <- subset(repdata, select = c("STATE__","STATE", "EVTYPE", "BGN_DATE", "END_DATE", 
                           "LENGTH", "WIDTH", "F", "MAG", "FATALITIES", "INJURIES", "PROPDMG", 
                           "PROPDMGEXP", "CROPDMG", "CROPDMGEXP", "WFO"))
#Subsets data to only post 1995 data (when the valuable data began)
repdata2 <- subset(repdata2, year(strptime(repdata$BGN_DATE, "%m/%d/%Y %H:%M:%S")) > 1995)

#This modifies variable types
#NA's introcuded here
repdata2[, c("STATE__", "LENGTH", "WIDTH", "MAG", "FATALITIES","INJURIES", "PROPDMG", 
             "CROPDMG")] <- lapply(repdata2[c("STATE__", "LENGTH", "WIDTH", "MAG", "FATALITIES",
                                              "INJURIES", "PROPDMG", "CROPDMG")], function(x) as.numeric(as.character(x)))
#adds POSIXlt data types for startand end dates
repdata2 <- cbind(repdata2, 
      NewStartDate= strptime(repdata2$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"),
      NewEndDate= strptime(repdata2$END_DATE, format = "%m/%d/%Y %H:%M:%S")
      )

#There is a bETTER way to do this...
amatch(c("poop","winter", "winter m"), c("winter mix", "poop"), maxDist = 4, method = 'dl') or use default 'osa'




#The following Code Summarizes the data by different groups
states <- repdata2 %>% 
    group_by(EVTYPE, STATE) %>% 
    summarise(instances = n(), totFat = sum(FATALITIES), totInjur = sum(INJURIES), totPropDam = sum(PROPDMG), totCropDam = sum(CROPDMG))

events <- repdata2 %>% 
    group_by(EVTYPE) %>% 
    summarise(instances = n(), totFat = sum(FATALITIES), totInjur = sum(INJURIES), totPropDam = sum(PROPDMG), totCropDam = sum(CROPDMG))
events <- arrange(events, -totFat)

years <- repdata2 %>% 
    group_by(Year = year(NewStartDate)) %>%
    summarise(instances = n(), totFat = sum(FATALITIES), totInjur = sum(INJURIES), totPropDam = sum(PROPDMG), totCropDam = sum(CROPDMG))

yearsEvent <- repdata2 %>% 
  group_by(Year = year(NewStartDate), EVTYPE) %>%
  summarise(instances = n(), totFat = sum(FATALITIES), totInjur = sum(INJURIES), totPropDam = sum(PROPDMG), totCropDam = sum(CROPDMG))
####End of Data sumamrization

#The following cod creates different plots
plot(years$Year, years$totFat, main = "Fatalities by Year, all event Types")

plot(yearsEvent$Year, yearsEvent$totFat, col = yearsEvent$EVTYPE)
#legend("right", c(1, 2), legend =unique(yearsEvent$EVTYPE), col = unique(yearsEvent$EVTYPE), pch = 2,  )



ggplot()

#hint: Check REFNUM 605943. PROPDMGEXP is mis-coded. This should be self-evident from the magnitude of the number
#code that also works
repdataConcise <- select(repdata, STATE__, STATE, EVTYPE, BGN_DATE, BGN_TIME, END_DATE, END_TIME, 
                         TIME_ZONE, LENGTH, WIDTH, F, MAG, FATALITIES, INJURIES, PROPDMG, 
                         PROPDMGEXP, CROPDMG, CROPDMGEXP, WFO)
