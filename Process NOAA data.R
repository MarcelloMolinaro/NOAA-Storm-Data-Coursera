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

#This modifies variable types; NA's introcuded here
repdata2[, c("STATE__", "LENGTH", "WIDTH", "MAG", "FATALITIES","INJURIES", "PROPDMG", 
             "CROPDMG")] <- lapply(repdata2[c("STATE__", "LENGTH", "WIDTH", "MAG", "FATALITIES",
                                              "INJURIES", "PROPDMG", "CROPDMG")], function(x) as.numeric(as.character(x)))
#adds POSIXlt data types for start and end dates
repdata2 <- cbind(repdata2, 
      NewStartDate= strptime(repdata2$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"),
      NewEndDate= strptime(repdata2$END_DATE, format = "%m/%d/%Y %H:%M:%S")
      )

#no need to address numeric or lowercase Exponents, they are not in this subset.
#treats Damage exponents and creates new columns
repdata2 <- cbind(repdata2, 
                  cumPropDam = case_when(
                                  repdata2$PROPDMGEXP == "B" ~ repdata2$PROPDMG * 1000000000,
                                  repdata2$PROPDMGEXP == "M" ~ repdata2$PROPDMG * 1000000,
                                  repdata2$PROPDMGEXP == "K" ~ repdata2$PROPDMG * 1000
                  ),
                  cumCropDam = case_when(
                                  repdata2$CROPDMGEXP == "B" ~ repdata2$CROPDMG * 1000000000,
                                  repdata2$CROPDMGEXP == "M" ~ repdata2$CROPDMG * 1000000,
                                  repdata2$CROPDMGEXP == "K" ~ repdata2$CROPDMG * 1000  
                  )
            )

#Converts every event type to lowercase to consolidate types
repdata2$EVTYPE <- tolower(repdata2$EVTYPE)

#This is required for easier event cleaning, might re-run below
events <- repdata2 %>% 
    group_by(EVTYPE) %>% 
    summarise(instances = n(), 
              totFat = sum(FATALITIES, na.rm = TRUE), 
              totInjur = sum(INJURIES, na.rm = TRUE), 
              totPropDam = sum(cumPropDam, na.rm = TRUE), 
              totCropDam = sum(cumCropDam, na.rm = TRUE))
events <- arrange(events, -totFat)

#GO CLEAN YOUR DATA! COME BACK HERE WHEN IT IS CLEAN!

#The following Code Summarizes the data by different groups

events <- repdata2 %>% 
    group_by(lookup) %>% 
    summarise(instances = n(), 
              totFat = sum(FATALITIES, na.rm = TRUE), 
              totInjur = sum(INJURIES, na.rm = TRUE), 
              totPropDam = sum(cumPropDam, na.rm = TRUE), 
              totCropDam = sum(cumCropDam, na.rm = TRUE))
events <- arrange(events, -totFat)



yearsEvent <- repdata2 %>% 
  group_by(Year = year(NewStartDate), lookup) %>%
  summarise(instances = n(), 
            totFat = sum(FATALITIES, na.rm = TRUE), 
            totInjur = sum(INJURIES, na.rm = TRUE), 
            totPropDam = sum(cumPropDam, na.rm = TRUE), 
            totCropDam = sum(cumCropDam, na.rm = TRUE))
yearsEvent <- arrange(yearsEvent, -totFat)
####End of Data sumamrization
#not usre I actually use all of those subsets


#hint: Check REFNUM 605943. PROPDMGEXP is mis-coded. This should be self-evident from the magnitude of the number
