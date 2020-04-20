#path <- getwd()
#file <- "repdata_data_StormData.csv.bz2"
#zippath <- paste(path, file, sep = "/")
#   unzip(zippath)

library(dplyr)
cahce = TRUE

if (!exists("repdata")) {
    repdata <- read.csv2("repdata_data_StormData.csv.bz2", header = TRUE, sep = ",")
}


repdataConcise <- select(repdata, STATE__, STATE, EVTYPE, BGN_DATE, BGN_TIME, END_DATE, END_TIME, 
                         TIME_ZONE, LENGTH, WIDTH, F, MAG, FATALITIES, INJURIES, PROPDMG, 
                         PROPDMGEXP, CROPDMG, CROPDMGEXP, WFO) #people warn against using "column names as if they were object names", but others are fine with it


repdata2 <- subset(repdata, select = c("STATE__","STATE", "EVTYPE", "BGN_DATE", "BGN_TIME", "END_DATE", "END_TIME", 
                           "TIME_ZONE", "LENGTH", "WIDTH", "F", "MAG", "FATALITIES", "INJURIES", "PROPDMG", 
                           "PROPDMGEXP", "CROPDMG", "CROPDMGEXP", "WFO" ))

#convert all this number mumbo jumbo into Names so people know what i'm doing?
repdata3 <- repdata2
repdata3[, c(1,9, 10, 12, 13:18)] <- sapply(repdata2[c(1,9, 10, 12, 13:18)], as.character)
repdata3[, c(1,9, 10, 12, 13:18)] <- sapply(repdata2[c(1,9, 10, 12, 13:18)], as.numeric)

repdata3[, c(5, 7)]  <- as.Date.POSIXct( #or is it lt?
repdata3[, c(6, 8)] <- as.POSIXlt() #what do we do for time?u