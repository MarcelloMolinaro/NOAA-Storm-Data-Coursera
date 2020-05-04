#clean this better thanever
#extracts data from table 2.1.something that I pasted into a textfile
eCodes <- read.table("Eventcodes.txt", sep = "\n", header = TRUE)

#Gets rid of event type designation ("c, m, z"), everything to lowercase
eCodes2 <- tolower(sapply(eCodes, function(x) gsub(" [CMZ]$", "", x)))
eCodes2 <- rbind(eCodes2, "CleanedUpData")

#list of messy data event types, already lowercase
crapdata <- unique(repdata2$EVTYPE)
#as result, converts from factors to chr type

#Determine which codes can be removed. Keep only prop.crop dmg greater than 1M, Fatalities >0, instances of only 1 or 2
leastevents <- subset(arrange(events, instances), instances < 3)
#These are purely exploratory to get the values I chose
arrange(leastevents, -totPropDam)
arrange(leastevents, -totPropDam)
arrange(leastevents, -totFat)
arrange(leastevents, -instances)

#list of EVTYPES to keep from <3 intance occurence
eveKeep<- unique(rbind(filter(leastevents, totPropDam >= 1000000) %>% select(EVTYPE),
              filter(leastevents, totCropDam >= 1000000) %>% select(EVTYPE),
              filter(leastevents, totFat >= 1) %>% select(EVTYPE)
            )
)


#list of EVTYPES that we can get rid of...
garbage <- filter(leastevents, (!EVTYPE %in% eveKeep$EVTYPE))
#list of "Summary" events we can get rid...
summary <- filter(events, grepl("summary", EVTYPE, fixed = TRUE))

#replaces bad data with placeholder
crapdata <- case_when(
      crapdata %in% garbage$EVTYPE ~ "garbage", 
      crapdata %in% summary$EVTYPE ~ "summary", 
      TRUE ~ crapdata 
)

#matches crapdata data codes to 48 oficial codes, then sets them next to eachother
matchNums <- sapply(crapdata, function(x) if(x !="garbage" && x != "summary"){amatch(x, eCodes2, maxDist = 10)} else {49})
sum(is.na(eCodes2[matchNums])) #count of NA's remaining
matchResult <- as.data.frame(cbind(crapdata, lookup = eCodes2[matchNums]), stringsAsFactors = FALSE)
#count NA's maxDist = 10 = 66 with several errors
sum(is.na(matchResult))

#Join the new clean codes to the original cleaned dataset
repdata2 <- left_join(repdata2, matchResult, by = c("EVTYPE" = "crapdata"))
#remove NA's
repdata2 <- repdata2[!is.na("lookup"),]


# bad indexes
9, 15, 17, 21, 22, 23, 28
