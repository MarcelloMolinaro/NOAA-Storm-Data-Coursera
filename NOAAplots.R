#plots

events <- repdata2 %>% 
  group_by(lookup) %>% 
  summarise(instances = n(), totFat = sum(FATALITIES), totInjur = sum(INJURIES), totPropDam = sum(cumPropDam), totCropDam = sum(cumCropDam))
events <- arrange(events, -totFat)

yearsEvent <- repdata2 %>% 
  group_by(Year = year(NewStartDate), lookup) %>%
  summarise(instances = n(), totFat = sum(FATALITIES), totInjur = sum(INJURIES), totPropDam = sum(cumPropDam), totCropDam = sum(cumCropDam))
yearsEvent <- arrange(yearsEvent, -totFat)

#The following code creates different plots
plot(years$Year, years$totFat, main = "Fatalities by Year, all event Types")

plot(yearsEvent$Year, yearsEvent$totFat, col = as.factor(yearsEvent$lookup))
#legend("right", c(1, 2), legend =unique(as.factor(yearsEvent$lookup)), col = unique(as.factor(yearsEvent$lookup)), pch = 2,  )
#too many, but nie plot!
plot(as.factor(yearsEvent$lookup), yearsEvent$totFat)

#----------------------------------------------------------#
#Bar plots of 10 most fatal and 10 most injurious Eventtypes

par(mfrow = c(2,2), #sets a 2x2 grid of plots
    las = 1, #can use to make labels vertical = 2
    mar = (c(5, 8, 4, 2)) # makes y margin larger (8, was 4)
  )
#Fatalities Total
eventsFat <- events
barplot(eventsFat$totFat[1:10],
        horiz = TRUE,
        main = "Total Fatalities by Event Type 1996-2011", 
        names.arg = eventsFat$lookup[1:10], 
        cex.names=0.8, xlab = "Fatalities")

#Fatalities per instance
eventsFatInst <- arrange(events, -(totFat/instances))
barplot(eventsFatInst$totFat[1:10]/eventsFatInst$instances[1:10], 
        horiz = TRUE,
        main = "Total Fatalities per Instance by Event Type 1996-2011", 
        names.arg = eventsFatInst$lookup[1:10], 
        cex.names=0.8,  xlab = "Fatalities")

#Injuries Total
eventsInjur <- arrange(events, -totInjur)
barplot(eventsInjur$totInjur[1:10], 
        horiz = TRUE,
        main = "Total Injuries by Event Type 1996-2011",
        names.arg = eventsInjur$lookup[1:10], 
        cex.names=0.8,  xlab = "Injuries")

#Injuries per Instance
eventsInjurInst <- arrange(events, -(totInjur/instances))
barplot(eventsInjurInst$totInjur[1:10]/eventsInjurInst$instances[1:10], 
        horiz = TRUE,
        main = "Total Injuries per Instance by Event Type 1996-2011", 
        names.arg = eventsInjurInst$lookup[1:10], 
        cex.names=.8, xlab = "Injuries")
#-----------------------------------------------------------#

#Plot to see trends over the years? Which events have become worse and which better?
#------------------------------------------#
library(lattice)

xyplot(data = yearsEvent, totFat ~ Year| lookup )
xyplot(data = yearsEvent, totFat ~ Year| lookup, group = lookup, type = c("smooth") )
xyplot(data = yearsEvent, totInjur ~ Year| lookup, group = lookup, type = c("p", "smooth") )


#------------------------------------------#

#------------------------------------------#
#Excessive Heat Events over time
par( mfrow = c(1,2), mar = (c(5, 4, 4, 4))) #make right mrgin slightly larger

heat <- filter(yearsEvent, lookup == "excessive heat") %>% arrange(Year)
plot(heat$Year, heat$totInjur, 
     type = "b", col = "turquoise", 
     xlab = "Year", ylab = "Total", 
     main  = "Excessive Heat Events over time: Injuries and Fatalities")
lines(heat$Year, heat$totFat, type = "b", col = "red")
legend("topleft", legend = c("Injuries", "Fatalities"), col = c("turquoise", "red"), lty = 1)
plot(heat$Year, heat$totInjur/heat$instances, 
     type = "b", col = "turquoise", 
     xlab = "Year", ylab = "Total", 
     main  = "Excessive Heat Events over time: Injuries and Fatalities per Event")
lines(heat$Year, heat$totFat/heat$instances, type = "b", col = "red")
legend("topleft", legend = c("Injuries per Event", "Fatalities per Event"), col = c("turquoise", "red"), lty = 1)
#------------------------------------------#

# Bar plot of Prop and Crop Dmg by event type
#show stacked bar plot of damages by event
#panel plot of events, each plot is event and x is year? Would be cool. Or at least the top 16?

ggplot()
# do some cool stuff with colors?


#plot to show injuries and fatalities on same plot, wasn't actually helpful
#tibble with only fat and injur
yEv <- yearsEvent[,c("Year", "lookup", "totFat", "totInjur")]
#tall tibble with fat and injur in single col
yev2 <- gather(yev3, key, value, -Year, -lookup)
barplot(yev2$value,
        main = "Total Fatalities by Event Type",
        col = yev2$Year,
        names.arg = yev2$lookup)