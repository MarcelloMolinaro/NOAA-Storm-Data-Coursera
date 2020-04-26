#plots

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

#xyplot(data = yearsEvent, totFat ~ Year| lookup )
#xyplot(data = yearsEvent, totFat ~ Year| lookup, group = lookup, type = c("smooth") )
xyplot(data = yearsEvent, totInjur ~ Year| lookup, group = lookup, type = c("p", "smooth") )
#------------------------------------------#

#Excessive Heat Events over time
#------------------------------------------#
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

#Graph of every event and its total Prop dam and Crop Dam
#------------------------------------------#
#ggplot() + 
  #geom_point(data = events, aes(x = lookup, y = totPropDam), color = "black") +
  #geom_point(data = events, aes(x = lookup, y = totCropDam), color = "red")+
  #labs(title = "Evnt sblarg", subtitle = "1996-2011", x  = "Event", y = "Total")

#Graph of the top 20 total Damage events

etall <- arrange(events, -(totCropDam+ totPropDam)) #Order by total Damage
etall <- etall[1:15,]  #Select top 15 only
etall <- etall %>% select(lookup, totCropDam, totPropDam) %>% #Select only Dam columns
        gather(key = "damType", value = "damValue", -lookup) #Convert to a tall table

etall <- etall %>% mutate(damValueBill = damValue/1000000000)
ggplot() +
  geom_bar(data = etall, aes(x= reorder(lookup, -damValueBill), y = damValueBill, fill = damType), stat= "identity") + 
  coord_flip()+
  labs(x = "Event", y = "Total Damage Value (Billions)", title = "Total Damage by Event Type in US: 1996-2011") +
  scale_fill_discrete(name = "Damage Type", labels = c("Crop", "Property"))

#------------------------------------------#

#It would be fun to look at biggest totDamage by state and label the worst ones
#states <- repdata2 %>% 
#group_by(lookup, STATE) %>% 
#  summarise(instances = n(), 
#            totFat = sum(FATALITIES, na.rm = TRUE), 
#            totInjur = sum(INJURIES, na.rm = TRUE), 
#            totPropDam = sum(cumPropDam, na.rm = TRUE), 
#            totCropDam = sum(cumCropDam, na.rm = TRUE))

#The following code creates different plots
plot(years$Year, years$totFat, main = "Fatalities by Year, all event Types")

plot(yearsEvent$Year, yearsEvent$totFat, col = as.factor(yearsEvent$lookup))
#legend("right", c(1, 2), legend =unique(as.factor(yearsEvent$lookup)), col = unique(as.factor(yearsEvent$lookup)), pch = 2,  )
#too many, but nie plot!
plot(as.factor(yearsEvent$lookup), yearsEvent$totFat)