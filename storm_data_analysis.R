library(R.utils)
library(rmarkdown)
library(knitr)
storm <- read.csv(bzfile("stormData.csv"))
head(storm)
names(storm)
#1)  Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

variables<-c("EVTYPE","FATALITIES","INJURIES","PROPDMG", "PROPDMGEXP","CROPDMG","CROPDMGEXP")
strmdata<-storm[variables]

dim(strmdata)
names(strmdata)
#a) aggregating EVTYPE wrt fatalities
Fatalities <- aggregate(FATALITIES ~ EVTYPE, data = strmdata, FUN = sum)
Top10_Fatalities <- Fatalities[order(-Fatalities$FATALITIES), ][1:10, ] 
Top10_Fatalities 

#b) aggregating EVTYPE wrt injuries
Injuries <- aggregate(INJURIES ~ EVTYPE, data = strmdata, FUN = sum)
Top10_Injuries <- Injuries[order(-Injuries$INJURIES), ][1:10, ] 
Top10_Injuries 

par(mfrow=c(1,2),mar=c(10,3,3,2))
barplot(Top10_Fatalities$FATALITIES,names.arg=Top10_Fatalities$EVTYPE,las=2,col="purple",ylab="fatalities",main="Top 10 fatalities")
barplot(Top10_Injuries$INJURIES,names.arg=Top10_Injuries$EVTYPE,las=2,col="purple",ylab="injuries",main="Top 10 Injuries")

#2. Across the United States, which types of events have the greatest economic consequences?

# Aggregate Data for Property Damage
propdmg <- aggregate(PROPDMG ~ EVTYPE, data = storm, FUN = sum)
propdmg <- propdmg[order(propdmg$PROPDMG, decreasing = TRUE), ]
# 10 most harmful causes of injuries
propdmgMax <- propdmg[1:10, ]
print(propdmgMax)

#Aggregate Data for Crop Damage
cropdmg <- aggregate(CROPDMG ~ EVTYPE, data = storm, FUN = sum)
cropdmg <- cropdmg[order(cropdmg$CROPDMG, decreasing = TRUE), ]
# 10 most harmful causes of injuries
cropdmgMax <- cropdmg[1:10, ]
print(cropdmgMax)

par(mfrow=c(1,2),mar=c(11,3,3,2))
barplot(propdmgMax$PROPDMG/(10^9), names.arg=propdmgMax$EVTYPE, las=2, col="gold", ylab="Prop.damage(billions)", main="Top10 Prop.Damages")
barplot(cropdmgMax$CROPDMG/(10^9), names.arg=cropdmgMax$EVTYPE, las=2, col="gold", ylab="Crop damage(billions)", main="Top10 Crop.Damages")

#merging both
totalDamage<- merge(propdmgMax,cropdmgMax,by.x = "EVTYPE", by.y = "EVTYPE")
totalDamage<-arrange(totalDamage,desc(PROPDMG + CROPDMG))
top_10_damages <- melt(totalDamage, id.vars="EVTYPE", variable.name = "Damage_Types")
head(top_10_damages, 5)

# Create chart
DamageChart <- ggplot(top_10_damages, aes(x=reorder(EVTYPE, -value/100000), y=value/100000))

# Plot data as bar chart
DamageChart = DamageChart + geom_bar(stat="identity", aes(fill=Damage_Types), position="dodge")

# Set x-axis label
DamageChart = DamageChart + xlab("Event Type") 

# Rotate x-axis tick labels 
DamageChart = DamageChart + theme(axis.text.x = element_text(angle=45, hjust=1))

# Set chart title and center it
DamageChart = DamageChart + ggtitle("Top 10 greatest economic consequences") + theme(plot.title = element_text(hjust = 0.5))

DamageChart


