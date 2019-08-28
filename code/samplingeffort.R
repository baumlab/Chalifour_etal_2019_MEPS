## Sampling effort calculations ##
library(here)
here()

# Load data frame: includes columns with all environmental and site data and rows for each observation down to one row per fish
all<- read.csv("data/all_clean.csv")

# Add column to categorize observations by season
all$Season<- NA
all$Season<- for(i in 1:nrow(all)) {
  all$Season[which(all$month%in%c("March", "April"))]<- "Spring"
  all$Season[which(all$month%in%c("May", "June", "July", "August"))]<- "Summer"
  all$Season[which(all$month%in%c("September", "October"))]<- "Fall"
}
all$Season<- as.factor(all$Season)
summary(all$Season)

# separate into two new data frames for each sampling year (2016, 2017)
all16<- subset(all, all$Year%in%0)
all17<- subset(all, all$Year%in%1)

library(plyr)

## Sampling effort values for Table S1: summarizing unique samping events (i.e. site*day visits) by season, habitat, year
S1_16<- all16[c("Site", "J.date", "Habitat", "Season")]
S1_16<- count(all16, .(Site, J.date, Habitat,Season))
summary(S1_16$Season[S1_16$Habitat%in%c("Marsh")])
summary(S1_16$Season[S1_16$Habitat%in%c("Eelgrass")])
summary(S1_16$Season[S1_16$Habitat%in%c("Sand flat")])

S1_17<- all17[c("Site", "J.date", "Habitat", "Season")]
S1_17<- count(all17, .(Site, J.date, Habitat,Season))
summary(S1_17$Season[S1_17$Habitat%in%c("Marsh")])
summary(S1_17$Season[S1_17$Habitat%in%c("Eelgrass")])
summary(S1_17$Season[S1_17$Habitat%in%c("Sand flat")])

### Mean +/- SD catch per sampling effort calculations ###
# count and aggregate to get abundance per site*day for each species
cpue<-count(all)
cpue<-ddply(cpue, .(Year, J.date, Habitat, Site, Species), summarize, abundance = sum(freq))
# set empty nets to 0 abundance
cpue$abundance[which(cpue$Species == 0)]<- 0 
# summarize abundance by sampling event across all species for each habitat (unique level = year*site*day)
cpue.hab<-ddply(cpue, .(Year, J.date, Habitat, Site), summarize, abundance = sum(abundance)) 
# note did to species level first to be able to accurately represent empty sets in abundance.

## separate into groups: CPUE calculations for each habitat type
# Mean catch per unit sampling effort for all fish in Eelgrass, both years
EG<- subset(cpue.hab, Habitat%in%c("Eelgrass"))
mean(EG$abundance); sd(EG$abundance)

# Mean catch per unit sampling effort for all fish in Marsh
M<- subset(cpue.hab, Habitat%in%c("Marsh"))
mean(M$abundance); sd(M$abundance)

# Mean catch per unit sampling effort for all fish in Sand flat
SF<- subset(cpue.hab, Habitat%in%c("Sand flat"))
mean(SF$abundance); sd(SF$abundance)

## Mean catch per unit sampling effort for all salmonids in marsh
salmon<- subset(cpue, Species%in%c("Chinook", "Chum", "Sockeye", "Pink", "Coho", "Steelhead"))
salmon2<- subset(salmon, Habitat%in%c("Marsh"))
# aggregate across all salmon species per site*day
salmon2<-ddply(salmon2, .(Year, J.date, Habitat, Site), summarise, abundance = sum(abundance))
# note this df is 65 entries, meaning 19 (84-65) sampling events across both years did not catch any salmon. In order to correctly incorporate these "empty" nets into the mean and variance, we need to add these 0s to the abundance column

abund<- data.frame(Year = 2, J.date = 1:19, Habitat = c("Marsh"), Site = c("N0"), abundance = 0)
salmon2<- rbind(salmon2, abund) # now have added 19 rows with 0 abundance
mean(salmon2$abundance); sd(salmon2$abundance)

## Mean catch per unit sampling effort for Chinook in marsh by year
chin<- subset(cpue, Species%in%c("Chinook"))
chin16<- subset(chin, Habitat%in%c("Marsh")); chin16<-subset(chin16, Year%in%c("0")) # 2016 Chinook catch in Marsh
chin17<- subset(chin, Habitat%in%c("Marsh"));chin17<- subset(chin17, Year%in%c("1")) # 2017 Chinook catch in Marsh

nrow(chin16) # 30 out of 44 sampling events had Chinook in 2016
ch16<- data.frame(Year = 2, J.date = 1:14, Habitat = "Marsh", Site = "N0", Species = "Chinook", abundance = 0) #add missing 0s
chin16<- rbind(chin16, ch16)
mean(chin16$abundance); sd(chin16$abundance)

nrow(chin17) # 33 out of 40 sampling events had Chinook in 2017 
ch17<- data.frame(Year = 2, J.date = 1:7, Habitat = "Marsh", Site = "N0", Species = "Chinook", abundance = 0) #add missing 0s
chin17<- rbind(chin17, ch17)
mean(chin17$abundance); sd(chin17$abundance)

## Mean catch per unit sampling effort for chum salmon (all habitats) for each year
chum<- subset(cpue, Species%in%c("Chum"))
chum16<- subset(chum, Year%in%c("0")) # 2016 chum catch across all habitats
chum17<- subset(chum, Year%in%c("1")) # 2017 chum catch across all habitats

nrow(chum16) # 17 out of 154 sampling events had chum in 2016
chm16<- data.frame(Year = 2, J.date = 1:137, Habitat = "All", Site = "N0", Species = "Chum", abundance = 0) #add missing 0s
chum16<- rbind(chum16, chm16)
mean(chum16$abundance); sd(chum16$abundance)

nrow(chum17) # 44 out of 134 sampling events had chum in 2017
chm17<- data.frame(Year = 2, J.date = 1:90, Habitat = "All", Site = "N0", Species = "Chum", abundance = 0) #add missing 0s
chum17<- rbind(chum17, chm17)
mean(chum17$abundance); sd(chum17$abundance)

## Mean catch per unit sampling effort for juvenile flatfish in Marsh
juvflat<- subset(cpue, Species%in%c("Unidentified flatfish"))
juvflat16<- subset(juvflat, Habitat%in%c("Marsh"));juvflat16<- subset(juvflat16,Year%in%c("0")) # 2016 catch in Marsh
juvflat17<- subset(juvflat, Habitat%in%c("Marsh"));juvflat17<- subset(juvflat17,Year%in%c("1")) # 2017 catch in Marsh

nrow(juvflat16) # only 7 incidences of juv flatfish in marsh in 2016 out of 44 sampling events
flat16<- data.frame(Year=2, J.date=1:37, Habitat="Marsh", Site="N0", Species = "flatty", abundance=0) #add missing 0s
juvflat16<- rbind(juvflat16, flat16)
mean(juvflat16$abundance); sd(juvflat16$abundance)

nrow(juvflat17) # only 6 incidences of juv flatfish in marsh in 2017 out of 40 sampling events
flat17<- data.frame(Year=2, J.date=1:34, Habitat="Marsh", Site="N0", Species = "flatty", abundance=0) #add missing 0s
juvflat17<- rbind(juvflat17, flat17)
mean(juvflat17$abundance); sd(juvflat17$abundance)

# Highest catch juv flatfish in a single day, sites combined by habitats
cpue.day<- count(all)
cpue.day<-ddply(cpue.day, .(Year, J.date, Habitat, Species), summarize, abundance = sum(freq))
cpue.day$abundance[which(cpue.day$Species == 0)]<- 0 # set empty nets to 0 abundance

juvflatday<- subset(cpue.day, Species%in%c("Unidentified flatfish"))
summary(juvflatday$abundance)
# in a single sampling effort (site*day)
summary(juvflat$abundance) ## same as by day, because 2 largest schools caught were at single sites on single days, and bigger than next biggest aggregate day.

##The marsh was also the source of a high catch of juvenile flatfish in 2017 (1,366 unidentified flatfish, Fig. 3a). Comment from Julia:i.e. in one day?? Response: no, over the year. Highest catch in one day was 606 at a single marsh site. Mean 2016 catch was only 1 +/- 4 fish/sampling event vs 2017 was 34 +/- 126 fish/sampling event in the marsh. 

## Mean catch per unit sampling effort for shiner surfperch (all habitats) by year
shiner<- subset(cpue, Species%in%c("Shiner surfperch"))
shiner16<- subset(shiner,Year%in%c("0")) # 2016 shiner catch
shiner17<- subset(shiner,Year%in%c("1")) # 2017 shiner catch

## 77 sampling events in 2016 included shiners, but 154 total sampling events in 2016 across all habs (50% empty!)
df16<- data.frame(Year=3, J.date=1:77, Habitat="Eelgrass", Site="N0", Species="Shiner surfperch", abundance=0) #add missing 0s
shiner16<- rbind(shiner16, df16)
mean(shiner16$abundance); sd(shiner16$abundance)

## 42 sampling events in 2017 included shiners, but 134 total sampling events in 2017 across all habs (69% empty!)
df17<- data.frame(Year=3, J.date=1:92, Habitat="Eelgrass", Site="N0", Species="Shiner surfperch", abundance=0) #add missing 0s
shiner17<- rbind(shiner17, df17)
mean(shiner17$abundance); sd(shiner17$abundance)

