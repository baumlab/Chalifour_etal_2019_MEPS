## Sampling effort calculations ##
library(here)

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

## Sampling effort values for Table S2: summarizing unique samping events (i.e. site*day visits) by season, habitat, year
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

