######## R Script for Chalifour et al MEPS Fig 4 Euler and Fig S1 NMDS Diagrams #########

library(here)

### Load Data
all<- read.csv("data/all_clean.csv") ### all catch by Species (includes metadata and single row per fish observation)  

library(plyr)
# create frequencies for each fish observation (i.e. 1 for each row)  
all<- count(all) 
# sum by species*site*day  
species.full<- ddply(all, .(Year, J.date, month, Habitat, Site, Species), summarize, abund = sum(freq))  
species.full$abund[which(species.full$Species == 0)]<- 0 # convert 0 counts back to 0s  
# view species list
levels(species.full$Species)  

# remove unidentified species and 0 species (empty sets) to reflect actual richness  
species.full$abund[which(species.full$Species%in%("0"))]<- NA
species.full$abund[which(species.full$Species%in%("Unidentified smelt"))]<- NA
species.full$abund[which(species.full$Species%in%("Unidentified sculpin"))]<- NA
species.full$abund[which(species.full$Species%in%("Unidentified sanddab"))]<- NA
species.full$abund[which(species.full$Species%in%("Unidentified salmonid"))]<- NA
species.full$abund[which(species.full$Species%in%("Unidentified larval fish 2"))]<- NA
species.full$abund[which(species.full$Species%in%("Unidentified larval fish"))]<- NA
species.full$abund[which(species.full$Species%in%("Unidentified gunnel"))]<- NA
species.full$abund[which(species.full$Species%in%("Unidentified greenling"))]<- NA
species.full$abund[which(species.full$Species%in%("Unidentified gadidae"))]<- NA
species.full$abund[which(species.full$Species%in%("Unidentified flatfish"))]<- NA

species.full<- na.omit(species.full) # remove all rows that contain NA values, run species summary again to check all empty levels are correct  
species.full$Species<- factor(species.full$Species) # drop unused species levels  
head(species.full) # can see we have data in format of unique rows for each habitat, species, site observation by day (except removed day) - abundance represents # of individuals of that species at that site in all 3 sets. 

## simplify data into species counts by habitat
species<- species.full[, c("Habitat", "Species")]  

######## VERSION 1: HABITAT LEVEL COMPARISON 
species.t<-as.data.frame.matrix(table(species[c(2,1)])) # now have species ~ habitat matrix, showing # occurences in each 
spp_pres<- ifelse(species.t > 0, 1, 0) # make matrix pres/abs
spp_pres<- ifelse(spp_pres > 0, TRUE, FALSE) # make matrix TRUE/FALSE

############ VENN (Euler) Diagram

# install.packages("eulerr")
library(eulerr)
# set global options for euler plot:
eulerr_options(labels = list(fontsize = 16), fills = list(fill = c("forestgreen","slateblue","goldenrod1"), alpha = 0.6), edges = list(col =c("forestgreen","slateblue","goldenrod1"), lty = c(1,2,10), lwd = 2), quantities = list(font = 4, fontsize = 14))

# run function on data matrix:
V1<- euler(spp_pres, shape = "ellipse")
# plot (note that plot output varies slightly with each iteration using euler, same results):
pdf("final.figures/Fig4.pdf", width = 6.65, height = 5.9, pointsize = 16)

plot(V1, quantities=TRUE)
dev.off()

citation("eulerr")

##### NMDS 
## 1 species by habitat, all sites pooled - NOT USED
species.m<-as.data.frame.matrix(table(species[c(1,2)])) # now have habitat~species matrix, showing # occurences in each 
library(vegan)
ord<- metaMDS(species.m) # stress near 0, may not have enough data

stressplot(ord) 

ordiplot(ord, type = "n")
orditorp(ord, display = "species", col = "blue", air = 0.01)
orditorp(ord, display="sites", cex=1.25, air=0.01)

## 2 species by site - this is final version USED
## simplify data into species counts by site
species2<- species.full[, c("Site", "Species")] 
species.m.s<- as.data.frame.matrix(table(species2[c(1,2)]))
ord2<- metaMDS(species.m.s) # stress 0.09265 -- much better
stressplot(ord2) # good, not too much scatter. non-metric fit R2=0.991, linear fit R2 = 0.96
gof<-goodness(ord2)
plot(ord2, display="sites", type="n")
points(ord2, display = "sites", cex=2*gof/mean(gof)) #display plot of goodness of fit proportions for each site

# plot, highlight habitat groupings and important species
dev.new() # open new device so that main plot window and save-able quartz device are both open
quartz(width = 8.26, height = 5, pointsize = 15, family = "Helvetica", dpi = 300, type = "pdf", bg = "white") # use quartz to retain identified points
dev.list() #view the list of devices to select the default plot window so you can view and save at the same time
dev.set(which = 5) # allows you to view the plot in plot window at same time as saving png (so that identify is retained)
par(oma = c(1,1,1,1))
colors=c("forestgreen","slateblue","goldenrod1") # set habitat colours 
habitat=c(rep("Eelgrass",7),rep("Marsh",5),rep("Sand flat",6)) # make vector for habitats to match site column
p1<- ordiplot(ord2, type = "n")
points(p1, "species", col = "grey", pch = "+", cex = 0.7)
ordiellipse(ord2,groups=habitat,kind = "ehull",draw="lines",col=colors,label=F)
orditorp(ord2, display="sites", label = FALSE, col=c(rep("forestgreen",7),rep("slateblue",5),rep("goldenrod1",6)), pch = c(rep(16,7),rep(17,5), rep(15,6)), pcex=0.7, air=0.01)
identify(p1, "species", col = "black", cex = 0.7, atpen = TRUE) # identify select species to show on plot, so not overcrowded
text(1, 0.6, "stress = 0.09")
quartz.save("final.figures/FigS1.pdf", width = 7.5, height = 6, pointsize = 16, type = "pdf", device = dev.cur())
dev.off()

# plot simple
png("final.figures/NMDS.png", width = 169, height = 150, units = "mm", res = 600, type = "cairo", pointsize = 16)
ordiplot(ord2, type = "n")
orditorp(ord2, display = "species", col = "grey", air = 0.05)
orditorp(ord2, display="sites", col = c(rep("forestgreen",7),rep("slateblue",5),rep("goldenrod1",6)), air=0.01)
dev.off()

