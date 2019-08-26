################### R Script for MEPS MS Euler and NMDS Diagrams ####################

library(here)
here()

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
# plot:
pdf("final.figures/Fig4.pdf", width = 6.65, height = 5.9, pointsize = 16)

plot(V1, quantities=TRUE)
dev.off()

citation("eulerr")

##### NMDS 
## 1 species by habitat, all sites pooled - not used
species.m<-as.data.frame.matrix(table(species[c(1,2)])) # now have habitat~species matrix, showing # occurences in each 
library(vegan)
ord<- metaMDS(species.m) # stress near 0, may not have enough data

stressplot(ord) 

ordiplot(ord, type = "n")
orditorp(ord, display = "species", col = "blue", air = 0.01)
orditorp(ord, display="sites", cex=1.25, air=0.01)

## 2 species by site - this is final version used
## simplify data into species counts by site
species2<- species.full[, c("Site", "Species")] 
species.m.s<- as.data.frame.matrix(table(species2[c(1,2)]))
ord2<- metaMDS(species.m.s) # stress 0.09265 -- much better
stressplot(ord2) # pretty good, not too much scatter. non-metric fit R2=0.991, linear fit R2 = 0.96
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


### OLD / OTHER GRAPHICS EXPLORATION
# plot
png("final.figures/NMDS.png", width = 169, height = 150, units = "mm", res = 600, type = "cairo", pointsize = 16)
ordiplot(ord2, type = "n")
orditorp(ord2, display = "species", col = "grey", air = 0.05)
orditorp(ord2, display="sites", col = c(rep("forestgreen",7),rep("slateblue",5),rep("goldenrod1",6)), air=0.01)
dev.off()

# highlight habitat groupings and important species
colors=c("forestgreen","slateblue","goldenrod1") # set habitat colours 
habitat=c(rep("Eelgrass",7),rep("Marsh",5),rep("Sand flat",6)) # make vector for habitats to match site column
shnam <- make.cepnames((names(species.m.s))) # make short names for species to display in plot
p1<- ordiplot(ord2, type = "n")
ordiellipse(ord2,groups=habitat,kind = "ehull",draw="lines",col=colors,label=F)
orditorp(ord2, display = "species", col = "grey", air = 0.01, p.max = 0.1)
orditorp(ord2, display="sites", label = FALSE, col=c(rep("forestgreen",7),rep("slateblue",5),rep("goldenrod1",6)),cex=1, air=0.01)
# highlight habitat groupings and important species
colors=c("forestgreen","slateblue","goldenrod1") # set habitat colours 
habitat=c(rep("Eelgrass",7),rep("Marsh",5),rep("Sand flat",6)) # make vector for habitats to match site column
shnam <- make.cepnames((names(species.m.s))) # make short names for species to display in plot
stems<- colSums(species.m.s) # give priority to label species based on higher abundance
ordiplot(ord2, display = "species", type = "n")
ordiellipse(ord2,groups=habitat,kind = "ehull",draw="lines",col=colors,label=F)
ordipointlabel(ord2, display = "species", col = "grey", pch = "+", cex = c(0.6), scaling = "symm")
orditorp(ord2, display="sites", label = FALSE, col=c(rep("forestgreen",7),rep("slateblue",5),rep("goldenrod1",6)),cex=1, air=0.01)

## add habitats as environmental variable
ef.hab<- data.frame(Site = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "M1", "M2", "M3", "M4", "M5", "SF1", "SF2", "SF3", "SF4", "SF5", "SF6"), Habitat = c(rep("Eelgrass",7),rep("Marsh",5),rep("Sand flat",6)))
ef<- envfit(ord2, ef.hab, permu = 999)
ef
plot(ord2, display = "species")
plot(ef, p.max = 0.1)
pl

### practice: vegan tutorial Oksanen 2015
library(MASS)
data("varespec")
vare.dis<- vegdist(varespec)
vare.mds0<-isoMDS(vare.dis)
plot(vare.dis)
stressplot(vare.mds0, vare.dis)
ordiplot(vare.mds0, type="t")

ord.dis<- vegdist(species.m.s)
ord.mdsM<-isoMDS(ord.dis)
plot(ord.dis)
stressplot(ord.mdsM, ord.dis)
stressplot(ord2)
ordiplot(ord.mdsM, type="t") #quite different, esp eelgrass - more dispersed.
ordiplot(ord2, type="t") #note data not squre root transformed. But IS submitted to Wisconsin double standardization, "or species divided by their maxima, and stands standardized to equal totals." Rotated solution so the largest variance of site scores will be on the first axis. Scaled the solution so that one unit corresponds to halving of community similarity from the replicate similarity. Species scores are calculated as weighted averages of site score and expanded to have equal variances to site scores

ordiplot(metaMDS(species.m.s, shrink=TRUE), type="t") #no change, i.e. expansion of species scores as above does not impact the final weighted species scores
rankindex(ef.hab,species.m.s) #suggests Kulcyzynski index is slightly better fit than Bray-Curtis (0.74 vs 0.64)
ord.dis2<- vegdist(species.m.s, method = "kulczynski")
ord.mds2<-isoMDS(ord.dis)
plot(ord.dis2)
stressplot(ord.mds2, ord.dis2) #error dissimilarities and ordination do not match
ordmds2<- metaMDS(ord.dis, distance = "kulczynski") #still says in results distance used is bray. Stress is lower though - 0.075 and wisconsin standardization is not done. Species scores are missing
ordiplot(ord.mds2, type="t") #result is same as ord.mdsM using isoMDS

# Principle Components Analysis 
pca<- rda(species.m.s)
pca
#Axis 1 explains 100.88/225.7 or 44.7 % of the total variance
plot(pca)
biplot(pca) #inertia is variance, meaning variance in abundance of species willa ffect output
biplot(pca, scaling = -1) #looks very different. "For this graph we specified scaling = -1. The results are scaled only when they are accessed, and we can flexibly change the scaling in plot, biplot and other commands. The negative values mean that species scores are divided by the species standard deviations so that abundant and scarce species will be approximately as far away from the origin."

pca2<- rda(species.m.s, scale=TRUE)
pca2 # Inertia is correlations, meaning all species are equal now
#Axis 1 explains 13.657/46 or 29.7 % of the total variance
biplot(pca2) #doesn't seem like a good fit, the species are all on top of each other in the middle
biplot(pca2, scaling = 3) #scaling = 3 is symmetric scaling in PCA, i.e. species and sites are equal variance

#Redundancy analysis
rda<- rda(species.m.s, ef.hab)
rda
#Inertia is variance, all constrained. Some constraints were aliased because they were redundant
#Axis 1 explains 100.88/225.7 or 44.7 % of total variance -- identical to unconstrained version above
plot(rda) #same as before just prints site names doubly as constrained environmental factor
cca<- cca(species.m.s ~ Habitat, ef.hab) #specify constraint as habitat group not site
plot(cca) #now see habitats clearly, with fish only in that habitat layered underneath, but no arrows. Appearance is quite different from original NMDS plot above but the species distribution between habitats is much more similar than other practice plots. 

## 5 Dissimilarities and environment -- assess species-environment relationships without ordination, or in full space
#adonis implements a multivariate analysis of variances using distance matrices.
#betadiversity between habitats

betad<- betadiver(species.m.s, "z") #z is exponent of Arrhenius model where # of species is dependent on size of study area. Note that example environmental data frame has areas included, so may not work well for our data set. But can examine factors as well
adonis(betad ~ Habitat, ef.hab, perm=200)
#dig deeper into the homogeneity of the groups (above just looks at differences in group means)
beta2<- with(ef.hab, betadisper(betad, Habitat))
beta2 
plot(beta2)
boxplot(beta2)
anova(beta2) #test to see if the mean distance of each habitat group centroid to the median is sig diff
permutest(beta2)
TukeyHSD(beta2)
## Apparently not significant at all
#As noted in passing by Anderson (2006) and in a related context by O'Neill (2000), estimates of dispersion around a central location (median or centroid) that is calculated from the same data will be biased downward. This bias matters most when comparing diversity among treatments with small, unequal numbers of samples. Setting bias.adjust=TRUE when using betadisper imposes a sqrt(n/(n-1)) correction (Stier et al. 2013).
beta3<- with(ef.hab, betadisper(betad, Habitat, bias.adjust = TRUE))
plot(beta3)
anova(beta3) #no major difference

