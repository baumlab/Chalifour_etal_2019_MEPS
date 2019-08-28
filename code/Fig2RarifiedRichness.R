#Fig_2_Chalifour_et_al_2019_MEPS_revised
#Rarified richness 
library(here)
here()

##load abiotic data
all<- read.csv("data/all_clean.csv")

### all catch by Species  
library(plyr)

##Summary  
levels(all$Species)

###Aggregate by Species*site*day, including all parameters that might be of interest  
#Start by counting all rows, note this includes each set (3 replicates per site)    
all<- count(all, c("Year", "J.date", "SIN_TIME", "round", "month", "Habitat", "Site", "Temp.surf", "DOmg.surf","pH.surf","Sal.surf","Set", "Species"))  
#Now sum by species*site*day  
all.sp<- ddply(all, .(Year, J.date, SIN_TIME, round, month, Habitat, Site, Temp.surf, DOmg.surf, pH.surf, Sal.surf, Species), summarize, abund = sum(freq))  
all.sp$abund[which(all.sp$Species == 0)]<- 0 #convert 0 counts back to 0s  

##Total abundance:  
sum(all.sp$abund)  
#2016 abundance:  
sum(all.sp[which(all.sp$Year == "0"),]$abund)  
#2017 abundance:
sum(all.sp[which(all.sp$Year == "1"),]$abund)  

# Rarefy 
## **V1: Habitat** compare by habitat, including site_day as replicates for each habitat matrix
#Script modified from Iacarella et al. 2018 (https://doi.org/10.1111/gcb.14090)

#Sample based species richness rarefaction curves using iNEXT  
#See https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12613 for details on iNEXT 

library(iNEXT)  
library(ggplot2)

species.full<- all.sp[, c("Year", "J.date", "month","round","Habitat", "Site", "Species", "abund")]  

##Remove unidentified species and 0 species (empty sets) to reflect actual richness  
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

species.full<- na.omit(species.full) #Now remove all rows that contain NA values, run species summary again to check all empty levels are correct  
species.full$Species<- factor(species.full$Species) ##drop unused species levels  
head(species.full) ## can see we have data in format of unique rows for each habitat, species, site observation by day (except removed day) - abundance represents # of individuals of that species at that site in all 3 sets. 

## simplify data into species counts by habitat, site, and day
species<- species.full[, c("Year", "J.date", "Habitat", "Site", "Species", "abund")]  

######## Panel 1: HABITAT LEVEL COMPARISON 
##Subset data into habitat categories for analysis (spp counts (presence) are rows, sites are columns), and make presence/absence matrix  
Marsh<- subset(species, Habitat %in% c("Marsh"))
##Create year_day_site identifier to capture all independent sampling events (still 3 sets/day pooled)
Marsh$year_day_Site<- paste(Marsh$Year, Marsh$J.date, Marsh$Site)
#now can remove columns for Year, J.date, Habitat and Site  
Marsh<-Marsh[,-c(1:4)] 
Marsh.t<- as.data.frame.matrix(table(Marsh[c(1,3)])) #now have species ~ sampling event matrix, showing the # of days where each of these species has been present for each site (and 0s for those not counted). This is already simplified to presence/absence
head(Marsh.t)

##Eelgrass  
Eelgrass<-subset(species,Habitat %in% c("Eelgrass")) 
Eelgrass$year_day_Site<- paste(Eelgrass$Year, Eelgrass$J.date, Eelgrass$Site)
Eelgrass<-Eelgrass[,-c(1:4)] 
Eelgrass.t<- as.data.frame.matrix(table(Eelgrass[c(1,3)])) 

##Sand flat    
Sandflat<-subset(species,Habitat %in% c("Sand flat")) 
Sandflat$year_day_Site<- paste(Sandflat$Year, Sandflat$J.date, Sandflat$Site)
Sandflat<-Sandflat[,-c(1:4)] 
Sandflat.t<- as.data.frame.matrix(table(Sandflat[c(1,3)]))  

##Make list of matrices (one for each habitat type)  
my.list<-list(Marsh.t,Eelgrass.t,Sandflat.t)
names(my.list)<-c("Marsh","Eelgrass","Sandflat")
str(my.list, list.len = 10)

##Run rarefaction on matrix list    
#1 using incidence aggregation  

t<- seq(1,180,by=1)
Rich.rar<-iNEXT(my.list, q =c(0), datatype = "incidence_raw", size=t, se = TRUE, conf = 0.95, nboot = 999)
Rich.rar$DataInfo  
#"For incidence data, the list $DataInfo includes the reference sample size (T), observed species richness (S.obs), total number of incidences (U), a sample coverage estimate (SC), and the first ten incidence frequency counts (Q1‐Q10)."

Rich.rar$iNextEst

ggiNEXT(Rich.rar, type=1)  

##Save results for graph  
f<-fortify(Rich.rar,type=1)
write.csv(f, "/Users/Lia/Documents/Git/Fraser-salmon/all.data/Rich.rar.csv")
f$site<- factor(f$site, levels=unique(f$site))
f.point <- f[which(f$method=="observed"),]
f.line <- f[which(f$method!="observed"),]
f.line$method <- factor(f.line$method, 
                        c("interpolated", "extrapolated"),
                        c("interpolation", "extrapolation"))


#set up shapes for habitat
shapes<- c(24,21,22)
names(shapes)<- names(my.list)

# set up colors for season
fills <- c("steelblue","forestgreen","goldenrod1")
names(fills) <- names(my.list)
colours <- c("steelblue","forestgreen","goldenrod1")
names(colours) <- names(my.list)

##Plot pretty  
gg.rich1<- ggplot(f, aes(x=x,y=y,shape=site)) + 
  geom_ribbon(aes(ymin=y.lwr, ymax=y.upr, fill=site), alpha=0.5) +
  geom_line(aes(linetype=method, colour=site), lwd=1.5, data=f.line) +
  scale_linetype_manual(values=c("solid","dashed"),breaks=c("interpolation","extrapolation"),labels=c("Interpolation","Extrapolation"),name="Method",guide=FALSE) +
  geom_point(aes(shape=site, fill=site, colour=site), size=5, data=f.point) + 
  scale_shape_manual(values=shapes, name="Habitat") + scale_colour_manual(values=colours, name="Habitat") + 
  scale_fill_manual(values=fills, name="Habitat") +
  labs(x="Number of sampling events", y="Species richness") +
  theme_bw() +   
  scale_y_continuous(limits=c(0,50),breaks=seq(0,50,by=10)) +
  scale_x_continuous(limits=c(0,180),breaks=seq(0,180,by=20)) +
  theme(axis.line = element_line(color="black", size = 0.5),
        axis.text.x=element_text(size=16),
        axis.title.x=element_text(size=18),
        axis.title.y=element_text(angle=90,size=18), 
        axis.text.y=element_text(size=16), 
        plot.margin = margin(6,6,6,6, "pt"),
        legend.position=c(.8,0.08),legend.justification=c(.8,.2),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16), 
        legend.key=element_blank(), legend.background = element_blank(),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank(), panel.border = element_blank())
gg.rich1

### FINAL PLOT 1.2, panel 0, above habitat plot reduced in size to fit with season panel plots

# set up colors for season
fills <- c("steelblue","forestgreen","goldenrod1")
names(fills) <- names(my.list)
colours <- c("steelblue","forestgreen","goldenrod1")
names(colours) <- names(my.list)

gg.rich2<- ggplot(f, aes(x=x,y=y,shape=site)) + 
  geom_ribbon(aes(ymin=y.lwr, ymax=y.upr, fill=site), alpha=0.5) +
  geom_line(aes(linetype=method, colour=site), lwd=1.5, data=f.line) +
  scale_linetype_manual(values=c("solid","dashed"),breaks=c("interpolation","extrapolation"),labels=c("Interpolation","Extrapolation"),name="Method",guide=FALSE) +
  geom_point(aes(shape=site, fill=site, colour=site), size=5, data=f.point) + 
  scale_shape_manual(values=shapes,guide=FALSE) + scale_colour_manual(values=colours,guide=FALSE) + 
  scale_fill_manual(values=fills,guide=FALSE) +
  labs(x="", y="Species richness") +
  theme_bw() +   
  scale_y_continuous(limits=c(0,50),breaks=seq(0,50,by=10)) +
  scale_x_continuous(limits=c(0,180),breaks=seq(0,180,by=40)) +
  theme(axis.line = element_line(color="black", size = 0.5),
        axis.text.x=element_text(size=12),
        axis.title.x=element_blank(),
        axis.title.y=element_text(angle=90,size=14), 
        axis.text.y=element_text(size=12), 
        plot.margin = margin(2,2,2,2, "pt"),
        panel.grid.minor=element_blank(),panel.grid.major=element_blank(), panel.border = element_blank())
gg.rich2


## **Panels 2,3,4: Season** compare by Season, Habitat; split into plot for each habitat  

#### Separate into data frames by Season*Habitat  
species.full$Site_day_year<- paste(species.full$Site, species.full$J.date, species.full$Year) #create new site_day_year label to identify each independent sampling event (sets still pooled). 

##Spring
Spring<-subset(species.full, month %in% c("March", "April")) ##Note narrowed the aggregation to site_day_year as opposed to site_round, which gives more data points per season for a smoother curve

Spring<-Spring[,-c(1:4,6)] #remove columns for year, jday, month, round, site 
#Marsh
MarshSpring<- subset(Spring, Habitat %in% c("Marsh")) 
MarshSpring<-MarshSpring[,-1] #remove column for Hab
MarshSp.t<- as.data.frame.matrix(table(MarshSpring[c(1,3)])) #now have species ~ site_day_year matrix, showing the # of sampling events where each of these species has been present for each site each round (and 0s for those not counted), across both years.  
#This has already been simplified to counts per sampling event, so is presence/absence (1s and 0s)
head(MarshSp.t)

#Eelgrass
EGSpring<-subset(Spring, Habitat %in% c("Eelgrass"))
EGSpring<-EGSpring[,-1] 
EGSp.t<- as.data.frame.matrix(table(EGSpring[c(1,3)]))   

#Sand flat  
SFSpring<-subset(Spring, Habitat %in% c("Sand flat"))
SFSpring<-SFSpring[,-1] 
SFSp.t<- as.data.frame.matrix(table(SFSpring[c(1,3)]))  

##Summer
Summer<-subset(species.full, month %in% c("May", "June", "July", "August"))

Summer<-Summer[,-c(1:4,6)] #remove columns for year, jday, month, round, site 
#Marsh
MarshSummer<- subset(Summer, Habitat %in% c("Marsh")) 
MarshSummer<-MarshSummer[,-1] 
MarshSu.t<- as.data.frame.matrix(table(MarshSummer[c(1,3)])) 

#Eelgrass
EGSummer<-subset(Summer, Habitat %in% c("Eelgrass")) 
EGSummer<-EGSummer[,-1] 
EGSu.t<- as.data.frame.matrix(table(EGSummer[c(1,3)])) 

#Sand flat  
SFSummer<-subset(Summer, Habitat %in% c("Sand flat")) 
SFSummer<-SFSummer[,-1] 
SFSu.t<- as.data.frame.matrix(table(SFSummer[c(1,3)]))  


##Fall (note 2016 only; not all marsh sites visited in October due to weather - will be limiting factor for sample size)
Fall<-subset(species.full, month %in% c("September", "October"))

Fall<-Fall[,-c(1:4,6)]
#Marsh
MarshFall<- subset(Fall, Habitat %in% c("Marsh")) 
MarshFall<-MarshFall[,-1] 
MarshF.t<- as.data.frame.matrix(table(MarshFall[c(1,3)])) 

#Eelgrass
EGFall<-subset(Fall, Habitat %in% c("Eelgrass")) 
EGFall<-EGFall[,-1] 
EGF.t<- as.data.frame.matrix(table(EGFall[c(1,3)]))   

#Sand flat  
SFFall<-subset(Fall, Habitat %in% c("Sand flat")) 
SFFall<-SFFall[,-1] 
SFF.t<- as.data.frame.matrix(table(SFFall[c(1,3)]))  


##Make list of matrices (one for each habitat type)   
##MARSH
my.listm<-list(MarshSp.t,MarshSu.t,MarshF.t)
names(my.listm)<-c("Marsh Early Spring","Marsh Summer","Marsh Fall")

##EELGRASS
my.listeg<-list(EGSp.t,EGSu.t,EGF.t)
names(my.listeg)<-c("Eelgrass Early Spring","Eelgrass Summer","Eelgrass Fall")

##SANDFLAT
my.listsf<-list(SFSp.t,SFSu.t,SFF.t)
names(my.listsf)<-c("Sandflat Early Spring","Sandflat Summer","Sandflat Fall")  


### panel 2 plot. Season comparison, broken into habitats: Marsh  
#Note "For incidence data, the list $DataInfo includes the reference sample size (T), observed species richness (S.obs), total number of incidences (U), a sample coverage estimate (SC), and the first ten incidence frequency counts (Q1‐Q10)."

#CAN SEE MUCH LOWER T AND U FOR MARSH IN FALL -- set size limits to 1:75 in order to compare

##Run rarefaction on matrix list    
#1 using incidence aggregation  
t<- seq(1,75,by=1)
Rich.marsh<-iNEXT(my.listm, q =c(0), datatype = "incidence_raw", size=t, se = TRUE, conf = 0.95, nboot = 999)
Rich.marsh$DataInfo  

##Save results for graph  
f<-fortify(Rich.marsh,type=1)
f$site<- factor(f$site, levels=unique(f$site))
f.point <- f[which(f$method=="observed"),]
f.line <- f[which(f$method!="observed"),]
f.line$method <- factor(f.line$method, 
                        c("interpolated", "extrapolated"),
                        c("interpolation", "extrapolation"))

# set up shapes for habitat
shapes <- c(24,24,24)
names(shapes) <- names(my.listm)

# set up colors for season
fills <- c("lightskyblue","steelblue","midnightblue")
names(fills) <- names(my.listm)
colours <- c("lightskyblue","steelblue","midnightblue")
names(colours) <- names(my.listm)

##Plot pretty  
gg.richm<- ggplot(f, aes(x=x,y=y,shape=site)) + 
  geom_ribbon(aes(ymin=y.lwr, ymax=y.upr, fill=site),  alpha=0.4) +
  geom_line(aes(linetype=method, colour=site), lwd=1.5, data=f.line) +
  scale_linetype_manual(values=c("solid","dashed"),breaks=c("interpolation","extrapolation"),labels=c("Interpolation","Extrapolation"),name="Method",guide=FALSE) + scale_colour_manual(values=colours,guide=FALSE) +
  geom_point(aes(shape=site, fill=site), colour="white", size=5, data=f.point) +
  scale_shape_manual(values=shapes,guide=FALSE) + scale_fill_manual(values=fills,guide=FALSE) +
  labs(x="", y="Species richness") +
  theme_bw() +   
  scale_x_continuous(limits=c(0,75),breaks=seq(0,75,by=15)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(size=12),
        axis.title.x=element_blank(),
        axis.title.y=element_text(angle=90,size=14), 
        axis.text.y=element_text(size=12), 
        panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(2,2,2,2, "pt"))
gg.richm

#**Can see for marsh that season is not significantly different.** 
#Most apparent is # of sampling events between seasons, which is highest in summer (May - Aug) and lowest in fall (Sept, Oct). When we look at the comparable point (sample size=7, for fall), the richness values across seasons are virtually identical. 


### panel 3, Season comparison, broken into habitats: Eelgrass  
#Fall is lowest again but much higher than marsh. U = 12, T = 59

##Run rarefaction on matrix list    
#1 using incidence aggregation  
t<- seq(1,75,by=1)
Rich.eg<-iNEXT(my.listeg, q =c(0), datatype = "incidence_raw", size=t, se = TRUE, conf = 0.95, nboot = 999)
Rich.eg$DataInfo  

##Save results for graph  
f<-fortify(Rich.eg,type=1)
f$site<- factor(f$site, levels=unique(f$site))
f.point <- f[which(f$method=="observed"),]
f.line <- f[which(f$method!="observed"),]
f.line$method <- factor(f.line$method, 
                        c("interpolated", "extrapolated"),
                        c("interpolation", "extrapolation"))

# set up shapes for habitat
shapes <- c(21, 21, 21)
names(shapes) <- names(my.listeg)

# set up colors for season
fills <- c("springgreen","forestgreen","darkgreen")
names(fills) <- names(my.listeg)
colours <- c("springgreen","forestgreen","darkgreen")
names(colours) <- names(my.listeg)

##Plot pretty  
gg.richeg<- ggplot(f, aes(x=x,y=y,shape=site)) + 
  geom_ribbon(aes(ymin=y.lwr, ymax=y.upr, fill=site),  alpha=0.4) +
  geom_line(aes(linetype=method, colour=site), lwd=1.5, data=f.line) +
  scale_linetype_manual(values=c("solid","dashed"),breaks=c("interpolation","extrapolation"),labels=c("Interpolation","Extrapolation"),name="Method",guide=FALSE) + scale_colour_manual(values=colours,guide=FALSE) +
  geom_point(aes(shape=site, fill=site), colour="white", size=5, data=f.point) +
  scale_shape_manual(values=shapes,guide=FALSE) + scale_fill_manual(values=fills,guide=FALSE) +
  labs(x="", y="Species richness") +
  theme_bw() +   
  scale_x_continuous(limits=c(0,75),breaks=seq(0,75,by=15)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(size=12),
        axis.title.x=element_blank(),
        axis.title.y=element_text(angle=90,size=14), 
        axis.text.y=element_text(size=12), 
        panel.grid.minor=element_blank(),panel.grid.major=element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(2,2,2,2, "pt"))
gg.richeg

# **Note that for eelgrass, summer is significantly higher** than spring and fall at the point of comparison (fall, #sampling events = 12). Summer now includes May. Fall is lowest  
                                                                                                            
### panel 4, Season comparison, broken into habitats: Sand flat  
#Fall is lowest again, between marsh and eelgrass. U = 9, T = 25  

##Run rarefaction on matrix list    
t<- seq(1,75,by=1)
Rich.sf<-iNEXT(my.listsf, q =c(0), datatype = "incidence_raw", size=t, se = TRUE, conf = 0.95, nboot = 999) 
Rich.sf$DataInfo  

##Save results for graph  
f<-fortify(Rich.sf,type=1)
f$site<- factor(f$site, levels=unique(f$site))
f.point <- f[which(f$method=="observed"),]
f.line <- f[which(f$method!="observed"),]
f.line$method <- factor(f.line$method, 
                        c("interpolated", "extrapolated"),
                        c("interpolation", "extrapolation"))
# set up shapes for habitat
shapes <- c(22, 22, 22)
names(shapes) <- names(my.listsf)

# set up colors for season
fills <- c("lightgoldenrod1","goldenrod1","darkgoldenrod3")
names(fills) <- names(my.listsf)
colours <- c("lightgoldenrod1","goldenrod1","darkgoldenrod3")
names(colours) <- names(my.listsf)

##Plot pretty  
gg.richsf<- ggplot(f, aes(x=x,y=y,shape=site)) + 
  geom_ribbon(aes(ymin=y.lwr, ymax=y.upr, fill=site),  alpha=0.4) +
  geom_line(aes(linetype=method, colour=site), lwd=1.5, data=f.line) +
  scale_linetype_manual(values=c("solid","dashed"),breaks=c("interpolation","extrapolation"),labels=c("Interpolation","Extrapolation"),name="Method", guide=FALSE) + scale_colour_manual(values=colours, guide=FALSE) +
  geom_point(aes(shape=site, fill=site), colour="white", size=5, data=f.point) +
  scale_shape_manual(values=shapes, guide=FALSE) + scale_fill_manual(values=fills, guide=FALSE) +
  labs(x="Number of sampling events", y="Species richness") +
  theme_bw() +   
  scale_x_continuous(limits=c(0,75),breaks=seq(0,75,by=15)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(angle=90,size=14), 
        axis.text.y=element_text(size=12), 
        panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(2,2,2,2, "pt"))
gg.richsf

# **Despite appearing different, with spring at highest richness, there is no significant difference between seasons for sand flat**. Similarly to eelgrass, the summer plateaus much more quickly, indicating more new spp encountered each sample during the spring months (March, April) vs the summer months (May,June, July, August(2017)). Limiting (fall) sample size is 9.


##Final combined figure with panels from fig 1 and fig 2 combined for seasonal richness by habitat  
#Can see that summer is significantly higher in eelgrass, higher but not significant in marsh, and that spring is higher but not significant in sand flat.  
                                                                                                       #plot  
library(gridExtra) #need for arrangeGrob
library(ggpubr) #need for ggarrange
library(BoutrosLab.plotting.general) #need for custom legend grob  

##Note need to use arrangeGrob to ensure saves correctly with all 4 plots in one.  

#Create legends for all plots  
hab_legend<- list(legend=list(labels = c("Marsh", "Eelgrass", "Sand flat"), colours = c("steelblue", "forestgreen", "goldenrod1"), title = "Habitat", border = "white", position = "topright"))
hab_legend<- legend.grob(legends = hab_legend, title.just = "left", title.cex = 1.3, label.cex = 1, size = 2, border = NULL)

marsh_legend<- list(legend=list( labels = c("Spring", "Summer", "Fall"), colours = c("lightskyblue","steelblue","midnightblue"), title = "Marsh", border = "white", position="topright"))
#turn into legend grob for grid arrange:
marsh_legend<- legend.grob(legends = marsh_legend, title.just = "left", title.cex=1.3, label.cex=1, size=2, border=NULL)

eel_legend<- list(legend=list( labels = c("Spring", "Summer", "Fall"), colours = c("springgreen","forestgreen","darkgreen"), title = "Eelgrass", border = "white", position="topright"))
#turn into legend grob for grid arrange:
eel_legend<- legend.grob(legends = eel_legend, title.just = "left", title.cex=1.3, label.cex=1, size=2, border=NULL)

sand_legend<- list(legend=list( labels = c("Spring", "Summer", "Fall"), colours = c("lightgoldenrod1","goldenrod1","darkgoldenrod3"), title = "Sand flat", border = "white", position="topright"))
#turn into legend grob for grid arrange:
sand_legend<- legend.grob(legends = sand_legend, title.just = "left", title.cex=1.3, label.cex=1, size=2, border=NULL)

#Convert plots to grobs  
gg.rich2<- ggplotGrob(gg.rich2)
gg.richm<- ggplotGrob(gg.richm)
gg.richeg<- ggplotGrob(gg.richeg)
gg.richsf<- ggplotGrob(gg.richsf)

## Add panel labels
a<- grobTree(textGrob("a", x=0.15, y=0.98, gp=gpar(col="black", fontsize=13, fontface="bold")))
b<- grobTree(textGrob("b", x=0.15, y=0.725, gp=gpar(col="black", fontsize=13, fontface="bold")))
c<- grobTree(textGrob("c", x=0.15, y=0.48, gp=gpar(col="black", fontsize=13, fontface="bold")))
d<-grobTree(textGrob("d", x=0.15, y=0.225, gp=gpar(col="black", fontsize=13, fontface="bold")))

ggarrange(arrangeGrob(gg.rich2, gg.richm, gg.richeg, gg.richsf, nrow=4), arrangeGrob(hab_legend, marsh_legend, eel_legend, sand_legend, nrow=4), widths = c(3.8,1.2)) + annotation_custom(a) + annotation_custom(b) + annotation_custom(c) + annotation_custom(d)