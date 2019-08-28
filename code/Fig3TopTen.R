#Fig_3_Chalifour_et_al_2019_MEPS

library(here)

### Top ten most abundant species by habitat type  

#load data

#2016
top16<- read.csv("data/2016catchsum.csv") #pre-summarized data from all_clean.csv

top16<- top16[,c(1:4)]

###Subset by habitat and pull out top ten species   
Mtop10_2016<- top16[which(top16$Habitat == c("Marsh")),]
Mtop10_2016<- subset(Mtop10_2016, Species %in% c("Three-spined stickleback", "Shiner surfperch", "Chinook", "Peamouth chub", "Chum", "Northern anchovy", "Staghorn sculpin", "Starry flounder", "Unidentified flatfish", "Pink"))
Mtop10_2016$Species<- factor(Mtop10_2016$Species) ## Drop unused levels
M16<- Mtop10_2016[order(Mtop10_2016$abundance, decreasing=TRUE),]
M16$Species<- factor(M16$Species, levels = c("Three-spined stickleback", "Chinook", "Peamouth chub", "Starry flounder", "Chum", "Shiner surfperch","Northern anchovy", "Staghorn sculpin","Unidentified flatfish", "Pink"))

EGtop10_2016<- top16[which(top16$Habitat == c("Eelgrass")),]
EGtop10_2016<- subset(EGtop10_2016, Species %in% c("Shiner surfperch", "Three-spined stickleback", "Bay pipefish", "Herring", "Surf smelt", "Pacific sand lance", "Tubesnout", "Starry flounder", "Saddleback gunnel", "Pile perch"))
EGtop10_2016$Species<- factor(EGtop10_2016$Species) ## Drop unused levels
EG16<- EGtop10_2016[order(EGtop10_2016$abundance, decreasing=TRUE),]
EG16$Species<- factor(EG16$Species, levels = c("Shiner surfperch", "Three-spined stickleback", "Bay pipefish", "Herring", "Surf smelt", "Pacific sand lance", "Tubesnout", "Starry flounder", "Saddleback gunnel", "Pile perch"))

##Shiners = 18509, Stickleback = 6868   
##Manually reduce outlier numbers to fit on plot:  
EG16$abundance[which(EG16$Species%in%("Shiner surfperch"))]<-1000
EG16$abundance[which(EG16$Species%in%("Three-spined stickleback"))]<-1000


SFtop10_2016<- top16[which(top16$Habitat == c("Sand flat")),]
SFtop10_2016<- subset(SFtop10_2016, Species %in% c("Shiner surfperch", "Three-spined stickleback", "Northern anchovy", "Herring", "Unidentified flatfish", "Arrow goby", "Chinook", "Starry flounder", "Unidentified larval fish 2", "Sand sole"))
SFtop10_2016$Species<- factor(SFtop10_2016$Species) ## Drop unused levels
SF16<- SFtop10_2016[order(SFtop10_2016$abundance, decreasing=TRUE),]
SF16$Species<- factor(SF16$Species, levels = c("Shiner surfperch", "Northern anchovy", "Starry flounder", "Unidentified larval fish 2", "Three-spined stickleback", "Arrow goby", "Unidentified flatfish", "Chinook", "Herring", "Sand sole"))  
levels(SF16$Species)<- c("Shiner surfperch", "Northern anchovy", "Starry flounder", "Larval fish", "Three-spined stickleback", "Arrow goby", "Unidentified flatfish", "Chinook", "Herring", "Sand sole")
##Shiners = 2373   
##Manually reduce outlier numbers to fit on plot:  
SF16$abundance[which(SF16$Species%in%("Shiner surfperch"))]<-1000

#2017
top17<- read.csv("data/2017catchsum.csv") #pre-summarized data from all_clean.csv
Mtop10_2017<- top17[which(top17$Habitat == c("Marsh")),]
Mtop10_2017<- subset(Mtop10_2017, Species %in% c("Three-spined stickleback", "Shiner surfperch", "Chinook", "Peamouth chub", "Chum", "Prickly sculpin", "Staghorn sculpin", "Starry flounder", "Unidentified flatfish", "Unidentified sculpin"))
Mtop10_2017$Species<- factor(Mtop10_2017$Species) ## Drop unused levels
M17<- Mtop10_2017[order(Mtop10_2017$abundance, decreasing=TRUE),]
M17$Species<- factor(M17$Species, levels = c("Unidentified flatfish", "Three-spined stickleback", "Starry flounder", "Chinook", "Staghorn sculpin", "Chum", "Peamouth chub", "Prickly sculpin", "Shiner surfperch", "Unidentified sculpin"))

EGtop10_2017<- top17[which(top17$Habitat == c("Eelgrass")),]
EGtop10_2017<- subset(EGtop10_2017, Species %in% c("Shiner surfperch", "Three-spined stickleback", "Bay pipefish", "Herring", "Surf smelt", "Pacific sand lance", "Tubesnout", "Starry flounder", "Chum", "Chinook"))
EGtop10_2017$Species<- factor(EGtop10_2017$Species) ## Drop unused levels
EG17<- EGtop10_2017[order(EGtop10_2017$abundance, decreasing=TRUE),]
EG17$Species<- factor(EG17$Species, levels = c("Three-spined stickleback", "Shiner surfperch", "Starry flounder", "Chum", "Herring", "Pacific sand lance", "Tubesnout",  "Chinook", "Surf smelt", "Bay pipefish"))
##Shiners = 3734, Stickleback = 3957    
##Manually reduce outlier numbers to fit on plot:  
EG17$abundance[which(EG17$Species%in%("Shiner surfperch"))]<-1000
EG17$abundance[which(EG17$Species%in%("Three-spined stickleback"))]<-1000

SFtop10_2017<- top17[which(top17$Habitat == c("Sand flat")),]
SFtop10_2017<- subset(SFtop10_2017, Species %in% c("Shiner surfperch", "Three-spined stickleback", "Chum", "Herring", "Unidentified flatfish", "Arrow goby", "Chinook", "Starry flounder", "Unidentified larval fish 2", "Pacific sand lance"))
SFtop10_2017$Species<- factor(SFtop10_2017$Species) ## Drop unused levels
SF17<- SFtop10_2017[order(SFtop10_2017$abundance, decreasing=TRUE),]
SF17$Species<- factor(SF17$Species, levels = c("Starry flounder", "Pacific sand lance","Shiner surfperch", "Unidentified flatfish", "Three-spined stickleback", "Chum", "Arrow goby", "Chinook", "Unidentified larval fish 2", "Herring"))


##Use whole data set for labels and legends  
top<- rbind(Mtop10_2016, Mtop10_2017, EGtop10_2016, EGtop10_2017, SFtop10_2016, SFtop10_2017)
levels(top$Species)
row.names(top)<- 1:59

##Add class back in referencing all.class2.csv (not in this data set)  
top$Class<- c(
  "Migratory", "Migratory","Migratory","Migratory","Resident","Resident","Resident","Resident","Resident","Resident","Migratory","Migratory","Resident","Resident","Resident","Resident","Resident","Resident","Resident","Resident","Resident","Resident","Migratory","Resident","Resident","Resident","Resident","Migratory","Resident","Resident","Resident","Migratory","Migratory","Resident","Migratory","Resident","Resident","Migratory","Resident","Resident","Resident","Migratory","Resident","Migratory","Resident","Resident","Resident","Resident","Resident","Unclassified","Resident","Migratory","Migratory","Resident","Migratory","Resident","Resident","Resident","Resident"
)


library(ggplot2)
library(cowplot)
library(gridExtra)
library(ggpubr)

## Create custom colour scale to match to species levels:  
sp_col<- c("firebrick","red","orangered","tomato1", "sienna1", "lightcoral",   "midnightblue","steelblue","darkslateblue", "royalblue1","royalblue4","dodgerblue","mediumblue","lightslateblue","deepskyblue","darkturquoise","lightseagreen","cyan","skyblue1","lightblue","paleturquoise", "gray87")
names(sp_col)<- c("Chinook","Chum","Pacific sand lance","Surf smelt", "Northern anchovy","Pink","Shiner surfperch","Three-spined stickleback","Starry flounder","Unidentified flatfish","Herring","Bay pipefish","Peamouth chub","Tubesnout","Staghorn sculpin","Prickly sculpin","Arrow goby","Sand sole","Pile perch","Saddleback gunnel","Unidentified sculpin","Larval fish")
colScale<- scale_fill_manual(values = sp_col, name = "Species") 

legndorder<- as.data.frame(sp_col) 
rownames(legndorder)[1]<- c("Chinook salmon");rownames(legndorder)[2]<- c("Chum salmon"); rownames(legndorder)[6]<- c("Pink salmon"); rownames(legndorder)[11]<- c("Pacific Herring")
legndorder$order<- c(1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3)
mig_leg<- subset(legndorder, order ==1)
colScale_mig<- scale_fill_manual(values = mig_leg$sp_col, name = "Migratory")

res_leg<- subset(legndorder, order ==2)
colScale_res<- scale_fill_manual(values = res_leg$sp_col, name = "Resident")

un_leg<- subset(legndorder, order ==3)
colScale_un<- scale_fill_manual(values=un_leg$sp_col, name = "Unclassified")

##Create label scale common for all species that matches to colour scale 
sp_labels<- c("Chinook salmon"="Chinook","Chum salmon"="Chum","Northern anchovy"="Anch","Peamouth chub"="Peam","Pink salmon"="Pink","Shiner surfperch"="Shiner","Staghorn sculpin"="Stag","Starry flounder"="Starry","Three-spined stickleback"="TS stick","Unidentified flatfish"="Un Flat","Prickly sculpin"="Prickly","Unidentified sculpin"="Un Sculp","Bay pipefish"="Pipe","Pacific Herring"="Herring","Pacific sand lance"="SLance","Pile perch"="Pile","Saddleback gunnel"="Saddle","Surf smelt"="S smelt","Tubesnout"="Tubesnout","Arrow goby"="A goby","Sand sole"="Sole","Larval fish"="Larval")

xScale<- scale_x_discrete(labels = sp_labels)  


###MAIN PLOTS TO ARRANGE TOGETHER FOR FINAL PLOT: 1 for each habitat*year combo:  

#M16
mplot<- ggplot(M16, aes(x = Species, y = abundance)) + 
  geom_bar(stat = "identity", aes(fill = Species)) +
  colScale +
  xScale +
  theme_cowplot() +
  labs(y = "", x = "") +
  scale_y_continuous(breaks = c(0, 250, 500, 750, 1000, 1250, 1500), labels = c("0", "250", "500", "750", "1000", "1250", ""), expand=c(0,0)) + expand_limits(y=c(0,1400)) +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, size = 10, vjust = 1, hjust = 1)
  )

#EG16
##Shiners = 18509, Stickleback = 6868 
egplot<- ggplot(EG16, aes(x = Species, y = abundance)) + 
  geom_bar(stat = "identity", aes(fill = Species)) +
  colScale +
  xScale +
  theme_cowplot() +
  scale_y_continuous(breaks = c(0, 250, 500, 750, 1000), labels = c("0", "250", "500", "750", "1000"), expand = c(0,0)) + 
  expand_limits(y=c(0, 1050)) +
  labs(y = "", x = "") +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, size = 10, vjust = 1, hjust = 1)
  )


#SF16
##Shiners = 2373
splot<- ggplot(SF16, aes(x = Species, y = abundance)) + 
  geom_bar(stat = "identity", aes(fill = Species)) +
  colScale +
  xScale +
  theme_cowplot() +
  scale_y_continuous(breaks = c(0, 250, 500, 750, 1000), labels = c("0", "250", "500", "750", "1000"), expand = c(0,0)) + 
  expand_limits(y=c(0, 1050)) +
  labs(y = "", x = "") +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, size = 10, vjust = 1, hjust = 1)
  )

#M17
mplot2<- ggplot(M17, aes(x = Species, y = abundance)) + 
  geom_bar(stat = "identity", aes(fill = Species)) +
  colScale +
  xScale +
  theme_cowplot() +
  labs(y = "", x = "") +
  scale_y_continuous(breaks = c(0, 250, 500, 750, 1000, 1250, 1500), labels = c("","","","","","",""), expand=c(0,0)) + expand_limits(y=c(0,1400)) +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10)
  )

#EG17  
##Shiners = 3734, Stickleback = 3957
egplot2<- ggplot(EG17, aes(x = Species, y = abundance)) + 
  geom_bar(stat = "identity", aes(fill = Species)) +
  colScale +
  xScale +
  theme_cowplot() +
  labs(y = "", x = "") +
  scale_y_continuous(breaks = c(0, 250, 500, 750, 1000), labels = c("","","","",""), expand = c(0,0)) + 
  expand_limits(y=c(0, 1050)) +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10)
  )

#SF17
splot2<- ggplot(SF17, aes(x = Species, y = abundance)) + 
  geom_bar(stat = "identity", aes(fill = Species)) +
  colScale +
  xScale +
  theme_cowplot() +
  scale_y_continuous(breaks = c(0, 250, 500, 750, 1000), labels =  c("","","","",""), expand = c(0,0)) + 
  expand_limits(y=c(0, 1050)) +
  labs(y = "", x = "") +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10)
  )  

##Plot together:
#First should remove 2nd printout page by converting individually to ggplotGrobs
mplot<- ggplotGrob(mplot)
mplot2<- ggplotGrob(mplot2) 
egplot<-ggplotGrob(egplot) 
egplot2<- ggplotGrob(egplot2) 
splot<- ggplotGrob(splot) 
splot2<- ggplotGrob(splot2)
### uses manual legend grobs and layout matrix:  
library(BoutrosLab.plotting.general)
library(grid)
library(ggplot2)

#create custom layout matrix  
lay<- rbind(c(8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8),
            c(8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8),
            c(8,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,8,3,3,3,3,3,3),
            c(8,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,8,3,3,3,3,3,3),
            c(8,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,8,3,3,3,3,3,3),
            c(8,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,8,3,3,3,3,3,3),
            c(8,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,8,3,3,3,3,3,3),
            c(8,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,8,3,3,3,3,3,3),
            c(8,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,8,3,3,3,3,3,3),
            c(8,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,8,3,3,3,3,3,3),
            c(8,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,8,3,3,3,3,3,3),
            c(8,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,8,3,3,3,3,3,3),
            c(8,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,8,3,3,3,3,3,3),
            c(8,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,8,3,3,3,3,3,3),
            c(8,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,8,3,3,3,3,3,3),
            c(8,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,8,3,3,3,3,3,3),
            c(8,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,8,3,3,3,3,3,3),
            c(8,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,8,3,3,3,3,3,3),
            c(8,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,8,3,3,3,3,3,3),
            c(8,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,8,3,3,3,3,3,3),
            c(8,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,8,3,3,3,3,3,3),
            c(8,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,8,3,3,3,3,3,3),
            c(8,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,8,3,3,3,3,3,3),
            c(8,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,8,3,3,3,3,3,3),
            c(8,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,8,3,3,3,3,3,3),
            c(8,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,8,3,3,3,3,3,3),
            c(8,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,8,3,3,3,3,3,3),
            c(8,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,8,3,3,3,3,3,3),
            c(8,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,8,3,3,3,3,3,3),
            c(8,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,8,3,3,3,3,3,3),
            c(8,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,8,3,3,3,3,3,3),
            c(8,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,8,3,3,3,3,3,3),
            c(8,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,8,8,8,8,8,8,8),
            c(8,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,8,8,8,8,8,8,8),
            c(8,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,8,8,8,8,8,8,8),
            c(8,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,8,8,8,8,8,8,8),
            c(8,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,8,8,8,8,8,8,8),
            c(8,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,8,8,8,8,8,8,8),
            c(8,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,8,8,8,8,8,8,8))
#create blank rectangle for margins and spaces  
legendspace = rectGrob(2,1,gp = gpar(fill = "blue", col=NULL))

#Create common legend for all plots  
legends2<- list(legend=list(labels = rownames(mig_leg), title = "Migratory", title.fontface = "bold", colours = as.character(mig_leg$sp_col), border = "white", position="topright"), legend=list(labels = rownames(res_leg), title = "Resident", title.fontface = "bold", colours = as.character(res_leg$sp_col), border = "white", position="topright"),legend=list(labels = rownames(un_leg), title = "Unclassified", title.fontface = "bold", colours = as.character(un_leg$sp_col), border = "white", position="topright"))
#turn into legend grob for grid arrange:
legendgrob1<- legend.grob(legends = legends2, title.just = "left", title.cex=1.1, label.cex=1, size = 2, border=NULL)


#####Text annotations to add onto plots  
#EG16  
##Shiners = 18509, Stickleback = 6868
ellips1<- grobTree(textGrob("~", x=0.158, y=0.628, gp=gpar(col="midnightblue", fontsize=15, fontface="bold")))
ellips2<- grobTree(textGrob("~", x=0.182, y=0.628, gp=gpar(col="steelblue", fontsize=15, fontface="bold")))
abun1<- grobTree(textGrob("18509", x=0.17, y=0.65, gp=gpar(col="black", fontsize=10), rot=45))
abun2<- grobTree(textGrob("6868", x=0.195, y=0.65, gp=gpar(col="black", fontsize=10), rot=45))

#SF16
##Shiners = 2373
ellips3<- grobTree(textGrob("~", x=0.158, y=0.295, gp=gpar(col="midnightblue", fontsize=15, fontface="bold")))
abun3<- grobTree(textGrob("2373", x=0.169, y=0.317, gp=gpar(col="black", fontsize=10), rot=45))

#EG17  
##Shiners = 3734, Stickleback = 3957
ellips4<- grobTree(textGrob("~", x=0.4635, y=0.628, gp=gpar(col="steelblue", fontsize=15, fontface="bold")))
abun4<- grobTree(textGrob("3957", x=0.475, y=0.65, gp=gpar(col="black", fontsize=10), rot=45))
ellips5<- grobTree(textGrob("~", x=0.487, y=0.628, gp=gpar(col="midnightblue", fontsize=15, fontface="bold")))
abun5<- grobTree(textGrob("3734", x=0.5, y=0.65, gp=gpar(col="black", fontsize=10), rot=45))

#YEAR
yr1<- grobTree(textGrob("2016", x=0.27, y=0.965, gp=gpar(col="black", fontsize=14, fontface="bold")))
yr2<- grobTree(textGrob("2017", x=0.57, y=0.965, gp=gpar(col="black", fontsize=14, fontface="bold")))

#Axes
ax_y2<-grobTree(textGrob("Total Catch", x=0.078, y=0.534, gp=gpar(col="black", fontsize=12), rot=90))

#Habitat
a<-grobTree(textGrob("A", x=0.045, y=0.92, gp=gpar(col="black", fontsize=13, fontface="bold")))
b<-grobTree(textGrob("B", x=0.045, y=0.62, gp=gpar(col="black", fontsize=13, fontface="bold")))
c<-grobTree(textGrob("C", x=0.045, y=0.29, gp=gpar(col="black", fontsize=13, fontface="bold")))

#Final plot
ggarrange(arrangeGrob(mplot, mplot2, legendgrob1, egplot, egplot2, splot, splot2,legendspace, layout_matrix = lay),align = "hv") + annotation_custom(abun1) + annotation_custom(abun2) + annotation_custom(ellips1) + annotation_custom(ellips2) + annotation_custom(ellips3) + annotation_custom(abun3) + annotation_custom(ellips4) + annotation_custom(abun4) + annotation_custom(ellips5) + annotation_custom(abun5) + annotation_custom(yr1) + annotation_custom(yr2) + annotation_custom(ax_y2) + annotation_custom(a) + annotation_custom(b) + annotation_custom(c)


```