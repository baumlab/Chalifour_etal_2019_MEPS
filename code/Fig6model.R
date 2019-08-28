#Fig_6_Chalifour_et_al_2019_MEPS_revised

## Model output panel plot

##Note code for original plots is in pursecatchmodels.Rmd and marshcatchmodels.Rmd --> load data and run parameter plot for each first. Here we simply convert the plots to grobs and arrange together in a grid  

#plot  

library(gridExtra) #need for arrangeGrob
library(ggpubr) #need for ggarrange
library(BoutrosLab.plotting.general) #need for custom legend grob  

##Note need to use arrangeGrob to ensure saves correctly with all 8 plots in one.  
#Convert plots to grobs  
gg.purse.chin<- ggplotGrob(gg.purse.chin)
gg.purse.chum<- ggplotGrob(gg.purse.chum)
gg.purse.migra<- ggplotGrob(gg.purse.migra)
gg.purse.res<- ggplotGrob(gg.purse.res)
gg.beach.chin<- ggplotGrob(gg.beach.chin)
gg.beach.chum<- ggplotGrob(gg.beach.chum)
gg.beach.migra<- ggplotGrob(gg.beach.migra)
gg.beach.res<- ggplotGrob(gg.beach.res)  

## Add panel labels
a<- grobTree(textGrob("a", x=0.107, y=0.92, gp=gpar(col="black", fontsize=13, fontface="bold")))
b<- grobTree(textGrob("b", x=0.36, y=0.92, gp=gpar(col="black", fontsize=13, fontface="bold")))
c<-grobTree(textGrob("c", x=0.608, y=0.92, gp=gpar(col="black", fontsize=13, fontface="bold")))
d<-grobTree(textGrob("d", x=0.862, y=0.92, gp=gpar(col="black", fontsize=13, fontface="bold")))
e<-grobTree(textGrob("e", x=0.103, y=0.47, gp=gpar(col="black", fontsize=13, fontface="bold")))
f<-grobTree(textGrob("f", x=0.36, y=0.47, gp=gpar(col="black", fontsize=13, fontface="bold")))
g<-grobTree(textGrob("g", x=0.61, y=0.47, gp=gpar(col="black", fontsize=13, fontface="bold")))
h<-grobTree(textGrob("h", x=0.8523, y=0.47, gp=gpar(col="black", fontsize=13, fontface="bold")))

#plot together
ggarrange(arrangeGrob(gg.beach.chin, gg.beach.chum, gg.beach.migra, gg.beach.res, gg.purse.chin, gg.purse.chum, gg.purse.migra, gg.purse.res, nrow=2, ncol=4), heights = c(1+3/4,1), align = "v")+ annotation_custom(a) + annotation_custom(b) + annotation_custom(c)+ annotation_custom(d) + annotation_custom(e) + annotation_custom(f)+ annotation_custom(g) + annotation_custom(h)

