##### Figure_A2_Abundance_panel_plot

###Load data
library(here)

all<- read.csv("data/all.class2.csv")

#convert months to numeric to compare monthly means instead of round means
all$month<- factor(all$month, levels = c("March", "April", "May", "June", "July", "August", "September", "October"), ordered = TRUE)
month.numeric<- as.numeric(all$month) #set month to numeric value based on levels, create object for ordering
all$month.numeric<-month.numeric #add to df
all<-all[order(month.numeric),] #order df by month

#count and aggregate to get abundance
library(plyr)
all<-count(all)
all<-ddply(all, .(Year, J.date, month.numeric, Habitat, Species, class), summarize, abundance = sum(freq))
all$abundance[which(all$Species == 0)]<- 0 #set empty nets to 0 abundance
all$abundance<- as.numeric(all$abundance)

#separate into species groups
chin<- subset(all, Species%in%c("Chinook"))
chum<- subset(all, Species%in%c("Chum"))
migratory<- subset(all, class%in%c("migratory")); migratory<-migratory[which(migratory$Species!= "Chinook"),]; migratory<- migratory[which(migratory$Species != "Chum"),]
resident<- subset(all, class%in%c("resident"))

#group by means for plots
chin<- ddply(chin, .(month.numeric, Habitat, Species, class), summarize, mean = mean(abundance), sd = sd(abundance))
chin$sd[which(is.na(chin$sd))]<- 0.05 ##set NAs and 0s to 0.05 so arrows can function in plot
chin$sd[which(chin$sd==0)]<-0.05

chum<- ddply(chum, .(month.numeric, Habitat, Species, class), summarize, mean = mean(abundance), sd = sd(abundance))
chum$sd[which(is.na(chum$sd))]<- 0.05 
chum$sd[which(chum$sd==0)]<-0.05

migratory<- ddply(migratory, .(month.numeric, Habitat, class), summarize, mean = mean(abundance), sd = sd(abundance))
migratory$sd[which(migratory$sd==0)]<-0.05
migratory$sd[which(is.na(migratory$sd))]<- 0.05 

resident<- ddply(resident, .(month.numeric, Habitat, class), summarize, mean = mean(abundance), sd = sd(abundance))
resident$sd[which(resident$sd==0)]<-0.05
resident$sd[which(is.na(resident$sd))]<- 0.05 

#set arrows x and ys for ease
set.seed(1)
x0<-chin$month.numeric[chin$Habitat%in%c("Marsh")]
y0<-chin$mean[chin$Habitat%in%c("Marsh")]+chin$sd[chin$Habitat%in%c("Marsh")]
y0.0<-chin$mean[chin$Habitat%in%c("Marsh")]-chin$sd[chin$Habitat%in%c("Marsh")]

x1<-chum$month.numeric[chum$Habitat%in%c("Marsh")]
y1<-chum$mean[chum$Habitat%in%c("Marsh")]+chum$sd[chum$Habitat%in%c("Marsh")]
y1.0<-chum$mean[chum$Habitat%in%c("Marsh")]-chum$sd[chum$Habitat%in%c("Marsh")]

x2<-migratory$month.numeric[migratory$Habitat%in%c("Marsh")]
y2<-migratory$mean[migratory$Habitat%in%c("Marsh")]+migratory$sd[migratory$Habitat%in%c("Marsh")]
y2.0<-migratory$mean[migratory$Habitat%in%c("Marsh")]-migratory$sd[migratory$Habitat%in%c("Marsh")]

x3<-resident$month.numeric[resident$Habitat%in%c("Marsh")]
y3<-resident$mean[resident$Habitat%in%c("Marsh")]+resident$sd[resident$Habitat%in%c("Marsh")]
y3.0<-resident$mean[resident$Habitat%in%c("Marsh")]-resident$sd[resident$Habitat%in%c("Marsh")]

x00<-chin$month.numeric[chin$Habitat%in%c("Eelgrass")]
y00<-chin$mean[chin$Habitat%in%c("Eelgrass")]+chin$sd[chin$Habitat%in%c("Eelgrass")]
y00.0<-chin$mean[chin$Habitat%in%c("Eelgrass")]-chin$sd[chin$Habitat%in%c("Eelgrass")]

x10<-chum$month.numeric[chum$Habitat%in%c("Eelgrass")]
y10<-chum$mean[chum$Habitat%in%c("Eelgrass")]+chum$sd[chum$Habitat%in%c("Eelgrass")]
y10.0<-chum$mean[chum$Habitat%in%c("Eelgrass")]-chum$sd[chum$Habitat%in%c("Eelgrass")]

x20<-migratory$month.numeric[migratory$Habitat%in%c("Eelgrass")]
y20<-migratory$mean[migratory$Habitat%in%c("Eelgrass")]+migratory$sd[migratory$Habitat%in%c("Eelgrass")]
y20.0<-migratory$mean[migratory$Habitat%in%c("Eelgrass")]-migratory$sd[migratory$Habitat%in%c("Eelgrass")]

x30<-resident$month.numeric[resident$Habitat%in%c("Eelgrass")]
y30<-resident$mean[resident$Habitat%in%c("Eelgrass")]+resident$sd[resident$Habitat%in%c("Eelgrass")]
y30.0<-resident$mean[resident$Habitat%in%c("Eelgrass")]-resident$sd[resident$Habitat%in%c("Eelgrass")]

x40<-chin$month.numeric[chin$Habitat%in%c("Sand flat")]
y40<-chin$mean[chin$Habitat%in%c("Sand flat")]+chin$sd[chin$Habitat%in%c("Sand flat")]
y40.0<-chin$mean[chin$Habitat%in%c("Sand flat")]-chin$sd[chin$Habitat%in%c("Sand flat")]

x50<-chum$month.numeric[chum$Habitat%in%c("Sand flat")]
y50<-chum$mean[chum$Habitat%in%c("Sand flat")]+chum$sd[chum$Habitat%in%c("Sand flat")]
y50.0<-chum$mean[chum$Habitat%in%c("Sand flat")]-chum$sd[chum$Habitat%in%c("Sand flat")]

x60<-jitter(migratory$month.numeric[migratory$Habitat%in%c("Sand flat")], amount = 0.5)
y60<-migratory$mean[migratory$Habitat%in%c("Sand flat")]+migratory$sd[migratory$Habitat%in%c("Sand flat")]
y60.0<-migratory$mean[migratory$Habitat%in%c("Sand flat")]-migratory$sd[migratory$Habitat%in%c("Sand flat")]

x70<-resident$month.numeric[resident$Habitat%in%c("Sand flat")]
y70<-resident$mean[resident$Habitat%in%c("Sand flat")]+resident$sd[resident$Habitat%in%c("Sand flat")]
y70.0<-resident$mean[resident$Habitat%in%c("Sand flat")]-resident$sd[resident$Habitat%in%c("Sand flat")]

ylabs<- as.vector(seq(0,180, by=40))
ylabs2<- as.vector(seq(0,260, by=50))
ylabs3<- as.vector(seq(0,1300, by=300))
ylabs4<- as.vector(seq(0,400, by=80))
#### plot

pdf("final.figures/FigA2.pdf", width = 13.4, height = 7.5, pointsize = 16)
par(mfrow = c(3, 4), mar = c(.3,.3,.3,.3), oma = c(1.5, 3, 2.5, 2.3))
#a
plot(mean~month.numeric, data=chin[chin$Habitat%in%c("Marsh"),], bty = "l", ylab = "", yaxt = "n", xlab = "", xlim = c(0.5,8.5), ylim = c(0,180), xaxt = "n", main = "Chinook", cex = 2.5,pch=17, bg="black",xpd=NA) %/% arrows(x0, y0, x0, y0.0, length=0.05, angle=90, code=3) 
axis(side = 1, at = c(1,2,3,4,5,6,7,8), labels= FALSE, tck =-0.02) 
axis(side = 2, at = seq(0,180, by=20), labels= FALSE, tck =-0.03)
text(y = seq(0,180, by=40),labels = ylabs, par("usr")[1], pos = 2, cex = 1, xpd=NA)
mtext("a", side=2, las = 2, line=-1, at = c(170), font = 2)
#b
plot(mean~month.numeric, data=chum[chum$Habitat%in%c("Marsh"),], bty = "l", ylab = "", xlab = "", xlim = c(0.5,8.5), ylim = c(0,180), yaxt = "n", xaxt = "n", main = "Chum",cex = 2.5, pch=17, bg="black",xpd=NA) %/% arrows(x1, y1, x1, y1.0, length=0.05, angle=90, code=3)
axis(side = 1, at = c(1,2,3,4,5,6,7,8), labels= FALSE, tck =-0.02) 
axis(side = 2, at = seq(0,180, by=20), labels= FALSE, tck =-0.03)
mtext("b", side=2, las = 2, line=-1, at = c(170), font = 2)
#legend
legend(4.5,240, c("Marsh","Eelgrass","Sandflat"), horiz = TRUE, cex = 1.3, pt.lwd=1, pt.cex=1.3, bty='n', pch=c(24,21,22), pt.bg = "black", text.font = 2, x.intersp = 0.5, xpd = NA)
#c
plot(mean~month.numeric, data=migratory[migratory$Habitat%in%c("Marsh"),], bty = "l", ylab = "", xlab = "", xlim = c(0.5,8.5), ylim = c(0,180), yaxt = "n", xaxt = "n",  cex = 2.5, main = "Migratory", pch=17, bg="black",xpd=NA) %/% arrows(x2, y2, x2, y2.0, length=0.05, angle=90, code=3)
axis(side = 1, at = c(1,2,3,4,5,6,7,8), labels= FALSE, tck =-0.02) 
axis(side = 2, at = seq(0,180, by=20), labels= FALSE, tck =-0.03)
mtext("c", side=2, las = 2, line=-1, at = c(170), font = 2)
#d
plot(mean~month.numeric, data=resident[resident$Habitat%in%c("Marsh"),], bty = "l", ylab = "", xlab = "", xlim = c(0.5,8.5), ylim = c(0,180), yaxt = "n", xaxt = "n", cex = 2.5, main = "Resident", pch=17, bg="black",xpd=NA) %/% arrows(x3, y3, x3, y3.0, length=0.05, angle=90, code=3)
axis(side = 1, at = c(1,2,3,4,5,6,7,8), labels= FALSE, tck =-0.02) 
axis(side = 2, at = seq(0,180, by=20), labels= FALSE, tck =-0.03)
mtext("d", side=2, las = 2, line=-1, at = c(170), font = 2)
#e
plot(x=x00, y=chin$mean[chin$Habitat%in%c("Eelgrass")], bty = "l", ylab = "", xlab = "", xaxt = "n", yaxt = "n", xlim = c(0.5,8.5), ylim = c(0,260), pch=c(16),cex = 2, bg="black",xpd=NA) %/% arrows(x00, y00, x00, y00.0, length=0.05, angle=90, code=3)
axis(side = 1, at = c(1,2,3,4,5,6,7,8), tck =-0.02, labels = NA) 
axis(side = 2, at = seq(0,260, by=25), labels= FALSE, tck =-0.03)
text(y = seq(0,260, by=50),labels = ylabs2, par("usr")[1], pos = 2, cex = 1, xpd=NA)
mtext("e", side=2, las = 2, line=-1, at = c(240), font = 2)
mtext(expression(paste("Mean catch")), side = 2,cex = 1, line = 2.08, outer= FALSE )
#f
plot(x=x10, y=chum$mean[chum$Habitat%in%c("Eelgrass")], bty = "l", ylab = "", xlab = "", yaxt = "n", xaxt = "n", xlim = c(0.5,8.5), ylim = c(0,260), cex = 2, pch=c(16),xpd=NA) %/% arrows(x10, y10, x10, y10.0, length=0.05, angle=90, code=3)
axis(side = 1, at = c(1,2,3,4,5,6,7,8), tck =-0.02, labels = NA) 
axis(side = 2, at = seq(0,260, by=25), labels= FALSE, tck =-0.03)
mtext("f", side=2, las = 2, line=-1, at = c(240), font = 2)
#g
plot(x=x20, y=migratory$mean[migratory$Habitat%in%c("Eelgrass")], bty = "l", ylab = "", xlab = "", yaxt = "n", xaxt = "n", xlim = c(0.5,8.5), ylim = c(0,260), cex = 2,pch=c(16), bg="black",xpd=NA) %/% arrows(x20, y20, x20, y20.0, length=0.05, angle=90, code=3)
axis(side = 1, at = c(1,2,3,4,5,6,7,8), tck =-0.02, labels = NA) 
axis(side = 2, at = seq(0,260, by=25), labels= FALSE, tck =-0.03)
mtext("g", side=2, las = 2, line=-1, at = c(240), font = 2)
#h
plot(x=x30, y=resident$mean[resident$Habitat%in%c("Eelgrass")], bty = "u", ylab = "", xlab = "", yaxt = "n", xaxt = "n", xlim = c(0.5,8.5), ylim = c(0,1300), pch=c(16),cex = 2, bg="black",xpd=NA) %/% arrows(x30, y30, x30, y30.0, length=0.05, angle=90, code=3)
axis(side = 1, at = c(1,2,3,4,5,6,7,8), tck =-0.02, labels = NA) 
axis(side = 4, at = seq(0,1300, by=150), labels= FALSE, tck =-0.03) %/%
  text(y=seq(0,1300, by=300),labels = ylabs3, par("usr")[1], pos = 4, cex = 1,offset = 17.6, xpd=NA)
mtext("h", side=2, las = 2, line=-1, at = c(1200), font = 2)
#i
plot(x=x40, y=chin$mean[chin$Habitat%in%c("Sand flat")], bty = "l", ylab = "", xlab = "", xaxt = "n", yaxt = "n", xlim = c(0.5,8.5), ylim = c(0,400), pch=c(15),cex = 2, bg="black",xpd=NA) %/% arrows(x40, y40, x40, y40.0, length=0.05, angle=90, code=3)
axis(side = 1, at = c(1,2,3,4,5,6,7,8), tck =-0.02, labels = NA) 
mtext(side = 1, "   March       May        July        Sept.          ", line=0.6, cex = 0.8)
axis(side = 2, at = seq(0,400, by=40), labels= FALSE, tck =-0.03)
text(y = seq(0,400, by=80),labels = ylabs4, par("usr")[1], pos = 2, cex = 1, xpd=NA)
mtext("i", side=2, las = 2, line=-1, at = c(380), font = 2)
#j
plot(x=x50, y=chum$mean[chum$Habitat%in%c("Sand flat")], bty = "l", ylab = "", xlab = "", yaxt = "n", xaxt = "n", xlim = c(0.5,8.5), ylim = c(0,400), cex = 2, pch=c(15),xpd=NA) %/% arrows(x50, y50, x50, y50.0, length=0.05, angle=90, code=3)
axis(side = 1, at = c(1,2,3,4,5,6,7,8), tck =-0.02, labels = NA) 
mtext(side = 1, "   March       May        July        Sept.          ", line=0.6, cex = 0.8)
axis(side = 2, at = seq(0,400, by=40), labels= FALSE, tck =-0.03)
mtext("j", side=2, las = 2, line=-1, at = c(380), font = 2)
#k
plot(x=x60, y=migratory$mean[migratory$Habitat%in%c("Sand flat")], bty = "l", ylab = "", xlab = "", yaxt = "n", xaxt = "n", xlim = c(0.5,8.5), ylim = c(0,400), cex = 2,pch=c(15), bg="black",xpd=NA) %/% arrows(x60, y60, x60, y60.0, length=0.05, angle=90, code=3)
axis(side = 1, at = c(1,2,3,4,5,6,7,8), tck =-0.02, labels = NA) 
mtext(side = 1, "   March       May        July        Sept.          ", line=0.6, cex = 0.8)
axis(side = 2, at = seq(0,400, by=40), labels= FALSE, tck =-0.03)
mtext("k", side=2, las = 2, line=-1, at = c(380), font = 2)
#l
plot(x=x30, y=resident$mean[resident$Habitat%in%c("Sand flat")], bty = "l", ylab = "", xlab = "", yaxt = "n", xaxt = "n", xlim = c(0.5,8.5), ylim = c(0,400), pch=c(15),cex = 2, bg="black",xpd=NA) %/% arrows(x70, y70, x70, y70.0, length=0.05, angle=90, code=3)
axis(side = 1, at = c(1,2,3,4,5,6,7,8), tck =-0.02, labels = NA) 
mtext(side = 1, "   March       May        July        Sept.          ", line=0.6, cex = 0.8)
axis(side = 2, at = seq(0,400, by=40), labels= FALSE, tck =-0.03) %/%
  text(y=seq(0,400, by=40),labels= FALSE, tck =-0.03)
mtext("l", side=2, las = 2, line=-1, at = c(380), font = 2)

dev.off()


