#Fig_5_MEPS_revised
#Abiotic_parameter_panel_plot
library(here)
here()

#fig.height=8, fig.width=3,, dpi=300, dev='pdf'

##load abiotic data
all<- read.csv("data/all_clean.csv")

##aggregate to look at annual means by habitat type (panels b:e)
library(tidyverse)

data<- all %>% group_by(Year, Habitat) %>% summarise(do = mean(DOmg.surf), do.min = min(DOmg.surf),do.max = max(DOmg.surf), ph = mean(pH.surf), ph.min = min(pH.surf), ph.max = max(pH.surf),sal = mean(Sal.surf), sal.min = min(Sal.surf),sal.max = max(Sal.surf), temp = mean(Temp.surf), temp.min = min(Temp.surf), temp.max = max(Temp.surf)) 
#add numeric X values for habitat  
data$hab<- as.numeric(c(1.7,1.4,2,1.8,1.5,2.1))

#set colour for years, shapes for habitat type
col <- adjustcolor(c("black", "white")[as.factor(data$Year)])
shapes<-c(21,24,22)

#define y axis labels  
temp.ylabs<- as.vector(seq(3,21, by = 3))
sal.ylabs<- as.vector(seq(0, 35, by=5))
do.ylabs<- as.vector(seq(4,16,by=2))
ph.ylabs<- as.vector(seq(6,10.5,by=1))

##aggregate temperature data by monthly means, habitat, and Year (panel a)
temp2<- all %>% 
  group_by(Year, Habitat, month) %>% 
  summarise(temp2 = mean(Temp.surf), temp2.min = min(Temp.surf), temp2.max = max(Temp.surf)) 

#convert months to numeric to compare monthly means
temp2$month<- factor(temp2$month, levels = c("March", "April", "May", "June", "July", "August", "September", "October"), ordered = TRUE)
month.numeric<- as.numeric(temp2$month) #set month to numeric value based on levels, create object for ordering
temp2$month.numeric<-month.numeric #add to df
temp2<-temp2[order(month.numeric),] #order df by month

#subset data for lines
t16<- temp2[c(temp2$Year == 0),]
t17<- temp2[c(temp2$Year == 1),]  

#set colour palette for years
col2 <- adjustcolor(c("black", "white")[as.factor(temp2$Year)], alpha.f = 0.8)
ylabs<- as.vector(seq(3, 21, by=3))

#jitter x axis to prevent overlapping points
set.seed(1)
x0<-jitter(temp2$month.numeric, amount = 0.2)

#plot
pdf("final.figures/Fig5.pdf", width = 15.35, height = 7.48, pointsize = 16)

par(mfrow = c(2,3), mar = c(1,1.5,1,1), oma = c(2, 1.5, 1, 0.5), mgp = c(0,0.5,0), cex = 1)
# a) temp by month
plot(x=temp2$month.numeric, y=temp2$temp2, type = 'n', xlim=c(0.9, 8), las=1, ylim=c(3, 21), ylab = "", main="", lwd=1, bty = "l", xlab = " ", xaxt='n', yaxt='n') %/%
  lines(temp2~month.numeric, data=t16[c(t16$Habitat == "Marsh"),], col=alpha("black", 0.7), lty=4) %/%  
  lines(temp2~month.numeric, data=t16[c(t16$Habitat == "Eelgrass"),], col=alpha("black", 0.7), lty=5) %/% 
  lines(temp2~month.numeric, data=t16[c(t16$Habitat == "Sand flat"),], col=alpha("black", 0.7)) %/% lines(temp2~month.numeric, data=t17[c(t17$Habitat == "Marsh"),], col=alpha("gray60", 0.9), lty=4) %/% 
  lines(temp2~month.numeric, data=t17[c(t17$Habitat == "Eelgrass"),], col=alpha("gray60", 0.9), lty=5) %/% 
  lines(temp2~month.numeric, data=t17[c(t17$Habitat == "Sand flat"),], col=alpha("gray60", 0.9)) %/%
  arrows(x0, temp2$temp2.min, x1=x0, temp2$temp2.max, length=0.05, angle= 90, code=3, col="gray") %/%
  points(x=x0, y=temp2$temp2, type='p', bg = col2, pch=c(21,24,22)[as.numeric(temp2$Habitat)])%/%
  axis(side = 1, at = c(1,2,3,4,5,6,7,8), labels= c("Mar","", "May","", "Jul", "","Sept",""), tck =-0.02) %/%
  mtext(expression(paste("Temperature", ~degree, "C")), side = 2, line = 1.5, outer = FALSE) %/%
  text(1,21, "a", font = 2) %/%
  axis(side = 2, at = seq(3,21, by=1), labels= FALSE, tck =-0.01) %/%
  axis(side = 2, at = seq(3,21, by=3), labels= FALSE, tck =-0.015) %/%
  text(y = seq(3,21, by=3), labels = ylabs, par("usr")[1], pos = 2, xpd=NA) 

# b) temp by year
attach(data)
plot(temp~hab, data = data, type = 'n', las=1,  ylab = "", ylim = c(3,21),yaxt='n', xlim=c(1.3,2.2),
       main="", lwd=1, bty = "l", xaxt='n', xlab="")
  arrows(hab, temp.min, hab, temp.max, length=0.05, angle= 90, code=3, col=c("black", "gray60")[as.factor(data$Year)])
  points(hab, temp, type='p', bg = col, pch=c(21,24,22)[as.numeric(data$Habitat)]) 
  text(1.3,21, "b", font = 2) 
  axis(side = 1, at = c(1.45, 1.75, 2.05), labels= c("Marsh", "Eelgrass", "Sand flat"), tck =-0.02) 
  axis(side = 2, at = seq(3,21, by=1), labels= FALSE, tck =-0.01)
  axis(side = 2, at = seq(3,21, by=3), labels= FALSE, tck =-0.015) 
  text(y = seq(3,21, by=3), labels = temp.ylabs, par("usr")[1], pos = 2, xpd=NA)

# legends
legend(2.516,22.1, c("2016", "2017"), pt.bg =c(col[1], col[4]), pch=22, cex = 1.2, pt.cex = 1.4, x.intersp = 2, bty='n', xpd=NA)
legend(2.5,17.8, pt.bg = c(col2[1:3]), c("Marsh","Eelgrass","Sandflat"), cex = 1.2, lwd = 1.5, pt.lwd=1, pt.cex=1.4, bty='n', pch=c(24,21,22), lty = c(4,5,1), col=alpha("black", 0.8), xpd=NA)
# space for legend
plot(0,type='n',axes=FALSE,ann=FALSE)

#c) salinity by year
plot(sal~hab, data = data, type = 'n', las=1,  ylab = "", ylim = c(0,35),yaxt='n', xlim=c(1.3,2.2),
     main="", lwd=1, bty = "l", xaxt='n', xlab="")
arrows(hab, sal.min, hab, sal.max, length=0.05, angle= 90, code=3, col=c("black", "gray60")[as.factor(data$Year)])
points(hab, sal, type='p', bg = col, pch=c(21,24,22)[as.numeric(data$Habitat)]) 
mtext(expression(paste("Salinity (ppt)")), side = 2,line = 1.5 , outer= FALSE ) 
text(1.3,35, "c", font = 2) 
axis(side = 1, at = c(1.45, 1.75, 2.05), labels= c("Marsh", "Eelgrass", "Sand flat"), tck =-0.02) 
axis(side = 2, at = seq(0,35, by=1), labels= FALSE, tck =-0.01)
axis(side = 2, at = seq(0,35, by=5), labels= FALSE, tck =-0.015) 
text(y = seq(0,35, by=5), labels = sal.ylabs, par("usr")[1], pos = 2, xpd=NA)

#d) DO by year
plot(do~hab, data = data, type = 'n', las=1,  ylab = "",ylim=c(4,16),yaxt='n', xlim=c(1.3,2.2), main="", lwd=1, bty = "l", xaxt='n', xlab="")
arrows(hab, do.min, hab, do.max, length=0.05, angle= 90, code=3, col=c("black", "gray60")[as.factor(data$Year)]) 
points(hab, do, type='p', bg = col, pch=c(21,24,22)[as.numeric(data$Habitat)]) 
mtext(expression(paste("Dissolved Oxygen (mg/L)")), side = 2, line = 1.5 , outer= FALSE ) 
text(1.3,16, "d", font = 2) 
axis(side = 1, at = c(1.45, 1.75, 2.05), labels= c("Marsh", "Eelgrass", "Sand flat"), tck =-0.02) 
axis(side = 2, at = seq(4,16, by=1), labels= FALSE, tck =-0.01) 
axis(side = 2, at = seq(4,16, by=2), labels= FALSE, tck =-0.015) 
text(y = seq(4,16, by=2), labels = do.ylabs, par("usr")[1], pos = 2, xpd=NA)

#e) pH by year
plot(ph~hab, data = data, type = 'n', las=1,  ylab = "",ylim=c(6,10.5),yaxt='n', xlim=c(1.3,2.2),main="", lwd=1, bty = "l", xaxt='n', xlab="")
arrows(hab, ph.min, hab, ph.max, length=0.05, angle= 90, code=3, col=c("black", "gray60")[as.factor(data$Year)]) 
points(hab, ph, type='p', bg = col, pch=c(21,24,22)[as.numeric(data$Habitat)]) 
axis(side = 1, at = c(1.45, 1.75, 2.05), labels= c("Marsh", "Eelgrass", "Sand flat"), tck =-0.02) 
mtext(expression(paste("pH (logarithmic scale)")), side = 2, line = 1.5 , outer= FALSE ) 
text(1.3,10.5, "e", font = 2) 
axis(side = 2, at = seq(6,10.5, by=.5), labels= FALSE, tck =-0.01) 
axis(side = 2, at = seq(6,10.5, by=1), labels= FALSE, tck =-0.015) 
text(y = seq(6,10.5, by=1), labels = ph.ylabs, par("usr")[1], pos = 2, xpd=NA) 
dev.off()

