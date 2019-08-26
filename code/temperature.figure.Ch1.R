
####Aggregate to look at means over time by habitat type
library(tidyverse)
all<- read.csv("/Users/Lia/Documents/Git/Fraser-salmon/all.data/all_clean.csv")
#temp<- all %>% 
  #group_by(Year, round, Habitat, month) %>% 
  #summarise(Temp = mean(Temp.surf), Temp.sd = sd(Temp.surf)) 
## above is old, below does not include rounds so means are by month
temp<- all %>% 
  group_by(Year, Habitat, month) %>% 
  summarise(Temp = mean(Temp.surf), Temp.sd = sd(Temp.surf)) 

#convert months to numeric to compare monthly means instead of round means
temp$month<- factor(temp$month, levels = c("March", "April", "May", "June", "July", "August", "September", "October"), ordered = TRUE)
month.numeric<- as.numeric(temp$month) #set month to numeric value based on levels, create object for ordering
temp$month.numeric<-month.numeric #add to df
temp<-temp[order(month.numeric),] #order df by month

#subset data for lines
t16<- temp[c(temp$Year == 0),]
t17<- temp[c(temp$Year == 1),]  

#set colour pallette for years
COL <- adjustcolor(c("black", "white")[as.factor(temp$Year)], alpha.f = 0.8)
ylabs<- as.vector(seq(5, 20, by=5))

###########  Temperature.png (updated to Temperature_month.png)
set.seed(1)
x0<-jitter(temp$month.numeric, amount = 0.2)
y0<-temp$Temp-temp$Temp.sd
y1<-temp$Temp+temp$Temp.sd

png("/Users/Lia/Documents/Git/Fraser-salmon/Ch1.Seasonal_diversity/final.figures/Temperature_month.png", width = 169, height = 180, units = "mm", res = 400, type = "cairo", pointsize = 16)
par(mai=c(.6,1,0.2,0.2))
plot(x=temp$month.numeric, y=temp$Temp, type = 'n', xlim=c(0.9, 8), las=1, ylim=c(4.5, 21), ylab = "", main="", cex.axis = 1, lwd=1, bty = "l", xlab = " ", xaxt='n', yaxt='n') %/%
  lines(Temp~month.numeric, data=t16[c(t16$Habitat == "Marsh"),], col=alpha("black", 0.6), lty=4) %/% 
  lines(Temp~month.numeric, data=t16[c(t16$Habitat == "Eelgrass"),], col=alpha("black", 0.6), lty=5) %/% 
  lines(Temp~month.numeric, data=t16[c(t16$Habitat == "Sand flat"),], col=alpha("black", 0.6)) %/% lines(Temp~month.numeric, data=t17[c(t17$Habitat == "Marsh"),], col=alpha("gray", 0.6), lty=4) %/% 
  lines(Temp~month.numeric, data=t17[c(t17$Habitat == "Eelgrass"),], col=alpha("gray", 0.6), lty=5) %/% 
  lines(Temp~month.numeric, data=t17[c(t17$Habitat == "Sand flat"),], col=alpha("gray", 0.6)) %/%
  arrows(x0, y0, x1=x0, y1, length=0.05, angle= 90, code=3, col=alpha(c("gray"),0.5)) %/%
  points(x=x0, y=temp$Temp, type='p',cex = 1.6,bg = COL, pch=c(21,24,22)[as.numeric(temp$Habitat)])%/%
  axis(side = 1, at = c(1, 2,3,4, 5,6, 7,8), labels= c("March","", "May","", "July", "August","September",""), tck =-0.02) %/%
  mtext(expression(paste("     Temperature", ~degree, "C")), side = 2,cex = 1.3, line = 2, outer= FALSE ) %/%
  axis(side = 2, at = seq(3.5,21.5, by=.5), labels= FALSE, tck =-0.01) %/%
  axis(side = 2, at = seq(4,21, by=1), labels= FALSE, tck =-0.03) %/%
  text(y = seq(5,22, by=5), labels = ylabs, par("usr")[1], pos = 2, cex = 1, offset = 1.2, xpd=TRUE); legend(0.7,22, pt.bg = c(COL[1:3]), c("Marsh","Eelgrass","Sandflat"), cex = 1, lwd = 1.5, pt.lwd=1, pt.cex=1.3, bty='n', pch=c(24,21,22), lty = c(4,5,1), col=alpha("black", 0.6)); legend(3,22, c("2016", "2017"), pt.bg =c(COL[1], COL[4]), pch=22, cex = 1, pt.cex = 1.3, x.intersp = 2, bty='n')
dev.off()

###########  Temperature_habitat.png
temp.hab<- all %>% 
  group_by(Year, Habitat) %>% 
  summarise(Temp = mean(Temp.surf), Temp.sd = sd(Temp.surf)) 

#convert habitat to numeric
temp.hab$Habitat<- factor(temp.hab$Habitat, levels = c("Marsh", "Eelgrass", "Sand flat"), ordered = TRUE)
Habitat.numeric<- as.numeric(temp.hab$Habitat) #set Habitat to numeric value based on levels, create object for ordering
temp.hab$Habitat.numeric<-Habitat.numeric #add to df
temp.hab<-temp.hab[order(Habitat.numeric),] #order df by Habitat

#subset data by year
t16.hab<- temp.hab[c(temp.hab$Year == 0),]
t17.hab<- temp.hab[c(temp.hab$Year == 1),]  

#plot
x1=t17.hab$Habitat.numeric + 0.2
ylabs2<- as.vector(seq(9, 19, by=2))

png("/Users/Lia/Documents/Git/Fraser-salmon/Ch1.Seasonal_diversity/final.figures/temperature_habitat.png", width = 169, height = 180, units = "mm", res = 400, type = "cairo", pointsize = 16)
par(mai=c(.6,1,0.2,0.2))
plot(Temp~Habitat.numeric, data = t16.hab, type = "n", bty = "l", xlab = "", xaxt = "n", ylab = "", yaxt = "n", xlim = c(0.5,3.5), ylim = c(9,19)) %/%
  arrows(t16.hab$Habitat.numeric, t16.hab$Temp + t16.hab$Temp.sd, t16.hab$Habitat.numeric, t16.hab$Temp - t16.hab$Temp.sd, length=0.05, angle= 90, code=3, col=alpha(c("gray"),0.5)) %/%
  points(x = t16.hab$Habitat.numeric, y = t16.hab$Temp, cex = 1.6,bg = COL, pch=c(21,24,22)[as.numeric(t16.hab$Habitat)]) %/%
  arrows(x1, t17.hab$Temp + t17.hab$Temp.sd, x1, t17.hab$Temp - t17.hab$Temp.sd, length=0.05, angle= 90, code=3, col=alpha(c("gray"),0.5)) %/%
  points(x = x1, y = t17.hab$Temp, cex = 1.6,bg = COL[4], pch=c(21,24,22)[as.numeric(t17.hab$Habitat)]) %/%
  axis(side = 1, at = c(1,2,3), labels= c("Marsh", "Eelgrass", "Sand flat"), tck =-0.02) %/%
  mtext(expression(paste("     Temperature", ~degree, "C")), side = 2,cex = 1.3, line = 2, outer= FALSE ) %/%
  axis(side = 2, at = seq(8.5,19.5, by=.5), labels= FALSE, tck =-0.01) %/%
  axis(side = 2, at = seq(9,19, by=1), labels= FALSE, tck =-0.03) %/%
  text(y = seq(9,19, by=2), labels = ylabs2, par("usr")[1], pos = 2, cex = 1, offset = 1.2, xpd=TRUE); legend(0.5,19.5, c("2016", "2017"), pt.bg =c(COL[1], COL[4]), pch=22, cex = 1, pt.cex = 1.3, x.intersp = 2, bty='n')
dev.off()


##Arrange together in 2 panel plot
library(grDevices) #need for making letters bold
#note in text function that vfont will be ignored if labels is set to an expression
#unfortunately none of the Hershey bold font options look like base graphics (vfont) BUT discovered that font=2 is perfect.

png("/Users/Lia/Documents/Git/Fraser-salmon/Ch1.Seasonal_diversity/final.figures/temp.panel.png", width = 338, height = 180, units = "mm", res = 600, type = "cairo", pointsize = 16)
  
par(mfrow = c(1,2), mai=c(.6,1,0.2,0.2))
plot(x=temp$month.numeric, y=temp$Temp, type = 'n', xlim=c(0.9, 8), las=1, ylim=c(4.5, 21), ylab = "", main="", cex.axis = 1, lwd=1, bty = "l", xlab = " ", xaxt='n', yaxt='n') %/%
  lines(Temp~month.numeric, data=t16[c(t16$Habitat == "Marsh"),], col=alpha("black", 0.6), lty=4) %/% 
  lines(Temp~month.numeric, data=t16[c(t16$Habitat == "Eelgrass"),], col=alpha("black", 0.6), lty=5) %/% 
  lines(Temp~month.numeric, data=t16[c(t16$Habitat == "Sand flat"),], col=alpha("black", 0.6)) %/% lines(Temp~month.numeric, data=t17[c(t17$Habitat == "Marsh"),], col=alpha("gray", 0.6), lty=4) %/% 
  lines(Temp~month.numeric, data=t17[c(t17$Habitat == "Eelgrass"),], col=alpha("gray", 0.6), lty=5) %/% 
  lines(Temp~month.numeric, data=t17[c(t17$Habitat == "Sand flat"),], col=alpha("gray", 0.6)) %/%
  arrows(x0, y0, x1=x0, y1, length=0.05, angle= 90, code=3, col=alpha(c("gray"),0.5)) %/%
  points(x=x0, y=temp$Temp, type='p',cex = 1.6,bg = COL, pch=c(21,24,22)[as.numeric(temp$Habitat)])%/%
  axis(side = 1, at = c(1, 2,3,4, 5,6, 7,8), labels= c("March","", "May","", "July", "August","September",""), tck =-0.02) %/%
  mtext(expression(paste("     Temperature", ~degree, "C")), side = 2,cex = 1.3, line = 2, outer= FALSE ) %/%
  text(8,21, "a)",cex = 1.1, font = 2) %/%
  axis(side = 2, at = seq(3.5,21.5, by=.5), labels= FALSE, tck =-0.01) %/%
  axis(side = 2, at = seq(4,21, by=1), labels= FALSE, tck =-0.03) %/%
  text(y = seq(5,22, by=5), labels = ylabs, par("usr")[1], pos = 2, cex = 1, offset = 1.2, xpd=TRUE); legend(0.7,22, pt.bg = c(COL[1:3]), c("Marsh","Eelgrass","Sandflat"), cex = 1, lwd = 1.5, pt.lwd=1, pt.cex=1.3, bty='n', pch=c(24,21,22), lty = c(4,5,1), col=alpha("black", 0.6)); legend(3,22, c("2016", "2017"), pt.bg =c(COL[1], COL[4]), pch=22, cex = 1, pt.cex = 1.3, x.intersp = 2, bty='n');plot(Temp~Habitat.numeric, data = t16.hab, type = "n", bty = "l", xlab = "", xaxt = "n", ylab = "", yaxt = "n", xlim = c(0.5,3.5), ylim = c(9,19)) %/%
  arrows(t16.hab$Habitat.numeric, t16.hab$Temp + t16.hab$Temp.sd, t16.hab$Habitat.numeric, t16.hab$Temp - t16.hab$Temp.sd, length=0.05, angle= 90, code=3, col=alpha(c("gray"),0.5)) %/%
  points(x = t16.hab$Habitat.numeric, y = t16.hab$Temp, cex = 1.6,bg = COL, pch=c(21,24,22)[as.numeric(t16.hab$Habitat)]) %/%
  arrows(x1, t17.hab$Temp + t17.hab$Temp.sd, x1, t17.hab$Temp - t17.hab$Temp.sd, length=0.05, angle= 90, code=3, col=alpha(c("gray"),0.5)) %/%
  points(x = x1, y = t17.hab$Temp, cex = 1.6,bg = COL[4], pch=c(21,24,22)[as.numeric(t17.hab$Habitat)]) %/%
  text(3.3,19, "b)",cex = 1.1, font=2) %/%
  axis(side = 1, at = c(1,2,3), labels= c("Marsh", "Eelgrass", "Sand flat"), tck =-0.02) %/%
  axis(side = 2, at = seq(8.5,19.8, by=.5), labels= FALSE, tck =-0.01) %/%
  axis(side = 2, at = seq(9,19, by=1), labels= FALSE, tck =-0.03) %/%
  text(y = seq(9,20, by=2), labels = ylabs2, par("usr")[1], pos = 2, cex = 1, offset = 1.2, xpd=TRUE); legend(0.5,19.6, c("2016", "2017"), pt.bg =c(COL[1], COL[4]), pch=22, cex = 1, pt.cex = 1.3, x.intersp = 2, bty='n')

dev.off()

