####Code for corrplots Ch 1 MEPS ms ####
library(here)
here()

b<- read.csv("data/beach.catch.csv")

#bchinhab

hab<- read.csv("data/site.char.marsh.csv")
hab2<- hab[, c(1,6,8:10,13,19,20)]

library(corrplot)

##all marsh: 
b$stnwidth<- hab2[match(b$Site, hab$Site),2] #add stnwidth to b by matching with Site #. Then repeat with all other habitat variables  
b$vegelev<- hab2[match(b$Site, hab$Site),3]
b$shtdensity<- hab2[match(b$Site, hab$Site),4]
b$shtdenhigh<- hab2[match(b$Site, hab$Site),5]
b$tcelev<- hab2[match(b$Site, hab$Site),6]
b$angbank<- hab2[match(b$Site, hab$Site),7]
b$meanturb<- hab2[match(b$Site, hab$Site),8]
library(robustHD)  
#standardize variables to be centered on the mean (mean becomes 0) using the standardize function from robustHD  
b$s.temp<-standardize(b$Temp.surf, centerFun = mean, scaleFun = sd)
b$s.sal<-standardize(b$Sal.surf, centerFun = mean, scaleFun = sd)
b$s.do<-standardize(b$DOmg.surf, centerFun = mean, scaleFun = sd)
b$s.pH<-standardize(b$pH.surf, centerFun = mean, scaleFun = sd)
b$s.J.date<-standardize(b$J.date, centerFun = mean, scaleFun = sd)
###Create variable j2 which is Julian day squared in order to represent J date as quadratic relationship instead of linear  
b$j2<- b$s.J.date^2
##standardize habitat variables using 'standardize' function from robustHD package  
b$stnwidth<- standardize(b$stnwidth, centerFun = mean, scaleFun = sd); b$stnwidth<-as.numeric(b$stnwidth)
b$vegelev<- standardize(b$vegelev, centerFun = mean, scaleFun = sd); b$vegelev<-as.numeric(b$vegelev)
b$shtdensity<- standardize(b$shtdensity, centerFun = mean, scaleFun = sd); b$shtdensity<-as.numeric(b$shtdensity)
b$shtdenhigh<- standardize(b$shtdenhigh, centerFun = mean, scaleFun = sd); b$shtdenhigh<-as.numeric(b$shtdenhigh)
b$tcelev<- standardize(b$tcelev, centerFun = mean, scaleFun = sd); b$tcelev<-as.numeric(b$tcelev)
b$angbank<- standardize(b$angbank, centerFun = mean, scaleFun = sd); b$angbank<-as.numeric(b$angbank)
b$meanturb<- standardize(b$meanturb, centerFun = mean, scaleFun = sd); b$meanturb<-as.numeric(b$meanturb)
##Pearson Corr with all vars  
Year<- b$Year
Julian.day<- b$J.date
J.day2<- b$s.J.date
Temp.<- b$s.temp
D.O.<- b$s.do
pH<- b$s.pH
Sal.<- b$s.sal
Veg.elev.<- b$vegelev
Mean.turb.<- b$meanturb
Width<- b$stnwidth
Shoot.den.<- b$shtdensity
Sht.den.high<- b$shtdenhigh
Chann.elev.<- b$tcelev
Ang.bank<- b$angbank
habcovar_all<- cbind.data.frame(Year,Temp., D.O.,Julian.day, J.day2,pH,Width,Veg.elev., Sal., Mean.turb., Shoot.den., Sht.den.high, Chann.elev., Ang.bank)

##  different color series
col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue","#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue"))
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                           "cyan", "#007FFF", "blue", "#00007F"))
wb <- c("white","grey", "black")


X<- cor(habcovar_all)
library(grDevices)
png("final.figures/FigA1a.png", width=10, height=8, units = "in", res=1000, type="cairo", pointsize = 16)
corrplot(X, tl.col = "black", col=heat.colors(25))
dev.off()
###########################

#Purse models
purse<- read.csv("data/purse.catch.csv")
purse$s.temp<-standardize(purse$Temp.surf, centerFun = mean, scaleFun = sd)
summary(purse$s.temp) #can see that mean is now 0, and SD is on the original scale of x (temp) -- i.e. predictor centering 
purse$s.sal<-standardize(purse$Sal.surf, centerFun = mean, scaleFun = sd)
purse$s.do<-standardize(purse$DOmg.surf, centerFun = mean, scaleFun = sd)
purse$s.pH<-standardize(purse$pH.surf, centerFun = mean, scaleFun = sd)  
purse$s.J.date<-standardize(purse$J.date, centerFun = mean, scaleFun = sd)  
###Create variable j2 which is Julian day squared in order to represent J date as quadratic relationship instead of linear  
purse$j2<- purse$s.J.date^2

p.hab<- read.csv("data/site.char.eelgrass.csv")
p.hab2<- p.hab[, c(3:6, 8)]
purse$leaf_area_index<- p.hab2[match(purse$Site, p.hab2$Site),4]
purse$meanturb<- p.hab2[match(purse$Site, p.hab2$Site),5]

##standardize
purse$leaf_area_index<- standardize(purse$leaf_area_index, centerFun = mean, scaleFun = sd); purse$leaf_area_index<-as.numeric(purse$leaf_area_index)
purse$meanturb<- standardize(purse$meanturb, centerFun = mean, scaleFun = sd); purse$meanturb<-as.numeric(purse$meanturb)

##Pearson Corr with all vars  
Year<- purse$Year
D.O.<- purse$s.do
Temp.<- purse$s.temp
Habitat<- as.numeric(purse$Habitat) ##convert to numeric factor levels for plots
Julian.day<- purse$J.date
J.day2<- purse$s.J.date
pH<- purse$s.pH
Mean.turb.<- purse$meanturb
Sal.<- purse$s.sal
L.A.I.<- purse$leaf_area_index


habcovar<- cbind.data.frame(Year, D.O., Temp., Habitat, Julian.day, J.day2, pH, Mean.turb., Sal., L.A.I.)
P<- cor(habcovar)
png("final.figures/FigA1b.png", width=10, height=8, units = "in", res=1000, type="cairo", pointsize = 16)
corrplot(P, tl.col = "black", col=heat.colors(25))
dev.off()

###################
pdf("final.figures/FigA1.pdf", width=9, height=15, pointsize = 16, colormodel = "cmyk")
par(mfrow = c(2,1), mar=c(1,1,1,1), xpd = NA, oma = c(0,0,0,0))
corrplot(X, tl.col = "black", col=heat.colors(25))
mtext("a", side=2, font = 2, las=2, line = -1, at = c(15))
corrplot(P, tl.col = "black", col=heat.colors(25))
mtext("b", side=2, font = 2, las=2, line = -1, at = c(11))
dev.off()
