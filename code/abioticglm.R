### Abiotic GLMs for Chalifour et al 2019 MEPS ms

library(here)
here()

##load abiotic data
all<- read.csv("data/all_clean.csv")

#change order of month factor
all$month<- factor(all$month, levels = c("March", "April", "May", "June", "July", "August", "September", "October"))

#change order of habitat factor
all$Habitat<- factor(all$Habitat, levels = c("Marsh", "Eelgrass", "Sand flat"))

library(MASS)
library(lme4)

#explore data distributions
plot(Temp.surf~J.date, data = all)
plot(all$Sal.surf)
plot(pH.surf~J.date, data = all)
plot(DOmg.surf~J.date, data = all)

library(robustHD)
#Use standardize function and then convert to numeric (from atomic vector)
all$s.temp<-as.numeric(standardize(all$Temp.surf, centerFun = mean, scaleFun = sd))
all$s.sal<-as.numeric(standardize(all$Sal.surf, centerFun = mean, scaleFun = sd))
all$s.do<-as.numeric(standardize(all$DOmg.surf, centerFun = mean, scaleFun = sd))
all$s.pH<-as.numeric(standardize(all$pH.surf, centerFun = mean, scaleFun = sd))



#test for normality 
shapiro.test(sample(all$Temp.surf, 100))
shapiro.test(sample(all$Sal.surf, 100))
shapiro.test(sample(all$pH.surf, 100)) #still sig at low sample
shapiro.test(sample(all$DOmg.surf, 100)) #still sig at low sample


summary(t<-glm(Temp.surf~Year + Habitat, data=all))
summary(glm(Temp.surf~Year + Habitat + month, data=all))
###TEMP MODEL
temp<-lmer(Temp.surf~Year + Habitat + month + (1|Site), data=all, verbose = 1)
summary(temp)

AIC(temp)
plot(temp)


summary(glm(s.temp~Year + Habitat, data=all))
## prefer unstandardized model b/c can understand changes to temp relative to diff factors 

summary(glm(Sal.surf~Year + Habitat, data=all))
sal<-lmer(Sal.surf~Year + Habitat + month + (1|Site), data=all)
summary(sal)

summary(glm(pH.surf~Year + Habitat, data=all))
pH<- lmer(pH.surf~Year + Habitat + month + (1|Site), data=all)
summary(pH)

summary(glm(DOmg.surf~Year + Habitat, data=all))
do<- lmer(DOmg.surf~Year + Habitat + month + (1|Site), data=all)
summary(do) 

##save data outputs in tables

library(memisc)
mtable1<-mtable("Temperature"=temp)
write.mtable(mtable1,file="data/temp.txt")
library(MuMIn)
r.squaredGLMM(temp)

mtable2<-mtable("Salinity"=sal)
write.mtable(mtable2,file="data/sal.txt")
r.squaredGLMM(sal)

mtable3<-mtable("Dissolved oxygen"=do)
write.mtable(mtable3,file="/data/do.txt")
r.squaredGLMM(do)

mtable4<-mtable("pH"=pH)
write.mtable(mtable4,file="/data/pH.txt")
r.squaredGLMM(pH)
