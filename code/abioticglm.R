### Abiotic GLMs for Chalifour et al 2019 MEPS ms

library(here)

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

#test for normality 
shapiro.test(sample(all$Temp.surf, 100))
shapiro.test(sample(all$Sal.surf, 100))
shapiro.test(sample(all$pH.surf, 100)) 
shapiro.test(sample(all$DOmg.surf, 100)) 

#models - first tested glm then glmm with site as a random factor. Reported glmm results

###temp model - used glmm results
summary(t<-glm(Temp.surf~Year + Habitat, data=all)) #explore linear temperature model
summary(glm(Temp.surf~Year + Habitat + month, data=all))

temp<-lmer(Temp.surf~Year + Habitat + month + (1|Site), data=all, verbose = 1)
summary(temp)

AIC(temp)
plot(temp)

#salinity model
summary(glm(Sal.surf~Year + Habitat, data=all))
sal<-lmer(Sal.surf~Year + Habitat + month + (1|Site), data=all)
summary(sal)

#pH model
summary(glm(pH.surf~Year + Habitat, data=all))
pH<- lmer(pH.surf~Year + Habitat + month + (1|Site), data=all)
summary(pH)

#DO model
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
