##Some Tips
# put the factors in order of least amount of levels to most amount of levels

##Set Directory
setwd("/Users/mpcl/Desktop/chelsea/rr")

##Packages
#install.packages("lme4")
#install.packages("ggplot2")
#install.packages("effects")
#install.packages("LMERConvenienceFunctions")
#install.packages("multcomp")
#install.packages("plyr")
#install.packages("pbkrtest")
#install.packages("lsmeans")
#install.packages("car")

library("lme4")
library("ggplot2")
library("effects")
library("LMERConvenienceFunctions")
library("multcomp") #glht
library("plyr")
library("pbkrtest")
library("lsmeans")
library("car")

###Create Data Structure
smallset <- read.table("smalldata4r.csv", header=TRUE, sep=",")

###Set Variables as Factors
##SmallSet
smallset$mus<- factor(smallset$mus)
smallset$reg<- factor(smallset$reg)
smallset$regn<- factor(smallset$regn)
smallset$fam<- factor(smallset$fam)
smallset$par<- factor(smallset$par)
smallset$timbre<- factor(smallset$timbre)

##SUM CODING
#ALL
smallset$reg.s<- smallset$reg
contrasts(smallset$reg.s) = contr.sum(7)
smallset$fam.s<- smallset$fam
contrasts(smallset$fam.s) = contr.sum(4)
smallset$mus.s<- smallset$mus
contrasts(smallset$mus.s) = contr.sum(2)

#### LMM #### LMM #### LMM #### LMM #### LMM #### LMM #### LMM ####

##Sum Coded Reg=Reg=7
lmmval.s7<-lmer(val~1+mus.s*fam.s*reg.s+(1+fam.s+reg.s|par)+(1+mus.s|timbre), data=smallset, control=lmerControl(optCtrl=list(maxfun=1000000))) #THIS WORKS
lmmten.s7<-lmer(ten~1+mus.s*fam.s*reg.s+(1+fam.s+reg.s|par)+(1+mus.s|timbre), data=smallset, control=lmerControl(optCtrl=list(maxfun=1000000))) #THIS WORKS
lmmenergy.s7<-lmer(energy~1+mus.s*fam.s*reg.s+(1+fam.s+reg.s|par)+(1+mus.s|timbre), data=smallset, control=lmerControl(optCtrl=list(maxfun=1000000))) #THIS WORKS
lmmpref.s7<-lmer(pref~1+mus.s*fam.s*reg.s+(1+fam.s+reg.s|par)+(1+mus.s|timbre), data=smallset, control=lmerControl(optCtrl=list(maxfun=1000000))) #THIS WORKS
lmmfamiliar.s7<-lmer(familiar~1+mus.s*fam.s*reg.s+(1+fam.s+reg.s|par)+(1+mus.s|timbre), data=smallset, control=lmerControl(optCtrl=list(maxfun=1000000))) #THIS WORKS

###ANOVAS

anovaval<-Anova(lmmval.s7, type="III", test.statistic="F")
anovaten<-Anova(lmmten.s7, type="III", test.statistic="F")
anovaenergy<-Anova(lmmenergy.s7, type="III", test.statistic="F")
anovapref<-Anova(lmmpref.s7, type="III", test.statistic="F")
anovafamiliar<-Anova(lmmfamiliar.s7, type="III", test.statistic="F")

###PLOTS

#val #r^2=.526
smallset$fittedval.s7 <- fitted(lmmval.s7)
mcp.fnc(lmmval.s7)
plot(smallset$val, smallset$fittedval.s7)
abline(fitval.s7<-lm(smallset$fittedval.s7~smallset$val), col="red")
legend("topright", bty="n", legend=paste("R^2 is", format(summary(fitval.s7)$adj.r.squared, digits=3)))

#ten #r^2=.425
smallset$fittedten.s7 <- fitted(lmmten.s7)
mcp.fnc(lmmten.s7)
plot(smallset$ten, smallset$fittedten.s7)
abline(fitten.s7<-lm(smallset$fittedten.s7~smallset$ten), col="red")
legend("topright", bty="n", legend=paste("R^2 is", format(summary(fitten.s7)$adj.r.squared, digits=3)))

#energy #r^2=.497
smallset$fittedenergy.s7 <- fitted(lmmenergy.s7)
mcp.fnc(lmmenergy.s7)
plot(smallset$energy, smallset$fittedenergy.s7)
abline(fitenergy.s7<-lm(smallset$fittedenergy.s7~smallset$energy), col="red")
legend("topright", bty="n", legend=paste("R^2 is", format(summary(fitenergy.s7)$adj.r.squared, digits=3)))

#pref #r^2=.509
smallset$fittedpref.s7 <- fitted(lmmpref.s7)
mcp.fnc(lmmpref.s7)
plot(smallset$pref, smallset$fittedpref.s7)
abline(fitpref.s7<-lm(smallset$fittedpref.s7~smallset$pref), col="red")
legend("topright", bty="n", legend=paste("R^2 is", format(summary(fitpref.s7)$adj.r.squared, digits=3)))

#familiar #r^2=.586
smallset$fittedfamiliar.s7 <- fitted(lmmfamiliar.s7)
mcp.fnc(lmmfamiliar.s7)
plot(smallset$familiar, smallset$fittedfamiliar.s7)
abline(fitfamiliar.s7<-lm(smallset$fittedfamiliar.s7~smallset$familiar), col="red")
legend("topright", bty="n", legend=paste("R^2 is", format(summary(fitfamiliar.s7)$adj.r.squared, digits=3)))

##LSMEANS - get predicted mean and confidence intervals 

lsmval<-lsmeans(lmmval.s7, ~reg.s|fam.s|mus.s, cov.reduce=FALSE)
lsmten<-lsmeans(lmmten.s7, ~reg.s|fam.s|mus.s, cov.reduce=FALSE)
lsmenergy<-lsmeans(lmmenergy.s7, ~reg.s|fam.s|mus.s, cov.reduce=FALSE)
lsmpref<-lsmeans(lmmpref.s7, ~reg.s|fam.s|mus.s, cov.reduce=FALSE)
lsmfamiliar<-lsmeans(lmmfamiliar.s7, ~reg.s|fam.s|mus.s, cov.reduce=FALSE)

##GRAPHS!!!!

lsmip(lmmval.s7, fam.s~reg.s|mus.s)
lsmip(lmmten.s7, fam.s~reg.s|mus.s)
lsmip(lmmenergy.s7, fam.s~reg.s|mus.s)
lsmip(lmmpref.s7, fam.s~reg.s|mus.s)
lsmip(lmmfamiliar.s7, fam.s~reg.s|mus.s)

save.image("~/Desktop/chelsea/rr/allfullmodels.RData")
