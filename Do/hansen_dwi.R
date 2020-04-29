###################################################
# name: hansen_dwi.r                              #
# author: Bernardo Magalhaes (UT Austin)          #
# description: Replication - Punishment and       #
#              Deterrence: Evidence from Drunk    #
#              Driving - By Benjamin Hansen       #
# date: February 25, 2020                         #
###################################################

library(stargazer)
library(starpolishr)
library(tidyverse)
library(rdrobust)

library(mosaic)
library(plyr)
library(sjPlot)
library(dplyr)

#library(rdd)
#library(rddtools)

#library(cobalt)

# read in the data
rm(list=ls(all=TRUE))
dwi = read.csv(url("https://raw.githubusercontent.com/bmagalhaes/Hansen-DUI/master/Data/hansen_dwi.csv"))

# create a dummy for the first cutoff

dwi = mutate(dwi, dui = ifelse(bac1 >= 0.08, 1, 0))
dwi = mutate(dwi, bandwidth = ifelse(bac1 >= 0.03 & 0.13 >= bac1, 1, 0))
dwi = mutate(dwi, bandwidth2 = ifelse(bac1 >= 0.055 & 0.105 >= bac1, 1, 0))
dwi = mutate(dwi, bac1sq = bac1^2)

# check for evidence of manipulaton on blood alcohol content
ggsave(hist_gg, width = 4, height = 3, file="hist1.png")

hist_gg = ggplot(data=dwi, aes(x = bac1)) +
  geom_histogram(binwidth = 0.001) +
  geom_vline(xintercept = 0.08 , color="red" , linetype = "dashed") +
  geom_vline(xintercept = 0.15 , color="red" , linetype = "dashed") +
  labs(x = "Blood Alcohol Content", y = "Frequency")+
  ylim(0, 2000)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey"))



ggplot(data=dwi, aes(x = bac1)) +
  geom_histogram(binwidth = 0.001) +
  geom_vline(xintercept = 0.08 , color="red" , linetype = "dashed") +
  geom_vline(xintercept = 0.15 , color="red" , linetype = "dashed") +
  labs(title = 'BAC Histogram', x = "Blood Alcohol Content", y = "Frequency")+
  ylim(0, 2000)+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), panel.grid.major.y = element_line(colour = "grey"))

png("histbac.png")
print(hist_gg)
dev.off()

male_a = lm(male ~ dui*bac1, data=dwi)
white_a = lm(white ~ dui*bac1, data=dwi)
age_a = lm(aged ~ dui*bac1, data=dwi)
acc_a = lm(acc ~ dui*bac1, data=dwi)

avg_a = c(mean(dwi$male), mean(dwi$white),mean(dwi$aged), mean(dwi$acc))
avg_a = round(avg_a, digits = 2)

star_a = stargazer(male_a,white_a,age_a,acc_a, omit = c("bac1", "first_cut:bac1", "Constant"),
          keep.stat = "n", add.lines = list(c("Mean of the dependent variable", avg_a)),
          dep.var.labels = c("Male", "White", "Age", "Accident"), covariate.labels = "DUI Threshold",
          model.numbers = FALSE, dep.var.caption  = "Panel A. All observations")

male_b = lm(male ~ dui*bac1, data=dwi, subset = bandwidth == 1)
white_b = lm(white ~ dui*bac1, data=dwi, subset = bandwidth == 1)
age_b = lm(aged ~ dui*bac1, data=dwi, subset = bandwidth == 1)
acc_b = lm(acc ~ dui*bac1, data=dwi, subset = bandwidth == 1)

avg_b = c(mean(dwi$male[dwi$bandwidth == 1]), mean(dwi$white[dwi$bandwidth == 1]),
          mean(dwi$aged[dwi$bandwidth == 1]), mean(dwi$acc[dwi$bandwidth == 1]))
avg_b = round(avg_b, digits = 2)

star_b = stargazer(male_b,white_b,age_b,acc_b, omit = c("bac1", "first_cut:bac1", "Constant"),
          keep.stat = "n", add.lines = list(c("Mean of the dependent variable", avg_b)),
          dep.var.labels = c("Male", "White", "Age", "Accident"), covariate.labels = "DUI Threshold",
          model.numbers = FALSE, dep.var.caption  = "Panel B. 0.03 =< BAC =< 0.13 bandwidth")

recid_a1 = lm(recidivism ~ dui + bac1 + white + male + aged + acc, data=dwi, subset = bandwidth == 1)
recid_a2 = lm(recidivism ~ dui*bac1 + white + male + aged + acc, data=dwi, subset = bandwidth == 1)
recid_a3 = lm(recidivism ~ dui*bac1 + dui*bac1sq + white + male + aged + acc, data=dwi, subset = bandwidth == 1)

avg_r1 = c(mean(dwi$recidivism[dwi$bandwidth == 1]), mean(dwi$recidivism[dwi$bandwidth == 1]),
           mean(dwi$recidivism[dwi$bandwidth == 1]))
avg_r1 = round(avg_r1, digits = 2)

stargazer(recid_a1,recid_a2,recid_a3, omit = c("bac1", "bac1sq", "Constant", "white", "male", "aged",
                                              "acc", "dui:bac1", "dui:bac1sq"),
                   keep.stat = "n", add.lines = list(c("Mean of the dependent variable", avg_r1)),
                   dep.var.labels = "Recidivism", covariate.labels = "DUI Threshold", 
                   model.numbers = FALSE, dep.var.caption  = "Panel A. 0.03 =< BAC =< 0.13 bandwidth")

recid_b1 = lm(recidivism ~ dui + bac1 + white + male + aged + acc, data=dwi, subset = bandwidth2 == 1)
recid_b2 = lm(recidivism ~ dui*bac1 + white + male + aged + acc, data=dwi, subset = bandwidth2 == 1)
recid_b3 = lm(recidivism ~ dui*bac1 + dui*bac1sq + white + male + aged + acc, data=dwi, subset = bandwidth2 == 1)

avg_r2 = c(mean(dwi$recidivism[dwi$bandwidth2 == 1]), mean(dwi$recidivism[dwi$bandwidth2 == 1]),
           mean(dwi$recidivism[dwi$bandwidth2 == 1]))
avg_r2 = round(avg_r2, digits = 2)

stargazer(recid_b1,recid_b2,recid_b3, omit = c("bac1", "bac1sq", "Constant", "white", "male", "aged",
                                               "acc", "dui:bac1", "dui:bac1sq"),
          keep.stat = "n", add.lines = list(c("Mean of the dependent variable", avg_r2)),
          dep.var.labels = "Recidivism", covariate.labels = "DUI Threshold", 
          model.numbers = FALSE, dep.var.caption  = "Panel B. 0.055 =< BAC =< 0.105 bandwidth")



# testing for covariate balance

white_est = rdrobust(dwi$white, dwi$bac1, c=0.08, p=1, h=0.05, kernel="uni", vce="hc1")
male_est =  rdrobust(dwi$male, dwi$bac1, c=0.08, p=1, h=0.05, kernel="uni", vce="hc1")
aged_est =  rdrobust(dwi$aged, dwi$bac1, c=0.08, p=1, h=0.05, kernel="uni", vce="hc1")
acc_est =  rdrobust(dwi$acc, dwi$bac1, c=0.08, p=1, h=0.05, kernel="uni", vce="hc1")

am = lm(white ~ bac1, data=dwi, subset = )

white_coef = white_est[["Estimate"]]
male_coef = male_est[["Estimate"]]
aged_coef = aged_est[["Estimate"]]
acc_coef = acc_est[["Estimate"]]

cov_test = rbind(white_coef, male_coef)
cov_test = rbind(cov_test, aged_coef)
cov_test = rbind(cov_test, acc_coef)
cov_test = as.data.frame(cov_test, row.names = c("White","Male","Age","Accident"))
cov_test = subset(cov_test, select = -c(tau.bc, se.rb))

cov_test$cov = c("white","Male","Age","Accident")
  
stargazer(cov_test, type='text', summary = FALSE, flip=TRUE)

#

reg_1 <- lm(recidivism ~ white + male + aged + acc + first_cut*bac1, data=dwi, subset = first_cut == 0)
summary(reg_1)

reg_2 <- lm(recidivism ~ white + male + aged + acc + first_cut*bac1, data=dwi, subset = first_cut == 1)
summary(reg_2)


rdplot(dwi$male, dwi$bac1, c = 0.08, p=2, kernel = "uni", h = 0.002, subset = dwi$bac1 < 0.15)

b = rd_est(white ~ bac1, dwi, cutpoint = 0.08, kernel = "uni", se.type = "HC1", subset = dwi$bac1 < 0.15, t.design = "geq", less = TRUE)
c = as.lm(b)


c = RDestimate(male ~ bac1, dwi, cutpoint = 0.08, kernel = "rectangular", bw = 0.05)
d = rdd_coef(b)

