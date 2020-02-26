###################################################
# name: hansen_dwi.r                              #
# author: Bernardo Magalhaes (UT Austin)          #
# description: Replication - Punishment and       #
#              Deterrence: Evidence from Drunk    #
#              Driving - By Benjamin Hansen       #
# date: February 25, 2020                         #
###################################################

library(mosaic)
library(plyr)

library(rdd)
library(rddtools)
library(rdrobust)
library(cobalt)

# read in the data
rm(list=ls(all=TRUE))
dwi = read.csv(url("https://raw.githubusercontent.com/bmagalhaes/Hansen-DUI/master/Data/hansen_dwi.csv"))

# create a dummy for the first cutoff

dwi = mutate(dwi, first_cut = ifelse(bac1 >= 0.08, 1, 0))

# check for evidence of manipulaton on blood alcohol content (bac1

ggplot(data=dwi, aes(x = bac1)) +
  geom_histogram(binwidth = 0.001) +
  geom_vline(xintercept = 0.08 , color="red" , linetype = "dashed") +
  geom_vline(xintercept = 0.15 , color="red" , linetype = "dashed") +
  labs(title = 'BAC histogram', x = "BAC", y = "Frequency")+
  ylim(0, 2000)+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor.x = element_blank(), panel.background = element_blank(), panel.grid.minor.y = element_line(colour = "grey"))

# The second thing we need to do is check for covariate balance. You will need to estimate
# equation (1) with white, male, age and accident (acc) as dependent variables.
# Are the covariate balanced at the cutoff?
#
# y i = X i ′ γ + α 1 DU I i + α 2 BA C i + α 3 BA C i × DU I i + u i

white = tapply(dwi$white, cut(dwi$bac1, seq(0, 0.45, by=0.002)), mean)
cond_mean = data.frame(white)
cond_mean = mutate(cond_mean, bac1 = seq(0, 0.448, by=0.002))
cond_mean = mutate(cond_mean, first_cut = ifelse(bac1 >= 0.08, 1, 0))
cond_mean = mutate(cond_mean, male = tapply(dwi$male, cut(dwi$bac1, seq(0, 0.45, by=0.002)), mean))
cond_mean = mutate(cond_mean, aged = tapply(dwi$aged, cut(dwi$bac1, seq(0, 0.45, by=0.002)), mean)/100)
cond_mean = mutate(cond_mean, acc = tapply(dwi$acc, cut(dwi$bac1, seq(0, 0.45, by=0.002)), mean))

ggplot(cond_mean, aes(x = bac1, y = white, color = factor(first_cut))) + 
  geom_point() +
  geom_vline(aes(xintercept = 0.08), color = 'red', size = 1) +
  geom_smooth(se = FALSE, method = lm) +
  ylim(0.8, 0.9) +
  xlim(0.03, 0.2) 

ggplot(cond_mean, aes(x = bac1, y = male, color = factor(first_cut))) + 
  geom_point() +
  geom_vline(aes(xintercept = 0.08), color = 'red', size = 1) +
  geom_smooth(se = FALSE, method = lm) +
  ylim(0.74, 0.82) +
  xlim(0.03, 0.2) 

ggplot(cond_mean, aes(x = bac1, y = aged, color = factor(first_cut))) + 
  geom_point() +
  geom_vline(aes(xintercept = 0.08), color = 'red', size = 1) +
  geom_smooth(se = FALSE, method = lm) +
  ylim(0.34, 0.38) +
  xlim(0.03, 0.2) 

ggplot(cond_mean, aes(x = bac1, y = acc, color = factor(first_cut))) + 
  geom_point() +
  geom_vline(aes(xintercept = 0.08), color = 'red', size = 1) +
  geom_smooth(se = FALSE, method = lm) +
  stat_smooth(size = 1.5, formula = acc ~ bac1) +
  ylim(0.05, 0.25) +
  xlim(0.03, 0.2) 

# Regressions

attach(dwi)
summary(rdrobust(y=white, x=bac1, c = 0.08, vce="hc1", bwselect = 0.05))

#

white_est = lm(white ~ first_cut*bac1, data=dwi)
male_est = lm(male ~ first_cut*bac1, data=dwi)
aged_est = lm(aged ~ first_cut*bac1, data=dwi)
acc_est = lm(acc ~ first_cut*bac1, data=dwi)

coeftest(white_est, vcov = vcovHC(white_est, "HC1"))
coeftest(male_est, vcov = vcovHC(male_est, "HC1"))
coeftest(aged_est, vcov = vcovHC(aged_est, "HC1"))
coeftest(acc_est, vcov = vcovHC(acc_est, "HC1"))

#

reg_1 <- lm(recidivism ~ white + male + aged + acc + first_cut*bac1, data=dwi, subset = first_cut == 0)
summary(reg_1)

reg_2 <- lm(recidivism ~ white + male + aged + acc + first_cut*bac1, data=dwi, subset = first_cut == 1)
summary(reg_2)

