###################################################
# name: hansen_dwi.r                              #
# author: Bernardo Magalhaes (UT Austin)          #
# description: Replication - Punishment and       #
#              Deterrence: Evidence from Drunk    #
#              Driving - By Benjamin Hansen       #
# date: February 25, 2020                         #
###################################################

# read in the data
dwi = read.csv(url("https://raw.githubusercontent.com/bmagalhaes/Hansen-DUI/master/Data/hansen_dwi.csv"))

# create a dummy for the first cutoff
dwi = mutate(dwi, first_cut = ifelse(bac1 >= 0.08, 1, 0))

# check for evidence of manipulaton on blood alcohol content (bac1

ggplot(data=dwi, aes(x = bac1)) +
  geom_histogram(binwidth = 0.001) +
  geom_vline(xintercept = 0.08 , color="yellow" , linetype = "dashed") +
  geom_vline(xintercept = 0.15 , color="green" , linetype = "dashed") +
  labs(title = 'BAC histogram', x = "BAC", y = "Frequency")+
  ylim(0, 2000)+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor.x = element_blank(), panel.background = element_blank(), panel.grid.minor.y = element_line(colour = "grey"))

# The second thing we need to do is check for covariate balance. You will need to estimate
# equation (1) with white, male, age and accident (acc) as dependent variables.
# Are the covariate balanced at the cutoff?
#
# y i = X i ′ γ + α 1 DU I i + α 2 BA C i + α 3 BA C i × DU I i + u i