library(nlme)
library(CorrMixed)
library(lme4)
library(lmerTest)
library(geepack)
#library(Zelig)
library(ggplot2)
library(geomtextpath)
library(car)
library(emmeans)
library(effectsize)
library(simr)

names(data)[1] <- "Id"

# a lot of this stuff should be wrapped in formulas but whatever

# Load the  data. 

data_eo <- read.table("/Users/zeleninam2/Documents/projects/Oxytocin_final_2024/behav_lmm/dat_EO_relPower_withbehav_missval.csv", header=T, sep=",")
data_ec <- read.table("/Users/zeleninam2/Documents/projects/Oxytocin_final_2024/behav_lmm/dat_EC_relPower_withbehav_missval.csv", header=T, sep=",")

data_eo$Drug <- factor(data_eo$Drug)
data_eo$TP <- factor(data_eo$TP)
data_eo$id <- factor(data_eo$id)

data_ec$Drug <- factor(data_ec$Drug)
data_ec$TP <- factor(data_ec$TP)
data_ec$id <- factor(data_ec$id)

#data$Alertness <- factor(data$Alertness)
#data$Sociability <- factor(data$Sociability)
#data$Excitement <- factor(data$Excitement)

# ------------------------------------------------------------------------------
# run the model

# THETA - Alertness
# (iterate through [Theta, Alpha, Beta] and [Alertness, Excitement, Sociability] manually)
# specify which data: eo or ec

t = lmer(Beta ~ Beta_base + TP*Drug*Alertness + (1|id), data=data_eo)

anova(t, type=3)

# ------------------------------------------------------------------------------
# Posthocs

# correlations between behavioral features and EEG
# for OT/PL separately
# --> for each time point

# split data between ot and pl

data_eo_ot_all<-data_eo[data_eo$Drug==1,]
data_eo_pl_all<-data_eo[data_eo$Drug==2,]

data_ec_ot_all<-data_ec[data_ec$Drug==1,]
data_ec_pl_all<-data_ec[data_ec$Drug==2,]

# analyze
# (this really should be a loop but I'm keeping it like this for clarity and to make changes easily)

# ----------------------------------------------
# ----------------------------------------------
# EYES OPEN
# ----------------------------------------------
# ----------------------------------------------

# ALERTNESS

# ------> alertness, theta by time point
for (timep in list(1,2,3,4,5,6)) {
  print(timep)
  data_my_tp_ot<-data_eo_ot_all[data_eo_ot_all$TP==timep,]
  data_my_tp_pl<-data_eo_pl_all[data_eo_pl_all$TP==timep,]
  
  print(cor.test(data_my_tp_ot$Alertness, data_my_tp_ot$Theta,use="pairwise.complete.obs",method = "spearman"))
  print(cor.test(data_my_tp_pl$Alertness, data_my_tp_pl$Theta,use="pairwise.complete.obs",method = "spearman"))
}

# ------> alertness, alpha by time point
for (timep in list(1,2,3,4,5,6)) {
  print(timep)
  data_my_tp_ot<-data_eo_ot_all[data_eo_ot_all$TP==timep,]
  data_my_tp_pl<-data_eo_pl_all[data_eo_pl_all$TP==timep,]
  
  print(cor.test(data_my_tp_ot$Alertness, data_my_tp_ot$Alpha,use="pairwise.complete.obs",method = "spearman"))
  print(cor.test(data_my_tp_pl$Alertness, data_my_tp_pl$Alpha,use="pairwise.complete.obs",method = "spearman"))
}

# ------> alertness, beta by time point
for (timep in list(1,2,3,4,5,6)) {
  print(timep)
  data_my_tp_ot<-data_eo_ot_all[data_eo_ot_all$TP==timep,]
  data_my_tp_pl<-data_eo_pl_all[data_eo_pl_all$TP==timep,]
  
  print(cor.test(data_my_tp_ot$Alertness, data_my_tp_ot$Beta,use="pairwise.complete.obs",method = "spearman"))
  print(cor.test(data_my_tp_pl$Alertness, data_my_tp_pl$Beta,use="pairwise.complete.obs",method = "spearman"))
}

# ----------------------------------------------

# EXCITEMENT

# eo; excitement; overall

# ------> Excitement, theta by time point
for (timep in list(1,2,3,4,5,6)) {
  print(timep)
  data_my_tp_ot<-data_eo_ot_all[data_eo_ot_all$TP==timep,]
  data_my_tp_pl<-data_eo_pl_all[data_eo_pl_all$TP==timep,]
  
  print(cor.test(data_my_tp_ot$Excitement, data_my_tp_ot$Theta,use="pairwise.complete.obs",method = "spearman"))
  print(cor.test(data_my_tp_pl$Excitement, data_my_tp_pl$Theta,use="pairwise.complete.obs",method = "spearman"))
}

# ------> Excitement, alpha by time point
for (timep in list(1,2,3,4,5,6)) {
  print(timep)
  data_my_tp_ot<-data_eo_ot_all[data_eo_ot_all$TP==timep,]
  data_my_tp_pl<-data_eo_pl_all[data_eo_pl_all$TP==timep,]
  
  print(cor.test(data_my_tp_ot$Excitement, data_my_tp_ot$Alpha,use="pairwise.complete.obs",method = "spearman"))
  print(cor.test(data_my_tp_pl$Excitement, data_my_tp_pl$Alpha,use="pairwise.complete.obs",method = "spearman"))
}

# ------> Excitement, beta by time point
for (timep in list(1,2,3,4,5,6)) {
  print(timep)
  data_my_tp_ot<-data_eo_ot_all[data_eo_ot_all$TP==timep,]
  data_my_tp_pl<-data_eo_pl_all[data_eo_pl_all$TP==timep,]
  
  print(cor.test(data_my_tp_ot$Excitement, data_my_tp_ot$Beta,use="pairwise.complete.obs",method = "spearman"))
  print(cor.test(data_my_tp_pl$Excitement, data_my_tp_pl$Beta,use="pairwise.complete.obs",method = "spearman"))
}

# ----------------------------------------------

# SOCIABILITY

# eo; Sociability; overall

# ------> Sociability, theta by time point
for (timep in list(1,2,3,4,5,6)) {
  print(timep)
  data_my_tp_ot<-data_eo_ot_all[data_eo_ot_all$TP==timep,]
  data_my_tp_pl<-data_eo_pl_all[data_eo_pl_all$TP==timep,]
  
  print(cor.test(data_my_tp_ot$Sociability, data_my_tp_ot$Theta,use="pairwise.complete.obs",method = "spearman"))
  print(cor.test(data_my_tp_pl$Sociability, data_my_tp_pl$Theta,use="pairwise.complete.obs",method = "spearman"))
}

# ------> Sociability, alpha by time point
for (timep in list(1,2,3,4,5,6)) {
  print(timep)
  data_my_tp_ot<-data_eo_ot_all[data_eo_ot_all$TP==timep,]
  data_my_tp_pl<-data_eo_pl_all[data_eo_pl_all$TP==timep,]
  
  print(cor.test(data_my_tp_ot$Sociability, data_my_tp_ot$Alpha,use="pairwise.complete.obs",method = "spearman"))
  print(cor.test(data_my_tp_pl$Sociability, data_my_tp_pl$Alpha,use="pairwise.complete.obs",method = "spearman"))
}

# ------> Sociability, beta by time point
for (timep in list(1,2,3,4,5,6)) {
  print(timep)
  data_my_tp_ot<-data_eo_ot_all[data_eo_ot_all$TP==timep,]
  data_my_tp_pl<-data_eo_pl_all[data_eo_pl_all$TP==timep,]
  
  print(cor.test(data_my_tp_ot$Sociability, data_my_tp_ot$Beta,use="pairwise.complete.obs",method = "spearman"))
  print(cor.test(data_my_tp_pl$Sociability, data_my_tp_pl$Beta,use="pairwise.complete.obs",method = "spearman"))
}

# ----------------------------------------------
# ----------------------------------------------
# EYES CLOSED
# ----------------------------------------------
# ----------------------------------------------


# ALERTNESS

# ------> alertness, theta by time point
for (timep in list(1,2,3,4,5,6)) {
  print(timep)
  data_my_tp_ot<-data_ec_ot_all[data_ec_ot_all$TP==timep,]
  data_my_tp_pl<-data_ec_pl_all[data_ec_pl_all$TP==timep,]
  
  print(cor.test(data_my_tp_ot$Alertness, data_my_tp_ot$Theta,use="pairwise.complete.obs",method = "spearman"))
  print(cor.test(data_my_tp_pl$Alertness, data_my_tp_pl$Theta,use="pairwise.complete.obs",method = "spearman"))
}

# ------> alertness, alpha by time point
for (timep in list(1,2,3,4,5,6)) {
  print(timep)
  data_my_tp_ot<-data_ec_ot_all[data_ec_ot_all$TP==timep,]
  data_my_tp_pl<-data_ec_pl_all[data_ec_pl_all$TP==timep,]
  
  print(cor.test(data_my_tp_ot$Alertness, data_my_tp_ot$Alpha,use="pairwise.complete.obs",method = "spearman"))
  print(cor.test(data_my_tp_pl$Alertness, data_my_tp_pl$Alpha,use="pairwise.complete.obs",method = "spearman"))
}

# ------> alertness, beta by time point
for (timep in list(1,2,3,4,5,6)) {
  print(timep)
  data_my_tp_ot<-data_ec_ot_all[data_ec_ot_all$TP==timep,]
  data_my_tp_pl<-data_ec_pl_all[data_ec_pl_all$TP==timep,]
  
  print(cor.test(data_my_tp_ot$Alertness, data_my_tp_ot$Beta,use="pairwise.complete.obs",method = "spearman"))
  print(cor.test(data_my_tp_pl$Alertness, data_my_tp_pl$Beta,use="pairwise.complete.obs",method = "spearman"))
}

# ----------------------------------------------

# EXCITEMENT

# ------> Excitement, theta by time point
for (timep in list(1,2,3,4,5,6)) {
  print(timep)
  data_my_tp_ot<-data_ec_ot_all[data_ec_ot_all$TP==timep,]
  data_my_tp_pl<-data_ec_pl_all[data_ec_pl_all$TP==timep,]
  
  print(cor.test(data_my_tp_ot$Excitement, data_my_tp_ot$Theta,use="pairwise.complete.obs",method = "spearman"))
  print(cor.test(data_my_tp_pl$Excitement, data_my_tp_pl$Theta,use="pairwise.complete.obs",method = "spearman"))
}

# ------> Excitement, alpha by time point
for (timep in list(1,2,3,4,5,6)) {
  print(timep)
  data_my_tp_ot<-data_ec_ot_all[data_ec_ot_all$TP==timep,]
  data_my_tp_pl<-data_ec_pl_all[data_ec_pl_all$TP==timep,]
  
  print(cor.test(data_my_tp_ot$Excitement, data_my_tp_ot$Alpha,use="pairwise.complete.obs",method = "spearman"))
  print(cor.test(data_my_tp_pl$Excitement, data_my_tp_pl$Alpha,use="pairwise.complete.obs",method = "spearman"))
}

# ------> Excitement, beta by time point
for (timep in list(1,2,3,4,5,6)) {
  print(timep)
  data_my_tp_ot<-data_ec_ot_all[data_ec_ot_all$TP==timep,]
  data_my_tp_pl<-data_ec_pl_all[data_ec_pl_all$TP==timep,]
  
  print(cor.test(data_my_tp_ot$Excitement, data_my_tp_ot$Beta,use="pairwise.complete.obs",method = "spearman"))
  print(cor.test(data_my_tp_pl$Excitement, data_my_tp_pl$Beta,use="pairwise.complete.obs",method = "spearman"))
}

# ----------------------------------------------

# SOCIABILITY

# ------> Sociability, theta by time point
for (timep in list(1,2,3,4,5,6)) {
  print(timep)
  data_my_tp_ot<-data_ec_ot_all[data_ec_ot_all$TP==timep,]
  data_my_tp_pl<-data_ec_pl_all[data_ec_pl_all$TP==timep,]
  
  print(cor.test(data_my_tp_ot$Sociability, data_my_tp_ot$Theta,use="pairwise.complete.obs",method = "spearman"))
  print(cor.test(data_my_tp_pl$Sociability, data_my_tp_pl$Theta,use="pairwise.complete.obs",method = "spearman"))
}

# ------> Sociability, alpha by time point
for (timep in list(1,2,3,4,5,6)) {
  print(timep)
  data_my_tp_ot<-data_ec_ot_all[data_ec_ot_all$TP==timep,]
  data_my_tp_pl<-data_ec_pl_all[data_ec_pl_all$TP==timep,]
  
  print(cor.test(data_my_tp_ot$Sociability, data_my_tp_ot$Alpha,use="pairwise.complete.obs",method = "spearman"))
  print(cor.test(data_my_tp_pl$Sociability, data_my_tp_pl$Alpha,use="pairwise.complete.obs",method = "spearman"))
}

# ------> Sociability, beta by time point
for (timep in list(1,2,3,4,5,6)) {
  print(timep)
  data_my_tp_ot<-data_ec_ot_all[data_ec_ot_all$TP==timep,]
  data_my_tp_pl<-data_ec_pl_all[data_ec_pl_all$TP==timep,]
  
  print(cor.test(data_my_tp_ot$Sociability, data_my_tp_ot$Beta,use="pairwise.complete.obs",method = "spearman"))
  print(cor.test(data_my_tp_pl$Sociability, data_my_tp_pl$Beta,use="pairwise.complete.obs",method = "spearman"))
}


# ? - spearman, pearson or kendall
# https://ishanjainoffical.medium.com/choosing-the-right-correlation-pearson-vs-spearman-vs-kendalls-tau-02dc7d7dd01d
# I chose spearman because we dont know if the relationship is monotonic


# ----------------------------------------------
# ----------------------------------------------

# PLOT THE PLOTS

# ----------------------------------------------
# ----------------------------------------------

# example - I found significance in omnibus test for EO, Alertness, Theta. 
# In posthoc correlations, there was a significant correlation between Theta and Alertness in tp5 and tp6, only in the PL group.

timep <- 5
data_eo_tp5<-data_eo[data_eo$TP==timep,]

# plotting Theta against Alertness

ggplot(data_eo_tp5, aes(x = Theta, y = Alertness, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
                   method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 20) + 
  ggtitle("Eyes open, Alertness against Theta, Time point 5")

# -------

# alertness, theta, tp6

timep <- 6
data_eo_tp5<-data_eo[data_eo$TP==timep,]

ggplot(data_eo_tp5, aes(x = Theta, y = Alertness, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 20) + 
  ggtitle("Eyes open, Alertness against Theta, Time point 6")

# -------

# sociability, beta, tp3

timep <- 3
data_eo_tp5<-data_eo[data_eo$TP==timep,]

ggplot(data_eo_tp5, aes(x = Beta, y = Sociability, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 20) + 
  ggtitle("Eyes open, Sociability against Beta, Time point 3")

# -------

# sociability, beta, tp5

timep <- 5
data_eo_tp5<-data_eo[data_eo$TP==timep,]

ggplot(data_eo_tp5, aes(x = Beta, y = Sociability, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 20) + 
  ggtitle("Eyes open, Sociability against Beta, Time point 5")

# -------

# EYES CLOSED

# Alertnerss, theta, tp1

timep <- 1
data_ec_tp5<-data_ec[data_ec$TP==timep,]

ggplot(data_ec_tp5, aes(x = Theta, y = Alertness, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 20) + 
  ggtitle("Eyes closed, Alertness against Theta, Time point 1")

# -------

# Sociability, beta, tp3

timep <- 3
data_ec_tp5<-data_ec[data_ec$TP==timep,]

ggplot(data_ec_tp5, aes(x = Beta, y = Sociability, color = Drug, shape=Drug)) +
  geom_point() +
  geom_smooth(aes(label = Drug),
              method = "lm", se = TRUE) +
  scale_color_manual(labels = c("Oxytocin", "Placebo"), values = c("blue", "red")) +
  scale_shape_manual(labels = c("Oxytocin", "Placebo"), values=c(15, 17))+
  theme_bw(base_size = 20) + 
  ggtitle("Eyes closed, Sociability against Beta, Time point 3")

# ----------------------------------------------
# ----------------------------------------------

# T-TESTS

# this rrrrreally should have been formulas...
# ----------------------------------------------
# ----------------------------------------------

# EO, Alertness, tp 5 and 6
timep <- 5
my_data_eo_ot<-data_eo_ot_all[data_eo_ot_all$TP==timep,]
my_data_eo_pl<-data_eo_pl_all[data_eo_pl_all$TP==timep,]

t.test(my_data_eo_ot$Alertness, my_data_eo_pl$Alertness)

timep <- 6
my_data_eo_ot<-data_eo_ot_all[data_eo_ot_all$TP==timep,]
my_data_eo_pl<-data_eo_pl_all[data_eo_pl_all$TP==timep,]

t.test(my_data_eo_ot$Alertness, my_data_eo_pl$Alertness)

# EO, Sociability, TP 3 and 5
timep <- 3
my_data_eo_ot<-data_eo_ot_all[data_eo_ot_all$TP==timep,]
my_data_eo_pl<-data_eo_pl_all[data_eo_pl_all$TP==timep,]

t.test(my_data_eo_ot$Sociability, my_data_eo_pl$Sociability)

timep <- 5
my_data_eo_ot<-data_eo_ot_all[data_eo_ot_all$TP==timep,]
my_data_eo_pl<-data_eo_pl_all[data_eo_pl_all$TP==timep,]

t.test(my_data_eo_ot$Sociability, my_data_eo_pl$Sociability)

# EC, Alertness, tp 1
timep <- 1
my_data_ec_ot<-data_ec_ot_all[data_ec_ot_all$TP==timep,]
my_data_ec_pl<-data_ec_pl_all[data_ec_pl_all$TP==timep,]

t.test(my_data_ec_ot$Alertness, my_data_ec_pl$Alertness)

# EC, Sociability, tp 3
timep <- 3
my_data_ec_ot<-data_ec_ot_all[data_ec_ot_all$TP==timep,]
my_data_ec_pl<-data_ec_pl_all[data_ec_pl_all$TP==timep,]

t.test(my_data_ec_ot$Sociability, my_data_ec_pl$Sociability)

# ----------------------------------------------
# ----------------------------------------------

# That's it. Doing stats in R was actually really pleasant, I'll soon invest 
# some time to learn to code in R more beautifully, with functions, classes etc.
# In the meanwhile, pls let me know if there were any typos :-/

# Code by Marie Zelenina, 2025-Jun-5