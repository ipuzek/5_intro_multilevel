rm(list = ls())
setwd("Z:/Lemma Training Materials/R/")
##############################################################################
# Module 5: Introduction to Multilevel Modelling R Practicals
#
#     P5.4: Adding Level 2 Explanatory Variables
#
#           Camille Szmaragd and George Leckie
#           Centre for Multilevel Modelling, 2011
##############################################################################

mydata <- read.table("5.4.txt", header = TRUE, sep = ",")

library(lme4)

mydata_un <- unique(mydata[, c(2, 7, 8, 9)])

cbind(Freq = table(mydata_un$schtype), Perc = prop.table(table(mydata_un$schtype)), Cum = cumsum(prop.table(table(mydata_un$schtype))))

cbind(Freq = table(mydata_un$schurban), Perc = prop.table(table(mydata_un$schurban)), Cum = cumsum(prop.table(table(mydata_un$schurban))))

cbind(Freq = table(mydata_un$schdenom), Perc = prop.table(table(mydata_un$schdenom)), Cum = cumsum(prop.table(table(mydata_un$schdenom))))

(fit1a <- lmer(score ~ cohort90 + female + sclass1 + sclass2 + sclass4 + (1 + cohort90 | schoolid), data = mydata, REML = FALSE))


# P5.4.1 Contextual effects

(fit2 <- update(fit1a, . ~ . + schtype))

(fit3 <- update(fit2, . ~ . + schurban))

(fit4 <- update(fit3, . ~ . + schdenom))


# P5.4.2 Cross - level interactions

mydata$cohort90Xschtype <- mydata$cohort90*mydata$schtype

(fit5 <- update(fit3, . ~ . + cohort90Xschtype))
