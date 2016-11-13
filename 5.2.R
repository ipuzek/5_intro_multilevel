rm(list = ls())
setwd("Z:/Lemma Training Materials/R/")
##############################################################################
# Module 5: Introduction to Multilevel Modelling R Practicals
#
#     P5.2: Adding Student - level Explanatory Variables: Random Intercept
#           Models
#
#           Camille Szmaragd and George Leckie 
#           Centre for Multilevel Modelling, 2011
##############################################################################

mydata <- read.table("5.2.txt", sep = ",", header = TRUE)

library(lme4)

fit <- lmer(score ~ cohort90 + (1 | schoolid), data = mydata, REML = FALSE)

summary(fit)

predscore <- fitted(fit)

datapred <- unique(data.frame(cbind(predscore = predscore, cohort90 = mydata$cohort90, schoolid = mydata$schoolid)))

xyplot(predscore ~ cohort90, data = datapred, groups = schoolid, type = c("p", "l"), col = "blue")

datapred <- datapred[order(datapred$schoolid, datapred$cohort90), ]

datapred$multiplecohorts <- rep(0, length(datapred$schoolid))

datapred$multiplecohorts[datapred$schoolid %in% unique(datapred$schoolid[duplicated(datapred$schoolid)])] <- 1

xyplot(predscore ~ cohort90, data = datapred[datapred$multiplecohorts == 1, ], groups = schoolid, type = c("p", "l"), col = "blue")
