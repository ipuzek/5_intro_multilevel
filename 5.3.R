rm(list = ls())
setwd("Z:/Lemma Training Materials/R/")
##############################################################################
# Module 5: Introduction to Multilevel Modelling R Practicals
#
#     P5.3: Allowing for Different Slopes across Schools: Random Slope
#           Models
#
#           Camille Szmaragd and George Leckie 
#           Centre for Multilevel Modelling, 2011
##############################################################################

mydata <- read.table("5.3.txt", header = TRUE, sep = ",")

library(lme4)

fit <- lmer(score ~ cohort90 + (1 + cohort90 | schoolid), data = mydata, REML = FALSE)

summary(fit)


# P5.3.1 Testing for random slopes

fita <- lmer(score ~ cohort90 + (1 | schoolid), data = mydata, REML = FALSE)

anova(fit, fita)


# P5.3.3 Examining intercept and slope residuals for schools

VarCorr(fit)$schoolid

myrandomeff <- ranef(fit, postVar = TRUE)

plot(myrandomeff[[1]], xlab = "Intercept (u0j)", ylab = "Slope of cohort90 (u1j)")

abline(h = 0, col = "red")

abline(v = 0, col = "red")

predscore <- fitted(fit)

datapred <- cbind(predscore = predscore, cohort90 = mydata$cohort90, schoolid = mydata$schoolid)

datapred <- data.frame(unique(datapred))

datapred <- datapred[order(datapred$schoolid, datapred$cohort90), ]

datapred$multiplecohorts <- rep(0, dim(datapred)[1])

datapred$multiplecohorts[datapred$schoolid %in% unique(datapred$schoolid[duplicated(datapred$schoolid)])] <- 1

xyplot(predscore ~ cohort90, data = datapred[datapred$multiplecohorts == 1, ], groups = schoolid, type = c("p", "l"), col = "blue")


# P5.3.4 Between - school variance as a function of cohort

x <- c(-6:8)

y <- 42.859 - 2.048*x + 0.161*x^2

plot(x, y, type = "l", xlim = c(-6, 10))


# P5.3.5 Adding a random coefficient for gender (dichotomous x )

(fit2a <- lmer(score ~ cohort90 + female + (1 + cohort90 | schoolid), data = mydata, REML = FALSE))

(fit2 <- lmer(score ~ cohort90 + female + (1 + cohort90 + female | schoolid), data = mydata, REML = FALSE))

anova(fit2, fit2a)


# P5.3.6 Adding a random coefficient for social class (categorical x )

mydata$sclass1 <- mydata$sclass == 1

mydata$sclass2 <- mydata$sclass == 2

mydata$sclass4 <- mydata$sclass == 4

(fit3a <- lmer(score ~ cohort90 + female + sclass1 +sclass2 + sclass4 + (1 + cohort90 | schoolid), data = mydata, REML = FALSE))

(fit3 <- lmer(score ~ cohort90 + female + sclass1 +sclass2 + sclass4 + (1 + cohort90 + sclass1 +sclass2 + sclass4 | schoolid), data = mydata, REML = FALSE))

anova(fit3, fit3a)

VarCorr(fit3)$schoolid

11.267  +  2*4.813  +  7.136

11.267  +  2*5.059  +  3.321

11.267

11.267  +  2*8.077  +  7.184
