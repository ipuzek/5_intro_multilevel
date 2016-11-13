rm(list = ls())
setwd("Z:/Lemma Training Materials/R/")
##############################################################################
# Module 5: Introduction to Multilevel Modelling R Practicals
#
#     P5.1: Comparing Groups using Multilevel Modelling
#
#           Camille Szmaragd and George Leckie
#           Centre for Multilevel Modelling, 2011
##############################################################################

mydata <- read.table(file = "5.1.txt", sep = ",", header = TRUE)

str(mydata)


# P5.1.1 A multilevel model of attainment with school effects

library(lme4)

nullmodel <- lmer(score ~ (1 | schoolid), data = mydata, REML = FALSE)

summary(nullmodel)


# P5.1.2 Examining school effects (residuals)

fit <- lm(score ~ 1, data = mydata)

summary(fit)

logLik(nullmodel)

logLik(fit)

u0 <- ranef(nullmodel, postVar = TRUE) 

u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ]) 

str(u0[1])

str(u0[[1]])

head(attr(u0[[1]], "postVar")[1, , ])

schoolid <- as.numeric(rownames(u0[[1]]))

u0tab <- cbind(schoolid, u0[[1]], u0se)

colnames(u0tab) <- c("schoolid","u0","u0se")

u0tab <- u0tab[order(u0tab$u0), ]

u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))

colnames(u0tab)[4] <- "u0rank"

u0tab <- u0tab[order(u0tab$schoolid), ]

u0tab[1:10, ]

plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", ylab = "conditional modes of r.e. for school_id:_cons")

segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 1.96*u0tab$u0se)

points(u0tab$u0rank, u0tab$u0, col = "blue")

abline(h = 0, col = "red")
