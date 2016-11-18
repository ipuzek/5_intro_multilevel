rm(list = ls())
setwd("IvanP/!!!Doktorat/5_intro_multilevel/")
##############################################################################
# Module 5: Introduction to Multilevel Modelling R Practicals
#
#     P5.1: Comparing Groups using Multilevel Modelling
#
#           Camille Szmaragd and George Leckie
#           Centre for Multilevel Modelling, 2011
##############################################################################

mydata <- read.table(file = "5.1.txt", sep = ",", header = TRUE)

# P5.1.1 A multilevel model of attainment with school effects

library(lme4)
library(arm)
library(broom)
library(purrr)
library(dplyr)

nullmodel <- lmer(
  score ~ (1 | schoolid), # varying intercept with no predictors
     data = mydata, REML = FALSE)

display(nullmodel)

coef(nullmodel) %>% str # imamo samo jedan koeficijent...

coef(nullmodel)$schoolid %>% head(20) # ...a to je intercept

fixef(nullmodel)[["(Intercept)"]] # to je prva vrijednost iz display()-a

ranef(nullmodel)$schoolid[["(Intercept)"]] %>% head

fixef(nullmodel)[["(Intercept)"]] + ranef(nullmodel)$schoolid[["(Intercept)"]] %>% 
  head # COEF! YEAH!

coef(nullmodel)$schoolid[["(Intercept)"]] %>% mean

augment(nullmodel) %>% as_data_frame()





# gelman 12.4 : quickly fitting multilevel models
lmer(1 + score ~ (1 | schoolid), # konstanta je (po novom) default, tako da je 1 + ... nepotrebno
     data = mydata, REML = FALSE) %>% 
  display()

summary(nullmodel)
display(nullmodel)
# glance(nullmodel); tidy(nullmodel)

fit <- lm(score ~ 1, data = mydata)

display(fit)




# logLik is most commonly used for a model fitted by maximum likelihood,
# and some uses, e.g. by AIC, assume this.
# So care is needed where other fit criteria have been used, 
# for example REML (the default for "lme").

# testiramo značajnost razlike između nul-modela i modela sa školama
logLik(nullmodel)
logLik(fit)

## digresija: obična regresija vs (anova) vs multilevel

lmer(score ~ (1 | schoolid), data = mydata, REML = FALSE)

lm(score ~ factor(schoolid), data = mydata) %>% glance

# ?aov # wrapper za lm()

## digresija 2:  niz regresija
niz <- mydata %>%
  # filter(schoolid == 1 | schoolid == 2) %>% 
  split(.$schoolid) %>%
  map(~ lm(score ~ 1, data = .x)) %>%
  map_df(tidy)

### plot that

library(ggplot2)

niz %>% 
  arrange(estimate) %>% 
  mutate(ymin = estimate - std.error,
         ymax = estimate + std.error) %>% 
  ggplot(aes(x = 1:nrow(niz), y = estimate)) + 
  geom_linerange(aes(ymin = ymin, ymax = ymax), size = .1) +
  geom_point(size = .3, colour = "green") +
  theme_dark()

lmer(score ~ (1 | schoolid), data = mydata, REML = FALSE) %>% 
  augment() %>% 
  as_data_frame() %>% 
  select(starts_with(".")) %>% 
  unique()


# P5.1.2 Examining school effects (residuals)

coef(nullmodel)

fixef(nullmodel)[["(Intercept)"]]
ranef(nullmodel, condVar = TRUE)$schoolid[["(Intercept)"]]

ranef(nullmodel, condVar = TRUE) %>% as_data_frame()

u0 <- ranef(nullmodel, condVar = TRUE) # used to be postVar
# extract the conditional modes of the random effects from a fitted model object.
# For linear mixed models the conditional modes of the random effects are also the
# conditional means

lattice::dotplot(u0) # caterpillar plot nabrzaka


# If condVar is TRUE the "postVar" attribute is an array of dimension j by j by k.

u0se <-
  attr(u0[[1]], "postVar")[1, , ] %>% 
  sqrt()

schoolid <- 
  rownames(u0[[1]]) %>% 
  as.numeric()

u0tab <-
  data_frame(schoolid = schoolid,
           u0 = u0[[1]]$`(Intercept)`,
           u0se = u0se)
u0tab <-
arrange(u0tab, u0) %>% 
  mutate(u0rank = 1:nrow(u0tab)) %>% 
  arrange(schoolid)


plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", ylab = "conditional modes of r.e. for school_id:_cons")

segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 1.96*u0tab$u0se)

points(u0tab$u0rank, u0tab$u0, col = "blue")

abline(h = 0, col = "red")

## digresija - group means - means

mydata %>% 
  group_by(schoolid) %>% 
  summarize(grupni.prosjek = mean(score)) %>% 
  mutate(grupna.odstupanja = grupni.prosjek - mean(mydata$score))
  
# ovime smo dobili grupna odstupanja, ali treba uzeti u obzir i individualna odstupanja
# od središta grupe

## još malo obične regresije - schoolid kao faktor

fit.lm.factor <- lm(score ~ as.factor(schoolid), data = mydata)
fit.lm.factor %>% 
  display()
  anova() %>% 
  
