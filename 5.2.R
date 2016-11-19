rm(list = ls())
setwd("IvanP/!!!Doktorat/5_intro_multilevel/")
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
library(arm)
library(broom)
library(tibble)
library(dplyr)

mydata <- as_tibble(mydata)

fit <- lmer(score ~ cohort90 + (1 | schoolid), data = mydata, REML = FALSE)

display(fit)

fixef(fit)

fixef(fit)[["(Intercept)"]]
fixef(fit)[["cohort90"]]

myd.fitted <- mydata %>% 
  add_column(.fitted = fitted(fit)) %>%
  unique()

tidy(fit) %>% 
  filter(group == "fixed") %>% 
  select(term, estimate)

# plot(fit)

# lattice::xyplot(.fitted ~ cohort90, data = myd.fitted, groups = schoolid, type = c("p", "l"), col = "blue")


myd.fitted %>% 
  ggplot(aes(x = cohort90, y = .fitted)) +
  geom_line(aes(group = schoolid), colour = "grey") +
  theme_dark()

# adding fixed effects
last_plot() + 
  geom_abline(intercept = fixef(fit)[["(Intercept)"]],
              slope = fixef(fit)[["cohort90"]],
              colour = "deeppink", size = 2)

# ZANIMLJIVA vrlo mala razlika...ajmo ju raščerečiti...
last_plot() + 
  geom_smooth(aes(y = score),
              method = "lm",
              colour = "deeppink",
              size = 2,
              alpha = .5)

# ...
fit.lm <- lm(score ~ cohort90, data = mydata)

last_plot() + 
  geom_abline(intercept = coef(fit.lm)[["(Intercept)"]],
              slope = coef(fit.lm)[["cohort90"]],
              colour = "blue")

# ludilo apsolutno

ggplot(mydata, aes(x = cohort90, y = score, group = as.factor(schoolid))) +
  geom_smooth(method = "lm", se = FALSE, alpha = .5, size = .2)


###
  
datapred <- datapred[order(datapred$schoolid, datapred$cohort90), ]

datapred$multiplecohorts <- rep(0, length(datapred$schoolid))

datapred$multiplecohorts[datapred$schoolid %in% unique(datapred$schoolid[duplicated(datapred$schoolid)])] <- 1

lattice::xyplot(predscore ~ cohort90, data = datapred[datapred$multiplecohorts == 1, ], groups = schoolid, type = c("p", "l"), col = "blue")

library(ggplot2)

# setup
augment(fit) %>% 
  as_data_frame() %>% 
  ggplot(aes(x = cohort90, y = .fitted))

# plot
last_plot() + geom_line(aes(group = schoolid))


###

coef(fit)$schoolid %>% as_tibble() # nrow = broj škola

resid(fit) %>% head # ima ih koliko i data.pointa (cca 34000)

fitted(fit) %>% head

# što je .mu?

myd.aug <- augment(fit) %>% as_tibble()

school_sort <- table(myd.aug$schoolid) %>% 
  sort(decreasing = TRUE)

school_filter <- school_sort[1:10] %>% names()

myd.aug %>%
  select(score, cohort90, schoolid, .fitted, .resid, .fixed, .mu) %>% 
  mutate(schoolid = as.factor(schoolid),
         cohort90 = as.factor(cohort90)) %>% 
  filter(schoolid %in% school_filter) %>%
  droplevels() %>% 
  ggplot(aes(x = .mu)) +
  geom_bar(aes(fill = cohort90)) +
  facet_wrap(~ schoolid, nrow = 2) # po ovome, conditional mean?

myd.aug %>%
  group_by(cohort90, schoolid) %>% 
  summarize(mean.score = mean(score)) %>% 
  filter(schoolid %in% school_filter) %>% 
  arrange(schoolid)
  
# ne kužim...