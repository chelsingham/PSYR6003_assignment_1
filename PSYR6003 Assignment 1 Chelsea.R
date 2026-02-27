#load tidyverse
library(tidyverse)

#question 1 - load data set
avengers <- read_csv("avengers.csv")
view(avengers)

#question 2
#clean data
avengers_clean <- drop_na(avengers)

#add CombatEffectiveness
avengers2 <- avengers_clean %>%
  mutate(CombatEffectiveness = agility+speed+strength+willpower)

view(avengers2)

#question 3
avengers3 <- avengers2 %>%
  filter(superpower == "no",
         died == "yes")

#export dataset to csv
write_csv(avengers3, "Avengers_Dataset")

#xeport dataset to spss
library(haven)
write_sav(avengers3, "Avengers_Dataset")

#descriptive stats for full dataset
avengers_descriptives <- avengers3 %>%
  summarise(mean(CombatEffectiveness),
            sd(CombatEffectiveness),
            min(CombatEffectiveness),
            max(CombatEffectiveness),
            mean(kills),
            sd(kills),
            min(kills),
            max(kills),
            mean(injuries),
            sd(injuries),
            min(injuries),
            max(injuries))

view(avengers_descriptives)

#descriptive stats by battlefield
avengers_descriptives_ns <- avengers3 %>%
  group_by(north_south) %>%
  summarise(mean(CombatEffectiveness),
            sd(CombatEffectiveness),
            min(CombatEffectiveness),
            max(CombatEffectiveness),
            mean(kills),
            sd(kills),
            min(kills),
            max(kills),
            mean(injuries),
            sd(injuries),
            min(injuries),
            max(injuries))

view(avengers_descriptives_ns)

#question 8
library(pwr)
library(effectsize)

#n no superpower
nrow(avengers2 %>%
       filter(superpower=="no"))

#n yes superpower
nrow(avengers2 %>%
       filter(superpower=="yes"))

#power analysis to determine necessary sample size
pwr.t.test(n = NULL, d = 0.5, sig.level = 0.05, power = 0.80, 
           type = c("two.sample"),
           alternative = c("two.sided"))

#power analysis to determine achievable power with given sample size
pwr.t.test(n = 32, d = 0.5, sig.level = 0.05, power = NULL, 
           type = c("two.sample"),
           alternative = c("two.sided"))

#question 9 
library(TOSTER)
?power_t_TOST
power_t_TOST(
  n = NULL,
  delta = 0,
  eqb = 0.5,
  alpha = .05,
  power = 0.8,
  type = "two.sample")

#question 10
library(metafor)
summary(escalc(measure = "SMD", 
               ti = 4.25,
               n1i = 32,
               n2i = 780,
               var.names = c("dp","variance")))



