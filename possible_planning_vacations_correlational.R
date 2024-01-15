setwd("~/Desktop/Time Use")
library(tidyverse)
library(lme4)
library(lmerTest)
data <-
  read_csv("Planning Vacations Correlational_May 2019_July 4, 2023_16.36.csv")

center_this <- function(x) {
  x - mean(x, na.rm = TRUE)
}

### panas_1 = positive
### panas_2 = negative
### panas_3 = good
### panas_4 = bad
### panas_5 = pleasant
### panas_6 = unpleasant

data <- data %>%
  mutate(across(
    c(contains("panas")),
    ~ case_when (. == "1" ~ 1,
                 . == "2" ~ 2,
                 . == "3" ~ 3,
                 . == "4" ~ 4,
                 . == "5" ~ 5)
  )) %>%
  rowwise() %>%
  mutate(positive = mean(c(panas_1, panas_3, panas_5)),
         negative = mean(c(panas_2, panas_4, panas_6))) %>%
  ungroup() %>%
  mutate(across(
    c(contains("agreeable_conscien_")),
    ~ case_when(
      . == "1" ~ 1,
      . == "2" ~ 2,
      . == "3" ~ 3,
      . == "4" ~ 4,
      . == "5" ~ 5,
      . == "6" ~ 6,
      . == "7" ~ 7
    )
  )) %>%
  mutate(across(
    c(agreeable_conscien_1, agreeable_conscien_3),
    ~ case_when(
      . == "7" ~ 1,
      . == "6" ~ 2,
      . == "5" ~ 3,
      . == "4" ~ 4,
      . == "3" ~ 5,
      . == "2" ~ 6,
      . == "1" ~ 7
    )
  )) %>%
  rowwise() %>%
  mutate(agreeableness = mean(c(
    agreeable_conscien_1, agreeable_conscien_2
  )),
  conscientiousness = mean(c(
    agreeable_conscien_3, agreeable_conscien_4
  ))) %>%
  ungroup() %>%
  mutate(
    income = case_when(
      hh_income == "1" ~ 1,
      hh_income == "2" ~ 2,
      hh_income == "3" ~ 3,
      hh_income == "4" ~ 4,
      hh_income == "5" ~ 5,
      hh_income == "6" ~ 6,
      hh_income == "7" ~ 7,
      hh_income == "8" ~ 8,
      hh_income == "9" ~ 9,
      hh_income == "10" ~ 10,
      hh_income == "11" ~ 11,
      hh_income == "12" ~ 12,
      hh_income == "13" ~ 13,
      hh_income == "14" ~ 14,
      hh_income == "15" ~ 15,
      hh_income == "16" ~ 16,
      hh_income == "17" ~ as.numeric(NA)
    )) %>%
  mutate(across(c(contains("pto")), ~ as.numeric(.))) %>%
  rename(
    act_commuting = pto_activities_1,
    act_working = pto_activities_2,
    act_shopping = pto_activities_3,
    act_preparingfood = pto_activities_4,
    act_housework = pto_activities_5,
    act_childcare = pto_activities_6,
    act_eating = pto_activities_7,
    act_praying = pto_activities_8,
    act_phone = pto_activities_9,
    act_tv = pto_activities_10,
    act_naprest = pto_activities_11,
    act_computer = pto_activities_12,
    act_relaxing = pto_activities_13,
    act_socializing = pto_activities_14,
    act_intimate_relations = pto_activities_15,
    act_excercising = pto_activities_16,
    act_doingnothing = pto_activities_17,
    act_waiting = pto_activities_18
  ) %>%
  mutate(
    sex=case_when(
      Gender=="1" ~ "Male",
      Gender=="-1" ~ "Female"
    ),
    age.numeric=as.numeric(Age),
  ) %>%
  mutate(across(c(starts_with("act"), age.numeric), ~ center_this(.x))) %>%
  distinct()

data$incomeFac<-factor(data$income)

DVs <- c("positive", "negative")
traits <- c("agreeableness", "conscientiousness", "incomeFac")


models_list <- list()


for (i in 1:length(DVs)) {
  # loop over DVs
  for (j in 1:length(traits)) {
    # loop over personality traits
    
    models <- eval(parse(
      text = paste0(
        "lm(",
        DVs[i],
        " ~ 1 + sex+age.numeric+
      (act_commuting+
      act_working+
        act_shopping+
        act_preparingfood+
        act_housework+
        act_childcare+
        act_eating+
        act_praying + 
        act_phone + 
        act_tv + 
        act_naprest +
        act_computer+ 
        act_relaxing+
        act_socializing+
        act_intimate_relations+
        act_excercising+
        act_doingnothing+
        act_waiting)*",
        traits[j],
        ", na.action = na.exclude, data = data)"
      )
    ))
    
    # Save model outputs in lists:
    models_list <- append(models_list, models) # list of models
    print(paste(DVs[i], traits[j])) # prints where you are at
  }
}


positive_agreeableness<- lm(positive~sex+age.numeric+ ((act_commuting+ act_working+ act_shopping+act_preparingfood+
     act_housework+
     act_childcare+
     act_eating+
     act_praying + 
     act_phone + 
     act_tv + 
     act_naprest +
     act_computer+ 
     act_relaxing+
     act_socializing+
     act_intimate_relations+
     act_excercising+
     act_doingnothing+
     act_waiting)*agreeableness), data=data)
summary(positive_agreeableness)

negative_agreeableness<- lm(negative~sex+age.numeric+ ((act_commuting+ act_working+ act_shopping+act_preparingfood+
                                                          act_housework+
                                                          act_childcare+
                                                          act_eating+
                                                          act_praying + 
                                                          act_phone + 
                                                          act_tv + 
                                                          act_naprest +
                                                          act_computer+ 
                                                          act_relaxing+
                                                          act_socializing+
                                                          act_intimate_relations+
                                                          act_excercising+
                                                          act_doingnothing+
                                                          act_waiting)*agreeableness), data=data)

summary(negative_agreeableness)


positive_conscientiousness<- lm(positive~sex+age.numeric+ ((act_commuting+ act_working+ act_shopping+act_preparingfood+
                                                              act_housework+
                                                              act_childcare+
                                                              act_eating+
                                                              act_praying + 
                                                              act_phone + 
                                                              act_tv + 
                                                              act_naprest +
                                                              act_computer+ 
                                                              act_relaxing+
                                                              act_socializing+
                                                              act_intimate_relations+
                                                              act_excercising+
                                                              act_doingnothing+
                                                              act_waiting)*conscientiousness), data=data)
summary(positive_conscientiousness)

negative_conscientiousness<- lm(negative~sex+age.numeric+ ((act_commuting+ act_working+ act_shopping+act_preparingfood+
                                                              act_housework+
                                                              act_childcare+
                                                              act_eating+
                                                              act_praying + 
                                                              act_phone + 
                                                              act_tv + 
                                                              act_naprest +
                                                              act_computer+ 
                                                              act_relaxing+
                                                              act_socializing+
                                                              act_intimate_relations+
                                                              act_excercising+
                                                              act_doingnothing+
                                                              act_waiting)*conscientiousness), data=data)

summary(negative_conscientiousness)


positive_incomeFac<- lm(positive~sex+age.numeric+ ((act_commuting+ act_working+ act_shopping+act_preparingfood+
                                                      act_housework+
                                                      act_childcare+
                                                      act_eating+
                                                      act_praying + 
                                                      act_phone + 
                                                      act_tv + 
                                                      act_naprest +
                                                      act_computer+ 
                                                      act_relaxing+
                                                      act_socializing+
                                                      act_intimate_relations+
                                                      act_excercising+
                                                      act_doingnothing+
                                                      act_waiting)*incomeFac), data=data)
summary(positive_incomeFac)

negative_incomeFac<- lm(negative~sex+age.numeric+ ((act_commuting+ act_working+ act_shopping+act_preparingfood+
                                                      act_housework+
                                                      act_childcare+
                                                      act_eating+
                                                      act_praying + 
                                                      act_phone + 
                                                      act_tv + 
                                                      act_naprest +
                                                      act_computer+ 
                                                      act_relaxing+
                                                      act_socializing+
                                                      act_intimate_relations+
                                                      act_excercising+
                                                      act_doingnothing+
                                                      act_waiting)*incomeFac), data=data)

summary(negative_incomeFac)
