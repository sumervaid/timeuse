library(haven)
library(tidyverse)
data<-read_sav("Time Poverty - Master Data File with Composites - v2.sav")
# E variables are Endline surveys
# B variables are Baseline surveys (Week 1)
# LOTR = Optimism (Endlin
# CESD = Depression (Endline)
### things to clarify with Ashley: 
# Does "V1" correspond to particpant id? 
# What are the "W2.1", "W3.1" etc? 
# What are the "A" columns referring to? 

diary<-data %>%
  dplyr::select("V1",contains(c("LOTR", "CESD","SPANE", "PSS", "hobbies_hours", 
                                             "commute_hours", 
                                             "working_hours", 
                                             "shopping_hours",
                                             "foodprep_hours",
                                             "housework_hours",
                                             "childcare_hours",
                                             "eating_hours",
                                             "pray_hours",
                                             "phone_hours",
                                             "tv_hours",
                                             "nap_rest_hours",
                                             "computer_hours",
                                             "soc_hours",
                                             "intimate_hours",
                                             "exercise_hours",
                                             "nothing_hours",
                                             "waiting_hours"))) %>%
  dplyr::select(-(contains(c("A_", "A_starttime", "W2.1", "W3.1", "W4.1", "W5.1")))) %>%
  pivot_longer(B_T1_hobbies_hours:W5_waiting_hours, names_to="Activity_Week", values_to="hours") %>%
  mutate(time=case_when(
    str_detect(Activity_Week, "B_T1") ~ 1,
    str_detect(Activity_Week, "W2_") ~ 2,
    str_detect(Activity_Week, "W3_") ~ 3,
    str_detect(Activity_Week, "W4_") ~ 4,
    str_detect(Activity_Week, "W5_") ~ 5,
    str_detect(Activity_Week, "E_T2") ~ 6,
  ),
  activity=case_when(
    str_detect(Activity_Week, "childcare_hours") ~ "Childcare",
    str_detect(Activity_Week, "commute_hours") ~ "Commute",
    str_detect(Activity_Week, "computer_hours") ~ "Computer",
    str_detect(Activity_Week, "eating_hours") ~ "Eating",
    str_detect(Activity_Week, "exercise_hours") ~ "Excercise",
    str_detect(Activity_Week, "foodprep_hours") ~ "Foodprep",
    str_detect(Activity_Week, "hobbies_hours") ~ "Hobbies",
    str_detect(Activity_Week, "housework_hours") ~ "Housework",
    str_detect(Activity_Week, "intimate_hours") ~ "Intimate",
    str_detect(Activity_Week, "nap_rest_hours") ~ "Nap/Rest",
    str_detect(Activity_Week, "nothing_hours") ~ "Nothing",
    str_detect(Activity_Week, "phone_hours") ~ "Phone",
    str_detect(Activity_Week, "pray_hours") ~ "Pray",
    str_detect(Activity_Week, "shopping_hours") ~ "Shopping",
    str_detect(Activity_Week, "soc_hours") ~ "Socializing",
    str_detect(Activity_Week, "tv_hours") ~ "Television",
    str_detect(Activity_Week, "waiting_hours") ~ "Waiting",
    str_detect(Activity_Week, "working_hours") ~ "Working"
  ))

timeuse<-diary %>%
  select(V1, activity, hours, time) %>%
  pivot_wider(names_from="activity", values_from="hours") %>%
  distinct()

wellbeing<-data %>%
  dplyr::select("V1", contains(c("SPANE_PA", "SPANE_NA"))) %>%
  dplyr::select(-(contains(c("A_", "A_starttime", "W2.1", "W3.1", "W4.1", "W5.1")))) %>%
  pivot_longer(B_SPANE_PA:W5_SPANE_NA, names_to="wellbeing_time", values_to="value") %>%
  mutate(time=case_when(
    str_detect(wellbeing_time, "B_SPANE") ~ 1,
    str_detect(wellbeing_time, "W2_SPANE") ~ 2,
    str_detect(wellbeing_time, "W3_SPANE") ~ 3,
    str_detect(wellbeing_time, "W4_SPANE") ~ 4,
    str_detect(wellbeing_time, "W5_SPANE") ~ 5,
    str_detect(wellbeing_time, "E_SPANE") ~ 6,
  ),
  wellbeing=case_when(
    str_detect(wellbeing_time, "_PA") ~ "Positive Affect",
    str_detect(wellbeing_time, "_NA") ~ "Negative Affect"
  )) %>%
  select(V1, wellbeing, value, time) %>%
  pivot_wider(names_from="wellbeing", values_from="value") %>%
  distinct()


pss<-data %>%
  dplyr::select("V1",ends_with("_PSS")) %>%
  dplyr::select(-(contains(c("A_", "ZB_","A_starttime", "W2.1", "W3.1", "W4.1", "W5.1")))) %>%
  pivot_longer(B_PSS:W5_PSS, names_to="wellbeing_time", values_to="value") %>%
  mutate(time=case_when(
    str_detect(wellbeing_time, "B_PSS") ~ 1,
    str_detect(wellbeing_time, "W2_PSS") ~ 2,
    str_detect(wellbeing_time, "W3_PSS") ~ 3,
    str_detect(wellbeing_time, "W4_PSS") ~ 4,
    str_detect(wellbeing_time, "W5_PSS") ~ 5,
    str_detect(wellbeing_time, "E_PSS") ~ 6,
  ),
  wellbeing=case_when(
    str_detect(wellbeing_time, "_PSS") ~ "Stress"
  )) %>%
  select(V1, wellbeing, value, time) %>%
  pivot_wider(names_from="wellbeing", values_from="value") %>%
  distinct()

diary<- full_join(timeuse, wellbeing, by=c("V1", "time")) %>%
  full_join(pss, by=c("V1", "time"))

optimism<-data %>%
  dplyr::select("V1",contains("_LOTR")) %>%
  dplyr::select(-c(contains("E_"))) %>%
  dplyr::mutate(B_T1_LOTR_2_r=case_when(
    B_T1_LOTR_2==5 ~ 1,
    B_T1_LOTR_2==4 ~ 2, 
    B_T1_LOTR_2==3 ~ 3,
    B_T1_LOTR_2==2 ~ 4,
    B_T1_LOTR_2==5 ~ 1
  )) %>%
  ungroup() %>%
  rowwise() %>%
  dplyr::mutate(
    optimism=mean(c(B_T1_LOTR_1, B_T1_LOTR_2_r, B_T1_LOTR_3, B_T1_LOTR_4, B_T1_LOTR_5, B_T1_LOTR_6), na.rm=TRUE)
  ) %>%
  dplyr::select(V1, optimism)

depression<- data %>%
  dplyr::select("V1","B_CESD")


center_this <- function(x) {
  x - mean(x, na.rm = TRUE)
}

dispositions<-full_join(depression, optimism, by=c("V1")) %>%
  ungroup() %>%
  mutate(
    depression_c=center_this(B_CESD),
    optimism_c=center_this(optimism)
  ) %>%
  select(-c(B_CESD, optimism))
  
analyze<-full_join(diary, dispositions, by="V1")


### Creating within and between-person components 
analyze<-analyze %>%
  dplyr::group_by(V1) %>%
  dplyr::mutate(across(
    Hobbies:Waiting, ~center_this(.x), .names="wp_{.col}"))%>%
  dplyr::ungroup() %>%
  dplyr::mutate(across(Hobbies:Waiting, ~mean(.x, na.rm=TRUE), .names="bp_{.col}")) %>%
  dplyr::mutate_all(~ifelse(is.nan(.), NA, .)) 


### Optimism Moderator 
model1_opt<-lmerTest::lmer(Stress~((wp_Hobbies+wp_Commute+wp_Working+wp_Shopping+wp_Foodprep+
                         wp_Housework+wp_Childcare+wp_Eating+wp_Pray+wp_Phone+wp_Television
                       +`wp_Nap/Rest`+wp_Computer+wp_Socializing+wp_Intimate+wp_Excercise+wp_Nothing+wp_Waiting)*optimism_c)+(1|V1), data=analyze)
summary(model1_opt)
model2_opt<-lmerTest::lmer(`Positive Affect`~((wp_Hobbies+wp_Commute+wp_Working+wp_Shopping+wp_Foodprep+
                                  wp_Housework+wp_Childcare+wp_Eating+wp_Pray+wp_Phone+wp_Television
                                +`wp_Nap/Rest`+wp_Computer+wp_Socializing+wp_Intimate+wp_Excercise+wp_Nothing+wp_Waiting)*optimism_c)+(1|V1), data=analyze)
summary(model2_opt)
model3_opt<-lmerTest::lmer(`Negative Affect`~((wp_Hobbies+wp_Commute+wp_Working+wp_Shopping+wp_Foodprep+
                                             wp_Housework+wp_Childcare+wp_Eating+wp_Pray+wp_Phone+wp_Television
                                           +`wp_Nap/Rest`+wp_Computer+wp_Socializing+wp_Intimate+wp_Excercise+wp_Nothing+wp_Waiting)*optimism_c)+(1|V1), data=analyze)

summary(model3_opt)

### Depression Moderator 

model1_cesd<-lmerTest::lmer(Stress~((wp_Hobbies+wp_Commute+wp_Working+wp_Shopping+wp_Foodprep+
                                       wp_Housework+wp_Childcare+wp_Eating+wp_Pray+wp_Phone+wp_Television
                                     +`wp_Nap/Rest`+wp_Computer+wp_Socializing+wp_Intimate+wp_Excercise+wp_Nothing+wp_Waiting)*depression_c)+(1|V1), data=analyze)
summary(model1_cesd)
model2_cesd<-lmerTest::lmer(`Positive Affect`~((wp_Hobbies+wp_Commute+wp_Working+wp_Shopping+wp_Foodprep+
                                                  wp_Housework+wp_Childcare+wp_Eating+wp_Pray+wp_Phone+wp_Television
                                                +`wp_Nap/Rest`+wp_Computer+wp_Socializing+wp_Intimate+wp_Excercise+wp_Nothing+wp_Waiting)*depression_c)+(1|V1), data=analyze)
summary(model2_cesd)
model3_cesd<-lmerTest::lmer(`Negative Affect`~((wp_Hobbies+wp_Commute+wp_Working+wp_Shopping+wp_Foodprep+
                                                  wp_Housework+wp_Childcare+wp_Eating+wp_Pray+wp_Phone+wp_Television
                                                +`wp_Nap/Rest`+wp_Computer+wp_Socializing+wp_Intimate+wp_Excercise+wp_Nothing+wp_Waiting)*depression_c)+(1|V1), data=analyze)

summary(model3_cesd)
