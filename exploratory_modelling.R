library(tidyverse)
library(lme4)
library(lmerTest)
data<-read_csv("covid_t1_t2_final.csv")

#### Affect Wellbeing: 

##Positive 
data$spane_1_t1
data$spane_1_t2

## Negative
data$spane_2_t1
data$spane_2_t2

## Good
data$spane_3_t1
data$spane_3_t2

## Bad
data$spane_4_t1
data$spane_4_t2

### Pleasant
data$spane_5_t1
data$spane_5_t2

### Unpleasant
data$spane_6_t1
data$spane_6_t2

#### Global Happiness
data$happiness_t1
data$happiness_t2

#### Meaning in Life: 

data$meaningpresence_1_t1
data$meaningpresence_1_t2

data$meaningpresence_2_t1
data$meaningpresence_1_t2

data$meaningpresence_3_t1
data$meaningpresence_3_t2


###### Calculating Global Positive Emotion
select_vars<-data[,c("spane_1_t1", "spane_3_t1", "spane_5_t1")]
data$positive_t1<-rowMeans(select_vars, na.rm = TRUE)
head(data$positive_t1)

select_vars<-data[,c("spane_1_t2", "spane_3_t2", "spane_5_t2")]
data$positive_t2<-rowMeans(select_vars, na.rm = TRUE)
head(data$positive_t2)

##### Calculating Global Negative Emotion
select_vars<-data[,c("spane_2_t1", "spane_4_t1", "spane_6_t1")]
data$negative_t1<-rowMeans(select_vars, na.rm = TRUE)
head(data$negative_t1)

select_vars<-data[,c("spane_2_t2", "spane_4_t2", "spane_6_t2")]
data$negative_t2<-rowMeans(select_vars, na.rm = TRUE)
head(data$negative_t2)

#### Calculating Global Meaning in Life

select_vars<-data[,c("meaningpresence_1_t1", "meaningpresence_2_t1", "meaningpresence_3_t1")]
data$meaning_t1<-rowMeans(select_vars, na.rm = TRUE)
data$meaning_t1[is.nan(data$meaning_t1)]<-NA
head(data$meaning_t1)

select_vars<-data[,c("meaningpresence_1_t2", "meaningpresence_2_t2", "meaningpresence_3_t2")]
data$meaning_t2<-rowMeans(select_vars, na.rm = TRUE)
data$meaning_t2[is.nan(data$meaning_t2)]<-NA
head(data$meaning_t2)


#### Subsetting relevant data
model_data<-data %>%
  dplyr::select(id,race_t1, age_t1,gender_t1, positive_t1, positive_t2, negative_t1, negative_t2, meaning_t1, meaning_t2,happiness_t1, happiness_t2,
                socialmedia_t1, socialmedia_t2, watchingtv_t1, watchingtv_t2, onthephone_t1, onthephone_t2, computerinternetemail_t1, computerinternetemail_t2)


data_1<-model_data %>%
  dplyr::select(id, age_t1, gender_t1, race_t1, positive_t1, positive_t2) %>%
  pivot_longer(
    cols=positive_t1:positive_t2,
    names_to = "Time",
    values_to="Positive"
  ) %>%
  separate(Time, c("r", "t"), sep="_") %>%
  select(-r)

data_2<-model_data %>%
  dplyr::select(id, socialmedia_t1, socialmedia_t2) %>%
  pivot_longer(
    cols=socialmedia_t1:socialmedia_t2,
    names_to = "Time",
    values_to="Socialmedia"
  ) %>%
  separate(Time, c("r", "t"), sep="_") %>%
  select(-r)

data_3<-model_data %>%
  dplyr::select(id,negative_t1, negative_t2) %>%
  pivot_longer(
    cols=negative_t1:negative_t2,
    names_to = "Time",
    values_to="Negative"
  ) %>%
  separate(Time, c("r", "t"), sep="_") %>%
  select(-r)

data_4<-model_data %>%
  dplyr::select(id, meaning_t1, meaning_t2) %>%
  pivot_longer(
    cols=meaning_t1:meaning_t2,
    names_to = "Time",
    values_to="Meaning"
  ) %>%
  separate(Time, c("r", "t"), sep="_") %>%
  select(-r)

data_5<-model_data %>%
  dplyr::select(id,happiness_t1, happiness_t2) %>%
  pivot_longer(
    cols=happiness_t1:happiness_t2,
    names_to = "Time",
    values_to="Happy"
  ) %>%
  separate(Time, c("r", "t"), sep="_") %>%
  select(-r)

data_6<-model_data %>%
  dplyr::select(id,watchingtv_t1, watchingtv_t2) %>%
  pivot_longer(
    cols=watchingtv_t1:watchingtv_t2,
    names_to = "Time",
    values_to="TV"
  ) %>%
  separate(Time, c("r", "t"), sep="_") %>%
  select(-r)


data_7<-model_data %>%
  dplyr::select(id,onthephone_t1, onthephone_t2) %>%
  pivot_longer(
    cols=onthephone_t1:onthephone_t2,
    names_to = "Time",
    values_to="Phone"
  ) %>%
  separate(Time, c("r", "t"), sep="_") %>%
  select(-r)

data_8<-model_data %>%
  dplyr::select(id, computerinternetemail_t1, computerinternetemail_t2) %>%
  pivot_longer(
    cols=computerinternetemail_t1:computerinternetemail_t2,
    names_to = "Time",
    values_to="Computer"
  ) %>%
  separate(Time, c("r", "t"), sep="_") %>%
  select(-r)


final<- data_1 %>%
  full_join(data_2, by=c("id", "t")) %>%
  full_join(data_3,by=c("id", "t")) %>%
  full_join(data_4, by=c("id", "t")) %>%
  full_join(data_5, by=c("id", "t")) %>%
  full_join(data_6, by=c("id", "t")) %>%
  full_join(data_7, by=c("id", "t")) %>%
  full_join(data_8, by=c("id", "t")) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  mutate(
    race=case_when(
      race_t1=="1" ~ "White",
      race_t1=="2" ~ "Black",
      race_t1=="3" ~ "Native American",
      race_t1=="4" ~ "Asian",
      race_t1=="5" ~ "Native Hawaiian",
      race_t1=="6" ~ "Hispanic",
      race_t1=="7" ~ "Middle Eastern"
    )
  )

final$race<-as.factor(final$race)

#### Models 
model_happy<-lmer(
  Happy ~ age_t1 + gender_t1 + Socialmedia + TV + Phone+Computer+(1|id), data=final
)
summary(model_happy)

model_positive<-lmer(
  Positive ~ age_t1 + gender_t1 + Socialmedia + TV + Phone+Computer+(1|id), data=final
)
summary(model_positive)

model_negative<-lmer(
  Negative ~ age_t1 + gender_t1 + Socialmedia + TV + Phone+Computer+(1|id), data=final
)
summary(model_negative)

model_meaning<-lmer(
  Meaning ~ age_t1 + gender_t1 + Socialmedia + TV + Phone+Computer+(1|id), data=final
)
summary(model_meaning)


library(sjPlot)
tab_model(model_happy, model_positive, model_negative, model_meaning,
          dv.labels = c("Happy", "Positive", "Negative", "Meaningfulness"),
          pred.labels= c("Intercept","Age", "Gender", "Social Media", "TV", "Phone Use", "Computer Use"))

