library(tidyverse)
library(lme4)
library(lmerTest)
library(interactions)
library(jtools)
data<-read_csv("covid_t1_t2_final.csv")

#### Creating some ggplot themes
theme_sumer<-function(){
  theme_minimal %+replace%
    theme(
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 20,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 14),               #font size
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 10),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 9),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10))
    )
}


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

library(reshape2)

model_data<-data %>%
  dplyr::select(id,race_t1, currentincome_t1, age_t1,gender_t1,personality1_t1,personality2_t1, personality4_t1, personality3_t1, positive_t1, positive_t2, negative_t1, negative_t2, meaning_t1, meaning_t2,happiness_t1, happiness_t2,
                socialmedia_t1, socialmedia_t2, socializing_t1, socializing_t2, watchingtv_t1, watchingtv_t2, onthephone_t1, onthephone_t2, computerinternetemail_t1, computerinternetemail_t2,
                commuting_t1, commuting_t2, working_t1, working_t2, schoollearning_t1, schoollearning_t2, shopping_t1, shopping_t2,
                personalhygiene_t1, personalhygiene_t2, preparingfood_t1, preparingfood_t2, doinghousework_t1, doinghousework_t2,
                eating_t1, eating_t2, praying_t1, praying_t2, nappingresting_t1, nappingresting_t2, relaxing_t1, relaxing_t2,
                doingnothing_t1, doingnothing_t1, exercising_t1, exercising_t2, hobbies_t1, hobbies_t2, goingoutdoors_t1, goingoutdoors_t2, doingnothing_t1, doingnothing_t2)%>%
  dplyr::mutate(
    gender_r=case_when(
      gender_t1 == 1 ~ "Male",
      gender_t1 == 2 ~ "Female",
      gender_t1 == 3 ~ "Non-Binary"
    )
  ) %>%
  reshape2::melt(
    id=c("id", "race_t1", "gender_t1","gender_r","age_t1", "currentincome_t1",  "personality1_t1","personality2_t1","personality4_t1","personality3_t1")
  ) %>%
  separate(variable, c("Var", "t"), sep="_") %>%
  tidyr::pivot_wider(
    names_from=Var,
    values_from=value
  ) %>%
  dplyr::rename(
    race=race_t1,
    age=age_t1,
    gender=gender_t1,
    stable=personality1_t1,
    optimistic=personality2_t1,
    extraverted=personality3_t1,
    open=personality4_t1
  ) %>% 
  mutate_all(~ifelse(is.nan(.), NA, .)) %>%
  mutate(
    race=case_when(
      race=="1" ~ "White",
      race=="2" ~ "Black",
      race=="3" ~ "Native American",
      race=="4" ~ "Asian",
      race=="5" ~ "Native Hawaiian",
      race=="6" ~ "Hispanic",
      race=="7" ~ "Middle Eastern"
    ),
    income=case_when(
      currentincome_t1 == "1" ~ 1,
      currentincome_t1 == "2" ~ 2,
      currentincome_t1 == "3" ~ 3,
      currentincome_t1 == "4" ~ 4,
      currentincome_t1 == "5" ~ 5,
      currentincome_t1 == "6" ~ 6,
      currentincome_t1 == "7" ~ 7,
      currentincome_t1 == "8" ~ 8,
      currentincome_t1 == "9" ~ 9,
      currentincome_t1 == "10" ~ 10,
      currentincome_t1 == "11" ~ 11,
      currentincome_t1 == "12" ~ 12,
      currentincome_t1 == "13" ~ 13
    )
  ) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(across(
    socialmedia:goingoutdoors, ~mean(.x, na.rm=TRUE), .names="wp_{.col}"))%>%
  dplyr::ungroup() %>%
  dplyr::mutate(across(socialmedia:goingoutdoors, ~mean(.x, na.rm=TRUE), .names="bp_{.col}")) %>%
  dplyr::mutate_all(~ifelse(is.nan(.), NA, .)) 

model_data$race<-as.factor(model_data$race)
model_data$incomeFac<-as.factor(model_data$income)


model_data_plot<-model_data %>%
  mutate(
    Time=case_when(
      t=="t1" ~ "Time 1",
      t=="t2" ~ "Time 2",
    ))
  

positive<-ggplot(data=model_data_plot, aes(x=positive, color=Time, fill=Time)) +  geom_density(alpha=.2) +scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#868686FF", "#EFC000FF")) + xlab("Positive") + ylab("Density") + theme_minimal() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                  panel.background = element_blank(), axis.text = element_text(size = 15), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
                                                                                                                                  legend.title=element_text(size=15), 
                                                                                                                                  legend.text=element_text(size=15))

negative<-ggplot(data=model_data_plot, aes(x=negative, color=Time, fill=Time)) +  geom_density(alpha=.2) +scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#868686FF", "#EFC000FF")) + xlab("Negative") + ylab("Density") + theme_minimal() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                          panel.background = element_blank(), axis.text = element_text(size = 15), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
                                                                                                                          legend.title=element_text(size=15), 
                                                                                                                          legend.text=element_text(size=15))

meaning<-ggplot(data=model_data_plot, aes(x=meaning, color=Time, fill=Time)) +  geom_density(alpha=.2) +scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#868686FF", "#EFC000FF")) + xlab("Meaning") + ylab("Density") + theme_minimal() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                          panel.background = element_blank(), axis.text = element_text(size = 15), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
                                                                                                                          legend.title=element_text(size=15), 
                                                                                                                          legend.text=element_text(size=15))

happy<-ggplot(data=model_data_plot, aes(x=happiness, color=Time, fill=Time)) +  geom_density(alpha=.2) +scale_color_manual(values = c("#868686FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#868686FF", "#EFC000FF")) + xlab("Happy") + ylab("Density") + theme_minimal() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                          panel.background = element_blank(), axis.text = element_text(size = 15), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
                                                                                                                          legend.title=element_text(size=15), 
                                                                                                                          legend.text=element_text(size=15))


#### Person-Mean Centering all the IVs
model_data$socialmedia<-model_data$socialmedia-model_data$wp_socialmedia
model_data$socializing<-model_data$socializing-model_data$wp_socializing
model_data$watchingtv<-model_data$watchingtv-model_data$wp_watchingtv
model_data$onthephone<-model_data$onthephone-model_data$wp_onthephone
model_data$computerinternetemail<-model_data$computerinternetemail-model_data$wp_computerinternetemail
model_data$commuting<-model_data$commuting-model_data$wp_commuting
model_data$working<-model_data$working-model_data$wp_working
model_data$schoollearning<-model_data$schoollearning-model_data$wp_schoollearning
model_data$shopping<-model_data$shopping-model_data$wp_shopping
model_data$personalhygiene<-model_data$personalhygiene-model_data$wp_personalhygiene
model_data$preparingfood<-model_data$preparingfood-model_data$wp_preparingfood
model_data$doinghousework<-model_data$doinghousework-model_data$wp_doinghousework
model_data$eating<-model_data$eating-model_data$wp_eating
model_data$praying<-model_data$praying-model_data$wp_praying
model_data$nappingresting<-model_data$nappingresting-model_data$wp_nappingresting
model_data$relaxing<-model_data$relaxing-model_data$wp_relaxing
model_data$doingnothing<-model_data$doingnothing-model_data$wp_doingnothing
model_data$exercising<-model_data$exercising-model_data$wp_exercising
model_data$hobbies<-model_data$hobbies-model_data$wp_hobbies
model_data$goingoutdoors<-model_data$goingoutdoors-model_data$wp_goingoutdoors

#### Sample-Mean Centering all the Between-Person Components of the IVs

model_data$bp_socialmedia<-model_data$wp_socialmedia-model_data$bp_socialmedia
model_data$bp_socializing<-model_data$wp_socializing-model_data$bp_socializing
model_data$bp_watchingtv<-model_data$wp_watchingtv-model_data$bp_watchingtv
model_data$bp_onthephone<-model_data$wp_onthephone-model_data$bp_onthephone
model_data$bp_computerinternetemail<-model_data$wp_computerinternetemail-model_data$bp_computerinternetemail
model_data$bp_commuting<-model_data$wp_commuting-model_data$bp_commuting
model_data$bp_working<-model_data$wp_working-model_data$bp_working
model_data$bp_schoollearning<-model_data$wp_schoollearning-model_data$bp_schoollearning
model_data$bp_shopping<-model_data$wp_shopping-model_data$bp_shopping
model_data$bp_personalhygiene<-model_data$wp_personalhygiene-model_data$bp_personalhygiene
model_data$bp_preparingfood<-model_data$wp_preparingfood-model_data$bp_preparingfood
model_data$bp_doinghousework<-model_data$wp_doinghousework-model_data$bp_doinghousework
model_data$bp_eating<-model_data$wp_eating-model_data$bp_eating
model_data$bp_praying<-model_data$wp_praying-model_data$bp_praying
model_data$bp_nappingresting<-model_data$wp_nappingresting-model_data$bp_nappingresting
model_data$bp_relaxing<-model_data$wp_relaxing-model_data$bp_relaxing
model_data$bp_doingnothing<-model_data$wp_doingnothing-model_data$bp_doingnothing
model_data$bp_exercising<-model_data$wp_exercising-model_data$bp_exercising
model_data$bp_hobbies<-model_data$wp_hobbies-model_data$bp_hobbies
model_data$bp_goingoutdoors<-model_data$wp_goingoutdoors-model_data$bp_goingoutdoors


#### Baseline Models 

DVs<-c("positive", "negative", "meaning", "happiness")
traits<-c("extraverted", "optimistic", "stable","open")

models_list<-list()


for(i in 1:length(DVs)) { # loop over DVs
  for(j in 1:length(traits)) { # loop over personality traits

    models <- eval(parse(text = paste0(
      "lmer(", DVs[i], " ~ 1 + race+gender+age+(socialmedia+socializing+watchingtv+onthephone+computerinternetemail+commuting+working+schoollearning+shopping + personalhygiene + preparingfood + doinghousework +eating+ praying+nappingresting+relaxing+doingnothing+exercising+hobbies+goingoutdoors)*", 
      traits[j],
      " + (1 | id), na.action = na.exclude, data = model_data)")))
    
    # Save model outputs in lists:
    models_list <- append(models_list, models) # list of models
    print(paste(DVs[i], traits[j])) # prints where you are at
  }
}



#### Inspecting all summaries
#happiness_stability<-summary(models_list[[16]])
#happiness_optimistic<-summary(models_list[[15]])
#happiness_extraverted<-summary(models_list[[14]])
#meaning_open<-summary(models_list[[13]])
#meaning_stablitity<-summary(models_list[[12]])
#meaning_optimistic<-summary(models_list[[11]])
#meaning_extraverted<-summary(models_list[[10]])
#negative_openness<-summary(models_list[[9]])
#negative_stablility<-summary(models_list[[8]])
#negative_optimistic<-summary(models_list[[7]])
#negative_extraverted<-summary(models_list[[6]])
#positive_open<-summary(models_list[[5]])
#positive_stability<-summary(models_list[[4]])
#positive_optimistic<-summary(models_list[[3]])
#positive_extraverted<-summary(models_list[[2]])
#happiness_open<-summary(models_list[[1]])

#### Inspect All Summaries 

positive_extraverted<-summary(models_list[[1]])
positive_optimistic<-summary(models_list[[2]])
positive_stable<-summary(models_list[[3]])
positive_open<-summary(models_list[[4]])
negative_extraverted<-summary(models_list[[5]])
negative_optimistic<-summary(models_list[[6]])
negative_stable<-summary(models_list[[7]])
negative_open<-summary(models_list[[8]])
meaning_extraverted<-summary(models_list[[9]])
meaning_optimistic<-summary(models_list[[10]])
meaning_stable<-summary(models_list[[11]])
meaning_open<-summary(models_list[[12]])
happiness_extraverted<-summary(models_list[[13]])
happiness_optimistic<-summary(models_list[[14]])
happiness_stable<-summary(models_list[[15]])
happiness_open<-summary(models_list[[16]])


#### Probing relevant Interactions

library(jtools)
library(wesanderson)
#########################  Happiness DV
# Stability Moderator


model<-models_list[[15]]
eating_stable_happiness<-sim_slopes(model, pred = "eating", modx="stable")
plot<-interact_plot(model, pred=eating, modx=stable) + xlab("Time Spent Eating") + ylab("Happiness") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Stability", `- 1 SD`="Low Stability") 

eating_stable_happiness_s<-ggplot(data=probe, aes(x=eating, y=happiness, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Eating (Hours)") + ylab("Happiness")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))


happiness_optimistic ### no sigmificant interactions

# Extroversion Moderator

## Watching TV
model<-models_list[[13]]
watchingtv_extroversion_happiness<-sim_slopes(model, pred = "watchingtv", modx="extraverted")
plot<-interact_plot(model, pred=watchingtv, modx=extraverted) + xlab("Time Spent Watching TV") + ylab("Happiness") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Extraversion", `- 1 SD`="Low Extraversion") 

watchingtv_extroversion_happiness_s<-ggplot(data=probe, aes(x=watchingtv, y=happiness, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Watching TV (Hours)") + ylab("Happiness")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

## Eating

eating_extroversion_happiness<-sim_slopes(model, pred = "eating", modx="extraverted")
plot<-interact_plot(model, pred=eating, modx=extraverted) + xlab("Time Spent Eating") + ylab("Happiness") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Extraversion", `- 1 SD`="Low Extraversion") 

eating_extroversion_happiness_s<-ggplot(data=probe, aes(x=eating, y=happiness, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Eating (Hours)") + ylab("Happiness")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))


## Napping / Resting

nappingresting_extroversion_happiness<-sim_slopes(model, pred = "nappingresting", modx="extraverted")
plot<-interact_plot(model, pred=nappingresting, modx=extraverted) + xlab("Time Spent Napping/resting") + ylab("Happiness") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Extraversion", `- 1 SD`="Low Extraversion") 

nappingresting_extroversion_happiness_s<-ggplot(data=probe, aes(x=nappingresting, y=happiness, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Napping/Resting (Hours)") + ylab("Happiness")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

summary(models_list[[14]])

# Openness Moderator 

## Watching TV
model<-models_list[[16]]
watchingtv_open_happiness<-sim_slopes(model, pred = "watchingtv", modx="open")
plot<-interact_plot(model, pred=watchingtv, modx=open) + xlab("Time Spent Watching TV (Hours)") + ylab("Happiness") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Openess", `- 1 SD`="Low Openess") 

watchingtv_open_happiness_s<-ggplot(data=probe, aes(x=watchingtv, y=happiness, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Watching TV (Hours)") + ylab("Happiness")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

#########################  Meaning in Life DV
######### Openness

## Time Spent Watching TV
model<-models_list[[12]]
watchingtv_open_meaning<-sim_slopes(model, pred = "watchingtv", modx="open")
plot<-interact_plot(model, pred=watchingtv, modx=open) + xlab("Time Spent Watching TV (Hours)") + ylab("meaning") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Openess", `- 1 SD`="Low Openess") 

watchingtv_open_meaning_s<-ggplot(data=probe, aes(x=watchingtv, y=meaning, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Watching TV (Hours)") + ylab("Meaningfulness")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

## Time Spent Commuting

commuting_open_meaning<-sim_slopes(model, pred = "commuting", modx="open")
plot<-interact_plot(model, pred=commuting, modx=open) + xlab("Time Spent Commuting (Hours)") + ylab("meaning") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Openess", `- 1 SD`="Low Openess") 

commuting_open_meaning_s<-ggplot(data=probe, aes(x=commuting, y=meaning, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Commuting (Hours)") + ylab("Meaningfulness")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))


## Time Spent Working

working_open_meaning<-sim_slopes(model, pred = "working", modx="open")
plot<-interact_plot(model, pred=working, modx=open) + xlab("Time Spent working (Hours)") + ylab("meaning") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Openess", `- 1 SD`="Low Openess") 

working_open_meaning_s<-ggplot(data=probe, aes(x=working, y=meaning, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Working (Hours)") + ylab("Meaningfulness")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

## Time Spent Relaxing

relaxing_open_meaning<-sim_slopes(model, pred = "relaxing", modx="open")
plot<-interact_plot(model, pred=relaxing, modx=open) + xlab("Time Spent relaxing (Hours)") + ylab("meaning") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Openess", `- 1 SD`="Low Openess") 

relaxing_open_meaning_s<-ggplot(data=probe, aes(x=relaxing, y=meaning, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Relaxing (Hours)") + ylab("Meaningfulness")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))


########## Stability

# Commuting 

model<-summary(models_list[[12]])
summary(model)

model<-models_list[[11]]
commuting_stable_meaning<-sim_slopes(model, pred = "commuting", modx="stable")
plot<-interact_plot(model, pred=commuting, modx=stable) + xlab("Time Spent commuting (Hours)") + ylab("meaning") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Stability", `- 1 SD`="Low Stability") 

commuting_stable_meaning_s<-ggplot(data=probe, aes(x=commuting, y=meaning, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Commuting (Hours)") + ylab("Meaningfulness")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

# Praying 

model<-summary(models_list[[11]])
summary(model)

model<-models_list[[11]]
praying_stable_meaning<-sim_slopes(model, pred = "praying", modx="stable")
plot<-interact_plot(model, pred=praying, modx=stable) + xlab("Time Spent praying (Hours)") + ylab("meaning") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Stability", `- 1 SD`="Low Stability") 

praying_stable_meaning_s<-ggplot(data=probe, aes(x=praying, y=meaning, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Praying (Hours)") + ylab("Meaningfulness")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

########## DV: Meaning in Life

## Optimism

model<-(models_list[[10]])
summary(model)
shopping_optimistic_meaning<-sim_slopes(model, pred = "shopping", modx="optimistic")
plot<-interact_plot(model, pred=shopping, modx=optimistic) + xlab("Time Spent shopping (Hours)") + ylab("meaning") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Optimism", `- 1 SD`="Low Optimism") 

shopping_optimistic_meaning_s<-ggplot(data=probe, aes(x=shopping, y=meaning, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Shopping (Hours)") + ylab("Meaningfulness")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))


## Extraversion

# Watching TV
model<-(models_list[[9]])
summary(model)

watchingtv_extraverted_meaning<-sim_slopes(model, pred = "watchingtv", modx="extraverted")
plot<-interact_plot(model, pred=watchingtv, modx=extraverted) + xlab("Time Spent watchingtv (Hours)") + ylab("meaning") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Extraversion", `- 1 SD`="Low Extraversion") 

watchingtv_extraverted_meaning_s<-ggplot(data=probe, aes(x=watchingtv, y=meaning, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Watching TV (Hours)") + ylab("Meaningfulness")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

# Commuting
model<-(models_list[[9]])
summary(model)

commuting_extraverted_meaning<-sim_slopes(model, pred = "commuting", modx="extraverted")
plot<-interact_plot(model, pred=commuting, modx=extraverted) + xlab("Time Spent commuting (Hours)") + ylab("meaning") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Extraversion", `- 1 SD`="Low Extraversion") 

commuting_extraverted_meaning_s<-ggplot(data=probe, aes(x=commuting, y=meaning, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Commuting (Hours)") + ylab("Meaningfulness")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))


# Relaxing

model<-(models_list[[9]])
summary(model)

relaxing_extraverted_meaning<-sim_slopes(model, pred = "relaxing", modx="extraverted")
plot<-interact_plot(model, pred=relaxing, modx=extraverted) + xlab("Time Spent relaxing (Hours)") + ylab("meaning") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Extraversion", `- 1 SD`="Low Extraversion") 

relaxing_extraverted_meaning_s<-ggplot(data=probe, aes(x=relaxing, y=meaning, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Relaxing (Hours)") + ylab("Meaningfulness")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

# Doing Nothing

model<-(models_list[[9]])
summary(model)

doingnothing_extraverted_meaning<-sim_slopes(model, pred = "doingnothing", modx="extraverted")
plot<-interact_plot(model, pred=doingnothing, modx=extraverted) + xlab("Time Spent doingnothing (Hours)") + ylab("meaning") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Extraversion", `- 1 SD`="Low Extraversion") 

doingnothing_extraverted_meaning_s<-ggplot(data=probe, aes(x=doingnothing, y=meaning, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Doing Nothing (Hours)") + ylab("Meaningfulness")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

########## Negative Feelings

#### Openness Trait

## Shopping
model<-(models_list[[8]])

shopping_open_negative<-sim_slopes(model, pred = "shopping", modx="open")
plot<-interact_plot(model, pred=shopping, modx=open) + xlab("Time Spent shopping (Hours)") + ylab("negative") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Openness", `- 1 SD`="Low Openness") 

shopping_open_negative_s<-ggplot(data=probe, aes(x=shopping, y=negative, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Shopping (Hours)") + ylab("Negative")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

## Preparing Food

preparingfood_open_negative<-sim_slopes(model, pred = "preparingfood", modx="open")
plot<-interact_plot(model, pred=preparingfood, modx=open) + xlab("Time Spent preparingfood (Hours)") + ylab("negative") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Openness", `- 1 SD`="Low Openness") 

preparingfood_open_negative_s<-ggplot(data=probe, aes(x=preparingfood, y=negative, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Preparing Food (Hours)") + ylab("Negative")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

## Watching TV 

watchingtv_open_negative<-sim_slopes(model, pred = "watchingtv", modx="open")
plot<-interact_plot(model, pred=watchingtv, modx=open) + xlab("Time Spent watchingtv (Hours)") + ylab("negative") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Openness", `- 1 SD`="Low Openness") 

watchingtv_open_negative_s<-ggplot(data=probe, aes(x=watchingtv, y=negative, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Watching TV (Hours)") + ylab("Negative")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

##### Stability Trait

negative_stablility<-summary(models_list[[8]])

### only trending interactions


##### Optimism Trait

## Shopping

model<-models_list[[6]]

shopping_optimistic_negative<-sim_slopes(model, pred = "shopping", modx="optimistic")
plot<-interact_plot(model, pred=shopping, modx=optimistic) + xlab("Time Spent Shopping (Hours)") + ylab("negative") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Optimism", `- 1 SD`="Low Optimism") 

shopping_optimistic_negative_s<-ggplot(data=probe, aes(x=shopping, y=negative, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Shopping (Hours)") + ylab("Negative")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

## Preparing Food 

preparingfood_optimistic_negative<-sim_slopes(model, pred = "preparingfood", modx="optimistic")
plot<-interact_plot(model, pred=preparingfood, modx=optimistic) + xlab("Time Spent Preparing Food (Hours)") + ylab("negative") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Optimism", `- 1 SD`="Low Optimism") 

preparingfood_optimistic_negative_s<-ggplot(data=probe, aes(x=preparingfood, y=negative, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Preparing Food (Hours)") + ylab("Negative")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

### Social Media 

socialmedia_optimistic_negative<-sim_slopes(model, pred = "socialmedia", modx="optimistic")
plot<-interact_plot(model, pred=socialmedia, modx=optimistic) + xlab("Time Spent Social Media (Hours)") + ylab("negative") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Optimism", `- 1 SD`="Low Optimism") 

socialmedia_optimistic_negative_s<-ggplot(data=probe, aes(x=socialmedia, y=negative, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Using Social Media (Hours)") + ylab("Negative")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

### Praying

praying_optimistic_negative<-sim_slopes(model, pred = "praying", modx="optimistic")
plot<-interact_plot(model, pred=praying, modx=optimistic) + xlab("Time Spent Praying (Hours)") + ylab("negative") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Optimism", `- 1 SD`="Low Optimism") 

praying_optimistic_negative_s<-ggplot(data=probe, aes(x=praying, y=negative, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Praying (Hours)") + ylab("Negative")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))


###### Extraversion Trait 

negative_extraverted<-summary(models_list[[5]])
model<-models_list[[5]]

## Watching TV

watchingtv_extraverted_negative<-sim_slopes(model, pred = "watchingtv", modx="extraverted")
plot<-interact_plot(model, pred=watchingtv, modx=extraverted) + xlab("Time Spent Watching TV (Hours)") + ylab("negative") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Extraversion", `- 1 SD`="Low Extraversion") 

watchingtv_extraverted_negative_s<-ggplot(data=probe, aes(x=watchingtv, y=negative, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Watching TV (Hours)") + ylab("Negative")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

## Eating

eating_extraverted_negative<-sim_slopes(model, pred = "eating", modx="extraverted")
plot<-interact_plot(model, pred=eating, modx=extraverted) + xlab("Time Spent Eating (Hours)") + ylab("negative") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Extraversion", `- 1 SD`="Low Extraversion") 

eating_extraverted_negative_s<-ggplot(data=probe, aes(x=eating, y=negative, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Eating (Hours)") + ylab("Negative")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))


## Relaxing

relaxing_extraverted_negative<-sim_slopes(model, pred = "relaxing", modx="extraverted")
plot<-interact_plot(model, pred=relaxing, modx=extraverted) + xlab("Time Spent Relaxing (Hours)") + ylab("negative") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Extraversion", `- 1 SD`="Low Extraversion") 

relaxing_extraverted_negative_s<-ggplot(data=probe, aes(x=relaxing, y=negative, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Relaxing (Hours)") + ylab("Negative")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))


## Hobbies

hobbies_extraverted_negative<-sim_slopes(model, pred = "hobbies", modx="extraverted")
plot<-interact_plot(model, pred=hobbies, modx=extraverted) + xlab("Time Spent in Hobbies (Hours)") + ylab("negative") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Extraversion", `- 1 SD`="Low Extraversion") 

hobbies_extraverted_negative_s<-ggplot(data=probe, aes(x=hobbies, y=negative, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent in Hobbies (Hours)") + ylab("Negative")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))


################## Positive Outcome

#### Openness Trait

model<-(models_list[[4]])


## Preparing Food 

preparingfood_open_positive<-sim_slopes(model, pred = "preparingfood", modx="open")
plot<-interact_plot(model, pred=preparingfood, modx=open) + xlab("Time Spent Preparing Food (Hours)") + ylab("positive") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Openness", `- 1 SD`="Low Openness") 

preparingfood_open_positive_s<-ggplot(data=probe, aes(x=preparingfood, y=positive, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Preparing Food (Hours)") + ylab("Positive")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

#### Stability Trait
model<-(models_list[[3]])

## Doing Housework
doinghousework_stability_positive<-sim_slopes(model, pred = "doinghousework", modx="stable")
plot<-interact_plot(model, pred=doinghousework, modx=stable) + xlab("Time Spent Preparing Food (Hours)") + ylab("positive") 

doinghousework_Stable_positive<-sim_slopes(model, pred = "doinghousework", modx="stable")
plot<-interact_plot(model, pred=doinghousework, modx=stable) + xlab("Time Spent Preparing Food (Hours)") + ylab("positive") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Stability", `- 1 SD`="Low Stability") 

doinghousework_stability_positive_s<-ggplot(data=probe, aes(x=doinghousework, y=positive, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Doing Housework (Hours)") + ylab("Positive")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))


### Optimistic Trait
positive_optimistic<-summary(models_list[[2]])
# no significant interactions

#### Extraverted Trait

## Eating
model<-models_list[[1]]
positive_extraverted
eating_extraverted_positive<-sim_slopes(model, pred = "eating", modx="extraverted")
plot<-interact_plot(model, pred=eating, modx=extraverted) + xlab("Time Spent Preparing Food (Hours)") + ylab("positive") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Extraversion", `- 1 SD`="Low Extraversion") 

eating_extraverted_positive_s<-ggplot(data=probe, aes(x=eating, y=positive, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Eating (Hours)") + ylab("Positive")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

## Praying 

praying_extraverted_positive<-sim_slopes(model, pred = "praying", modx="extraverted")
plot<-interact_plot(model, pred=praying, modx=extraverted) + xlab("Time Spent Preparing Food (Hours)") + ylab("positive") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Extraversion", `- 1 SD`="Low Extraversion") 

praying_extraverted_positive_s<-ggplot(data=probe, aes(x=praying, y=positive, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent Praying (Hours)") + ylab("Positive")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

## Hobbies

hobbies_extraverted_positive<-sim_slopes(model, pred = "hobbies", modx="extraverted")
plot<-interact_plot(model, pred=hobbies, modx=extraverted) + xlab("Time Spent Preparing Food (Hours)") + ylab("positive") 

probe<-plot$data %>%
  dplyr::filter(modx_group=="+ 1 SD" | modx_group=="- 1 SD")

probe$Personality<-recode_factor(probe$modx_group, `+ 1 SD`="High Extraversion", `- 1 SD`="Low Extraversion") 

hobbies_extraverted_positive_s<-ggplot(data=probe, aes(x=hobbies, y=positive, group=Personality, colour=Personality, fill=Personality))+geom_line(size=1.5)+xlab("Time Spent in  Hobbies (Hours)") + ylab("Positive")+scale_color_manual(values=c("#F3DF6C", "#5BBCD6")) + scale_fill_manual(values=c("#F3DF6C", "#5BBCD6")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 25), axis.title.x = element_text(size=15), axis.title.y=element_text(size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15))

