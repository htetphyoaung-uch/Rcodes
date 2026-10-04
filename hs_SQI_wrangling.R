setwd("/Users/htetphyoaung/TrainR/house officers")
rm(list=ls())

library(tidyverse)

sqi <- readxl::read_excel("SQI_data2025.xlsx")

dim(sqi)
names(sqi)

mStats::codebook(sqi)

sum(is.na(sqi$age))

sqi$sex
sqi$sex <- as.factor(sqi$sex)
sqi$sex <-factor(sqi$sex,
                 levels=c(1,2),
                 labels=c("Male", "Female"))
sqi$sex

sqi$marital
sqi$marital <- as.factor(sqi$marital)
sqi$marital <-factor(sqi$marital,
                      levels=c(1,2,3),
                      labels=c("Single", "Married", "Other"))
sqi$marital

sqi$religion
sqi$religion <- as.factor(sqi$religion)
sqi$religion<-factor(sqi$religion,
                       levels=c(1,2,3,4,5),
                       labels=c("Buddhist", "Christan", "Musilm","Hindu","Other"))

sqi$religion

sqi$living
sqi$living <- as.factor(sqi$living)
sqi$living<-factor(sqi$living,
                     levels=c(1,2,3,4),
                     labels=c("Uni Dormitory", "Private Hostel", 
                              "With Family/Relatives","Other"))
sqi$living


#component 1 : subjective sleep quality

sqi<- sqi %>% 
  mutate(sub_sleep_quality = B10_quality_overall)

sqi$sub_sleep_quality

#component 2 : sleep latancy

sqi<- sqi %>% 
  mutate(minsleep_score = case_when(minsleep <= 15 ~ "0",
                                    minsleep > 15 & minsleep <= 30 ~ "1",
                                    minsleep > 30 & minsleep <= 60 ~ "2",
                                    minsleep > 60 ~ "3"))

sqi$minsleep_score <- as.numeric(sqi$minsleep_score) 
sqi$minsleep_score

sqi<- sqi %>% 
  mutate(B6a_score = B6_a)
sqi$B6a_score

sqi<- sqi %>% 
  mutate(twototal = B6a_score + minsleep_score)

sqi<- sqi %>% 
  mutate(sleep_latency = case_when( twototal <= 0 ~ "0",
                           twototal >= 1 & twototal <= 2 ~ "1",
                           twototal > 2 & twototal <= 4 ~ "2",
                           twototal > 4 ~ "3"))

sqi$sleep_latency <- as.numeric(sqi$sleep_latency)
sqi$sleep_latency

#component 3 : sleep duration

sqi<- sqi %>% 
  mutate(sleep_duration = case_when(actual_sleep > 7 ~ "0",
                                    actual_sleep <= 7 & actual_sleep > 6 ~ "1",
                                    actual_sleep <= 6 & actual_sleep > 5 ~ "2",
                                    actual_sleep <=5 ~ "3"))

sqi$sleep_duration <- as.numeric(sqi$sleep_duration)
sqi$sleep_duration

#component 4 : habitual sleep efficiency

sqi <- sqi %>% 
  mutate(habit_s_efficiency = (actual_sleep / hr_in_bed)*100)

sqi<- sqi %>% 
  mutate(hse_score = case_when(habit_s_efficiency >= 85 ~ "0",
                               habit_s_efficiency >= 75 & minsleep < 85 ~ "1",
                               habit_s_efficiency >= 65 & minsleep < 75 ~ "2",
                                    minsleep < 65 ~ "3"))

sqi$hse_score <- as.numeric(sqi$hse_score )
sqi$hse_score

#component 5 : sleep disturbances

sqi <- sqi %>% 
  mutate(s_d_total = B6_b + B6_c + B6_d + B6_e + B6_f + B6_g + B6_h + B6_i)

sqi<- sqi %>% 
  mutate(s_d_score = case_when(s_d_total <= 0 ~ "0",
                               s_d_total >= 1 & s_d_total <= 9 ~ "1",
                               s_d_total >= 10 & s_d_total <= 18 ~ "2",
                               s_d_total > 18 ~ "3"))
sqi$s_d_score <- as.numeric(sqi$s_d_score)

sqi$s_d_score

# component 6 : sleep medications

sqi <- sqi %>% 
  mutate(sleep_medication = B7_sleep_pill)

# component 7 : Daytime dysfunction

sqi <- sqi %>% 
  mutate(daytime_dysfun = B8_trouble_awake + B9_problem)

sqi<- sqi %>% 
  mutate(daytime_dysfun_score = case_when( daytime_dysfun <= 0 ~ "0",
                                           daytime_dysfun >= 1 & daytime_dysfun <= 2 ~ "1",
                                           daytime_dysfun > 2 & daytime_dysfun <= 4 ~ "2",
                                           daytime_dysfun > 4 ~ "3"))
sqi$daytime_dysfun_score <- as.numeric(sqi$daytime_dysfun_score)

# PSQI score

sqi <- sqi %>% 
  mutate(psqi_score = sub_sleep_quality + sleep_latency + sleep_duration +
           hse_score + s_d_score + sleep_medication + daytime_dysfun_score)

sqi$psqi_score

#saving processed data

library(rio)

export(sqi,"sqi_clean_2025.rds")


