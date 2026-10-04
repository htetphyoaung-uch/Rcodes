setwd("c:/docs/house officers")
rm(list=ls())

library(tidyverse)

edrink <- readxl::read_excel("edrink_data_ummg.xlsx")

edrink$sex
edrink$sex <- as.factor(edrink$sex)
edrink$sex <- factor(edrink$sex,
                     levels=c(1,2),
                     labels=c("Male", "Female"))
edrink$sex

edrink$marital
edrink$marital <- as.factor(edrink$marital)
edrink$marital <- factor(edrink$marital,
                         levels=c(1,2,3),
                         labels=c("Single", "Married", "Other"))
edrink$marital

edrink$religion
edrink$religion <- as.factor(edrink$religion)
edrink$religion <- factor(edrink$religion,
                          levels=c(1,2,3,4,5),
                          labels=c("Buddhist", "Christan", "Islam","Hindu","Other"))

edrink$religion

edrink$living
edrink$living <- as.factor(edrink$living)
edrink$living <- factor(edrink$living,
                        levels=c(1,2,3,4),
                        labels=c("School Dormitory", "Private Hostel", 
                                 "With Family/Relatives","Other"))
edrink$living

yesno <- function(values){
  if (is.numeric(values)){
    as.factor(values)
  }
  factor(values,
         levels=c(1,2),
         labels=c("Yes", "No"))
}


edrink$friends <- yesno(edrink$friends)
edrink$friends

edrink$family <- yesno(edrink$family)
edrink$family

#Awareness

edrink$B1 <- yesno(edrink$B1)
edrink$B1


edrink <- edrink %>% rename("Red Bull" = B2a)
edrink <- edrink %>% rename("Shark" = B2b)
edrink <- edrink %>% rename("LIPOVITAN-D" = B2c)
edrink <- edrink %>% rename("M-150" = B2d)
edrink <- edrink %>% rename("Burn" = B2e)
edrink <- edrink %>% rename("Speed" = B2f)
edrink <- edrink %>% rename("Carabao" = B2g)
edrink <- edrink %>% rename("Rocker" = B2h)
edrink <- edrink %>% rename("Other" = B2i)


edrink <- edrink %>% rename("Caffeine" = B3a)
edrink <- edrink %>% rename("Vitamins" = B3b)
edrink <- edrink %>% rename("Sugars" = B3c)
edrink <- edrink %>% rename("Flavouring Agents" = B3d)
edrink <- edrink %>% rename("Preservatives" = B3e)
edrink <- edrink %>% rename("Others" = B3f)

edrink <- edrink %>% rename("Increase physical resistance" = B4a)
edrink <- edrink %>% rename("Increase study concentration" = B4b)
edrink <- edrink %>% rename("provide alertness" = B4c)
edrink <- edrink %>% rename("Don't Know" = B4d)


edrink <- edrink %>% rename("Headache" = B5a)
edrink <- edrink %>% rename("Tremor" = B5b)
edrink <- edrink %>% rename("Nervousness" = B5c)
edrink <- edrink %>% rename("Palpitation" = B5d)
edrink <- edrink %>% rename("Insomnia" = B5e)
edrink <- edrink %>% rename("Anxiety" = B5f)

edrink$B6
edrink$B6 <- as.factor(edrink$B6)
edrink$B6 <- factor(edrink$B6,
                    levels=c(1,2,3),
                    labels=c("Good for Health", "Bad for Health", "Don't Know"))
edrink$B6

edrink$B7
edrink$B7 <- as.factor(edrink$B7)
edrink$B7 <- factor(edrink$B7,
                    levels=c(1,2,3),
                    labels=c("Good for Health", "Bad for Health", "Don't Know"))
edrink$B7


edrink <- edrink %>% rename("Hyperacidity" = B8a)
edrink <- edrink %>% rename("Sleep Disturbance" = B8b)
edrink <- edrink %>% rename("Tuberculosis" = B8c)
edrink <- edrink %>% rename("Type 2 Diabetes" = B8d)
edrink <- edrink %>% rename("Hypertension" = B8e)
edrink <- edrink %>% rename("Hyperlipidaemia" = B8f)
edrink <- edrink %>% rename("Overweight/Obse" = B8g)
edrink <- edrink %>% rename("Anaemia" = B8h)


# Attitude

alti <- function(values){
  if (is.numeric(values)){
    as.factor(values)
  }
  factor(values,
         levels=c(1,2,3),
         labels=c("Disagree", "Neutral","Agree"))
}


edrink$C1 <- alti(edrink$C1)
edrink$C2 <- alti(edrink$C2)
edrink$C3 <- alti(edrink$C3)
edrink$C4 <- alti(edrink$C4)
edrink$C5 <- alti(edrink$C5)
edrink$C6 <- alti(edrink$C6)
edrink$C7 <- alti(edrink$C7)
edrink$C8 <- alti(edrink$C8)
edrink$C9 <- alti(edrink$C9)
edrink$C10 <- alti(edrink$C10)


# Practice

edrink$D1 <- yesno(edrink$D1)


edrink$D3
edrink$D3 <- as.factor(edrink$D3)
edrink$D3 <- factor(edrink$D3,
                    levels=c(1,2,3,4),
                    labels=c("Everyday", "five to six", "Three to four", "Two or less"))
edrink$D3


edrink$D4
edrink$D4 <- as.factor(edrink$D4)
edrink$D4 <- factor(edrink$D4,
                    levels=c(1,2,3,4),
                    labels=c("Advertisements", "Sellers' Recommend",
                             "Family/Friend recommend", "Others"))
edrink$D4


edrink <- edrink %>% rename("Sports" = D5a)
edrink <- edrink %>% rename("To stay awake" = D5b)
edrink <- edrink %>% rename("Mental Enhancer" = D5c)
edrink <- edrink %>% rename("Energy Boost" = D5d)
edrink <- edrink %>% rename("To study" = D5e)
edrink <- edrink %>% rename("Weight control" = D5f)


edrink$D7 <- yesno(edrink$D7)


library(rio)

export(edrink,"edrink_ummg_clean.rds")
















