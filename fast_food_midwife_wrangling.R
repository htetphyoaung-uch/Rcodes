setwd("/Users/htetphyoaung/TrainR/house officers")
rm(list=ls())

library(tidyverse)

food <- readxl::read_excel("hs_fastfood_midwife.xlsx")

dim(food)
names(food)

mStats::codebook(food)

sum(is.na(food$age))

food$sex
food$sex <- as.factor(food$sex)
food$sex <- factor(food$sex,
                   levels=c(1,2),
                   labels=c("Male", "Female"))
food$sex

food$marital
food$marital <- as.factor(food$marital)
food$marital <- factor(food$marital,
                       levels=c(1,2,3),
                       labels=c("Single", "Married", "Other"))
food$marital

food$religion
food$religion <- as.factor(food$religion)
food$religion <- factor(food$religion,
                        levels=c(1,2,3,4,5),
                        labels=c("Buddhist", "Christan", "Islam","Hindu","Other"))

food$religion

food$living
food$living <- as.factor(food$living)
food$living <- factor(food$living,
                      levels=c(1,2,3,4),
                      labels=c("School Dormitory", "Private Hostel", 
                               "With Family/Relatives","Other"))
food$living

# scoring

food <- food %>% 
  mutate(B1_score = recode(food$B1,
                           "3" = 3,
                           "2" = 0,
                           "1" = 0))

food <- food %>% 
  mutate(B2_score = recode(food$B2,
                           "3" = 2,
                           "2" = 1,
                           "1" = 0))

food <- food %>% 
  mutate(B3_score = recode(food$B3,
                           "1" = 2,
                           "2" = 0,
                           "3" = 0,
                           "4" = 0))

food <- food %>% 
  mutate(B4_score = recode(food$B4,
                           "1" = 2,
                           "2" = 0,
                           "3" = 0,
                           "4" = 0))

food <- food %>% 
  mutate(B5_score = recode(food$B5,
                           "1" = 0,
                           "2" = 0,
                           "3" = 2,
                           "4" = 0))

food <- food %>% 
  mutate(B6_score = recode(food$B6, 
                           "1" = 0,
                           "2" = 0,
                           "3" = 0,
                           "4" = 2))

food <- food %>% 
  mutate(B7_score = recode(food$B7,
                           "1" = 0,
                           "2" = 0,
                           "3" = 0,
                           "4" = 2))

food <- food %>% 
  mutate(B8_score = recode(food$B8,
                           "1" = 2,
                           "2" = 0,
                           "3" = 0))

food <- food %>% 
  mutate(knowledge_score = B1_score+B2_score+B3_score+B4_score+B5_score+
           B6_score+B7_score+B8_score)

#############

food$B1
food$B1 <- as.factor(food$B1)
food$B1 <- factor(food$B1,
                  levels = c(1,2,3),
                  labels = c("Never","Sometimes","Always"))
food$B1

food$B2
food$B2 <- as.factor(food$B2)
food$B2 <- factor(food$B2,
                  levels = c(1,2,3),
                  labels = c("Don't Know","Some diseases","Yes"))
food$B2

food$B3
food$B3 <- as.factor(food$B3)
food$B3 <- factor(food$B3,
                  levels = c(1,2,3,4),
                  labels = c("Sugar/Sodium","Proteins","Vitamins","Minerals"))
food$B3

food$B4
food$B4 <- as.factor(food$B4)
food$B4 <- factor(food$B4,
                  levels = c(1,2,3,4),
                  labels = c("Easily prepared/processed food","Staple food",
                             "Alternative to fruit/veggie","Fortified food"))
food$B4

food$B5
food$B5 <- as.factor(food$B5)
food$B5 <- factor(food$B5,
                  levels = c(1,2,3,4),
                  labels = c("Disease prevention","Health benefits",
                             "Increased risk of NCDs","Keeps slim and fit"))
food$B5

food$B6
food$B6 <- as.factor(food$B6)
food$B6 <- factor(food$B6,
                  levels = c(1,2,3,4),
                  labels = c("Healthy food","Components of diet plan",
                             "Alternatives to daily fruits/veggies","Unhealthy food"))
food$B6

food$B7
food$B7 <- as.factor(food$B7)
food$B7 <- factor(food$B7,
                  levels = c(1,2,3,4),
                  labels = c("Fiber","Essential nutrients",
                             "Proteins","Refined carbohydrates"))
food$B7

food$B8
food$B8 <- as.factor(food$B8)
food$B8 <- factor(food$B8,
                  levels = c(1,2,3),
                  labels = c("Artificial products","Dairy products",
                             "Fruit or Veggies"))
food$B8

food <- food %>% rename( "Health Workers" = B9_a)
food <- food %>% rename( "Pamphlet/Journal/Magazine" = B9_b)
food <- food %>% rename( "Internet/Social Media" = B9_c)
food <- food %>% rename( "Family/Friends/Peers" = B9_d)

#############

food$C1
food$C1 <- as.factor(food$C1)
food$C1 <- factor(food$C1,
                  levels=c(1,2),
                  labels=c("Yes", "No"))
food$C1

food <- food %>% rename( "Instant Noodles" = C2_a)
food <- food %>% rename( "Deep-fried foods" = C2_b)
food <- food %>% rename( "Cake/Biscuits/Icecreams" = C2_c)
food <- food %>% rename( "Canned foods" = C2_d)
food <- food %>% rename( "Sandwich/Burger" = C2_e)
food <- food %>% rename( "Crips/Chips" = C2_f)
food <- food %>% rename( "Street food/Barbecue" = C2_g)


food$C3
food$C3 <- as.factor(food$C3)
food$C3 <- factor(food$C3,
                  levels = c(1,2,3,4),
                  labels = c("Everyday","Five-Six days",
                             "Three-Four days","Two or less"))
food$C3

food$C4
food$C4 <- as.factor(food$C4)
food$C4 <- factor(food$C4,
                  levels = c(1,2,3,4),
                  labels = c("Morning","Noon",
                             "Evening","Night"))
food$C4

food <- food %>% rename( "Tasty" = C5_a)
food <- food %>% rename( "Cheap" = C5_b)
food <- food %>% rename( "Easily Avialiable" = C5_c)
food <- food %>% rename( "Less Time consuming" = C5_d)

names(food)

fastfood_clean <- food[,c(1:33,42)]

#saving clean data

library(rio)

export(fastfood_clean,"fastfood_clean.rds")


