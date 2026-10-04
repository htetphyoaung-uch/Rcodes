setwd("c:/docs/house officers")
rm(list=ls())

library(tidyverse)

tpreg <- readxl::read_excel("k_p_teenage_preg.xlsx")

dim(tpreg)
names(tpreg)

mStats::codebook(tpreg)

sum(is.na(tpreg$age))

tpreg$gender

tpreg$gender <- as.factor(tpreg$gender)

tpreg$gender<-factor(tpreg$gender,
                levels=c(1,2),
                labels=c("Male", "Female"))

tpreg$gender

tpreg$marital

tpreg$marital <- as.factor(tpreg$marital)

tpreg$marital<-factor(tpreg$marital,
                     levels=c(1,2,3),
                     labels=c("Single", "Married", "Other"))

tpreg$marital

tpreg$religion

tpreg$religion <- as.factor(tpreg$religion)

tpreg$religion<-factor(tpreg$religion,
                      levels=c(1,2,3,4,5),
                      labels=c("Buddhist", "Christan", "Musilm","Hindu","Other"))

tpreg$religion

tpreg$father_edu

tpreg$father_edu <- as.factor(tpreg$father_edu)

tpreg$father_edu<-factor(tpreg$father_edu,
                       levels=c(1,2,3,4,5,6,7),
                       labels=c("Illiterate", "Read and write", "Primary","Middle",
                                "High","University", "Graduated"))

tpreg$father_edu

tpreg$mother_edu

tpreg$mother_edu <- as.factor(tpreg$mother_edu)

tpreg$mother_edu<-factor(tpreg$mother_edu,
                         levels=c(1,2,3,4,5,6,7),
                         labels=c("Illiterate", "Read and write", "Primary","Middle",
                                  "High","University", "Graduated"))

tpreg$mother_edu

tpreg$family_type

tpreg$family_type <- as.factor(tpreg$family_type)

tpreg$family_type<-factor(tpreg$family_type,
                      levels=c(1,2,3),
                      labels=c("Nuclear", "Extended", "Three generations"))

tpreg$family_type

tpreg$current_relation

tpreg$current_relation <- as.factor(tpreg$current_relation)

tpreg$current_relation<-factor(tpreg$current_relation,
                       levels=c(1,2,3,4,5),
                       labels=c("Live together", "Separated", "Divorced",
                                "One of Parents died","Both dead"))

tpreg$current_relation

tpreg$living

tpreg$living <- as.factor(tpreg$living)

tpreg$living<-factor(tpreg$living,
                     levels=c(1,2,3,4),
                     labels=c("Uni Dormitory", "Private Hostel", 
                              "With Family/Relatives","Other"))

tpreg$living

tpreg$friend_married

tpreg$friend_married <- as.factor(tpreg$friend_married)

tpreg$friend_married<-factor(tpreg$friend_married,
                     levels=c(1,2),
                     labels=c("Yes", "No"))

tpreg$friend_married

tpreg$friend_preg

tpreg$friend_preg <- as.factor(tpreg$friend_preg)

tpreg$friend_preg<-factor(tpreg$friend_preg,
                             levels=c(1,2),
                             labels=c("Yes", "No"))
tpreg$friend_preg

tpreg$friend_g_bf

tpreg$friend_g_bf <- as.factor(tpreg$friend_g_bf)

tpreg$friend_g_bf<-factor(tpreg$friend_g_bf,
                          levels=c(1,2),
                          labels=c("Yes", "No"))
tpreg$friend_g_bf

tpreg$b_gf

tpreg$b_gf <- as.factor(tpreg$b_gf)

tpreg$b_gf<-factor(tpreg$b_gf,
                          levels=c(1,2,3),
                          labels=c("Never", "Currently don't have",
                                   "Currently have"))
tpreg$b_gf

#Section B Knowledge

tpreg$B1

tpreg$B1 <- as.factor(tpreg$B1)

tpreg$B1<-factor(tpreg$B1,
                          levels=c(1,2),
                          labels=c("Yes", "No"))
tpreg$B1


tpreg$B2

tpreg$B2 <- as.factor(tpreg$B2)

tpreg$B2<-factor(tpreg$B2,
                 levels=c(1,2),
                 labels=c("Yes", "No"))
tpreg$B2

tpreg$B4

tpreg$B4 <- as.factor(tpreg$B4)

tpreg$B4<-factor(tpreg$B4,
                 levels=c(1,2),
                 labels=c("Yes", "No"))
tpreg$B4

tpreg$sti

tpreg$sti <- as.factor(tpreg$sti)

tpreg$sti<-factor(tpreg$sti,
                 levels=c(1,2),
                 labels=c("Yes", "No"))
tpreg$sti

tpreg$C1

tpreg$C1 <- as.factor(tpreg$C1)

tpreg$C1<-factor(tpreg$C1,
                  levels=c(1,2,3,4,5),
                  labels=c("Strongly Disagree", "Disagree","Neutral",
                           "Agree","Strongly Agree"))
tpreg$C1

tpreg$C2

tpreg$C2 <- as.factor(tpreg$C2)

tpreg$C2<-factor(tpreg$C2,
                 levels=c(1,2,3,4,5),
                 labels=c("Strongly Disagree", "Disagree","Neutral",
                          "Agree","Strongly Agree"))
tpreg$C2

tpreg$C3

tpreg$C3 <- as.factor(tpreg$C3)

tpreg$C3<-factor(tpreg$C3,
                 levels=c(1,2,3,4,5),
                 labels=c("Strongly Disagree", "Disagree","Neutral",
                          "Agree","Strongly Agree"))
tpreg$C3

tpreg$C4

tpreg$C4 <- as.factor(tpreg$C4)

tpreg$C4<-factor(tpreg$C4,
                 levels=c(1,2,3,4,5),
                 labels=c("Strongly Disagree", "Disagree","Neutral",
                          "Agree","Strongly Agree"))
tpreg$C4

tpreg$C5

tpreg$C5 <- as.factor(tpreg$C5)

tpreg$C5<-factor(tpreg$C5,
                 levels=c(1,2,3,4,5),
                 labels=c("Strongly Disagree", "Disagree","Neutral",
                          "Agree","Strongly Agree"))
tpreg$C5

tpreg$C6

tpreg$C6 <- as.factor(tpreg$C6)

tpreg$C6<-factor(tpreg$C6,
                 levels=c(1,2,3,4,5),
                 labels=c("Strongly Disagree", "Disagree","Neutral",
                          "Agree","Strongly Agree"))
tpreg$C6

tpreg$C7

tpreg$C7 <- as.factor(tpreg$C7)

tpreg$C7<-factor(tpreg$C7,
                 levels=c(1,2,3,4,5),
                 labels=c("Strongly Disagree", "Disagree","Neutral",
                          "Agree","Strongly Agree"))
tpreg$C7

tpreg$C8

tpreg$C8 <- as.factor(tpreg$C8)

tpreg$C8<-factor(tpreg$C8,
                 levels=c(1,2,3,4,5),
                 labels=c("Strongly Disagree", "Disagree","Neutral",
                          "Agree","Strongly Agree"))
tpreg$C8

library(rio)

export(tpreg,"tpreg_clean.rds")









