setwd("/Users/htetphyoaung/TrainR/house officers")
rm(list=ls())

library(tidyverse)

bd <- readxl::read_excel("Blood Donation Data_EC.xlsx")

dim(bd)
names(bd)

mStats::codebook(bd)

bd$sex
bd$sex <- as.factor(bd$sex)
bd$sex <-factor(bd$sex,
                levels=c(1,2),
                labels=c("Male", "Female"))
bd$sex

bd$marital
bd$marital <- as.factor(bd$marital)
bd$marital <- factor(bd$marital,
                     levels = c(1,2,3),
                     labels = c("Single","Married","Others"))
bd$marital

bd$living
bd$living <- as.factor(bd$living)
bd$living <- factor(bd$living,
                    levels=c(1,2,3,4),
                    labels=c("School Dormitory", "Private Hostel", 
                             "With Family/Relatives","Other"))
bd$living

bd$blood_gp
bd$blood_gp <- as.factor(bd$blood_gp)
bd$blood_gp <-factor(bd$blood_gp,
                     levels=c(1,2),
                     labels=c("Yes", "No"))
bd$blood_gp


bd$what_gp
bd$what_gp <- as.factor(bd$what_gp)
bd$what_gp <- factor(bd$what_gp,
                     levels=c(1,2,3,4),
                     labels=c("A", "B","AB","O"))
bd$what_gp

##Knowledge Score

bd$B1

bd <- bd %>% 
  mutate(B1_score = case_when(16<= B1 & B1<= 18  ~ "1",
                              B1 > 18 ~ "0"))

bd$B1_score
bd$B1_score <- as.numeric(bd$B1_score)

bd$B2

bd <- bd %>% mutate( B2_score = recode ( bd$B2,
                                         "1" = 0,
                                         "2" = 0,
                                         "3" = 1,
                                         "4" = 0,
                                         "5" = 0,
                                         "6" = 0))
bd$B2_score


bd$B3

bd <- bd %>% mutate(B3_score = recode(bd$B3,
                                      "1"= 0,
                                      "2"= 2,
                                      "3"= 0,
                                      "4"= 0))
bd$B3_score

bd$B4_a

bd <- bd %>% mutate(B4_a_score = B4_a)

bd$B4_a_score

bd$B4_b

bd <- bd %>% mutate(B4_b_score = B4_b)

bd$B4_b_score

bd$B4_c

bd <- bd %>% mutate(B4_c_score = B4_c)

bd$B4_c_score

bd$B4_d

bd <- bd %>% mutate(B4_d_score = B4_d)

bd$B4_d_score

bd$B4_e

bd <- bd %>% mutate(B4_e_score = B4_e)

bd$B4_e_score

bd$B5_a

bd <- bd %>% mutate(B5_a_score = B5_a)

bd$B5_a_score

bd$B5_b

bd <- bd %>% mutate(B5_b_score = B5_b)

bd$B5_b_score

bd$B5_c

bd <- bd %>% mutate(B5_c_score = recode(bd$B5_c,
                                        "1"= 0,
                                        "0"= 1))
bd$B5_c_score

bd$B5_d

bd <- bd %>% mutate(B5_d_score = B5_d)

bd$B5_d_score


bd$B6_a

bd <- bd %>% mutate(B6_a_score = B6_a)

bd$B6_a_score

bd$B6_b

bd <- bd %>% mutate(B6_b_score = B6_b)

bd$B6_b_score

bd$B6_c

bd <- bd %>% mutate(B6_c_score = B6_c)

bd$B6_c_score

### Knowledge_score

bd <- bd %>% 
  rowwise() %>% 
  mutate(K_score = sum(B1_score,B2_score,B3_score,B4_a_score,B4_b_score,B4_c_score,
                       B4_d_score,B4_e_score,B5_a_score,B5_b_score,B5_c_score,
                       B5_d_score,B6_a_score,B6_b_score,B6_c_score,na.rm = T))

bd <- bd %>% ungroup()

##Attitude score

bd$C1
bd <- bd %>% mutate(C1_score = recode(bd$C1,
                                      "1"= 5,
                                      "2"= 4,
                                      "3"= 3,
                                      "4"= 2,
                                      "5"= 1))
bd$C1_score

bd$C2

bd <- bd %>% mutate(C2_score = C2)

bd$C2_score

bd$C3
bd <- bd %>% mutate(C3_score = recode(bd$C3,
                                      "1"= 5,
                                      "2"= 4,
                                      "3"= 3,
                                      "4"= 2,
                                      "5"= 1))
bd$C3_score

bd$C4

bd <- bd %>% mutate(C4_score = C4)

bd$C4_score

bd$C5
bd <- bd %>% mutate(C5_score = recode(bd$C5,
                                      "1"= 5,
                                      "2"= 4,
                                      "3"= 3,
                                      "4"= 2,
                                      "5"= 1))
bd$C5_score


bd$C6

bd <- bd %>% mutate(C6_score = C6)

bd$C6_score

bd$C7
bd <- bd %>% mutate(C7_score = recode(bd$C7,
                                      "1"= 5,
                                      "2"= 4,
                                      "3"= 3,
                                      "4"= 2,
                                      "5"= 1))
bd$C7_score

bd$C8
bd <- bd %>% mutate(C8_score = recode(bd$C8,
                                      "1"= 5,
                                      "2"= 4,
                                      "3"= 3,
                                      "4"= 2,
                                      "5"= 1))
bd$C8_score

## Attitude Score

bd <- bd %>% 
  rowwise() %>% 
  mutate(A_score = sum(C1_score,C2_score,C3_score,C4_score,C5_score,C6_score,
                       C7_score,C8_score, na.rm = T))

bd <- bd %>% ungroup()

bd <- bd %>% rename("Low Hb level" = B4_a)
bd <- bd %>% rename("Recent travel to Malaria Area" = B4_b)
bd <- bd %>% rename("Recent blood donation" = B4_c)
bd <- bd %>% rename("Tatto/Piercing last 6 months" = B4_d)
bd <- bd %>% rename("Pregnancy" = B4_e)

bd <- bd %>% rename("Reduce Iron level" = B5_a)
bd <- bd %>% rename("Improved CVS health" = B5_b)
bd <- bd %>% rename("Burn Calories" = B5_c)
bd <- bd %>% rename("Others" = B5_d)

bd <- bd %>% rename("Hospitals" = B6_a)
bd <- bd %>% rename("Blood Banks" = B6_b)
bd <- bd %>% rename("Mobile Donation Units" = B6_c)

bd <- bd %>% rename("Health Care Personal" = B7_a)
bd <- bd %>% rename("Family/Peer" = B7_b)
bd <- bd %>% rename("TV/radio" = B7_c)
bd <- bd %>% rename("Internet/Social Media" = B7_d)
bd <- bd %>% rename("Journal/Magazine" = B7_e)


bd <- bd %>% rename("Altruism_desire to help others" = D3_a)
bd <- bd %>% rename("Family/peer influence" = D3_b)
bd <- bd %>% rename("Incentives(gifts/certificates)" = D3_c)
bd <- bd %>% rename("Health benefits" = D3_d)
bd <- bd %>% rename("Other" = D3_e)

bd <- bd %>% rename("Fear of needles/pain" = D4_a)
bd <- bd %>% rename("Lack of awareness about B/d donation" = D4_b)
bd <- bd %>% rename("Concerns about health risks" = D4_c)
bd <- bd %>% rename("Inconvenience" = D4_d)
bd <- bd %>% rename("Not eligible to donate" = D4_e)

bd$D1
bd$D1 <- as.factor(bd$D1)
bd$D1 <-factor(bd$D1,
               levels=c(1,2),
               labels=c("Yes", "No"))
bd$D1

bd$D5
bd$D5 <- as.factor(bd$D5)
bd$D5 <-factor(bd$D5,
               levels=c(1,2,3),
               labels=c("Yes", "No", "Maybe"))
bd$D5

bd <- bd %>% rename("More convenient donation locations" = D6_a)
bd <- bd %>% rename("Better awareness campaigns" = D6_b)
bd <- bd %>% rename("Incentives(gifts or certificates)" = D6_c)
bd <- bd %>% rename("Peer/family encouragement" = D6_d)
bd <- bd %>% rename("Others()" = D6_e)


alti <- function(values){
  if (is.numeric(values)){
    as.factor(values)
  }
  factor(values,
         levels=c(1,2,3,4,5),
         labels=c("Strongly Agree", "Agree","Neutral","Disagree",
                  "Strongly Disagree"))
}

bd$C1 <- alti(bd$C1)
bd$C2 <- alti(bd$C2)
bd$C3 <- alti(bd$C3)
bd$C4 <- alti(bd$C4)
bd$C5 <- alti(bd$C5)
bd$C6 <- alti(bd$C6)
bd$C7 <- alti(bd$C7)
bd$C8 <- alti(bd$C8)

names(bd)


bd <- bd[,c(1:52,68,77)]



library(rio)

export(bd,"bd_clean_EC.rds")






