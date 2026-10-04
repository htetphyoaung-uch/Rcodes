setwd("c:/docs/house officers")
rm(list=ls())

library(tidyverse)

con <- readxl::read_excel("Contraception1.xlsx")

dim(con)
names(con)

mStats::codebook(con)

#section A Socio_demo

sum(is.na(con$age))

con$race
con$race <- as.factor(con$race)
con$race <-factor(con$race,
                 levels=c(1,2),
                 labels=c("Burmese", "Others"))
con$race

con$religion
con$religion <- as.factor(con$religion)
con$religion <-factor(con$religion,
                  levels=c(1,2),
                  labels=c("Buddhist", "Others"))
con$religion

con$edu

con$edu <- as.factor(con$edu)

con$edu <-factor(con$edu,
                 levels=c(1,2,3,4,5,6),
                 labels=c("Illiterate", "Read and write", "Primary","Middle",
                                  "High","University/Graduated"))
con$edu

con$occupation
con$occupation <- as.factor(con$occupation)
con$occupation <-factor(con$occupation,
                      levels=c(1,2),
                      labels=c("Have a job", "Dependant"))
con$occupation

#Section B: Knowledge

con$B1

con$B1 <- as.factor(con$B1)

con$B1 <-factor(con$B1,
                 levels=c(1,2),
                 labels=c("Yes", "No"))
con$B1

con$B2_a

con$B2_a <- as.factor(con$B2_a)

con$B2_a <-factor(con$B2_a,
                levels=c(1,0),
                labels=c("Yes", "No"))
con$B2_a

con$B2_b

con$B2_b <- as.factor(con$B2_b)

con$B2_b <-factor(con$B2_b,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$B2_b

con$B2_c

con$B2_c <- as.factor(con$B2_c)

con$B2_c <-factor(con$B2_c,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$B2_c

con$B2_d

con$B2_d <- as.factor(con$B2_d)

con$B2_d <-factor(con$B2_d,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$B2_d

con$B2_e

con$B2_e <- as.factor(con$B2_e)

con$B2_e <-factor(con$B2_e,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$B2_e

con$B2_f

con$B2_f <- as.factor(con$B2_f)

con$B2_f <-factor(con$B2_f,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$B2_f

con$B2_g

con$B2_g <- as.factor(con$B2_g)

con$B2_g <-factor(con$B2_g,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$B2_g


con$B3

con$B3 <- as.factor(con$B3)

con$B3 <-factor(con$B3,
                levels=c(1,2),
                labels=c("2yr", "Other"))
con$B3

con$B4

con$B4 <- as.factor(con$B4)

con$B4 <-factor(con$B4,
                levels=c(1,2),
                labels=c("45days", "Other"))
con$B4

con$B5_a

con$B5_a <- as.factor(con$B5_a)

con$B5_a <-factor(con$B5_a,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$B5_a

con$B5_b

con$B5_b <- as.factor(con$B5_b)

con$B5_b <-factor(con$B5_b,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$B5_b

con$B5_c

con$B5_c <- as.factor(con$B5_c)

con$B5_c <-factor(con$B5_c,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$B5_c

con$B5_d

con$B5_d <- as.factor(con$B5_d)

con$B5_d <-factor(con$B5_d,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$B5_d

con$B6_a

con$B6_a <- as.factor(con$B6_a)

con$B6_a <-factor(con$B6_a,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$B6_a

con$B6_b

con$B6_b <- as.factor(con$B6_b)

con$B6_b <-factor(con$B6_b,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$B6_b

con$B6_c

con$B6_c <- as.factor(con$B6_c)

con$B6_c <-factor(con$B6_c,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$B6_c

con$B7

con$B7 <- as.factor(con$B7)

con$B7 <-factor(con$B7,
                  levels=c(1,2,3),
                  labels=c("Yes", "No", "Don't know"))
con$B7

con$B8_a

con$B8_a <- as.factor(con$B8_a)

con$B8_a <-factor(con$B8_a,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$B8_a

con$B8_b

con$B8_b <- as.factor(con$B8_b)

con$B8_b <-factor(con$B8_b,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$B8_b

con$B8_c

con$B8_c <- as.factor(con$B8_c)

con$B8_c <-factor(con$B8_c,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$B8_c

con$B8_d

con$B8_d <- as.factor(con$B8_d)

con$B8_d <-factor(con$B8_d,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$B8_d

con$B8_e

con$B8_e <- as.factor(con$B8_e)

con$B8_e <-factor(con$B8_e,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$B8_e

con$B8_f

con$B8_f <- as.factor(con$B8_f)

con$B8_f <-factor(con$B8_f,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$B8_f

#Attitude Questions

con$C1

con$C1 <- as.factor(con$C1)

con$C1 <-factor(con$C1,
                levels=c(1,2,3),
                labels=c("Disagree", "Neutral","Agree"))
con$C1

con$C2

con$C2 <- as.factor(con$C2)

con$C2 <-factor(con$C2,
                levels=c(1,2,3),
                labels=c("Disagree", "Neutral","Agree"))
con$C2

con$C3

con$C3 <- as.factor(con$C3)

con$C3 <-factor(con$C3,
                levels=c(1,2,3),
                labels=c("Disagree", "Neutral","Agree"))
con$C3

con$C4

con$C4 <- as.factor(con$C4)

con$C4 <-factor(con$C4,
                levels=c(1,2,3),
                labels=c("Disagree", "Neutral","Agree"))
con$C4

con$C5

con$C5 <- as.factor(con$C5)

con$C5 <-factor(con$C5,
                levels=c(1,2,3),
                labels=c("Disagree", "Neutral","Agree"))
con$C5

con$C6

con$C6 <- as.factor(con$C6)

con$C6 <-factor(con$C6,
                levels=c(1,2,3),
                labels=c("Disagree", "Neutral","Agree"))
con$C6

# Practice

con$D1
con$D1 <-factor(con$D1,
                levels=c(1,2),
                labels=c("Yes", "No"))
con$D1

con$D2

con$D2 <-factor(con$D2,
                levels=c(1,2,3,4),
                labels=c("Want to conceive", "Health problems","Peer pressure",
                         "Other"))
con$D2

con %>% 
  filter(D1 == "No") %>% 
  ggplot(aes(D2))+
  geom_bar()

con$D3

con$D3 <-factor(con$D3,
                levels=c(1,2,3,4,5,6,7),
                labels=c("COC pill", "Injection","IUCD","Dermal implant","Condom m/f",
                         "Sterilization m/f", "Emergency pill"))
con$D3

con %>% 
  filter(D1 == "Yes") %>% 
  ggplot(aes(D3))+
  geom_bar()

con$D5
con$D5 <-factor(con$D5,
                levels=c(1,2),
                labels=c("Yes", "No"))
con$D5

con$D6
con$D6 <-factor(con$D6,
                levels=c(1,2,3),
                labels=c("Health facilities", "Drug store/shops","NGO/INGOs"))
con$D6

con$D7
con$D7 <-factor(con$D7,
                levels=c(1,2),
                labels=c("Yes", "No"))
con$D7

con$D8_a
con$D8_a <-factor(con$D8_a,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$D8_a

con$D8_b
con$D8_b <-factor(con$D8_b,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$D8_b

con$D8_c
con$D8_c <-factor(con$D8_c,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$D8_c

con$D8_d
con$D8_d <-factor(con$D8_d,
                  levels=c(1,0),
                  labels=c("Yes", "No"))
con$D8_d

con$D9
con$D9 <-factor(con$D9,
                levels=c(1,2),
                labels=c("Yes", "No"))
con$D9

##########

#knowledge score

con <- con %>% 
  mutate(B1_score = recode(con$B1, "Yes" = 1, "No"= 0))

con <- con %>% 
  mutate(B2_a_sc = recode(con$B2_a, "Yes" = 1, "No"= 0))

con <- con %>% 
  mutate(B2_b_sc = recode(con$B2_b, "Yes" = 1, "No"= 0))

con <- con %>% 
  mutate(B2_c_sc = recode(con$B2_c, "Yes" = 1, "No"= 0))

con <- con %>% 
  mutate(B2_d_sc = recode(con$B2_d, "Yes" = 1, "No"= 0))

con <- con %>% 
  mutate(B2_e_sc = recode(con$B2_e, "Yes" = 1, "No"= 0))

con <- con %>% 
  mutate(B2_f_sc = recode(con$B2_f, "Yes" = 1, "No"= 0))

con <- con %>% 
  mutate(B2_g_sc = recode(con$B2_g, "Yes" = 1, "No"= 0))

con <- con %>% 
  mutate(B3_score = recode(con$B3, "2yr" = 1, "Other"= 0))

con <- con %>% 
  mutate(B4_score = recode(con$B4, "45days" = 1, "Other"= 0))


con <- con %>% 
  mutate(B5_a_sc = recode(con$B5_a, "Yes" = 1, "No"= 0))

con <- con %>% 
  mutate(B5_b_sc = recode(con$B5_b, "Yes" = 1, "No"= 0))

con <- con %>% 
  mutate(B5_c_sc = recode(con$B5_c, "Yes" = 1, "No"= 0))

con <- con %>% 
  mutate(B5_d_sc = recode(con$B5_d, "Yes" = 1, "No"= 0))

con <- con %>% 
  mutate(B6_a_sc = recode(con$B6_a, "Yes" = 1, "No"= 0))

con <- con %>% 
  mutate(B6_b_sc = recode(con$B6_b, "Yes" = 1, "No"= 0))

con <- con %>% 
  mutate(B6_c_sc = recode(con$B6_c, "Yes" = 1, "No"= 0))


con <- con %>% 
  mutate(B7_score = recode(con$B7, "Yes" = 1, "No" = 0, "Don't know" = 0))

con<- con %>% 
  mutate(knowledge_score = B1_score + B2_a_sc + B2_b_sc + B2_c_sc + B2_d_sc +
           B2_e_sc + B2_f_sc + B2_g_sc + B3_score + B4_score + B5_a_sc + B5_b_sc+
           B5_c_sc + B5_d_sc + B6_a_sc + B6_b_sc + B6_c_sc + B7_score)

############
#Attitude

con <- con %>% 
  mutate(C1_score = recode(con$C1, "Disagree" = 1, "Neutral" = 2, "Agree" = 3))

con <- con %>% 
  mutate(C2_score = recode(con$C2, "Disagree" = 1, "Neutral" = 2, "Agree" = 3))

con <- con %>% 
  mutate(C3_score = recode(con$C3, "Disagree" = 1, "Neutral" = 2, "Agree" = 3))

con <- con %>% 
  mutate(C4_score = recode(con$C4, "Disagree" = 1, "Neutral" = 2, "Agree" = 3))

con <- con %>% 
  mutate(C5_score = recode(con$C5, "Disagree" = 1, "Neutral" = 2, "Agree" = 3))

con <- con %>% 
  mutate(C6_score = recode(con$C6, "Disagree" = 1, "Neutral" = 2, "Agree" = 3))

con <- con %>% 
  mutate(Attitude_score = C1_score + C2_score + C3_score + C4_score + 
           C5_score + C6_score)

names(con)

contra <- con[,c(1:53,72,79)]

library(rio)

export(contra,"contraception_clean.rds")








