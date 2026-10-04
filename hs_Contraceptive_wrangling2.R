setwd("c:/docs/house officers")
rm(list=ls())

library(tidyverse)

con <- readxl::read_excel("Contraception.xlsx")

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



con$B7

con$B7 <- as.factor(con$B7)

con$B7 <-factor(con$B7,
                  levels=c(1,2,3),
                  labels=c("Yes", "No", "Don't know"))
con$B7


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
  mutate(B3_score = recode(con$B3, "2yr" = 1, "Other"= 0))

con <- con %>% 
  mutate(B4_score = recode(con$B4, "45days" = 1, "Other"= 0))





con <- con %>% 
  mutate(B7_score = recode(con$B7, "Yes" = 1, "No" = 0, "Don't know" = 0))

con<- con %>% 
  mutate(knowledge_score = B1_score + B2_a + B2_b + B2_c + B2_d +
           B2_e + B2_f + B2_g + B3_score + B4_score + B5_a + B5_b+
           B5_c + B5_d + B6_a + B6_b + B6_c + B7_score)

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

contra <- con[,c(1:53,59,66)]

library(rio)

export(contra,"contraception_clean2.rds")








