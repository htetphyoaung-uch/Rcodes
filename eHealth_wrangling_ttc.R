setwd("c:/docs/house officers")
rm(list=ls())

library(tidyverse)

eh <- readxl::read_excel("eHealth_data_ttc.xlsx")

dim(eh)
names(eh)

mStats::codebook(eh)

eh$sex
eh$sex <- as.factor(eh$sex)
eh$sex <-factor(eh$sex,
                levels=c(1,2),
                labels=c("Male", "Female"))
eh$sex

eh$marital
eh$marital <- as.factor(eh$marital)
eh$marital <-factor(eh$marital,
                    levels=c(1,2,3),
                    labels=c("Single", "Married", "Other"))
eh$marital


eh$father_edu
eh$father_edu <- as.factor(eh$father_edu)
eh$father_edu <-factor(eh$father_edu,
                       levels=c(1,2,3,4,5,6),
                       labels=c("Illiterate", "Read and write", "Primary",
                                "Middle","High", "Uni/Graduate"))
eh$father_edu

eh$mother_edu
eh$mother_edu <- as.factor(eh$mother_edu)
eh$mother_edu <-factor(eh$mother_edu,
                       levels=c(1,2,3,4,5,6),
                       labels=c("Illiterate", "Read and write", "Primary",
                                "Middle","High", "Uni/Graduate"))
eh$mother_edu

eh$father_occu
eh$father_occu <- as.factor(eh$father_occu)
eh$father_occu <-factor(eh$father_occu,
                        levels=c(1,2,3,4,5,6,7),
                        labels=c("Goverment Staff", "Private worker", 
                                 "Own Business", "Farmer","Manual Worker",
                                 "No current work", "Other"))
eh$father_occu

eh$mother_occu
eh$mother_occu <- as.factor(eh$mother_occu)
eh$mother_occu <-factor(eh$mother_occu,
                        levels=c(1,2,3,4,5,6,7),
                        labels=c("Goverment Staff", "Private worker", 
                                 "Own Business", "Farmer","Manual Worker",
                                 "No current work", "Other"))
eh$mother_occu

eh <- eh %>% rename("Smart Phone" = B1_a)
eh <- eh %>% rename("Tablet" = B1_b)
eh <- eh %>% rename("Laptop" = B1_c)
eh <- eh %>% rename("Desktop" = B1_d)
eh <- eh %>% rename("Other" = B1_e)


eh$B2
eh$B2 <- as.factor(eh$B2)
eh$B2 <-factor(eh$B2,
               levels=c(1,2),
               labels=c("Yes", "No"))
eh$B2


eh$B4
eh$B4 <- as.factor(eh$B4)
eh$B4 <-factor(eh$B4,
               levels=c(1,2),
               labels=c("Yes", "No"))
eh$B4

eh <- eh %>% rename("Search engines" = B5_a)
eh <- eh %>% rename("Official Website" = B5_b)
eh <- eh %>% rename("General health website" = B5_c)
eh <- eh %>% rename("Social media" = B5_d)
eh <- eh %>% rename("Online Disscussion Group" = B5_e)
eh <- eh %>% rename("Mobile Apps" = B5_f)
eh <- eh %>% rename("Others" = B5_g)

eh <- eh %>% 
  mutate(eHealth_score = C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10)


eh$C1
eh$C1 <- as.factor(eh$C1)
eh$C1 <-factor(eh$C1,
               levels=c(1,2,3,4,5),
               labels=c("Not useful at all", "Not useful", "Unsure",
                        "Useful","Very useful"))
eh$C1

eh$C2
eh$C2 <- as.factor(eh$C2)
eh$C2 <-factor(eh$C2,
               levels=c(1,2,3,4,5),
               labels=c("Not important at all", "Not important", "Unsure",
                        "important","Very important"))
eh$C2


attitude <- function(values){
  if (is.numeric(values)){
    as.factor(values)
  }
  factor(values,
         levels=c(1,2,3,4,5),
         labels=c("Strongly Disagree", "Disagree","Undecided","Agree",
                  "Strongly Agree"))
}

eh$C3 <- attitude(eh$C3)
eh$C4 <- attitude(eh$C4)
eh$C5 <- attitude(eh$C5)
eh$C6 <- attitude(eh$C6)
eh$C7 <- attitude(eh$C7)
eh$C8 <- attitude(eh$C8)
eh$C9 <- attitude(eh$C9)
eh$C10 <- attitude(eh$C10)

library(rio)

export(eh,"eh_clean_ttc.rds")









