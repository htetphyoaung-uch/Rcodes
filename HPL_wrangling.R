setwd("c:/docs/house officers")
rm(list=ls())

library(tidyverse)

hpl <- readxl::read_excel("HPL_data.xlsx")

dim(hpl)
names(hpl)

mStats::codebook(hpl)

hpl$sex
hpl$sex <- as.factor(hpl$sex)
hpl$sex <-factor(hpl$sex,
                levels=c(1,2),
                labels=c("Male", "Female"))
hpl$sex

hpl$marital
hpl$marital <- as.factor(hpl$marital)
hpl$marital <-factor(hpl$marital,
                    levels=c(1,2,3),
                    labels=c("Single", "Married", "Other"))
hpl$marital

hpl$father_edu
hpl$father_edu <- as.factor(hpl$father_edu)
hpl$father_edu <-factor(hpl$father_edu,
                       levels=c(1,2,3,4,5,6),
                       labels=c("Illiterate", "Read and write", "Primary",
                                "Middle","High", "Uni/Graduate"))
hpl$father_edu

hpl$mother_edu
hpl$mother_edu <- as.factor(hpl$mother_edu)
hpl$mother_edu <-factor(hpl$mother_edu,
                       levels=c(1,2,3,4,5,6),
                       labels=c("Illiterate", "Read and write", "Primary",
                                "Middle","High", "Uni/Graduate"))
hpl$mother_edu

hpl$father_occu
hpl$father_occu <- as.factor(hpl$father_occu)
hpl$father_occu <-factor(hpl$father_occu,
                        levels=c(1,2,3,4,5,6,7),
                        labels=c("Goverment Staff", "Private worker", 
                                 "Own Business", "Farmer","Manual Worker",
                                 "No current work", "Other"))
hpl$father_occu

hpl$mother_occu
hpl$mother_occu <- as.factor(hpl$mother_occu)
hpl$mother_occu <-factor(hpl$mother_occu,
                        levels=c(1,2,3,4,5,6,7),
                        labels=c("Goverment Staff", "Private worker", 
                                 "Own Business", "Farmer","Manual Worker",
                                 "No current work", "Other"))
hpl$mother_occu

#hpl score

hpl <- hpl %>% 
  mutate(hpl_score = B1 + B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 +
           B11 + B12 + B13 + B14 + B15 + B16 + B17 + B18 + B19 + B20 +
           B21 + B22 + B23 + B24 + B25 + B26 + B27 + B28 + B29 + B30 +
           B31 + B32 + B33 + B34 + B35 + B36 + B37 + B38 + B39 + B40 +
           B41 + B42 + B43 + B44 + B45 + B46 + B47 + B48 + B49 + B50 +
           B51 + B52)






lifestyle <- function(values){
  if (is.numeric(values)){
    as.factor(values)
  }
  factor(values,
         levels=c(1,2,3,4),
         labels=c("Never", "Sometimes","Often","Routinely"))
}

hpl$B1 <- lifestyle(hpl$B1)
hpl$B2 <- lifestyle(hpl$B2)
hpl$B3 <- lifestyle(hpl$B3)
hpl$B4 <- lifestyle(hpl$B4)
hpl$B5 <- lifestyle(hpl$B5)
hpl$B6 <- lifestyle(hpl$B6)
hpl$B7 <- lifestyle(hpl$B7)
hpl$B8 <- lifestyle(hpl$B8)
hpl$B9 <- lifestyle(hpl$B9)

hpl$B10 <- lifestyle(hpl$B10)
hpl$B11 <- lifestyle(hpl$B11)
hpl$B12 <- lifestyle(hpl$B12)
hpl$B13 <- lifestyle(hpl$B13)
hpl$B14 <- lifestyle(hpl$B14)
hpl$B15 <- lifestyle(hpl$B15)
hpl$B16 <- lifestyle(hpl$B16)
hpl$B17 <- lifestyle(hpl$B17)
hpl$B18 <- lifestyle(hpl$B18)
hpl$B19 <- lifestyle(hpl$B19)

hpl$B20 <- lifestyle(hpl$B20)
hpl$B21 <- lifestyle(hpl$B21)
hpl$B22 <- lifestyle(hpl$B22)
hpl$B23 <- lifestyle(hpl$B23)
hpl$B24 <- lifestyle(hpl$B24)
hpl$B25 <- lifestyle(hpl$B25)
hpl$B26 <- lifestyle(hpl$B26)
hpl$B27 <- lifestyle(hpl$B27)
hpl$B28 <- lifestyle(hpl$B28)
hpl$B29 <- lifestyle(hpl$B29)

hpl$B30 <- lifestyle(hpl$B30)
hpl$B31 <- lifestyle(hpl$B31)
hpl$B32 <- lifestyle(hpl$B32)
hpl$B33 <- lifestyle(hpl$B33)
hpl$B34 <- lifestyle(hpl$B34)
hpl$B35 <- lifestyle(hpl$B35)
hpl$B36 <- lifestyle(hpl$B36)
hpl$B37 <- lifestyle(hpl$B37)
hpl$B38 <- lifestyle(hpl$B38)
hpl$B39 <- lifestyle(hpl$B39)

hpl$B40 <- lifestyle(hpl$B40)
hpl$B41 <- lifestyle(hpl$B41)
hpl$B42 <- lifestyle(hpl$B42)
hpl$B43 <- lifestyle(hpl$B43)
hpl$B44 <- lifestyle(hpl$B44)
hpl$B45 <- lifestyle(hpl$B45)
hpl$B46 <- lifestyle(hpl$B46)
hpl$B47 <- lifestyle(hpl$B47)
hpl$B48 <- lifestyle(hpl$B48)
hpl$B49 <- lifestyle(hpl$B49)

hpl$B50 <- lifestyle(hpl$B50)
hpl$B51 <- lifestyle(hpl$B51)
hpl$B52 <- lifestyle(hpl$B52)


library(rio)

export(hpl,"hpl_clean.rds")









