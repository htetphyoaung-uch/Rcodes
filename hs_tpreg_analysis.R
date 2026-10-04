setwd("c:/docs/house officers")
rm(list=ls())

library(tidyverse)


tpreg<- readRDS("tpreg_clean.rds")

dim(tpreg)
names(tpreg)

mStats::codebook(tpreg)

#Section A. Socio-demographic characteristics 

sum(is.na(tpreg$age))

fivenum(tpreg$age)
mean(tpreg$age)

tpreg %>% 
  ggplot(aes(age)) +
  geom_boxplot(fill="cyan")+
  coord_flip()+
  theme_minimal()

tpreg %>% 
  ggplot(aes(age)) +
  geom_histogram(fill="cyan", bins = 5)+
  theme_minimal()

tpreg %>% 
  ggplot(aes(age,fill = gender)) +
  geom_density(aes(alpha = 0.2))+
  theme_minimal()+
  theme(legend.position = "")

mStats::tab(tpreg,gender,marital,religion,father_edu,mother_edu, family_type,
            current_relation,living,friend_married,friend_preg,friend_g_bf,
            b_gf)

#pie chart for sex
library(ggpie)

dat1 <- tpreg %>%
  count(gender)%>%
  arrange(desc(gender)) %>%
  mutate(percent = round(n*100/sum(n),1),
         lab_pos = cumsum(percent)-0.5*percent)

dat1$label <- paste(dat1$gender,"\n",round(dat1$percent),"%",sep="")
dat1

ggplot(dat1,
       aes(x = "",
           y = percent,
           fill = gender)) +
  geom_bar(width = 1,
           stat = "identity",
           color = "black") +
  geom_text(aes(y = lab_pos, label = label),
            color = "black") +
  coord_polar("y",
              start = 0,
              direction = -1) +
  scale_fill_brewer(palette = "Set2")+ 
  theme_void()+
  theme(legend.position = "FALSE")

#Marital status

percent_marital <-tpreg %>%
  count(marital)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_marital %>%
  ggplot(aes(marital,percent,fill= marital))+
  geom_col()+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="Marital Status",
       x="",
       y="Percent")

#Religion

tpreg %>% 
  ggplot(aes(religion, fill= religion))+
  geom_bar()+
  scale_fill_brewer(palette = "Set2")+ 
  theme(legend.position = "FALSE")



percent_religion <-tpreg %>%
  count(religion)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_religion %>%
  ggplot()+
  geom_col(aes(religion,percent,fill= religion))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="Religion",
       x="",
       y="Percent")

#Expenditure

tpreg %>% 
  ggplot(aes(expenditure)) +
  geom_boxplot(fill="cyan")+
  coord_flip()+
  theme_minimal()

tpreg %>% 
  ggplot(aes(expenditure)) +
  geom_histogram(fill="cyan")+
  theme_minimal()

fivenum(tpreg$expenditure)
mean(tpreg$expenditure)

#Father's education

tpreg %>% 
  ggplot(aes(father_edu, fill= father_edu))+
  geom_bar()+
  scale_fill_brewer(palette = "Set2")+ 
  theme(legend.position = "FALSE")

percent_fedu <-tpreg %>%
  count(father_edu)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_fedu %>%
  ggplot()+
  geom_col(aes(father_edu,percent,fill= father_edu))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="Father's Education",
       x="",
       y="Percent")

#mother's education

tpreg %>% 
  ggplot(aes(mother_edu, fill= mother_edu))+
  geom_bar()+
  scale_fill_brewer(palette = "Set2")+ 
  theme(legend.position = "FALSE")

percent_medu <-tpreg %>%
  count(mother_edu)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_medu %>%
  ggplot()+
  geom_col(aes(mother_edu,percent,fill= mother_edu))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="Mother's Education",
       x="",
       y="Percent")

#family type

tpreg %>% 
  ggplot(aes(family_type, fill= family_type))+
  geom_bar()+
  scale_fill_brewer(palette = "Set2")+ 
  theme(legend.position = "FALSE")

percent_ftype <-tpreg %>%
  count(family_type)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_ftype %>%
  ggplot()+
  geom_col(aes(family_type,percent,fill= family_type))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="Family Type",
       x="",
       y="Percent")

#number of family members

fivenum(tpreg$family_members)

tpreg %>% 
  ggplot(aes(family_members)) +
  geom_boxplot(fill="cyan")+
  coord_flip()+
  theme_minimal()

#current relation of parents

tpreg %>% 
  ggplot(aes(current_relation, fill= current_relation))+
  geom_bar()+
  scale_fill_brewer(palette = "Set2")+ 
  theme(legend.position = "FALSE")

percent_crelation <-tpreg %>%
  count(current_relation)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_crelation %>%
  ggplot()+
  geom_col(aes(current_relation,percent,fill= current_relation))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="Current Relation Status of Parents",
       x="",
       y="Percent")


#Currently living condition 

tpreg %>% 
  ggplot(aes(living, fill= living))+
  geom_bar()+
  scale_fill_brewer(palette = "Set2")+ 
  theme(legend.position = "FALSE")

percent_living <-tpreg %>%
  count(living)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_living %>%
  ggplot()+
  geom_col(aes(living,percent,fill= living))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="Current Living Status",
       x="",
       y="Percent")


#close friend married

dat3 <- tpreg %>%
  count(friend_married)%>%
  arrange(desc(friend_married)) %>%
  mutate(percent = round(n*100/sum(n),1),
         lab_pos = cumsum(percent)-0.5*percent)

dat3$label <- paste(dat3$friend_married,"\n",round(dat3$percent),"%",sep="")
dat3

ggplot(dat3,
       aes(x = "",
           y = percent,
           fill = friend_married)) +
  geom_bar(width = 1,
           stat = "identity",
           color = "black") +
  geom_text(aes(y = lab_pos, label = label),
            color = "black") +
  coord_polar("y",
              start = 0,
              direction = -1) +
  scale_fill_brewer(palette = "Set2")+ 
  theme_void()+
  theme(legend.position = "FALSE")

#preg in friend

dat4 <- tpreg %>%
  count(friend_preg)%>%
  arrange(desc(friend_preg)) %>%
  mutate(percent = round(n*100/sum(n),1),
         lab_pos = cumsum(percent)-0.5*percent)

dat4$label <- paste(dat4$friend_preg,"\n",round(dat4$percent),"%",sep="")
dat4

ggplot(dat4,
       aes(x = "",
           y = percent,
           fill = friend_preg)) +
  geom_bar(width = 1,
           stat = "identity",
           color = "black") +
  geom_text(aes(y = lab_pos, label = label),
            color = "black") +
  coord_polar("y",
              start = 0,
              direction = -1) +
  scale_fill_brewer(palette = "Set2")+ 
  theme_void()+
  theme(legend.position = "FALSE")

#friends have boy/girl fri

dat5 <- tpreg %>%
  count(friend_g_bf)%>%
  arrange(desc(friend_g_bf)) %>%
  mutate(percent = round(n*100/sum(n),1),
         lab_pos = cumsum(percent)-0.5*percent)

dat5$label <- paste(dat5$friend_g_bf,"\n",round(dat5$percent),"%",sep="")
dat5

ggplot(dat5,
       aes(x = "",
           y = percent,
           fill = friend_g_bf)) +
  geom_bar(width = 1,
           stat = "identity",
           color = "black") +
  geom_text(aes(y = lab_pos, label = label),
            color = "black") +
  coord_polar("y",
              start = 0,
              direction = -1) +
  scale_fill_brewer(palette = "Set2")+ 
  theme_void()+
  theme(legend.position = "FALSE")

#do you have BF/GF

mStats::tab(tpreg, b_gf, by = gender)

tpreg %>% 
  ggplot(aes(gender, fill = b_gf)) +
  geom_bar(stat = "count",
           position="fill",
           show.legend = T) +
  labs(title = "Percentage barplot",
       x = "",
       y = "") +
  theme_minimal()+
  theme(legend.position = "bottom")

tpreg %>% 
  ggplot(aes(b_gf, fill= b_gf))+
  geom_bar()+
  scale_fill_brewer(palette = "Set2")+ 
  theme(legend.position = "FALSE")

tpreg %>% 
  ggplot(aes(b_gf, fill= b_gf))+
  geom_bar()+
  facet_wrap(~gender)+
  scale_fill_brewer(palette = "Set2")+ 
  theme(legend.position = "FALSE")

percent_b_gf <-tpreg %>%
  count(b_gf)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_b_gf %>%
  ggplot()+
  geom_col(aes(b_gf,percent,fill= b_gf))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="",
       x="",
       y="Percent")

#Section B. Knowledge questions

mStats::tab(tpreg,B1,B2,B4,sti)

#one occasion can get preg

dat6 <- tpreg %>%
  count(B1)%>%
  arrange(desc(B1)) %>%
  mutate(percent = round(n*100/sum(n),1),
         lab_pos = cumsum(percent)-0.5*percent)

dat6$label <- paste(dat6$B1,"\n",round(dat6$percent),"%",sep="")
dat6

ggplot(dat6,
       aes(x = "",
           y = percent,
           fill = B1)) +
  geom_bar(width = 1,
           stat = "identity",
           color = "black") +
  geom_text(aes(y = lab_pos, label = label),
            color = "black") +
  coord_polar("y",
              start = 0,
              direction = -1) +
  scale_fill_brewer(palette = "Set2")+ 
  theme_void()+
  theme(legend.position = "FALSE")

#do you know contraceptives

dat7 <- tpreg %>%
  count(B2)%>%
  arrange(desc(B2)) %>%
  mutate(percent = round(n*100/sum(n),1),
         lab_pos = cumsum(percent)-0.5*percent)

dat7$label <- paste(dat7$B2,"\n",round(dat7$percent),"%",sep="")
dat7

ggplot(dat7,
       aes(x = "",
           y = percent,
           fill = B2)) +
  geom_bar(width = 1,
           stat = "identity",
           color = "black") +
  geom_text(aes(y = lab_pos, label = label),
            color = "black") +
  coord_polar("y",
              start = 0,
              direction = -1) +
  scale_fill_brewer(palette = "Set2")+ 
  theme_void()+
  theme(legend.position = "FALSE")

#contraceptives multiple response

library(ufs)

tpreg %>% 
  filter(B2=="Yes") %>% 
  multiResponse(c("oc_pill","depo","iucd","e_pill","condom","intradermal"))

#where to get contraceptives

dat8 <- tpreg %>%
  count(B4)%>%
  arrange(desc(B4)) %>%
  mutate(percent = round(n*100/sum(n),1),
         lab_pos = cumsum(percent)-0.5*percent)

dat8$label <- paste(dat8$B4,"\n",round(dat8$percent),"%",sep="")
dat8

ggplot(dat8,
       aes(x = "",
           y = percent,
           fill = B4)) +
  geom_bar(width = 1,
           stat = "identity",
           color = "black") +
  geom_text(aes(y = lab_pos, label = label),
            color = "black") +
  coord_polar("y",
              start = 0,
              direction = -1) +
  scale_fill_brewer(palette = "Set2")+ 
  theme_void()+
  theme(legend.position = "FALSE")

#places to get contraceptives (multiple response)

tpreg %>% 
  filter(B4=="Yes") %>% 
  multiResponse(c("hospital","mch","private","ngo","drug_store","vender"))

#information source

tpreg %>% 
  multiResponse(c("source_parent","source_friends","source_talks","source_media",
                  "source_internet"))
#know sti

mStats::tab(tpreg,sti)

dat9 <- tpreg %>%
  count(sti)%>%
  arrange(desc(sti)) %>%
  mutate(percent = round(n*100/sum(n),1),
         lab_pos = cumsum(percent)-0.5*percent)

dat9$label <- paste(dat9$sti,"\n",round(dat8$percent),"%",sep="")
dat9

ggplot(dat9,
       aes(x = "",
           y = percent,
           fill = sti)) +
  geom_bar(width = 1,
           stat = "identity",
           color = "black") +
  geom_text(aes(y = lab_pos, label = label),
            color = "black") +
  coord_polar("y",
              start = 0,
              direction = -1) +
  scale_fill_brewer(palette = "Set2")+ 
  theme_void()+
  theme(legend.position = "FALSE")

#STI (multiple Response)

tpreg %>% 
  filter(sti=="Yes") %>% 
  multiResponse(c("sti_syphilis","sti_gono","sti_HBV","sti_HIV","sti_Herpes"))

#Sectioin C. Perceptions

mStats::tab(tpreg,C1,C2,C3,C4,C5,C6,C7,C8)

percent_C1 <-tpreg %>%
  count(C1)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_C1 %>%
  ggplot()+
  geom_col(aes(C1,percent,fill= C1))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="",
       x="",
       y="Percent")

percent_C2 <-tpreg %>%
  count(C2)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_C2 %>%
  ggplot()+
  geom_col(aes(C2,percent,fill= C2))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="",
       x="",
       y="Percent")

percent_C3 <-tpreg %>%
  count(C3)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_C3 %>%
  ggplot()+
  geom_col(aes(C3,percent,fill= C3))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="",
       x="",
       y="Percent")

percent_C4 <-tpreg %>%
  count(C4)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_C4 %>%
  ggplot()+
  geom_col(aes(C4,percent,fill= C4))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="",
       x="",
       y="Percent")

percent_C5 <-tpreg %>%
  count(C5)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_C5 %>%
  ggplot()+
  geom_col(aes(C5,percent,fill= C5))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="",
       x="",
       y="Percent")

percent_C6 <-tpreg %>%
  count(C6)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_C6 %>%
  ggplot()+
  geom_col(aes(C6,percent,fill= C6))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="",
       x="",
       y="Percent")

percent_C7 <-tpreg %>%
  count(C7)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_C7 %>%
  ggplot()+
  geom_col(aes(C7,percent,fill= C7))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="",
       x="",
       y="Percent")

percent_C8 <-tpreg %>%
  count(C8)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_C8

percent_C8 %>%
  ggplot()+
  geom_col(aes(C8,percent,fill= C8))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="",
       x="",
       y="Percent")


percent_C8

percent_C8 %>%
  ggplot(aes(C8,percent,fill= C8,label= percent))+
  geom_col()+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="",
       x="",
       y="Percent")+
  geom_text(size= 3,
            vjust= -0.3)


#cronbach alpha for reliability

library(psych)

a <- tpreg %>% 
  select(C1, C2, C3, C4, C5, C6, C7 , C8)

a$C1 <- as.numeric(a$C1)
a$C2 <- as.numeric(a$C2)
a$C3 <- as.numeric(a$C3)
a$C4 <- as.numeric(a$C4)
a$C5 <- as.numeric(a$C5)
a$C6 <- as.numeric(a$C6)
a$C7 <- as.numeric(a$C7)
a$C8 <- as.numeric(a$C8)


alpha(a)



