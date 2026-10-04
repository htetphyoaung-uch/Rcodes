setwd("c:/docs/house officers")
rm(list=ls())

library(tidyverse)


con <- readRDS("contraception_clean2.rds")

dim(con)
names(con)

mStats::codebook(con)

###Socio_demographics
#age

mean(con$age)
fivenum(con$age)

con %>% 
  ggplot(aes(age)) +
  geom_histogram(fill="cyan", bins = 10)+
  theme_minimal()+
  labs(title="Histogram of Age Distribution",
       x="Age",
       y="Frequency")

con %>% 
  ggplot(aes(age)) +
  geom_density(fill="cyan")+
  theme_minimal()+
  labs(title="Age Distribution",
       x="Age",
       y="Density")

#frequency table for categorized variables

mStats::tab(con, race, religion, edu, occupation)

#race
#bar plot(%)

percent_race <-con %>%
  count(race)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_race %>%
  ggplot()+
  geom_col(aes(race,percent,fill= race))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme(legend.position = "FALSE")+
  labs(title="Race of respondant",
       x="",
       y="Percent")


#religion
#bar plot(%)

percent_religion <-con %>%
  count(religion)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_religion %>%
  ggplot()+
  geom_col(aes(religion,percent,fill= religion))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme(legend.position = "FALSE")+
  labs(title="Religion",
       x="",
       y="Percent")

#edu
#bar plot(%)

percent_edu <-con %>%
  count(edu)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_edu %>%
  ggplot()+
  geom_col(aes(edu,percent,fill= edu))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme(legend.position = "FALSE")+
  labs(title="Education level of respondant",
       x="",
       y="Percent")

#Occupation
#bar plot(%)

percent_occupation <-con %>%
  count(occupation)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_occupation %>%
  ggplot()+
  geom_col(aes(occupation,percent,fill= occupation))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme(legend.position = "FALSE")+
  labs(title="Occupation status of respondant",
       x="",
       y="Percent")

#age at marriage
mean(con$age_marriage)
fivenum(con$age_marriage)

con %>% 
  ggplot(aes(age_marriage))+
  geom_density(fill = "cyan")+
  theme_minimal()+
  labs(title="Age at marriage",
       x="Age(yr)",
       y="Density")

#family income

mean(con$fam_income)
fivenum(con$fam_income)

con %>% 
  ggplot(aes(fam_income))+
  geom_density(fill = "cyan")+
  theme_minimal()+
  labs(title="Monthly family Income",
       x="Family Income(Kyats)",
       y="Density")

#no. of pregnancies

fivenum(con$n_preg)

con %>% 
  ggplot(aes(n_preg))+
  geom_bar(fill = "cyan")+
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+
  labs(title="Number of pregnancies of Respondents",
       x="No. of Pregnancies",
       y="Frequency")
  

con %>% 
  ggplot(aes(n_preg))+
  geom_density(fill = "cyan")+
  theme_minimal()+
  labs(title="Number of pregnancies distribution",
       x="No. of Pregnancies",
       y="Density")

#no. of children

fivenum(con$n_child)

con %>% 
  ggplot(aes(n_child))+
  geom_bar(fill = "cyan")+
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+
  labs(title="Number of children of Respondents",
       x="No. of children",
       y="Frequency")

con %>% 
  ggplot(aes(n_child))+
  geom_density(fill = "cyan")+
  theme_minimal()+
  labs(title="Number of children distribution",
       x="No. of children",
       y="Density")

#no. of abortions

fivenum(con$n_abort)

con %>% 
  ggplot(aes(n_abort))+
  geom_bar(fill = "cyan")+
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+
  labs(title="Number of abortions of Respondents",
       x="No. of abortions",
       y="Frequency")

con %>% 
  ggplot(aes(n_abort))+
  geom_density(fill = "cyan")+
  theme_minimal()+
  labs(title="Number of abortion distribution",
       x="No. of abortion",
       y="Density")

#no. of SB

fivenum(con$n_sb)

con %>% 
  ggplot(aes(n_sb))+
  geom_bar(fill = "cyan")+
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+
  labs(title="Number of still birth of Respondents",
       x="No. of still birth",
       y="Frequency")

con %>% 
  ggplot(aes(n_sb))+
  geom_density(fill = "cyan")+
  theme_minimal()+
  labs(title="Number of still birth distribution",
       x="No. of still birth",
       y="Density")


# Knowledge Questions

#B1

con %>% 
  ggplot(aes(B1))+
  geom_bar(fill = "cyan")+
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+
  labs(title="Know the Contraceptive methods",
       x="",
       y="Frequency")

#B2

con <- con %>% rename( "COC_pill" = "B2_a")
con <- con %>% rename( "Injection/Depo" = "B2_b")
con <- con %>% rename( "IUCD" = "B2_c")
con <- con %>% rename( "Dermal_implant" = "B2_d")
con <- con %>% rename( "Condom(M/F)" = "B2_e")
con <- con %>% rename( "Sterilization(M/F)" = "B2_f")
con <- con %>% rename( "Emergency_pill" = "B2_g")

#multiple response on contraceptive methods

library(ufs)

con %>% 
  filter(B1 =="Yes") %>% 
  multiResponse(c("COC_pill","Injection/Depo","IUCD","Dermal_implant",
                  "Condom(M/F)","Sterilization(M/F)","Emergency_pill"))

#B3 Minimum separation for each preg

library(ggpie)

dat1 <- con %>%
  count(B3)%>%
  arrange(desc(B3)) %>%
  mutate(percent = round(n*100/sum(n),1),
         lab_pos = cumsum(percent)-0.5*percent)

dat1$label <- paste(dat1$B3,"\n",round(dat1$percent),"%",sep="")
dat1

ggplot(dat1,
       aes(x = "",
           y = percent,
           fill = B3)) +
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
  theme(legend.position = "FALSE")+
  labs(title="Response for minimum separation of pregnancies")
  

#B4 when should start contraception

dat2 <- con %>%
  count(B4)%>%
  na.omit() %>% 
  arrange(desc(B4)) %>%
  mutate(percent = round(n*100/sum(n),1),
         lab_pos = cumsum(percent)-0.5*percent)

dat2$label <- paste(dat2$B4,"\n",round(dat2$percent),"%",sep="")
dat2

ggplot(dat2,
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
  theme(legend.position = "FALSE")+
  labs(title="When should start contraception after child birth")

#B5 Adventages of Contraception

con <- con %>% rename("Separation_bt_preg" = "B5_a")
con <- con %>% rename("Less_eco_burden" = "B5_b")
con <- con %>% rename("Health_benefits(M/C)" = "B5_c")
con <- con %>% rename("Prevent_unwant_preg" = "B5_d")

con %>% 
  filter(B1 =="Yes") %>% 
  multiResponse(c("Separation_bt_preg","Less_eco_burden","Health_benefits(M/C)",
                  "Prevent_unwant_preg"))

#B6 disadventages of Contraception

con <- con %>% rename("Irregular_menstruation" = "B6_a")
con <- con %>% rename("Weight_gain/loss" = "B6_b")
con <- con %>% rename("Headache/Migarine" = "B6_c")


con %>% 
  filter(B1 =="Yes") %>% 
  multiResponse(c("Irregular_menstruation","Weight_gain/loss",
                  "Headache/Migarine"))

#B7 danger 

dat3 <- con %>%
  count(B7)%>%
  na.omit() %>% 
  arrange(desc(B7)) %>%
  mutate(percent = round(n*100/sum(n),1),
         lab_pos = cumsum(percent)-0.5*percent)

dat3$label <- paste(dat3$B7,"\n",round(dat3$percent),"%",sep="")
dat3

ggplot(dat3,
       aes(x = "",
           y = percent,
           fill = B7)) +
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
  theme(legend.position = "FALSE")+
  labs(title="Is it dangerous if the pregnancies are too close?")

#B8 Knowledge source

con <- con %>% rename( "Health_workers" = "B8_a")
con <- con %>% rename( "Internet/Social_media" = "B8_b")
con <- con %>% rename( "Radio/TV" = "B8_c")
con <- con %>% rename( "Friends/Relatives" = "B8_d")
con <- con %>% rename( "Poster/journals/Pamphalets" = "B8_e")
con <- con %>% rename( "NGO/INGO/CSOs" = "B8_f")

con %>% 
  filter(B1 =="Yes") %>% 
  multiResponse(c("Health_workers","Internet/Social_media", "Radio/TV",
                  "Friends/Relatives","Poster/journals/Pamphalets","NGO/INGO/CSOs"))

#knowledge score

mean(con$knowledge_score, na.rm = T)
fivenum(con$knowledge_score, na.rm = T)

con %>% 
  ggplot(aes(knowledge_score))+
  geom_density(fill = "cyan")+
  theme_minimal()+
  labs(title="Knowledge Score Distribution",
       x="Knowledge Score",
       y="Density")

library(ggridges)
library(patchwork)
library(viridis)
library(hrbrthemes)

con %>% 
  ggplot(aes(knowledge_score,edu, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01,
                               alpha = 0.3) +
  labs(title = "Knowledge Score by Education level",
       y = "Education Level") +
  theme_bw() +
  theme(legend.position="none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8))



#Attitude

mStats::tab(con, "C1", "C2", "C3", "C4" , "C5", "C6" )

#C1

percent_C1<-con %>%
  count(C1)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_C1 %>%
  ggplot()+
  geom_col(aes(C1,percent,fill= C1))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme(legend.position = "FALSE")+
  labs(title="Everyone at legal age should know the contraceptive methods",
       x="",
       y="Percent")

#C2

percent_C2<-con %>%
  count(C2)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_C2 %>%
  ggplot()+
  geom_col(aes(C2,percent,fill= C2))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme(legend.position = "FALSE")+
  labs(title="Contraceptive methods should be used after consultation 
       with health care personals",
       x="",
       y="Percent")

#C3


percent_C3<-con %>%
  count(C3)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_C3 %>%
  ggplot()+
  geom_col(aes(C3,percent,fill= C3))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme(legend.position = "FALSE")+
  labs(title="Contraception should be used to prevent unwanted pregnancies",
       x="",
       y="Percent")

#C4

percent_C4<-con %>%
  count(C4)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_C4 %>%
  ggplot()+
  geom_col(aes(C4,percent,fill= C4))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme(legend.position = "FALSE")+
  labs(title="IUCD method should be used only with skilled health personals",
       x="",
       y="Percent")

#C5

percent_C5<-con %>%
  count(C5)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_C5 %>%
  ggplot()+
  geom_col(aes(C5,percent,fill= C5))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme(legend.position = "FALSE")+
  labs(title="One should always consult with the spouse 
       to use appropriate contraceptive method",
       x="",
       y="Percent")

#C6
percent_C6<-con %>%
  count(C6)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_C6 %>%
  ggplot()+
  geom_col(aes(C6,percent,fill= C6))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme(legend.position = "FALSE")+
  labs(title="Contraception can reduce abortions and maternal mortality",
       x="",
       y="Percent")

#attitude score

mean(con$Attitude_score, na.rm = T)
fivenum(con$Attitude_score, na.rm = T)

con %>% 
  ggplot(aes(con$Attitude_score))+
  geom_density(fill = "cyan")+
  theme_minimal()+
  labs(title="Attitude Score Distribution",
       x="Attitude Score",
       y="Density")



# Practice

#D1 Do you currently use any contraceptives?

mStats::tab(con, "D1")

dat4 <- con %>%
  count(D1)%>%
  na.omit() %>% 
  arrange(desc(D1)) %>%
  mutate(percent = round(n*100/sum(n),1),
         lab_pos = cumsum(percent)-0.5*percent)

dat4$label <- paste(dat4$D1,"\n",round(dat4$percent),"%",sep="")
dat4

ggplot(dat4,
       aes(x = "",
           y = percent,
           fill = D1)) +
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
  theme(legend.position = "FALSE")+
  labs(title="Do you currently use any contraceptives?")

# D2 If no, Why

percent_D2 <-con %>%
  count(D2)%>%
  na.omit() %>% 
  mutate(percent=round(n*100/sum(n),1))

percent_D2 %>%
  ggplot()+
  geom_col(aes(D2,percent,fill= D2))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme(legend.position = "FALSE")+
  labs(title="If not using any contraceptions, Why? (n= 111)",
       x="",
       y="Percent")

# D3 If yes, what method are you using?

percent_D3 <-con %>%
  filter(D1 == "Yes") %>% 
  count(D3)%>%
  na.omit() %>% 
  mutate(percent=round(n*100/sum(n),1))

percent_D3 %>%
  ggplot()+
  geom_col(aes(D3,percent,fill= D3))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme(legend.position = "FALSE")+
  labs(title="If yes, what method are you using?(n= 38)",
       x="",
       y="Percent")

#D4 How long using contraceptions?

con %>% 
  filter(D1 == "Yes") %>% 
  mStats::summ(D4)

con %>% 
  filter(D1 == "Yes") %>% 
  ggplot(aes(D4))+
  geom_density(fill = "cyan")+
  theme_minimal()+
  labs(title="Duration(yr) of contraceptives usage(n=38)",
       x="Year",
       y="Density")

#D5 Did you consult with the health care personals 
#when you start using current method?

dat5 <- con %>%
  count(D5)%>%
  na.omit() %>% 
  arrange(desc(D5)) %>%
  mutate(percent = round(n*100/sum(n),1),
         lab_pos = cumsum(percent)-0.5*percent)

dat5$label <- paste(dat5$D5,"\n",round(dat5$percent),"%",sep="")
dat5

ggplot(dat5,
       aes(x = "",
           y = percent,
           fill = D5)) +
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
  theme(legend.position = "FALSE")+
  labs(title="Do you consult with the health care personals
       when you start using current method?(n=38)")

#D6 Where did you get contraceptives?

percent_D6 <-con %>%
  filter(D1 == "Yes") %>% 
  count(D6)%>%
  na.omit() %>% 
  mutate(percent=round(n*100/sum(n),1))

percent_D6 %>%
  ggplot()+
  geom_col(aes(D6,percent,fill= D6))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme(legend.position = "FALSE")+
  labs(title="Where did you get contraceptives?(n=38)",
       x="",
       y="Percent")

#D7 Did you get any health problems while using contraceptives?

con %>% 
  filter(D1== "Yes") %>% 
  mStats::tab(D7, na.rm = T)

dat6 <- con %>%
  count(D7)%>%
  na.omit() %>% 
  arrange(desc(D7)) %>%
  mutate(percent = round(n*100/sum(n),1),
         lab_pos = cumsum(percent)-0.5*percent)

dat6$label <- paste(dat6$D7,"\n",round(dat6$percent),"%",sep="")
dat6

ggplot(dat6,
       aes(x = "",
           y = percent,
           fill = D7)) +
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
  theme(legend.position = "FALSE")+
  labs(title="Did you get any health problems 
       while using contraceptives?(n=38)")

# If yes, what is it?

con <- con %>% rename( "Irregular menstrautions" = "D8_a")
con <- con %>% rename( "Weight gain/loss" = "D8_b")
con <- con %>% rename( "Headache/migarine_2" = "D8_c")
con <- con %>% rename( "others" = "D8_d")

con %>% 
  filter(D1 =="Yes" & D7 == "Yes") %>% 
  multiResponse(c("Irregular menstrautions","Weight gain/loss", 
                  "Headache/migarine_2","others"))

#D9 did you consult with health care personals?

dat7 <- con %>%
  count(D9)%>%
  na.omit() %>% 
  arrange(desc(D9)) %>%
  mutate(percent = round(n*100/sum(n),1),
         lab_pos = cumsum(percent)-0.5*percent)

dat7$label <- paste(dat7$D9,"\n",round(dat7$percent),"%",sep="")
dat7

ggplot(dat7,
       aes(x = "",
           y = percent,
           fill = D9)) +
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
  theme(legend.position = "FALSE")+
  labs(title="Did you consult with health care personals?(n=13)")

#Knowledge by occupation

con %>%
  group_by(occupation) %>%
  mutate(mean_by_occupation = mean(knowledge_score)) %>%
  ungroup() %>%
  ggplot(aes(occupation, knowledge_score, colour = occupation,
             show.legend = F)) +
  coord_flip() +
  geom_jitter(show.legend = F,
              size = 4,
              alpha = 0.2,
              width = 0.05)+
  stat_summary(fun = mean, geom = "point", size = 8, show.legend = F) +
  geom_hline(aes(yintercept = mean(knowledge_score)),
             colour = "gray70",
             linewidth = 0.9) +
  geom_segment(aes(x = occupation, xend = occupation,
                   y = mean(knowledge_score), yend = mean_by_occupation),
               size = 2, show.legend = F) +
  labs(title = "Knowledge score by occupation",
       x = "Occupation",
       y = "Knowledge_score") +
  theme(legend.position = "none") +
  theme_bw()


#knowledge by edu

con %>%
  select(edu,knowledge_score) %>% 
  group_by(edu) %>%
  mutate(mean_by_edu = mean(knowledge_score)) %>%
  ungroup() %>%
  ggplot(aes(edu, knowledge_score, colour = edu,
             show.legend = F)) +
  coord_flip() +
  geom_jitter(show.legend = F,
              size = 3,
              alpha = 0.2,
              width = 0.05)+
  stat_summary(fun = mean, geom = "point", size = 8, show.legend = F) +
  geom_hline(aes(yintercept = mean(knowledge_score)),
             colour = "gray70",
             linewidth = 0.9) +
  geom_segment(aes(x = edu, xend = edu,
                   y = mean(knowledge_score), yend = mean_by_edu),
               size = 2, show.legend = F) +
  labs(title = "Knowledge score by education",
       x = "Education level",
       y = "Knowledge_score") +
  theme(legend.position = "none") +
  theme_minimal()

library(tidyquant)
library(ggdist)
library(gghalves)

con %>% 
  ggplot(aes(occupation, knowledge_score,fill=occupation)) + 
  stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = .1, outlier.shape = NA) +
  stat_dots(side = "left", dotsize = 1, justification = 1.1, binwidth = .1)+
  coord_flip() + theme(panel.grid.major = element_line(linetype = "blank"),
                       panel.grid.minor = element_line(linetype = "blank"),
                       panel.background = element_rect(fill = "white")) +
  theme_minimal()+
  labs(title = "Raincloud plot",
       subtitle = "Knowledge score with Occupation")



