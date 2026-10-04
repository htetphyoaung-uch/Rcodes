setwd("/Users/htetphyoaung/TrainR/house officers")
rm(list=ls())

library(tidyverse)


food <- readRDS("fastfood_clean.rds")

mStats::codebook(food)
dim(food)

#Socio-demographics

#age

mean(food$age)
fivenum(food$age)

food %>% 
  ggplot(aes(age)) +
  geom_bar(fill="cyan")+
  theme_minimal()+
  scale_x_continuous(limits = c(16,26))+
  labs(title="Age Distribution of Students",
       x="Age",
       y="Frequency")

food %>% 
  ggplot(aes(age)) +
  geom_histogram(aes(y= ..density..), bins = 13, fill = "cyan")+
  geom_density(alpha=.2, fill="cyan", adjust = 1.3)+
  geom_vline(aes(xintercept=mean(age)), color="darkred",
             linetype="dashed")+
  theme_minimal()+
  scale_x_continuous(limits = c(16,28))+
  labs(title="Age Distribution of Students",
       x="Age",
       y="")


#frequency table for catagorized variables

mStats::tab(food, sex, marital,religion,living)

#Sex

#library(ggpie)

dat1 <- food %>%
  count(sex)%>%
  arrange(desc(sex)) %>%
  mutate(percent = round(n*100/sum(n),1),
         lab_pos = cumsum(percent)-0.5*percent)

dat1$label <- paste(dat1$sex,"\n",round(dat1$percent),"%",sep="")
dat1

ggplot(dat1,
       aes(x = "",
           y = percent,
           fill = sex)) +
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

#marital status
#percent bar plot

percent_marital <- food %>%
  count(marital)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_marital %>%
  ggplot()+
  geom_col(aes(marital,percent,fill= marital))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  labs(title="Marital Status of Students",
       x="",
       y="Percent")

#religion
#percent bar plot

percent_religion <- food %>%
  count(religion)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_religion %>%
  ggplot()+
  geom_col(aes(religion,percent,fill= religion))+
  geom_text(aes(religion,
                percent, 
                label = scales::percent(percent/100)),
            vjust = -0.1, 
            colour="black")+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  labs(title="Religion of Students",
       x="",
       y="Percent")

#expenditure

mean(food$expend)

cut <- food %>% 
  select("expend") %>% 
  filter(expend < 600000) 

mean(cut$expend)

fivenum(food$expend)

options(scipen = 999)

#remove outlier

food %>% 
  filter(expend < 600000) %>% 
  ggplot(aes(expend))+
  geom_density(fill = "cyan")+
  theme_minimal()+
  labs(title = "Students' Expenditure per Month",
       x = "Kyats",
       y = "Density")

#Current living status

percent_living <- food %>%
  count(living)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_living %>%
  ggplot()+
  geom_col(aes(living,percent,fill= living))+
  geom_text(aes(living,percent, label = scales::percent(percent/100)),
            vjust = -0.5, 
            colour="black")+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  labs(title="Current Living Status",
       x="",
       y="Percent")

#Amount of expenditure spending on food

mean(food$spend_food)
fivenum(food$spend_food)

food %>% 
  filter(spend_food < 400000) %>%  #removing outlier
  ggplot(aes(spend_food))+
  geom_density(fill = "cyan")+
  theme_minimal()+
  labs(title = "Students' Expenditure for food per Month",
       x = "Kyats",
       y = "Density")

#percentage of expenditure for food

food <- food %>% 
  mutate(percent_expend = ((spend_food/expend)*100))

mean(food$percent_expend)
fivenum(food$percent_expend)

food %>% 
  ggplot(aes(percent_expend)) +
  geom_histogram(aes(y= ..density..), bins = 15, fill = "cyan")+
  geom_density(alpha=.2, fill="cyan", adjust = 1.3)+
  geom_vline(aes(xintercept=mean(percent_expend)), color="darkred",
             linetype="dashed")+
  theme_minimal()+
  scale_x_continuous(limits = c(0,100))+
  labs(title="Percentage of expenditure used for food",
       x="Percent",
       y="")



### Knowledge Questions

#B1 Are you aware about nutritional information and ingredients content in each 
#of fast food that you consumed?

mStats::tab(food, B1, B2, B3, B4, B5, B6, B7, B8)

percent_B1 <- food %>%
  count(B1)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_B1 %>%
  ggplot()+
  geom_col(aes(B1,percent,fill= B1))+
  geom_text(aes(B1,percent, label = scales::percent(percent/100)),
            vjust = -0.5, 
            colour="black")+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  labs(title="Aware of nutritional information and ingredients in foods",
       x="",
       y="Percent")

#B2 Are you aware of diseases that will affect you from consumption of fast food

percent_B2 <- food %>%
  count(B2)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_B2 %>%
  ggplot()+
  geom_col(aes(B2,percent,fill= B2))+
  geom_text(aes(B2,percent, label = scales::percent(percent/100)),
            vjust = -0.5, 
            colour="black")+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  labs(title="Aware of diseases from consumption of fast-foods",
       x="",
       y="Percent")

#B3 Which ingredients generally fast-food rich in?

percent_B3 <- food %>%
  count(B3)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_B3 %>%
  ggplot()+
  geom_col(aes(B3,percent,fill= B3))+
  geom_text(aes(B3,percent, label = scales::percent(percent/100)),
            vjust = -0.5, 
            colour="black")+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  labs(title="Which ingredients generally fast-foods rich in?",
       x="",
       y="Percent")

#B4 What does fast food generally refer to?

percent_B4 <- food %>%
  count(B4)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_B4 %>%
  ggplot()+
  geom_col(aes(B4,percent,fill= B4))+
  geom_text(aes(B4,percent, label = scales::percent(percent/100)),
            vjust = -0.5, 
            colour="black")+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  theme(axis.text.x = element_text(ang= 10))+
  labs(title="What does fast-foods generally refer to?",
       x="",
       y="Percent")

#B5 Over-consumption of fast food contribute to?

percent_B5 <- food %>%
  count(B5)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_B5 %>%
  ggplot()+
  geom_col(aes(B5,percent,fill= B5))+
  geom_text(aes(B5,percent, label = scales::percent(percent/100)),
            vjust = -0.5, 
            colour="black")+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  theme(axis.text.x = element_text(ang= 10))+
  labs(title="Over-consumption of fast-foods contribute to?",
       x="",
       y="Percent")

#B6 How should fast food be categorized?

percent_B6 <- food %>%
  count(B6)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_B6 %>%
  ggplot()+
  geom_col(aes(B6,percent,fill= B6))+
  geom_text(aes(B6,percent, label = scales::percent(percent/100)),
            vjust = -0.5, 
            colour="black")+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  theme(axis.text.x = element_text(ang= 15))+
  labs(title="How should fast-foods be categorized?",
       x="",
       y="Percent")

#B7 Instant noodles and deep-fried foods are usually rich in which of the
#following?

percent_B7 <- food %>%
  count(B7)%>%
  na.omit() %>% 
  mutate(percent=round(n*100/sum(n),1))

percent_B7 %>%
  ggplot()+
  geom_col(aes(B7,percent,fill= B7))+
  geom_text(aes(B7,percent, label = scales::percent(percent/100)),
            vjust = -0.5, 
            colour="black")+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  theme(axis.text.x = element_text(ang= 15))+
  labs(title="Instant noodles and deep-fried foods are usually rich in?",
       x="",
       y="Percent")

#B8 Which ingredients are usually added to fast food to enhance their flavor?

percent_B8 <- food %>%
  count(B8)%>%
  na.omit() %>% 
  mutate(percent=round(n*100/sum(n),1))

percent_B8 %>%
  ggplot()+
  geom_col(aes(B8,percent,fill= B8))+
  geom_text(aes(B8,percent, label = scales::percent(percent/100)),
            vjust = -0.5, 
            colour="black")+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  labs(title="Which ingredients are usually added to fast-food to enhance their flavor?",
       x="",
       y="Percent")

# B9 Knowledge Source

library(ufs)

k_source <- food %>% 
  multiResponse(c("Health Workers","Pamphlet/Journal/Magazine",
                  "Internet/Social Media","Family/Friends/Peers"))

k_source %>% 
  select(`Option`,`Percentage of (171) cases`) %>% 
  arrange(desc(`Percentage of (171) cases`))

# knowledge score

mean(food$knowledge_score)
fivenum(food$knowledge_score)

food %>% 
  ggplot(aes(knowledge_score))+
  geom_density(fill = "cyan")+
  theme_minimal()+
  labs(title = "Students' Knowledge score",
       x = "",
       y = "Density")

food <- food %>% 
  mutate(score_group = case_when( knowledge_score <= 10 ~ "1",
                                  knowledge_score  > 10 ~ "2"))

food$score_group
food$score_group <- as.factor(food$score_group)
food$score_group <- factor(food$score_group,
                           levels=c(1,2),
                           labels=c("Poor Knowledge", "Good Knowledge"))

#Knowledge group

dat4 <- food %>%
  count(score_group)%>%
  arrange(desc(score_group)) %>%
  mutate(percent = round(n*100/sum(n),1),
         lab_pos = cumsum(percent)-0.5*percent)

dat4$label <- paste(dat4$score_group,"\n",round(dat4$percent),"%",sep="")
dat4

ggplot(dat4,
       aes(x = "",
           y = percent,
           fill = score_group)) +
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
  labs(title = "Students' Knowledge Groups")

# knowledge score




  
library(tidyquant)
library(ggdist)
#library(gghalves)

food %>% 
  ggplot(aes(knowledge_score)) + 
  stat_halfeye(adjust = .5,
               width = .3, 
               .width = 0, 
               justification = -.3,
               point_colour = NA,
               fill = "steelblue1") +
  geom_boxplot(width = .1, outlier.shape = NA)+
  stat_dots(side = "left",
            dotsize = 0.7, 
            justification = 1.1, 
            binwidth = 0.3)+
  theme_minimal()+
  theme(axis.text.y=element_blank())+
  labs(title = "Raincloud plot",
       subtitle = "Knowledge score distribution of students")




## Practice on Fast-food

#C1 Did you eat any kind of fast-food during last week?

mStats::tab(food, C1)

dat3 <- food %>%
  count(C1)%>%
  arrange(desc(C1)) %>%
  mutate(percent = round(n*100/sum(n),1),
         lab_pos = cumsum(percent)-0.5*percent)

dat3$label <- paste(dat3$C1,"\n",round(dat3$percent),"%",sep="")
dat3

ggplot(dat3,
       aes(x = "",
           y = percent,
           fill = C1)) +
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
  labs(title="Did you eat any kind of fast-food during last week?")



# C2 if Yes, Which kind of fast food do you usually eat?(multiple response)

eat <-food %>% 
  filter(C1 =="Yes") %>% 
  multiResponse(c("Instant Noodles","Deep-fried foods","Cake/Biscuits/Icecreams",
                  "Canned foods","Sandwich/Burger","Crips/Chips",
                  "Street food/Barbecue"))

eat %>% 
  select(`Option`,`Percentage of (142) cases`) %>% 
  arrange(desc(`Percentage of (142) cases`))


# C3 How many times did you consume fast food during the last week?

percent_C3 <- food %>%
  filter(C1 == "Yes") %>% 
  count(C3)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_C3 %>%
  ggplot()+
  geom_col(aes(C3,percent,fill= C3))+
  geom_text(aes(C3,percent, label = scales::percent(percent/100)),
            vjust = -0.5, 
            colour="black")+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  labs(title="How many times did you consume fast-foods during the last week?",
       x="",
       y="Percent")


food$C3

food <- food %>% 
  mutate( C33 = as.numeric(C3))

food$C33

food <- food %>% 
  mutate(C3_group = recode(food$C33,
                           "1" = 1,
                           "2" = 1,
                           "3" = 2,
                           "4" = 2))

food$C3_group
food$C3_group <- as.factor(food$C3_group)
food$C3_group <- factor(food$C3_group,
                        levels=c(1,2),
                        labels=c("High Consumption", "Low Consumption"))

dat5 <- food %>%
  na.omit() %>% 
  count(C3_group)%>%
  arrange(desc(C3_group)) %>%
  mutate(percent = round(n*100/sum(n),1),
         lab_pos = cumsum(percent)-0.5*percent)

dat5$label <- paste(dat5$C3_group,"\n",round(dat5$percent),"%",sep="")
dat5

ggplot(dat5,
       aes(x = "",
           y = percent,
           fill = C3_group)) +
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
  labs(title = "Fast-Foods Consumption Groups")



# C4 Which time do you prefer to eat fast food?

percent_C4 <- food %>%
  filter(C1 == "Yes") %>% 
  count(C4)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_C4 %>%
  ggplot()+
  geom_col(aes(C4,percent,fill= C4))+
  geom_text(aes(C4,percent, label = scales::percent(percent/100)),
            vjust = -0.5, 
            colour="black")+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(panel.grid.major = element_line(linetype = "blank"),
        legend.position = "none")+
  labs(title="Which time do you prefer to eat fast-foods?",
       x="",
       y="Percent") 



# C5 Why do you eat fast food?(multiple response)


why_eat <- food %>% 
  multiResponse(c("Tasty","Cheap","Easily Avialiable","Less Time consuming"))

why_eat %>% 
  select(`Option`,`Percentage of (142) cases`) %>% 
  arrange(desc(`Percentage of (142) cases`))

food %>% 
  na.omit() %>% 
  mStats::tab(C1,C3,C4)


