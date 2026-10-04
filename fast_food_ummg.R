setwd("c:/docs/house officers")
rm(list=ls())

library(tidyverse)


food <- readRDS("food_2_2024_clean.rds")

mStats::codebook(food)
dim(food)

#Socio-demographics

#age

mean(food$age)
fivenum(food$age)

food %>% 
  ggplot(aes(age)) +
  geom_density(fill="cyan")+
  theme_minimal()+
  labs(title="",
       x="Age",
       y="Density")

#frequency table for catagorized variables

mStats::tab(food, sex, marital,religion,living)

#Sex

library(ggpie)

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
  labs(title="",
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
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  labs(title="",
       x="",
       y="Percent")

#expenditure

mean(food$expend)
fivenum(food$expend)

options(scipen = 999)

food %>% 
  ggplot(aes(expend))+
  geom_density(fill = "cyan")+
  theme_minimal()+
  labs(title = "",
       x = "Kyats",
       y = "Density")

#Current living status

percent_living <- food %>%
  count(living)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_living %>%
  ggplot()+
  geom_col(aes(living,percent,fill= living))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  labs(title="",
       x="",
       y="Percent")

#Amount of expenditure spending on food

mean(food$spend_food)
fivenum(food$spend_food)

food %>% 
  ggplot(aes(spend_food))+
  geom_density(fill = "cyan")+
  theme_minimal()+
  labs(title = "",
       x = "Kyats",
       y = "Density")

#percentage of expenditure for food

food <- food %>% 
  mutate(percent_expend = ((spend_food/expend)*100))

mean(food$percent_expend)
fivenum(food$percent_expend)

food %>% 
  ggplot(aes(percent_expend))+
  geom_boxplot()+
  scale_x_continuous(limits = c(0,100))+
  theme_minimal()+
  labs( title = "")


### Knowledge Questions

#B1 Are you aware about nutritional information and ingredients content in each 
#of fast food that you consumed?

percent_B1 <- food %>%
  count(B1)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_B1 %>%
  ggplot()+
  geom_col(aes(B1,percent,fill= B1))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  labs(title="",
       x="",
       y="Percent")

#B2 Are you aware of diseases that will affect you from consumption of fast food

percent_B2 <- food %>%
  count(B2)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_B2 %>%
  ggplot()+
  geom_col(aes(B2,percent,fill= B2))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  labs(title="",
       x="",
       y="Percent")

#B3 Which ingredients generally fast-food rich in?

percent_B3 <- food %>%
  count(B3)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_B3 %>%
  ggplot()+
  geom_col(aes(B3,percent,fill= B3))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  labs(title="",
       x="",
       y="Percent")

#B4 What does fast food generally refer to?

percent_B4 <- food %>%
  count(B4)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_B4 %>%
  ggplot()+
  geom_col(aes(B4,percent,fill= B4))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  theme(axis.text.x = element_text(ang= 10))+
  labs(title="",
       x="",
       y="Percent")

#B5 Over-consumption of fast food contribute to?

percent_B5 <- food %>%
  count(B5)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_B5 %>%
  ggplot()+
  geom_col(aes(B5,percent,fill= B5))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  theme(axis.text.x = element_text(ang= 10))+
  labs(title="",
       x="",
       y="Percent")

#B6 How should fast food be categorized?

percent_B6 <- food %>%
  count(B6)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_B6 %>%
  ggplot()+
  geom_col(aes(B6,percent,fill= B6))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  theme(axis.text.x = element_text(ang= 15))+
  labs(title="",
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
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  theme(axis.text.x = element_text(ang= 15))+
  labs(title="",
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
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  labs(title="",
       x="",
       y="Percent")

# B9 Knowledge Source

library(ufs)

food %>% 
  multiResponse(c("Health Workers","Pamphlet/Journal/Magazine",
                  "Internet/Social Media","Family/Friends/Peers"))

# knowledge score

mean(food$knowledge_score)
fivenum(food$knowledge_score)

food %>% 
  ggplot(aes(knowledge_score))+
  geom_density(fill = "cyan")+
  theme_minimal()+
  labs(title = "",
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
  labs(title = "")

# knowledge score by sex

food %>%
  select(sex,knowledge_score) %>% 
  group_by(sex) %>%
  mutate(mean_by_sex = mean(knowledge_score)) %>%
  ungroup() %>%
  ggplot(aes(sex, knowledge_score, colour = sex,
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
  geom_segment(aes(x = sex, xend = sex,
                   y = mean(knowledge_score), yend = mean_by_sex),
               size = 2, show.legend = F) +
  labs(title = "Knowledge score by sex",
       x = "",
       y = "Knowledge_score") +
  theme(legend.position = "none") +
  theme_minimal()

library(tidyquant)
library(ggdist)
library(gghalves)

food %>% 
  ggplot(aes(sex, knowledge_score,fill= sex)) + 
  stat_halfeye(adjust = 1,
               width = .3, 
               .width = 0, 
               justification = -.3,
               point_colour = NA) +
  stat_dots(side = "left", dotsize = 2, justification = 1.1, binwidth = .1)+
  coord_flip() + theme(panel.grid.major = element_line(linetype = "blank"),
                       panel.grid.minor = element_line(linetype = "blank"),
                       panel.background = element_rect(fill = "white")) +
  theme_minimal()+
  theme(legend.position = "none") +
  labs(title = "Raincloud plot",
       subtitle = "")




## Practice on Fast-food

#C1 Did you eat any kind of fast-food during last week?

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
  labs(title="")



# C2 if Yes, Which kind of fast food do you usually eat?(multiple response)

food %>% 
  filter(C1 =="Yes") %>% 
  multiResponse(c("Instant Noodles","Deep-fried foods","Cake/Biscuits/Icecreams",
                  "Canned foods","Sandwich/Burger","Crips/Chips",
                  "Street food/Barbecue"))


# C3 How many times did you consume fast food during the last week?

percent_C3 <- food %>%
  filter(C1 == "Yes") %>% 
  count(C3)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_C3 %>%
  na.omit() %>% 
  ggplot()+
  geom_col(aes(C3,percent,fill= C3))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(legend.position = "FALSE")+
  labs(title="",
       x="",
       y="Percent")
food$C3

food <- food %>% 
  mutate( C33 = as.numeric(C3))

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
                        labels=c("Low Consumption", "High Consumption"))

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
  labs(title = "")



# C4 Which time do you prefer to eat fast food?

percent_C4 <- food %>%
  filter(C1 == "Yes") %>% 
  count(C4)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_C4 %>%
  ggplot()+
  geom_col(aes(C4,percent,fill= C4))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme_minimal()+
  theme(panel.grid.major = element_line(linetype = "blank"),
        legend.position = "none")+
  labs(title="",
       x="",
       y="Percent") 



# C5 Why do you eat fast food?(multiple response)


food %>% 
  multiResponse(c("Tasty","Cheap","Easily Avialiable","Less Time consuming"))

food %>% 
  na.omit() %>% 
  mStats::tab(sex, marital,living, B1, B2, B3, B4, B5, B6, B6, B7, B8, C1, C3,C4)


library(psych)

harmonic.mean(food$age)

mean(food$age)



