setwd("c:/docs/house officers")
rm(list=ls())

library(tidyverse)
library(tidyquant)
library(ggdist)
library(gghalves)

sqi<- readRDS("sqi_clean3.rds")

dim(sqi)
names(sqi)

# Socio demographics
#age

mean(sqi$age)
fivenum(sqi$age)

sqi %>% 
  ggplot(aes(age)) +
  geom_histogram(fill="cyan", bins = 15)+
  theme_minimal()+
  labs(title="Histogram of Age Distribution",
       x="Age",
       y="Frequency")

# frequency table for catagorized veriables

mStats::tab(sqi, sex, marital, religion, living)

#sex
library(ggpie)

dat1 <- sqi %>%
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

#marital
#bar plot(%)

percent_marital <-sqi %>%
  count(marital)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_marital %>%
  ggplot()+
  geom_col(aes(marital,percent,fill= marital))+
  geom_text(aes(marital,percent, label = scales::percent(percent/100)),
            vjust = -0.5, 
            colour="black")+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="Marital Status",
       x="",
       y="Percent")

#religion
#bar plot(%)

percent_religion <-sqi %>%
  count(religion)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_religion %>%
  ggplot()+
  geom_col(aes(religion,percent,fill= religion))+
  geom_text(aes(religion,percent, label = scales::percent(percent/100)),
            vjust = -0.5, 
            colour="black")+
  theme_minimal()+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="Religion",
       x="",
       y="Percent")

#expenditure per month

options(scipen = 999)

mean(sqi$expend)
fivenum(sqi$expend)

sqi %>% 
  ggplot(aes(expend)) +
  geom_histogram(fill="cyan", bins = 15)+
  theme_minimal()+
  labs(title="Histogram of Average Expendicture per Month",
       x="Kyats",
       y="Frequency")

#living status
#bar plot(%)

percent_living <-sqi %>%
  count(living)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_living %>%
  ggplot()+
  geom_col(aes(living,percent,fill= living))+
  theme_minimal()+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="Current Living Status",
       x="",
       y="Percent")

#time go to bed

mean(sqi$bedtime)
fivenum(sqi$bedtime)

sqi %>% ggplot(aes(bedtime)) +
  geom_density(fill= "cyan")+
  labs(title="Average Time for Going to Bed",
       x="PM",
       y="Density")

# bedtime by sex

sqi %>% ggplot(aes(bedtime, fill = sex)) +
  geom_density(alpha = 0.3)+
  labs(title="Bedtime Distribution by Sex",
       x="PM",
       y="Density")+
  theme_minimal()

sqi %>% 
  ggplot(aes(sex, bedtime,fill= sex)) + 
  stat_halfeye(adjust = .5, width = .3, .width = c(0.5, 1)) + 
  stat_dots(side = "left", dotsize = .7, justification = 1.05, binwidth = .1)+
  coord_flip()


# minute to sleep

mean(sqi$minsleep)
fivenum(sqi$minsleep)

sqi %>% 
  ggplot(aes(minsleep)) +
  geom_density(fill="cyan")+
  theme_minimal()+
  labs(title="Time required to sleep",
                       x="Minutes",
                       y="Density")

#wake up time

mean(sqi$wakeup)
fivenum(sqi$wakeup)

#hr spent in bed

mean(sqi$hr_in_bed)
fivenum(sqi$hr_in_bed)

a <-sqi %>% ggplot(aes(hr_in_bed)) +
  geom_density(fill="cyan")+
  labs(title="Time Spent in Bed",
       x="Hours",
       y="Density")
a

#actual sleep

mean(sqi$actual_sleep)
fivenum(sqi$actual_sleep)

b <-sqi %>% ggplot(aes(actual_sleep)) +
  geom_density(fill="cyan")+
  labs(title="Actual Sleep",
       x="Hours",
       y="Density")
b

library(ggpubr)

ggarrange(a, b ,
          ncol = 2, nrow = 1)

s <- sqi %>% select(hr_in_bed, actual_sleep)

boxplot(s, beside = T)

mStats::summ(sqi, hr_in_bed, actual_sleep)

# habitual sleep efficiency (actual sleep / hr spent in bed *100)

mean(sqi$habit_s_efficiency)
fivenum(sqi$habit_s_efficiency)

sqi %>% 
  ggplot(aes(habit_s_efficiency))+
  geom_boxplot()+
  stat_boxplot(geom="errorbar",width=0.05)+
  xlim(50,100)+
  coord_flip()+
  labs(title="Habitual Sleep Efficiency",
       x="Percents",
       y="")

# 1.Subjective sleep quality(participants ratings on their sleep)

sqi <- sqi %>% 
  mutate(ssq = sub_sleep_quality)

sqi$ssq<- as.factor(sqi$ssq)                    
sqi$ssq <- factor(sqi$ssq,
                  levels = c(0,1,2,3),
                  labels = c("Very Bad","Fairly Bad","Fairly Good","Very Good"))

percent_ssq <-sqi %>%
  count(ssq)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_ssq %>%
  ggplot()+
  geom_col(aes(ssq,percent,fill= ssq))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  scale_y_continuous(limits = c(0,100))+
  labs(title="Subjective Sleep Quality",
       subtitle = "Participants rating their sleep quality for themselves",
       x="",
       y="Percent")

# 2. sleep latancy (score = ssq + B6a)

sqi <- sqi %>% 
  mutate(sl = sleep_latency)

sqi$sl<- as.factor(sqi$sl)                    
sqi$sl <- factor(sqi$sl,
                  levels = c(0,1,2,3),
                  labels = c("No sleep difficulty","Slight sleep difficulty",
                             "Some sleep difficulty","Severe sleep difficulty"))


percent_sl <-sqi %>%
  count(sl)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_sl %>%
  ggplot()+
  geom_col(aes(sl,percent,fill= sl))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme(legend.position = "FALSE")+
  theme(axis.text.x = element_text(ang= 15))+
  labs(title="Sleep Latancy Scoring",
       x="",
       y="Percent")

# 3. Sleep duration (actual sleep score B5)

sqi <- sqi %>% 
  mutate(sleep_duration_1 = sleep_duration)

sqi$sleep_duration_1 <- as.factor(sqi$sleep_duration_1)                    
sqi$sleep_duration_1 <- factor(sqi$sleep_duration_1,
                 levels = c(0,1,2,3),
                 labels = c("No sleep difficulty","Slight sleep difficulty",
                            "Some sleep difficulty","Severe sleep difficulty"))

percent_sleep_duration_1 <-sqi %>%
  count(sleep_duration_1)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_sleep_duration_1 %>%
  ggplot()+
  geom_col(aes(sleep_duration_1,percent,fill= sleep_duration_1))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,100))+
  theme(legend.position = "FALSE")+
  theme(axis.text.x = element_text(ang= 15))+
  labs(title="Sleep Duration Scoring",
       x="",
       y="Percent")

# 4. Habitual sleep efficiency score

sqi <- sqi %>% 
  mutate(hse_score_1 = hse_score)

sqi$hse_score_1 <- as.factor(sqi$hse_score_1)                    
sqi$hse_score_1 <- factor(sqi$hse_score_1,
                               levels = c(0,1,2,3),
                               labels = c("No sleep difficulty","Slight sleep difficulty",
                                          "Some sleep difficulty","Severe sleep difficulty"))

percent_hse_score_1 <-sqi %>%
  count(hse_score_1)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_hse_score_1 %>%
  na.omit() %>% 
  ggplot()+
  geom_col(aes(hse_score_1,percent,fill= hse_score_1))+
  scale_y_continuous(limits = c(0,100))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  theme(axis.text.x = element_text(ang= 15))+
  labs(title="Habitual Sleep Efficiency Scoring",
       x="",
       y="Percent")


# 5. Sleep Disturbances (B6_b to B6_i)

sqi <- sqi %>% 
  mutate(s_d_score_1 = s_d_score)

sqi$s_d_score_1 <- as.factor(sqi$s_d_score_1)                    
sqi$s_d_score_1 <- factor(sqi$s_d_score_1,
                          levels = c(0,1,2,3),
                          labels = c("No sleep difficulty","Slight sleep difficulty",
                                     "Some sleep difficulty","Severe sleep difficulty"))

percent_s_d_score_1<-sqi %>%
  count(s_d_score_1)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_s_d_score_1 %>%
  ggplot()+
  geom_col(aes(s_d_score_1,percent,fill= s_d_score_1))+
  scale_y_continuous(limits = c(0,100))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  labs(title="Sleep Disturbances Scoring",
       x="",
       y="Percent")

# 6. use of sleeping medications

sqi <- sqi %>% 
  mutate(sleep_med = sleep_medication)

sqi$sleep_med <- as.factor(sqi$sleep_med)                    
sqi$sleep_med <- factor(sqi$sleep_med,
                          levels = c(0,1,2,3),
                          labels = c("Not During Past Month","Less Than Once a Week",
                                     "One or Two Times a Week","Three or More Times a Week"))

percent_sleep_med<-sqi %>%
  count(sleep_med)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_sleep_med %>%
  ggplot()+
  geom_col(aes(sleep_med,percent,fill= sleep_med))+
  scale_y_continuous(limits = c(0,100))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE")+
  theme(axis.text.x = element_text(ang= 15))+
  labs(title="Use Of Sleeping Medications",
       x="",
       y="Percent")

# 7. Daytime Dysfunction Scoring (B8 + B9)

sqi <- sqi %>% 
  mutate(daytime_dysfun_score_1 = daytime_dysfun_score)

sqi$daytime_dysfun_score_1 <- as.factor(sqi$daytime_dysfun_score_1)                    
sqi$daytime_dysfun_score_1 <- factor(sqi$daytime_dysfun_score_1,
                          levels = c(0,1,2,3),
                          labels = c("No sleep difficulty","Slight sleep difficulty",
                                     "Some sleep difficulty","Severe sleep difficulty"))


percent_daytime_dysfunction_score_1 <- sqi %>%
  count(daytime_dysfun_score_1)%>%
  mutate(percent=round(n*100/sum(n),1))

percent_daytime_dysfunction_score_1 %>%
  ggplot()+
  geom_col(aes(daytime_dysfun_score_1,percent,fill= daytime_dysfun_score_1))+
  scale_y_continuous(limits = c(0,100))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "FALSE",axis.text.x = element_text(ang= 15))+
  labs(title="Daytime Dysfunction Scoring",
       x="",
       y="Percent")


## total psqi score

mean(sqi$psqi_score, na.rm = T)
fivenum(sqi$psqi_score)

#Overall pqsi
sqi %>% ggplot(aes(psqi_score)) +
  geom_density(fill = "cyan")+
  labs(title="Sleep Quality Score Distribution",
       x="PSQI Score",
       y="Density") + 
  theme(panel.background = element_rect(fill = "gray89"))

# by Sex

sqi %>% 
  na.omit() %>% 
  group_by(sex) %>% 
  summarise(mean(psqi_score))

sqi %>%
  group_by(sex) %>%
  mutate(mean_by_sex = mean(psqi_score)) %>%
  ungroup() %>%
  mutate(sex = fct_reorder(sex, mean_by_sex)) %>%
  ggplot(aes(sex, psqi_score, colour = sex,
             show.legend = F)) +
  coord_flip() +
  geom_jitter(show.legend = F,
              size = 4,
              alpha = 0.2,
              width = 0.05) +
  stat_summary(fun = mean, geom = "point", size = 8, show.legend = F) +
  geom_hline(aes(yintercept = mean(psqi_score)),
             colour = "gray70",
             linewidth = 0.9) +
  geom_segment(aes(x = sex, xend = sex,
                   y = mean(psqi_score), yend = mean_by_sex),
               size = 2, show.legend = F) +
  labs(title = "PSQI Score by Sex",
       x = "Sex",
       y = "PSQI Score") +
  theme(legend.position = "none") +
  theme_bw()

sqi %>% ggplot(aes(psqi_score, fill = sex)) +
  geom_density(alpha = 0.3)+
  labs(title="Sleep Quality Score Distribution by Sex",
       x="PSQI Score",
       y="Density")+
  theme_minimal()

sqi %>% 
  ggplot(aes(sex,psqi_score,fill= sex)) + 
  stat_halfeye(adjust = .5, width = .3, .width = c(0.5, 1)) + 
  stat_dots(side = "left", dotsize = 1, justification = 1.05, binwidth = .1)+
  coord_flip()+
  theme(legend.position = "F")



library(ggridges)
library(patchwork)
library(viridis)
library(hrbrthemes)


sqi %>% 
  ggplot(aes(psqi_score,sex , fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01,
                               alpha = 0.8) +
  scale_fill_viridis( option = "C") +
  labs(title = 'PSQI',
       y = "Sex") +
  theme_bw() +
  theme(legend.position="none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8))

sqi %>% 
  ggplot(aes(psqi_score,sex , fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01,
                               alpha = 0.8) +
  labs(title = 'PSQI',
       y = "Sex") +
  theme_bw() +
  theme(legend.position="none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8))








