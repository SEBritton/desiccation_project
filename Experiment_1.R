#Chapter 2: Melanin Desiccation Hypothesis
#Sarah Britton and Goggy Davidowitz

# Packages and Libraries
list.of.packages <- c("ggplot2", "dplyr", "rlang", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(rlang)
library(dplyr)
library(ggpubr)
library(ggthemr)

#read in data
desiccation_data <- read.csv(file="Desiccation_Data_Raw_F22.csv")
image_data <- read.csv(file="Desiccation_Photo_Data_F23.csv")

#combine data sets/ clean data
combo_data <-left_join(desiccation_data, image_data, by = "ID")
desiccation_clean <- combo_data %>% 
  select(1:4,13:20,37:45) %>% 
  mutate(percent_mass_change = (100*percent_mass_change)) %>% 
  mutate(log_mass_change= log(percent_mass_change))

#Settings for plots
treatment_labels = c("Crowded", "Solitary") #assigns labels to each treatment 
ggthemr("fresh", type="inner", layout="clean", spacing = 1)
colors <- c(swatch()[2], swatch()[4])

#look at data
hist(desiccation_clean$log_mass_change)
qqnorm(desiccation_clean$log_mass_change)
#left skewed so log transformed 

hist(desiccation_clean$delta_osmo)
qqnorm(desiccation_clean$delta_osmo)
#looks normal

#Result plots
#Effect of treatment on melanin
ggplot(desiccation_clean, aes(Treatment, avg_percent)) +
  geom_boxplot(aes(fill=Treatment), show.legend = FALSE) + 
  scale_fill_manual(values=colors) +
  stat_summary(aes(group=Treatment), fun=mean, shape="diamond", size=0.8) +
  scale_x_discrete(labels=treatment_labels) +
  annotate(geom="text", size=3.5, x = 2, y=100, label="t = 12.70, df = 128.29, p < 0.001") +
  xlab("Treatment") + ylab("Percent Melanization")

#Percent mass change plots
mass_change_1 <- ggplot(desiccation_clean, aes(Treatment, percent_mass_change)) +
  geom_boxplot(aes(fill=Treatment), show.legend=FALSE) + 
  scale_fill_manual(values=colors) +
  stat_summary(aes(group=Treatment), fun=mean, shape="diamond", size=0.8) +
  scale_x_discrete(labels=treatment_labels) +
  annotate(geom="text", size= 3.5, x = 1, y=30, label="t = 3.69, df = 128, p < 0.001") +
  xlab("Treatment") + ylab("Percent Loss Body Mass")

mass_change_1

mass_change_2 <- ggplot(desiccation_clean, aes(x=avg_percent, y=percent_mass_change))+ 
  geom_point(aes(color=Treatment))+
  geom_smooth(method=lm) +
  scale_color_manual(values=colors, labels=treatment_labels) +
  annotate(geom="text", size = 3, x = 65, y=32, label="y = 15.86-1.00(percent melanin)-0.96(mass)") +
  annotate(geom="text", size = 3, x = 72, y=31, label="r-squared = 0.08") +
  annotate(geom="text", size = 3, x = 79, y=30, label="percent melanin: p = 0.007") +
  annotate(geom="text", size = 3, x = 76, y=29, label="body mass: p = 0.020") +
  xlab("Percent Melanization") + ylab("Percent Loss Body Mass")

mass_change_2

ggarrange(mass_change_1, mass_change_2, 
          font.label = list(size=12, family="Times New Roman"),labels=c("A", "B"),
          ncol = 2, hjust=-5.5, align="hv", widths=c(1.6,2))


#Osmolality change plots
osmo_change_1 <- ggplot(desiccation_clean, aes(Treatment, delta_osmo)) +
  geom_boxplot(aes(fill=Treatment), show.legend = FALSE) + 
  scale_fill_manual(values=colors) +
  stat_summary(aes(group=Treatment), fun=mean, shape="diamond", size=0.8) +
  scale_x_discrete(labels=treatment_labels) +
  annotate(geom="text", size= 3.5, x = 2, y=40, label="t = 0.77, df = 31, p = 0.45") +
  xlab("Treatment") + ylab("Osmolality Change")

osmo_change_1

osmo_change_2 <- ggplot(desiccation_clean, aes(x=avg_percent, y=delta_osmo)) + 
  geom_point(aes(color=Treatment)) +
  geom_smooth(method=lm) +
  scale_color_manual(values = colors, labels = treatment_labels) +
  xlab("Percent Melanization") + ylab("Osmolality Change")

osmo_change_2

osmo_3 <- ggplot(desiccation_clean, aes(x=before_mass, y=delta_osmo)) + 
  geom_point(aes(color=Treatment), show.legend = FALSE) +
  geom_smooth(method=lm) +
  scale_color_manual(values=colors, labels=treatment_labels) +
  annotate(geom="text", size = 3, x = 6.2, y=45, label="y = 25.35+0.05(percent melanin)-5.85(mass)") +
  annotate(geom="text", size = 3, x = 6.5, y=39, label="r-squared = 0.13") +
  annotate(geom="text", size = 3, x = 7, y=33, label="percent melanin: p = 0.707") +
  annotate(geom="text", size = 3, x = 6.8, y=27, label="body mass: p = 0.107") +
  xlab("Body Mass") + ylab("Osmolality Change")

osmo_3


osmo_4 <- ggplot(desiccation_clean, aes(x=percent_mass_change, y=delta_osmo)) + 
  geom_point(aes(color=Treatment), show.legend = FALSE)+
  geom_smooth(method=lm)+
  scale_color_manual(values=colors, labels=treatment_labels) +
  xlab("Percent Mass Change") + ylab("Osmolality Change")

osmo_4

ggarrange(osmo_change_1, osmo_change_2, osmo_3,
          font.label = list(size = 12, family="Times New Roman"),labels=c("A", "B", "C", "D"), 
          ncol = 1, hjust=-5.5, align="hv", widths=c(1, 2, 1))

#Extra body size plots
ggplot(desiccation_clean, aes(x=before_mass, y=avg_percent)) + 
  geom_point(aes(color=Treatment))+
  geom_smooth(method=lm, aes(color=Treatment))+
  scale_color_manual(values=colors, labels=treatment_labels) +
  xlab("Body Mass") + ylab("Melanin")

ggplot(desiccation_clean, aes(x = before_mass, y = percent_mass_change)) + 
  geom_point(aes(color = Treatment)) +
  geom_smooth(method = lm) +
  scale_color_manual(values = colors, labels = treatment_labels) +
  xlab("Body Mass") + ylab("Percent Change")

#Summary stats
desiccation_clean %>%
  group_by(Treatment) %>%
  summarize(Percent_mean = mean(avg_percent, na.rm = TRUE),
            Percent_sd = sd(avg_percent, na.rm = TRUE),
            Change_mean = mean(percent_mass_change, na.rm = TRUE),
            Change_sd = sd(percent_mass_change, na.rm = TRUE),
            Osmo_mean = mean(delta_osmo, na.rm = TRUE),
            Osmo_sd = sd(delta_osmo, na.rm = TRUE),
            Mass_mean = mean(before_mass, na.rm = TRUE),
            Mass_sd = sd(before_mass, na.rm = TRUE)) %>%
  as.data.frame

#Stats
#Treatment and melanin
t.test(avg_percent~ Treatment, data=desiccation_clean)

#Body size and melanin
t.test(before_mass ~ Treatment, data=desiccation_clean)

melanin_mass <- lm(avg_percent ~ before_mass, data=desiccation_clean)
summary(melanin_mass)

#percent mass change
mass_cat<- lm(log_mass_change ~ Treatment + before_mass, data=desiccation_clean)
summary(mass_cat)

mass_cont <- lm(log_mass_change ~ avg_percent + before_mass, data=desiccation_clean)
summary(mass_cont)


#osmolality change
osmo_cat<- lm(delta_osmo ~ Treatment + before_mass, data=desiccation_clean)
summary(osmo_cat)

osmo_cont <- lm(delta_osmo ~ avg_percent + before_mass, data=desiccation_clean)
summary(osmo_cont)

mass_osmo <- lm(delta_osmo ~ before_mass, data=desiccation_clean)
summary(mass_osmo)

osmo <- lm(log_mass_change ~ delta_osmo, data=desiccation_clean)
summary(osmo)

#split up data set for osmolality
osmo_crowded <- desiccation_clean %>% 
  filter(Treatment == "C" & delta_osmo != 0)

osmo_solo <- desiccation_clean %>% 
  filter(Treatment == "S" & delta_osmo != 0)

#is change different than 0?
t.test(osmo_crowded$delta_osmo, mu = 0, alternative = "two.sided")
#p=0.063

t.test(osmo_solo$delta_osmo, mu = 0, alternative = "two.sided")
#p=0.907













