#Chapter 2: Melanin Desiccation Hypothesis
#Sarah Britton and Goggy Davidowitz

# Libraries
list.of.packages <- c("ggplot2", "dplyr", "rlang")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(rlang)
library(dplyr)

#read in data
desiccation_data <- read.csv(file="Desiccation_Data_Raw_F22.csv")
image_data <- read.csv(file="Desiccation_Photo_Data_F23.csv")

#combine/ clean data
combo_data <-left_join(desiccation_data, image_data, by = "ID")
desiccation_clean <- combo_data %>% 
  select(1:4,13:20,37:45) %>% 
  mutate(percent_mass_change = (100*percent_mass_change)) %>% 
  mutate(log_mass_change= log(percent_mass_change))

#These lists will be used for making plots
treatment_colors = c("gray30","green4") #assigns colors to each treatment for figures
treatment_labels = c("Crowded", "Solitary") #assigns labels to each treatment 

#look at data
hist(desiccation_clean$log_mass_change)
qqnorm(desiccation_clean$log_mass_change)

hist(desiccation_clean$delta_osmo)
qqnorm(desiccation_clean$delta_osmo)

#Result plots
ggplot(desiccation_clean, aes(Treatment, avg_percent))+
  geom_boxplot(aes(color=Treatment))+ geom_point(aes(color=Treatment))+ 
  scale_color_manual(values=treatment_colors, labels=treatment_labels) +
  theme_classic(base_size = 20) + 
  xlab("Treatment") + ylab("Percent Melanization")

ggplot(desiccation_clean, aes(Treatment, percent_mass_change))+
  geom_boxplot(aes(color=Treatment))+ geom_point(aes(color=Treatment))+ 
  scale_color_manual(values=treatment_colors, labels=treatment_labels) +
  theme_classic(base_size = 20) + 
  xlab("Treatment") + ylab("Percent Loss Body Mass")

ggplot(desiccation_clean, aes(x=avg_percent, y=percent_mass_change))+ geom_point(aes(color=Treatment))+
  geom_smooth(method=lm)+
  scale_color_manual(values=treatment_colors, labels=treatment_labels) +
  theme_classic(base_size = 20) + 
  xlab("Percent Melanization") + ylab("Percent Loss Body Mass")

ggplot(desiccation_clean, aes(Treatment, delta_osmo))+
  geom_boxplot(aes(color=Treatment))+ geom_point(aes(color=Treatment))+ 
  scale_color_manual(values=treatment_colors, labels=treatment_labels) +
  theme_classic(base_size = 20) + 
  xlab("Treatment") + ylab("Osmolality Change")

ggplot(desiccation_clean, aes(x=avg_percent, y=delta_osmo))+ geom_point(aes(color=Treatment))+
  geom_smooth(method=lm)+
  scale_color_manual(values=treatment_colors, labels=treatment_labels) +
  theme_classic(base_size = 20) + 
  xlab("Percent Melanization") + ylab("Osmolality Change")


#Summary stats
desiccation_clean %>%
  group_by(Treatment) %>%
  summarize(Percent_mean = mean(avg_percent, na.rm = TRUE),
            Percent_sd = sd(avg_percent, na.rm = TRUE),
            Change_mean = mean(percent_mass_change, na.rm = TRUE),
            Change_sd = sd(percent_mass_change, na.rm = TRUE),
            Osmo_mean = mean(delta_osmo, na.rm = TRUE),
            Osmo_sd = sd(delta_osmo, na.rm = TRUE)) %>%
  as.data.frame

#Stats
t.test(avg_percent~ Treatment, data=desiccation_clean)

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












