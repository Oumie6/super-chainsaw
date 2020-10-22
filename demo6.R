library(tidyverse)
library(dplyr)
library(ggplot2)
library(patchwork)

subjects <- read.csv('C:/Users/oumth89/Documents/ExercisesR/exercise_data_not_tidy.csv')

feelings <- read.csv('C:/Users/oumth89/Documents/ExercisesR/exercise_data_not_tidy_add_in.csv')

data <- full_join(subjects, feelings, by='subid')

data_wide <- data %>% pivot_wider(names_from='emotion', values_from='self_report')

data
data_wide
full_data2 <- data %>% mutate(
                height = na_if(height, -1),
                sex = factor(sex, levels=c(1,2), labels=c('female', 'male')))
                
full_data2 <- as_tibble(full_data2)
full_data2 <- rename(full_data2, pain = current_feeling_pain)
full_data2 <- rename(full_data2, physical_work = percentage_work_time_physical)
full_data2 <- rename(full_data2, sitting_work = percentage_work_time_sitting)
full_data2 <- rename(full_data2, depression = self_assessed_depression)
full_data2 <- rename(full_data2, anxiety = self_assessed_anxiety)

full_data2 <- data %>% mutate(height=na_if(height, -1),
              bmi = weight/ (height^2)) %>%
              select(c(subid, height, weight, bmi, physical_work))

full_data2 %>% group_by(sex) %>% summarise(mean(weight, na.rm = T))
full_data2 %>% group_by(sex, subid) %>% select(sex, subid, age, height) %>%
              summarise_all(mean, na.rm = T) %>% tally()
tally()
?tally

for(column in colnames(data)){
  print(paste(column, 'has', sum(is.na(data[,column])), 'NAs'))
}

for(column in colnames(data)){
  print(paste('The name is' ,column))
}





P1 <-  data <- data %>% mutate(
  sex = factor(sex, levels=c(1,2), labels=c('female', 'male')))

pdf('C:/Users/oumth89/Documents/ExercisesR/Age_distrubution2.pdf')
p11
dev.off()
p11 <- ggplot(full_data2, aes(x=age, fill=sex)) +
  geom_histogram(position='identity', alpha=0.6, aes(y=..density..)) +
  labs(title="Age distribution", x="Age", y="Density") +
  theme_minimal() +
  geom_density(alpha=0.2) +
  stat_function(fun = dnorm, n = 101, args = list(mean = mean(data$age, na.rm=T), sd = sd(data$age, na.rm=T)), colour='red')

  
ggsave('C:/Users/oumth89/Documents/ExercisesR/Age_distrubution.pdf')


p2 <- ggplot(full_data2, aes(y=age, x=height)) +
  geom_jitter(alpha=0.4) +
  geom_smooth(method = 'lm')

p11 + p2
p11 / p2
