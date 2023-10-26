library(ggplot2)
library(lubridate)
library(reshape2)
library(dplyr)
library(igraph)

data <- read.csv("/Users/riddhidange/Desktop/Academics/fall2023/FA550/Assignment 2/Sleep_health_and_lifestyle_dataset.csv")
#View(data)
#summary(data)

#1 Histogram
hist(data$`Sleep.Duration`, breaks = 20, main="Histogram of Sleep Duration", xlab="Sleep.Duration (hours)", col="skyblue", border="black")
#2 Scatterplot
plot(data$`Sleep.Duration`, data$`Quality.of.Sleep`, main="Scatter Plot of Sleep Duration vs. Quality of Sleep", xlab="Sleep Duration (hours)", ylab="Quality of Sleep", col=ifelse(data$Gender=="Male", "blue", "pink"))

#3 Bar Graph
ggplot(data, aes(x = `BMI.Category`, y = `Physical.Activity.Level`)) +
  geom_bar(stat = "summary", fun = "mean", fill = "yellow") +
  labs(title = "Average Physical Activity Level by BMI Category",
       x = "BMI Category",
       y = "Average Physical Activity Level (minutes/day)")

#4 Line Graph
bp_age_df <- data.frame(
  age = data$Age,
  systolic_bp = data$`Blood.Pressure`)

ggplot(bp_age_df, aes(x = age, y = systolic_bp)) +
  geom_line() +
  labs(title = "Blood Pressure Over Age", x = "Age", y = "Systolic Blood Pressure (mmHg)")

#5 Boxplot
boxplot(data$`Stress.Level` ~ data$Gender, col=c("blue", "pink"), xlab="Gender", ylab="Stress Level", main="Box Plot of Stress Levels by Gender")

#6 Pie Chart
sleep_disorder_counts <- table(data$`Sleep.Disorder`)
pie(sleep_disorder_counts, labels = names(sleep_disorder_counts), main="Sleep Disorder Distribution", col=c("red", "yellow", "green"))

#7 Bar Graph
gender_means <- data %>%
  group_by(Gender) %>%
  summarise(mean_sleep_duration = mean(Sleep.Duration))

ggplot(gender_means, aes(x = Gender, y = mean_sleep_duration, fill = Gender)) +
  geom_col() +
  labs(title = "Average Sleep Duration by Gender", x = "Gender", y = "Sleep Duration (hours)") +
  scale_fill_manual(values = c("pink", "blue"))

#8 Violin Chart
ggplot(data, aes(x = `Occupation`, y = `Sleep.Duration`)) +
  geom_violin() +
  labs(title = "Sleep Duration by Occupation", x = "Occupation", y = "Sleep Duration (hours)")




