url <- "https://archive.ics.uci.edu/ml/datasets/adult"
data <- read.csv("./data/adult_sal.csv")

library(dplyr)
library(ggplot2)

table(data$education_num)

table(data$education)
"There are 16 types of education. From preschool all the way to masters. The
education_num seems to have been ordered ascending from preschool to masters.
The important numbers are 9 (High school), 10(Some college), 13(Bachelor),
14(Master), 16(Doctorate)"

ggplot(data, aes(x = education_num)) +
  geom_bar(aes(fill = income)) +
  theme_bw()

ggplot(data, aes(x = education)) +
  geom_bar(aes(fill = income)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.35))

"From this plot we can see that the adults with >50K income does come from
higher education categories. The number of adults with <50K income from high
school, some college, is slightly less than from the bachelor category, but
considering the number of people with high school degree and college degree
the probability is very low for both the categories. Without even looking
at the actual probability values, we can already see that the probability
of having an income >50K does increases as education level increases.
It is very consistent from HS-College-Bachelors-Masters-Prof-Doctorate"

#Let us find the actual probability
Education <- c(unique(data$education), unique(data$education))
Income <- c(rep("<=50K", 16), rep(">50K", 16))
Probability <- c()

for (i in 1:16) {
  temp.data <- filter(data, education == Education[i])
  prob <- table(temp.data$income) / length(temp.data$income)
  Probability[i] <- prob[1]
  Probability[i + 16] <- prob[2]
}
Probability <- round(Probability * 100, 2)

edu_income <- data.frame(cbind(Education, Income, Probability))
rm(Education, Income, Probability, prob, i, temp.data)

"We can see it so clearly! The probability to gain >50K income does increase
consistenly! Education must be a determining factor in building our logistic
regression model to predict income."

#Let us get the probability table using education_num
Education_num <- c(unique(data$education_num), unique(data$education_num))
Income <- c(rep("<=50K", 16), rep(">50K", 16))
Probability <- c()

for (i in 1:16) {
  temp.data <- filter(data, education_num == Education_num[i])
  prob <- table(temp.data$income) / length(temp.data$income)
  Probability[i] <- prob[1]
  Probability[i + 16] <- prob[2]
}
Probability <- round(Probability * 100, 2)

edu_num_income <- data.frame(cbind(Education_num, Income, Probability))
rm(Education_num, Income, Probability, prob, i, temp.data)

edu_num_income$Education_num <- as.numeric(edu_num_income$Education_num)
edu_num_income$Probability <- as.numeric(edu_num_income$Probability)

#Creating a line graph
ggplot(filter(edu_num_income, Income == ">50K"),
       aes(x = Education_num, y = Probability, group = 1)) +
  coord_cartesian(xlim = c(1,16), ylim = c(0,80)) +
  ylab("Probability (Percentage)") +
  xlab("Education") +
  ggtitle("Probability of >50K Income by Education") +
  geom_line(color = "#f47e7e", size = 2) +
  theme_bw()

ggplot(filter(edu_num_income, Income == "<=50K"),
       aes(x = Education_num, y = Probability, group = 1)) +
  coord_cartesian(xlim = c(1,16), ylim = c(20,100)) +
  ylab("Probability (Percentage)") +
  xlab("Education") +
  ggtitle("Probability of <=50K Income by Education") +
  geom_line(color = "#f47e7e", size = 2) +
  theme_bw()

"We can clearly see the trend"