url <- "https://archive.ics.uci.edu/ml/datasets/adult"
data <- read.csv("adult_sal.csv")

library(dplyr)
library(ggplot2)

mean(data$age) #38.58165
tapply(data$age, data$sex, mean)

"The UCI website says that the age is a continuous variable but it really are
discrete isn't it? The age data only consists of integers after all. The males
are on average older than the females. The mean ages do not differ that much.
Is this the mean age of US citizens in general?"

ggplot(data, aes(x = age)) +
  geom_bar(aes(fill = income)) +
  xlab("Age") +
  ylab("Count") +
  ggtitle("Income by Age") +
  theme_bw()

"From this plot we can see that most people have ages between 25 and 40. But
most people who have >50K income comes from those of age between 40 and 50. The
probability to have >50K income rises as age rises from 25 to 40, continue to
increase until the age of 50 (num of total people decrease bu the num of those
with >50K income stagnant, meaning the probability increase.), and then declines."

#Finding the actual probability
length(unique(data$age))
"There are 73 different ages from 17 to 90 (there should be 74 but there is no
one who is 89 years old"

Age <- c(sort(unique(data$age)), sort(unique(data$age)))
Income <- c(rep("<=50K", 73), rep(">50K", 73))
Probability <- c()

for (i in 1:73) {
  temp.data <- filter(data, age == Age[i])
  prop <- table(temp.data$income) / length(temp.data$income)
  Probability[i] <- prop[1]
  Probability[i + 73] <- prop[2]
}
Probability <- round(Probability * 100, 2)

age_income <- data.frame(cbind(Age, Income, Probability))
rm(i, Age, Income, Probability, temp.data, prop)

age_income$Probability[is.na(age_income$Probability)] <- 0
age_income$Age <- as.numeric(age_income$Age)
age_income$Probability <- as.numeric(age_income$Probability)

"Ohh we can see it so clearly, the trend is so clear! Now all we have to do is
to visualize the probability trend."

#Probability of having <= 50K income
ggplot(age_income[1:73,], aes(x = Age, y = Probability), group = 1) +
  geom_line(color = "#ffc266", size = 2) +
  ylab("Probability (Percentage)") +
  ggtitle("Less Than or Equal 50K Income") +
  theme_bw()

#Probability of having > 50K income
ggplot(age_income[74:146,], aes(x = Age, y = Probability), group = 1) +
  geom_line(color = "#8899ee", size = 2) +
  ylab("Probability (Percentage)") +
  ggtitle("More Than 50K Income") +
  theme_bw()

"We can clearly see the trend! The probabilities are behaving as expected!"

"We can see that there are some crazy fluctuations in the probability around
the end of the graph. It happened because the sample size in the older age
group is so small. For example for age >77, there are less than 30 samples of
every age group but there are 700-900 samples for the 19 to 63 age group."

#To see the number of people of certain age
table(data$age)

"In conclusion, there is a clear trend of income based on age. Age will be an
important factor when we are predicting income."