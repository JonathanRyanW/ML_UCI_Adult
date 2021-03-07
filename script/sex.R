url <- "https://archive.ics.uci.edu/ml/datasets/adult"
data <- read.csv("adult_sal.csv")

library(dplyr)
library(ggplot2)

table(data$sex)
"Most of the adults are males (21790 / 32561 = 67%)."

ggplot(data, aes(x = sex)) +
  geom_bar(aes(fill = income)) +
  xlab("Sex") +
  ylab("Count") +
  ggtitle("Income by Sex") +
  theme_bw()

"From this plot we can see that most of the adults with >50K income is male. We
can also see that the probability to get >50K income is higher for males than
females."

Sex <- c("Male", "Female", "Male", "Female")
Income <- c("<=50K", "<=50K", ">50K", ">50K")
Probability <- c()

for (i in 1:2) {
  temp.data <- filter(data, sex == Sex[i])
  prob <- table(temp.data$income) / length(temp.data$income)
  Probability[i] <- prob[1]
  Probability[i + 2] <- prob[2]
}

sex_income <- data.frame(cbind(Sex, Income, Probability))
rm(i, Probability, Income, prob, Sex, temp.data)
