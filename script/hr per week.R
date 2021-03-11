url <- "https://archive.ics.uci.edu/ml/datasets/adult"
data <- read.csv("./data/adult_sal.csv")

library(dplyr)
library(ggplot2)

#Plot a histogram of hours worked per week
ggplot(data, aes(x = hr_per_week)) +
  geom_histogram()

"OMG there is so many people who said that they work for 40 hours a week! Maybe
people just said that because that is the convention of how many hours per week
someone is supposed to be working."