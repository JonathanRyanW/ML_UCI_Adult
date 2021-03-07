url <- "https://archive.ics.uci.edu/ml/datasets/adult"
data <- read.csv("./data/adult_sal.csv")

library(dplyr)
library(ggplot2)
library(caTools)

table(data$marital)
"There are 7 types of marital status with sums as indicated"

ggplot(data, aes(marital)) +
  geom_bar(aes(fill = sex)) +
  theme_bw()

ggplot(data, aes(x = reorder(marital, marital, function(x)-length(x)))) +
  geom_bar(aes(fill = income)) +
  ylab("Count") +
  xlab("Marital Status") +
  ggtitle("Income by Marital Status") +
  theme_bw()

"Ok now this is very interesting. The graphs showed us that people who are not
married or who are divorced are very unlikely to have >50K salary. It's absurd
to think that they don't have >50K because they are never married or divorced.
However it makes perfect sense to presume that the reason why they are not
married or divorced is because they are poor, as indicated by <50K salary.
This is a very sad truth about marriage. Most marriage fails simply because
the spouses are arguing about money or some other problems that stems from lack
of money. It is very sad, the fact that they could stay together, they could
live happily together with their kids and families, only if they have enough
money to not argue about stems-from-lack-of-money problems."