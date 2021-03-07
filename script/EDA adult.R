url <- "https://archive.ics.uci.edu/ml/datasets/adult"
data <- read.csv("adult_sal.csv")

library(dplyr)
library(ggplot2)
library(caTools)

table(data$income)
"There are only 2 types of income, <= 50K and >50k. A vast majority of adults
have <=50K income(24720 / 32561 = 76%) . This is expected. It would be very
weird if most people have >50K income. If that happened we would have to doubt
the whole dataset's integrity."

unique(data$type_employer)
"There are 9 types of employer. 1 of the category is ? which probably means
the adult was not willing to divulge his/her employer."

unique(data$occupation)
"There are 15 different types of occupation."

table(data$relationship)
"There are 6 types of realtionship. I'm not exactly sure what this values means.
Oh, on closer inspection i think i get it. This is their role in the family.
There are some questions though, the ownchild value, does everyone who own a
child still live with their spouse? what if some of them split up or divorced?
"

'Most of the values of capital gain and capital loss are 0. I am not sure if
these variables can even be used to predict the income variable.'

table(data$country)
length(unique(data$country))
"There are 42 different countries. Most of them (29170 / 32561 = 89.5%) are
US citizens. The sample count of every other country are tiny compared to the 
USA. To find the effect of country to income graphically would be a hard task.
"

mean(data$hr_per_week) #40.43746
"I will assume that this variable represents the number of hours work in a week.
This variable is potentially a good predictor for income. However that might not
be the case because even if someone works very long hour, they could be doing
some shallow work at McDonalds, not in some large company offering huge wages.
"
