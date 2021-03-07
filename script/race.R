url <- "https://archive.ics.uci.edu/ml/datasets/adult"
data <- read.csv("adult_sal.csv")

library(ggplot2)

table(data$race)
table(data$race) / length(data$race)

ggplot(data, aes(x = race)) +
  geom_bar(aes(fill = income)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10,
                                   angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5))

"There are 5 races of adults. Most of them are white (85.4%), followed by black
(9.6%), and other races. We can clearly see from the plots that the probability
to have >50K income is way higher for white people than any other race. Most of
the adults that do have >50K income also comes from this race. Therefore race
must be an important variable to predict income categories."