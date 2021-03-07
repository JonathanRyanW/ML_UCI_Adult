url <- "https://archive.ics.uci.edu/ml/datasets/adult"
data <- read.csv("./data/adult_sal.csv")

library(dplyr)
library(ggplot2)

table(data$relationship)
"There are 6 types of realtionship. I'm not exactly sure what this values means.
Oh, on closer inspection i think i get it. This is their role in the family.
There are some questions though, the ownchild value, does everyone who own a
child still live with their spouse? what if some of them split up or divorced?
"

ggplot(data, aes(x = relationship)) +
  geom_bar(aes(fill = income)) +
  coord_cartesian(ylim = c(0,15000)) +
  ylab("Count") +
  xlab("Relationship") +
  ggtitle("Income by Relationship") +
  theme_bw()

"The probabilitiy to have >50K income is highest if the adult is a husband or a
wife. This makes perfect sense. If someone is a child that person is probably
not old enough to get a job, or even if he/she already have a job, they would
be in the early stage of their career, that is why they have not reach the
50K salary mark. If someone is unmarried or not in family, chances are they
have some life problems that prevents them from marrying. This problem indicates
that they are poor, very low chance that they do have >50K salary."


