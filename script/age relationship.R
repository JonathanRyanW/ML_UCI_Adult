url <- "https://archive.ics.uci.edu/ml/datasets/adult"
data <- read.csv("./data/adult_sal.csv")

library(ggplot2)
library(dplyr)

#Finding relationship based on age
ggplot(data, aes(x = age)) +
  geom_histogram(aes(fill = relationship), binwidth = 5) +
  theme_bw()

"There are so many husbands but very few wives. This is expected since we already
know that most of the adults in the data are males."

"There are 73 different age and 6 different marital status. If we want to make
a data frame consisting of the probabilities of being in a specific marital
status in a specific age we would have a dataframe of 73 * 6 = 438 observations
"

#Creating this table
Age <- rep(sort(unique(data$age)), 6)

Relationship <- c()
for (i in 1:6) {
  Relationship <- append(Relationship, rep(names(table(data$relationship))[i], 73))
}

age_relationship <- data.frame(cbind(Age, Relationship))

Probability <- c()
for (i in 1:438) {
  temp.data <- filter(data, age == age_relationship[i,1])
  Probability <- append(Probability,
                        sum(temp.data$relationship == age_relationship[i,2]) / length(temp.data$relationship))
}

age_relationship$Probability <- round(100 * Probability, 2)
age_relationship$Age <- as.numeric(age_relationship$Age)
rm(temp.data, Age, i, Probability, Relationship)

"We have successfully created a data frame of probability (in percentage) of
an adult of specific age to be in a specific relationship status."

#Creating a function to easily get the probability from the data frame
age.relationship <- function(age, relationship){
  result <- age_relationship[age_relationship$Age == age & age_relationship$Relationship == relationship,3]
  return(result)
}

#Plotting Unmarried probabilities
ggplot(filter(age_relationship, Relationship == "Unmarried"),
       aes(x = Age, y = Probability, group = 1)) +
  geom_line(color = "#4f92ce", size = 2) +
  ggtitle("Probability of Unmarried by Age") +
  ylab("Probability(Percentage)") +
  theme_bw()

#Plotting Husband probabilities
ggplot(filter(age_relationship, Relationship == "Husband"),
       aes(x = Age, y = Probability, group = 1)) +
  geom_line(color = "#4f92ce", size = 2) +
  ggtitle("Probability of Husband by Age") +
  ylab("Probability(Percentage)") +
  theme_bw()

#Plotting Wife probabilities
ggplot(filter(age_relationship, Relationship == "Wife"),
       aes(x = Age, y = Probability, group = 1)) +
  geom_line(color = "#4f92ce", size = 2) +
  ggtitle("Probability of Wife by Age") +
  ylab("Probability(Percentage)") +
  theme_bw()

#Plotting Not in family probabilities
ggplot(filter(age_relationship, Relationship == "Not-in-family"),
       aes(x = Age, y = Probability, group = 1)) +
  geom_line(color = "#4f92ce", size = 2) +
  ggtitle("Probability of Not-in-Family by Age") +
  ylab("Probability(Percentage)") +
  theme_bw()

#Plotting Own-child probabilities
ggplot(filter(age_relationship, Relationship == "Own-child"),
       aes(x = Age, y = Probability, group = 1)) +
  geom_line(color = "#4f92ce", size = 2) +
  ggtitle("Probability of Own-child by Age") +
  ylab("Probability(Percentage)") +
  theme_bw()

"We can do all the other probabilities, We know for certain now that age does
affects the probability of someone being a husband or wife or being unmarried.
The Own-child probability is very interesting. People stop being a child by 
roughly the age of 30. That would mean that the value Own-child does not mean
owning a child, but rather being a child living with a parent."