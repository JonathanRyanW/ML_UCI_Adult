"The data for this project was processed with the Pre ML Data Cleaning script."

head(data)

#Split the data into a train and test set using the caTools library
library(caTools)
set.seed(101)
sample <- sample.split(data$income, SplitRatio = 0.7)

library(dplyr)
train <- filter(data, sample)
test <- filter(data, !sample)

#Explore the glm() function with help(glm)
help(glm)

#Use all the features to train a glm() model on the training data set, pass the
#argument family=binomial(logit) into the glm function


model <- glm(income ~ . , family = binomial(link = "logit"), train)
summary(model)

Prediction <- predict(model, test)
Prediction <- 1 / (1 + exp(-1 * Prediction))

Prediction[Prediction > 0.5] <- 1
Prediction[Prediction < 0.5] <- 0

result <- data.frame(cbind(test$income, Prediction))
result$V1 <- result$V1 - 1

tp = sum(result$V1 == 1 & result$Prediction == 1)
fp = sum(result$V1 == 0 & result$Prediction == 1)
tn = sum(result$V1 == 0 & result$Prediction == 0)
fn = sum(result$V1 == 1 & result$Prediction == 0)

#Creating a confusion matrix
confusion.matrix <- matrix(nrow = 2, ncol = 2)
colnames(confusion.matrix) <- c("Predicted <= 50K", "Predicted > 50K")
rownames(confusion.matrix) <- c("Actual <= 50K", "Actual > 50K")

confusion.matrix[1,1] <- tn
confusion.matrix[1,2] <- fp
confusion.matrix[2,1] <- fn
confusion.matrix[2,2] <- tp

correct <- tp + tn
false <- fp + fn

correct/sum(tn, tp, fn, fp)
false/sum(tn, tp, fn, fp)

confusion.matrix.percentage <- confusion.matrix / sum(tn, tp, fn, fp)

"Our model is right 84.5% of the time. But i am sure that we can do better! We
have not yet explored other features of the glm function. I am sure that i we
do, we will find some hidden gems to improve our model."

#Use new.model <- step(your.model.name) to create a new model.
new.model <- step(model)
summary(new.model)

new.Prediction <- predict(new.model, test)
new.Prediction <- 1 / (1 + exp(-1 * new.Prediction))

new.Prediction[new.Prediction > 0.5] <- 1
new.Prediction[new.Prediction < 0.5] <- 0

new.result <- data.frame(cbind(test$income, new.Prediction))
new.result$V1 <- new.result$V1 - 1

tp = sum(new.result$V1 == 1 & new.result$new.Prediction == 1)
fp = sum(new.result$V1 == 0 & new.result$new.Prediction == 1)
tn = sum(new.result$V1 == 0 & new.result$new.Prediction == 0)
fn = sum(new.result$V1 == 1 & new.result$new.Prediction == 0)

#Creating a confusion matrix
new.confusion.matrix <- matrix(nrow = 2, ncol = 2)
colnames(confusion.matrix) <- c("Predicted <= 50K", "Predicted > 50K")
rownames(confusion.matrix) <- c("Actual <= 50K", "Actual > 50K")

new.confusion.matrix[1,1] <- tn
new.confusion.matrix[1,2] <- fp
new.confusion.matrix[2,1] <- fn
new.confusion.matrix[2,2] <- tp

new.confusion.matrix.percentage <- confusion.matrix / sum(tn, tp, fn, fp)

"The new model is sligthly better than the previous one! The step function
worked!"