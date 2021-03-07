#Read in the adult_sal.csv file and set it to a data frame called adult.
data <- read.csv("./data/adult_sal.csv")

#Check the head of adult
head(data)

#You should notice the index has been repeated. Drop this column.
library(dplyr)
data <- select(data, -X)

#Check the head,str, and summary of the data now.
head(data)
str(data)
summary(data)

#Use table() to check out the frequency of the type_employer column.
table(data$type_employer)

#How many Null values are there for type_employer?
sum(is.null(data$type_employer))

"There are 0 nulls. However there is a lot of ? values. I will assume that
? is in fact null. Therefore there are 1836 nulls"

#What are the two smallest groups?
head(sort(table(data$type_employer)), 2)

"The answer is Never worked (7 observations) and without pay (14 observations)"

#Combine these two smallest groups into a single group called "Unemployed"
class(data$type_employer)
type_employer <- c()
for (i in 1:32561) {
  if (data$type_employer[i] %in% c("Never-worked", "Without-pay")) {
    type_employer <- append(type_employer, "Unemployed")
  } else {
    type_employer <- append(type_employer, data$type_employer[i])
  }
}
data$type_employer <- type_employer
rm(i, type_employer)

#Making sure the data is correct
sum(data$type_employer == "Never-worked") #0
sum(data$type_employer == "Without-pay") #0
sum(data$type_employer == "Unemployed") #21

#Combine State and Local gov jobs into a category called SL-gov
SL_gov <- c()
for (i in 1:32561) {
  if (data$type_employer[i] %in% c("State-gov", "Local-gov")) {
    SL_gov <- append(SL_gov, "SL-gov")
  } else {
    SL_gov <- append(SL_gov, data$type_employer[i])
  }
}
data$type_employer <- SL_gov
rm(i, SL_gov)

#Making sure the data is correct
sum(data$type_employer == "State-gov") #0
sum(data$type_employer == "Local-gov") #0
sum(data$type_employer == "SL-gov") #3391

#Combine self-employed jobs into a category called self-emp.
self_emp <- c()
for (i in 1:32561) {
  if (data$type_employer[i] %in% c("Self-emp-inc", "Self-emp-not-inc")) {
    self_emp <- append(self_emp, "self-emp")
  } else {
    self_emp <- append(self_emp, data$type_employer[i])
  }
}
data$type_employer <- self_emp
rm(i, self_emp)

#Making sure the data is correct
sum(data$type_employer == "Self-emp-inc") #0
sum(data$type_employer == "Self-emp-not-inc") #0
sum(data$type_employer == "self-emp") #3657

#Checking the results
table(data$type_employer)

#Use table() to look at the marital column
table(data$marital)

#Reduce this to three groups: Married, Not-Married, Never-Married
#Married Never-married   Not-Married 
#15417         10683          6461 

"Since 4443 + 993 + 1025 = 6461, we will assume that Not-Married is Divorced +
Widowed + Separated"

#Creating Not-Married values
Not_Married <- c()
for (i in 1:32561) {
  if (data$marital[i] %in% c("Divorced", "Widowed", "Separated")) {
    Not_Married <- append(Not_Married, "Not-Married")
  } else {
    Not_Married <- append(Not_Married, data$marital[i])
  }
}
data$marital <- Not_Married
rm(i, Not_Married)

#Making sure the data is correct
sum(data$marital == "Divorced") #0
sum(data$marital == "Widowed") #0
sum(data$marital == "Separated") #0
sum(data$marital == "Not-Married") #6461

"Since 23 + 418 + 14976 = 15417, we will assume that Married = Married-AF-spouse +
Married-spouse-absent + Married-civ-spouse"

#Creating Married values
Married <- c()
for (i in 1:32561) {
  if (data$marital[i] %in% c("Married-AF-spouse",
                             "Married-spouse-absent",
                             "Married-civ-spouse")) {
    Married <- append(Married, "Married")
  } else {
    Married <- append(Married, data$marital[i])
  }
}
data$marital <- Married
rm(i, Married)

#Making sure the data is correct
sum(data$marital == "Married-AF-spouse") #0
sum(data$marital == "Married-spouse-absent") #0
sum(data$marital == "Married-civ-spouse") #0
sum(data$marital == "Married") #15417

#Check the country column using table()
table(data$country)

#Group these countries together however you see fit.
names(table(data$country))

North.America <- c("Canada", "Mexico", "Cuba", "Dominican-Republic",
                   "Jamaica", "Haiti", "United-States", "Puerto-Rico",
                   "Outlying-US(Guam-USVI-etc)", "Guatemala")
Latin.and.South.America <- c("El-Salvador", "Honduras", "Nicaragua", "Ecuador",
                     "Columbia", "Peru", "Trinadad&Tobago")
Asia <- c("China", "India", "Iran", "Laos", "Japan", "Cambodia",
          "Philippines", "Thailand", "Vietnam", "Taiwan", "Hong")
Europe <- c("England", "France", "Germany", "Ireland", "Italy", "Greece",
            "Holand-Netherlands", "Hungary", "Portugal", "Poland","Scotland",
            "Yugoslavia")
Other <- c("South")

"South will be assumed to be either South Africa or South Sudan, both in the
Africa continent. Hong is assumed as Hong Kong"

data$country[data$country %in% North.America] <- "North.America"
data$country[data$country %in% Latin.and.South.America] <- "Latin.and.South.America"
data$country[data$country %in% Asia] <- "Asia"
data$country[data$country %in% Europe] <- "Europe"
data$country[data$country %in% c("South","?")] <- "Other"

#Cleaning variables that won't be used anyomre
rm(Asia, Europe, North.America, Latin.and.South.America, Other)

#Use table() to confirm the groupings
table(data$country)

#Check the str of adult again.
str(data)

#Make sure any of the columns we changed have factor levels with factor()
data$marital <- factor(data$marital)
data$country <- factor(data$country)

#Grouping occupation
table(data$occupation)

Others <- c("Armed-Forces", "Priv-house-serv", "Protective-serv",
            "Transport-moving", "Other-service", "Farming-fishing")
Shallow.work <- c("Adm-clerical", "Handlers-cleaners", "Craft-repair",
                  "Machine-op-inspct")
Deep.work <- c("Sales", "Tech-support", "Exec-managerial", "Prof-specialty")

data$occupation[data$occupation %in% Others] <- "Others"
data$occupation[data$occupation %in% Shallow.work] <- "Shallow.work"
data$occupation[data$occupation %in% Deep.work] <- "Deep.work"

rm(Deep.work, Shallow.work, Others)

table(data$occupation)

#Grouping education
table(data$education)
names(table(data$education))

No.High.School <- c("Preschool", "1st-4th", "5th-6th", "7th-8th", "9th",
                    "10th", "11th", "12th")
High.School <- c("HS-grad")
Bachelor <- c("Assoc-acdm", "Assoc-voc", "Bachelors", "Some-college")
Master <- c("Masters")
Doctorate <- c("Prof-school", "Doctorate")

unique(paste(data$education, data$education_num))
"We can find the corresponding education_num for each of the education values
with this code. We found out that our grouping is very good. It matches the
order perfectly."

data$education[data$education %in% No.High.School] <- "No.High.School"
data$education[data$education %in% High.School] <- "High.School"
data$education[data$education %in% Bachelor] <- "Bachelor"
data$education[data$education %in% Master] <- "Master"
data$education[data$education %in% Doctorate] <- "Doctorate"

rm(Bachelor, Doctorate, High.School, Master, No.High.School)

table(data$education)

"The data looks so much better!"

#Install and load the Amelia package
library(Amelia)

#Convert any cell with a '?' or a ' ?' value to a NA value
data[data == "?" | data == " ?"] <- NA

#Finding NAs
sapply(data, function(x){any(is.na(x))})
"It turns out only the type_employer and the occupation column has NA values.
"
sum(is.na(data$occupation) | is.na(data$type_employer))
"There are 1843 rows with NA in the data."

missmap(data,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

#What is col=c('yellow','black') doing?
"That command tells Amelia to use yellow to indicate NAs and black to indicate
observed values."

#Use na.omit() to omit NA data from the adult data frame.
data <- na.omit(data)

"There should be 32651 - 1843 = 30808 data, but instead there is only 30718. We
are short 30808 - 30718 = 90 data"

#Use missmap to check that all the NA values were in fact dropped.
missmap(data,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

"Yup. There really is no NA in the data anymore"

#Use ggplot2 to create a histogram of ages, colored by income.
ggplot(data, aes(x = age)) +
  geom_bar(aes(fill = income)) +
  xlab("Age") +
  ylab("Count") +
  ggtitle("Income by Age") +
  theme_bw()

#Plot a histogram of hours worked per week
ggplot(data, aes(x = hr_per_week)) +
  geom_histogram()

#Rename the country column to region column to better reflect the factor levels
data <- rename(data, region = country)

"Oh this actually makes perfect sense."

#Create a barplot of region with the fill color defined by income class.
#Optional: Figure out how rotate the x axis text for readability

ggplot(data, aes(x = reorder(region, region, function(x)-length(x)))) +
  geom_bar(aes(fill = income)) +
  xlab("Region") +
  ylab("Count") +
  ggtitle("Income by Region") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_bw()

#data variables need to be change into factors
data$type_employer <- factor(data$type_employer)
data$education <- factor(data$education)
data$occupation <- factor(data$occupation)
data$relationship <- factor(data$relationship)
data$race <- factor(data$race)
data$sex <- factor(data$sex)
data$income <- factor(data$income)