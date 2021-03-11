data <- read.csv("./data/adult_sal.csv")
library(dplyr)
library(ggplot2)

"fnlwgt is Final Weight. It is the number of population represented by the
observation in the referenced month. For further reading, please open the link
in the url.fnlwgt variable"

url.fnlwgt <- "https://www.census.gov/programs-surveys/sipp/methodology/weighting.html"