
# Decision Tree Rerun 2026 ------------------------------------------------

#### R Workspace Preparation ####

#turn off scientific notation
options(scipen = 999)

#load data
training20 <- read.csv(file.choose()) #training dataset made up of 20% of overall data
testing80 <- read.csv(file.choose()) #testing dataset made up of remaining 80% survey data
predicted80 <-read.csv(file.choose()) #80% dataset containing manual and model predicted 
                                      #classifications to be used in model evaluation

#The libraries required for this analysis are listed below. 
#They may have to be installed manually first using the 'Packages' tab in console.

#load libraries
#libraries for data handling
library(dplyr)
library(tidyr)
library(data.table)

#libraries for data visualisation
library(ggplot2)
library(ggrepel)

#libraries for decision tree modelling
library(rpart)
library(rpart.plot)
library(caret)


# Decision Tree -----------------------------------------------------------
#Following this: https://www.geeksforgeeks.org/r-language/decision-tree-in-r-programming/
#and: https://steviep42.github.io/biosml/book/decision-trees.html

#### Data Preparation  -------

summary(training20) #summarises the data to check variable data type

#Below changes the data types to be compatible with decision tree construction
training20$fid <- as.integer(training20$fid)
training20$Shelter <- as.logical(training20$Shelter)
training20$Slack <- as.logical(training20$Slack)

summary(testing80) #summarises the data to check variable data type

#Below changes the data types to be compatible with decision tree construction
testing80$fid <- as.integer(testing80$fid)
testing80$Shelter <- as.logical(testing80$Shelter)
testing80$Slack <- as.logical(testing80$Slack)


#### Decision tree model construction  -------

tree_model <- rpart(formula = New_sort ~ FT + Sub + Macs + Depth + Shelter + Slack, 
                      data = training20,
                      method="class")

#### Visualise tree  -------

tree_model_test <- rpart.plot(tree_model, 
                              extra = 104,
                              box.palette = list("royalblue2", "palegreen2", "goldenrod1", "salmon"), 
                              nn = FALSE)

#Optional: code to export publication quality png to your project directory 
png("new_DT.png", res = 300, width = 16, height = 10, units = "cm")
tree_model_test <- rpart.plot(tree_model, 
                              extra = 104,
                              box.palette = list("royalblue2", "palegreen2", "goldenrod1", "salmon"), 
                              nn = FALSE)
dev.off()


#### Predicting testing80  -------
testing80$Predicted_Classification <- predict(tree_model, newdata = testing80[c("FT", "Sub", "Macs", "Depth", "Shelter", "Slack" )], 
                                              type = "class")

#Save R dataframe to csv to complete a manual classification of the testing80 dataset
#to undertake model evaluation (see below)
write.csv(testing80, "predicted80.csv", row.names = FALSE)

#### Manual v Model Classifications -------

summary(predicted80) #summarises the data to check variable data type

#Below changes the data types to be compatible with decision tree construction
predicted80$fid <- as.integer(predicted80$fid)

#Ensuring factor levels are set the same in each column
classes <- c("Adult", "Juvenile", "Spawning", "Unsuitable")

predicted80$Predicted_Classification <- factor(
  predicted80$Predicted_Classification,
  levels = classes
)

predicted80$Manual_sort <- factor(
  predicted80$Manual_sort,
  levels = classes
)

#Checks that each column has the same classes
levels(predicted80$Predicted_Classification)
levels(predicted80$Manual_sort)

#Confusion matrix to compare results 
caret::confusionMatrix(
  data = predicted80$Predicted_Classification,
  reference = predicted80$Manual_sort
)


