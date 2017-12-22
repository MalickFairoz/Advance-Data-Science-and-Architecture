
## Week 3 Assignments 




## Decission Tree Classification with Titanic Dataset 



##Installing packages and loading the library 
install.packages('rattle')
install.packages('rpart.plot')
library(datasets)
## Reading the titanic dataset of traning and testing
Titanictrain <- read.csv("D:/2 ADS Fall 17/Assignments/3 titanic train.csv")
Titanictest <- read.csv("D:/2 ADS Fall 17/Assignments/3 titanic test.csv")

# Classification Tree with "Recursive Partitioning and Regression Trees"
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=Titanictrain,
             method="class")
summary(fit)
library (rpart.plot)
rpart.plot(fit)
##Predicting the Values by testing with test dataset using predict keyword 
Prediction <- predict(fit, Titanictest, type = "class")
##Converting to dataframe
submit <- data.frame(PassengerId = Titanictest$PassengerId, Survived = Prediction)
##Writing the predicted output to excel file
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)








## Decission Tree Regression with Energy Efficiency Dataset



## Intalling the packages 
install.packages("rpart")
install.packages("rpart.plot")
install.packages("xlsx")
##Loading the Library 
library("xlsx")
library (rpart)
library (rpart.plot)
##Retriving the input data from CSV file
file1 <- "D:/2 ADS Fall 17/suporting documents/sre 3/Energy Efficiency ENB2012_data.xlsx"
## Reading the retrived CSV file
datafile <- read.xlsx(file,1)
datafile
## To change the work work directory 
setwd("C:\\Users\\sayee\\Downloads\\ADS\\Assignment3")
## Retriving the CSV file
file <- "Energy Efficiency ENB2012_data.xlsx"
## Reading the file 
datafile <- read.xlsx(file,1)
datafile
##Building the Decission Tree Model with Regression for output "y1"
tree <- rpart(Y1 ~ X1	+X2	+X3	+X4	+X5+	X6+	X7+	X8 ,method="anova", data=datafile)
##Ploting the tree 
rpart.plot(tree)
##Building the Decission Tree Model with Regression for output "y1 and y2"
tree <- rpart(Y1+Y2 ~ X1	+X2	+X3	+X4	+X5+	X6+	X7+	X8 ,method="anova", data=datafile)
##Ploting the tree 
rpart.plot(tree)
##Building the Decission Tree Model with Regression for output "y2"
tree <- rpart(Y2 ~ X1	+X2	+X3	+X4	+X5+	X6+	X7+	X8 ,method="anova", data=datafile)
##Ploting the tree 
rpart.plot(tree)
##To view the data file 
View(datafile)
