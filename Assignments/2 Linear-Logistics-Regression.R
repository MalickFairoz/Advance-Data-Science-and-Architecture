
## PART - 1--   LINEAR REGRESSION



## Building a model for Multiple Linear Regression using the function

## Reading the CSV data file and creating a model for multiple linear Regression
library(datasets)
LinearRegression <- read.csv("D:/2 ADS Fall 17/Assignments/2 LR.csv")
lm(LinearRegression)
str(LinearRegression)
lmr <- lm(Yield ~ Factor.1 + Factor.2, data=LinearRegression)
lmr

summary(lmr)
## In summary the F0 = 211.9 is proved 

## Now Computing the F0 value step by step process manually 
LinearRegression

##Converting the output data from the file into matrix "y"
y <- as.matrix(LinearRegression[4],row.names=0,col.names=0)
rownames(y) <- colnames(y) <- NULL
print(y)

##Converting the input data from the file into matrix "x"
x <- as.matrix(cbind(rep(1,17),LinearRegression[2:3]),row.names=0,col.names=0)
rownames(x) <- colnames(x) <- NULL
print(x)
x
y

## Transposing the matrix x
t(x)
## Multiplying the matrixes
t(x)%*%x
t(x)%*%y

##Inversing the matrix
solve((t(x)%*%x))

##The BETA value is obtained in matrix "b"
b <- solve((t(x)%*%x))%*%(t(x)%*%y)
b[1]

## The estimated regression co efficients are 
b0 = b[1]
b1 = b[2]
b2 = b[3]

## Computing the yt (y dash)
#yt <- sum(b0+(b1*x1)+(b2*x2))

y5 <- sum(b0+(b1*x[5,2])+(b2*x[5,3]))
y5

##The observed fifth response value is 
y[5]


##The residual corresponding to this value is 

e5 = y[5] - y5
e5


## The Hat matrix H is calculated using the design matrix X

h <- x%*%(solve(t(x)%*%x))%*%t(x)
h

##Defining value for J in matrix which has ones with nXn
j <- matrix(rep(1,17), nr=17, nc=17)
j

##Calculating the value for SSR

ssr <- t(y)%*%(h - ((1/17)*j))%*%y
ssr


#Calculating the value for MSR

msr <- ssr/2
msr

## Defining the value for I identity matix

i <- diag(17)
i


##Calculating the value for SSE 

sse <- t(y)%*%(i-h)%*%y
sse

#Calculating the value for MSE

mse <- sse/(17-(2+1))
mse


# Finally the F0 Value is computed by the followiing formula

F0 <- msr/mse
F0






############################################################################################################


## PART - 2--   Logistics REGRESSION





##Reading the csv data file and store in a variable

LogisticsRegression <- read.csv("D:/2 ADS Fall 17/Assignments/2 LoanDatafile.csv")



##Factorizing the dependent and categorial variable for creating the model

LogisticsRegression$Decision=as.factor(LogisticsRegression$Decision)
contrasts(LogisticsRegression$Decision)

LogisticsRegression$Res_status=as.factor(LogisticsRegression$Res_status)
contrasts(LogisticsRegression$Res_status)

LogisticsRegression$Occupation=as.factor(LogisticsRegression$Occupation)
contrasts(LogisticsRegression$Occupation)

LogisticsRegression$Job_status=as.factor(LogisticsRegression$Job_status)
contrasts(LogisticsRegression$Job_status)

LogisticsRegression$Liab_ref=as.factor(LogisticsRegression$Liab_ref)
contrasts(LogisticsRegression$Liab_ref)

LogisticsRegression$Acc_ref=as.factor(LogisticsRegression$Acc_ref)
contrasts(LogisticsRegression$Acc_ref)


##The Logistic Regression model is created and stored in "logr"


logr <- glm(Decision ~ Res_status + Occupation + Job_status + Liab_ref + Acc_ref, data=LogisticsRegression,family = "binomial")

##Making a dataframe for storing the input values of set 1

DataFrame1 <- data.frame(Res_status="owner", Occupation="creative_",Job_status="governmen", Liab_ref="f", Acc_ref="given")
DataFrame1

##  The input values of set 1 are used in the logistics regression model and the predicted output is displayed

output1 <- predict(logr, DataFrame1  ,type='response')
output1

##Making a dataframe for storing the input values of set 2

DataFrame2 <- data.frame(Res_status="rent", Occupation="creative_",Job_status="governmen", Liab_ref="f", Acc_ref="given")
DataFrame2

##  The input values of set 2 are used in the logistics regression model and the predicted output is displayed 

output2 <- predict(logr, DataFrame2  ,type='response')
output2










