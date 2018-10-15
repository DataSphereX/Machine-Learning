cars = read.csv("cars.csv")
carsTest = read.csv("test.csv")
head(cars)
dim(cars)
str(cars)
cars$Engineer = as.factor(cars$Engineer)
cars$MBA = as.factor(cars$MBA)
cars$license = as.factor(cars$license)
summary(cars)

boxplot(cars$Age ~cars$Engineer, main = "Age vs Eng.")
boxplot(cars$Age ~cars$MBA, main ="Age Vs MBA")

boxplot(cars$Salary ~cars$Engineer, main = "Salary vs Eng.")
boxplot(cars$Salary ~cars$MBA, main = "Salary vs MBA.")
hist(cars$Work.Exp, col = "red", main = "Distribution of work exp")

table(cars$license,cars$Transport)
boxplot(cars$Work.Exp ~ cars$Gender)
#hypothesis
boxplot(cars$Salary~cars$Transport, main="Salary vs Transport")
boxplot(cars$Age~cars$Transport, main="Age vs Transport")
boxplot(cars$Distance~cars$Transport, main="Distance vs Transport")
table(cars$Gender,cars$Transport)

#missing value
anyNA(cars)
cars[!complete.cases(cars), ]

library(DMwR)
cars = knnImputation(cars, 5)

# Normalize continuous variables
cars$Salary = log(cars$Salary)

carsTest$Salary = log(carsTest$Salary)
carsTest$Engineer = as.factor(carsTest$Engineer)
carsTest$MBA = as.factor(carsTest$MBA)
carsTest$license = as.factor(carsTest$license)

#test and train data
library(caret)
random <- createDataPartition(cars$Transport, p=0.70, list=FALSE)
cars_train <- cars[ random,]
cars_test <- cars[-random,]

library(e1071)
#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(cars_train$Transport ~., data=cars_train)
#What does the model say? Print the model summary
Naive_Bayes_Model


#Prediction on the train dataset
NB_Predictions=predict(Naive_Bayes_Model,cars_train)
table(NB_Predictions,cars_train$Transport)

#Prediction on the test dataset
NB_Predictions=predict(Naive_Bayes_Model,cars_test)
table(NB_Predictions,cars_test$Transport)

# prediction for test sample
NB_Predictions=predict(Naive_Bayes_Model,carsTest)
NB_Predictions

#LDA
cars = read.csv("cars.csv")
carsTest = read.csv("test.csv")
cars[145,4] = 0

# Normalize continuous variables
cars$Salary = log(cars$Salary)
carsTest$Salary = log(carsTest$Salary)
cars$Gender<-ifelse(cars$Gender=="Male",1,0)
carsTest$Gender<-ifelse(carsTest$Gender=="Male",1,0)

random <- createDataPartition(cars$Transport, p=0.70, list=FALSE)
cars_train <- cars[ random,]
cars_test <- cars[-random,]

library(MASS)
fit.ld=lda(Transport~., data=cars_train, cv=TRUE)
fit.ld

LDA_predictions = predict(fit.ld,cars_train)
table(LDA_predictions$class, cars_train$Transport)

LDA_predictions = predict(fit.ld,cars_test)
table(LDA_predictions$class, cars_test$Transport)

predict(fit.ld,carsTest)

#knn
library(class)

cars = read.csv("cars.csv")
carsTest = read.csv("test.csv")

cars[145,4] = 0

# Normalize continuous variables
cars$Salary = log(cars$Salary)
carsTest$Salary = log(carsTest$Salary)

cars$Gender<-ifelse(cars$Gender=="Male",1,0)
carsTest$Gender<-ifelse(carsTest$Gender=="Male",1,0)


random <- createDataPartition(cars$Transport, p=0.70, list=FALSE)
cars_train <- cars[ random,]
cars_test <- cars[-random,]

trControl <- trainControl(method  = "cv", number  = 10)
fit.knn <- train(Transport ~ .,
                 method     = "knn",
                 tuneGrid   = expand.grid(k = 2:20),
                 trControl  = trControl,
                 metric     = "Accuracy",
                 preProcess = c("center","scale"),
                 data       = cars_train)
fit.knn

KNN_predictions = predict(fit.knn,cars_train)
table(KNN_predictions, cars_train$Transport)

KNN_predictions = predict(fit.knn,cars_test)
table(KNN_predictions, cars_test$Transport)

predict(fit.knn,carsTest)
