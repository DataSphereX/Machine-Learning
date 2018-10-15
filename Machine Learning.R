setwd("E://Machine Learning/Assignments")
mydata <- read.csv("cars_train.csv")
testdata <- read.csv("cars_test.csv")
View (mydata)

library(ggplot2)
library(CNAnorm)
#install.packages("CNAnorm")
Transport <- as.factor("Transport")
Gender <- as.factor("Gender")
Engineer <- as.factor("Engineer")
MBA <- as.factor("MBA")
license <- as.factor("license")

#For cars vs other means of transport
mydata$Transport <- ifelse(mydata$Transport =="Car",1,0)
testdata$Transport <- ifelse(testdata$Transport =="Car",1,0)

#View (mydata$Transport)
str(mydata)
summary(mydata)

# ggplot cars/ Gender:
table(mydata$Transport)
table(testdata$Transport)

#No. of male vs Female:
table(mydata$Gender) # Shows number of male to female 
prop.table(table(mydata$Gender)) # Proportion of male to female
ggplot(mydata, aes(x=Gender)) + geom_bar() # Barchart of Gender

#Gender vs Transport  (facet_wrap):
ggplot(mydata, aes(x= Gender))  + theme_bw()+ 
  facet_wrap(~Transport ) + geom_bar()+ 
  labs(y= "No. Of People", title = "Gender vs Transport") # Faceting Gender to Transport

# Cars vs gender:
prop.table(table(mydata$Gender))
ggplot(mydata, aes(x=Work.Exp)) + geom_histogram(binwidth = 1)
ggplot(mydata, aes(x=Transport)) + geom_bar() + theme_bw()
ggplot(mydata, aes(x=Gender, fill= Transport)) + geom_bar()

#Age & Transport as car:
ggplot(mydata, aes(x=Transport))
ggplot(mydata, aes(x=Age)) + geom_histogram(binwidth = 5) 

ggplot(mydata, aes(x=Age)) + geom_histogram(binwidth = 1) +
  facet_wrap(~Transport)

#license & Transport as car:
ggplot(mydata, aes(x=license)) + geom_bar()
nrow(mydata)
ggplot(mydata, aes(x=license)) + geom_bar()  + facet_wrap(~Transport)
table(mydata$Transport)

#Split the data:

# Hypothesis test:
logit <- lm(formula=mydata$Transport~., data=mydata)
summary(logit)
# Age & License are significant

#CART Model:
## setting the control paramter inputs for rpart:
library(rpart)
library(rpart.plot)
ctrl = rpart.control(minsplit=100, minbucket = 10, cp = 0, xval = 10)
destree <- rpart(formula = mydata$Transport ~ ., 
                 data = mydata, method = "class", 
                 control =ctrl)

rpart.plot(destree)
fancyRpartPlot(destree)

printcp(destree)
plotcp(destree)

#Predicting the test set results:
ypred1 <- predict(destree, testdata, type="prob")
ypred1<-as.data.frame(ypred1)
ycm1<-as.data.frame(ypred1)
ypred1<-ifelse(ypred1$`1`>ypred1$`0`,ypred1$`1`,ypred1$`0`)
y_cm1<-ifelse(ycm1$`1`>ycm1$`0`,1,0)

#Confusion Matrix:
cm1=table(testdata[,1],y_cm1)
cm1

#Finding Optimal mtry values:
library(randomForest)
tclassifier <- tuneRF(x = mydata,
                      y=mydata$Transport, 
                      mtryStart = 1, 
                      ntreeTry=100,
                      stepFactor = 1.5, 
                      improve = 0.0001, 
                      trace=TRUE, 
                      plot = TRUE, 
                      doBest = TRUE, 
                      nodesize = 5, 
                      importance=TRUE )


tclassifier <- tuneRF(mydata$Transport, 
                      ntreeTry=100)
set.seed(1234)
rantree = randomForest(x = mydata,
                       y = mydata$Transport,
                       ntree = 5,
                       nodesize = 5,
                       mtry=5,
                       importance = TRUE)
print(rantree)


install.packages("party")
library(party)
library(randomForest)
View(readingSkills)
output.forest <- randomForest(nativeSpeaker ~ age + shoeSize + score, 
                              data = readingSkills)
print(output.forest)


check <- randomForest(license ~ Salary+ Age, data= mydata)
print(check)






