install.packages("irr")
install.packages("caret")
install.packages("gains")
install.packages("ROCR")
install.packages("glmnet")
library(glmnet)
library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)
library(irr)
library(caret)
library(ROCR)
library(gains)
options(scipen=999)
#reading R file
snacks<-read.csv(file.choose(),header = TRUE)
#filtering the data for Brand B
snacks %>%
          filter(X3==1)%>%
          summarise(n())
snacks %>%
        filter(X17==1)%>%
        summarise(n())
snacks %>%
        filter(X10==1)%>%
        summarise(n())
summary(snacks$X31)
str(snacks$X31)
str(snacks)
#Brand B respondents as good
snacks %>%
         filter(X24==5 |X24==6 | X24==7 | X24==8| X24==9| X24==10) %>%
         summarise(n())
#Brand A respondents as good
snacks %>%
  filter(X23==5 |X23==6 | X23==7 | X23==8| X23==9| X23==10) %>%
  summarise(n())
snacks %>%
  filter(X24==5 |X24==6 | X24==7 | X24==8| X24==9| X24==10) %>%
  filter(X23==5 |X23==6 | X23==7 | X23==8| X23==9| X23==10) %>%
  summarise(n())
#Exploring data
dim(snacks)
str(snacks)
View(snacks)
#checling missing values
summary(is.na(snacks))
#BUILDING model for b
#removing unwanted columns
brandB<-select(snacks,X3,X10,X17,X24,X31)

#checking for missing values
colSums(is.na(brandB))
dim(brandB)

typeof(brandB$X10)
brandB$GoodBad<-as.integer(brandB$GoodBad)
#creating a rating column
brandB<-brandB %>%
  mutate(GoodBad= ifelse(brandB$X24>5,1,0))
dim(brandB)
#splitting data
set.seed(200)
index<-sample(nrow(brandB),0.80*nrow(brandB),replace = F)
training<-brandB[index, ]
testing<-testing[-index, ]
View(training)
View(testing)


table(brandB$GoodBad) / nrow(brandB)
table(training$GoodBad)/nrow(training)
table(testing$GoodBad)/nrow(testing)
#viewing tables
View(training)
View(testing)
View(brandB)
#building logistic model
training$GoodBad<-as.factor(training$GoodBad)
logreg<-glm(data = training,training$GoodBad~.,family = "binomial",maxit =100)
summary(logreg)

#validating the model

predictedvalues<-predict(logreg,type="response",newdata=testing)
head(predictedvalues)
tail(predictedvalues)
#the value of 1S
table(training$'GoodBad')/nrow(training)
#predecting cutoff values
predictcutoff<-ifelse(predictedvalues >= 0.5,1,0)

#kappa and confusion matrix
kappa2(data.frame(testing$GoodBad,predictcutoff))
confusionMatrix(testing$GoodBad,predictcutoff,positive = "1")

summary(predictcutoff)
dim(predictcutoff)
str(predictcutoff)
predictcutoff<-as.factor(predictcutoff)
testing$GoodBad<-as.factor(testing$GoodBad)
predictcutoff<-as.factor(predictcutoff)
testing$GoodBad<-as.factor(testing$GoodBad)







