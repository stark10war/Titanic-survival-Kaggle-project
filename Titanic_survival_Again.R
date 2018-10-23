######################Titanic dataset############################################

#Setting environment
library(car)
library(caret)
library(tools)
library(ggplot2)
library(lmtest)
library(rpart)
library(rpart.plot)
library(randomForest)
library(neuralnet)
library(magrittr)
library(lubridate)

#importing dataset
path<- "D:/Shashank R files/Titanic dataset/Titanic-survival-Kaggle-project"
setwd(path)

Titanic.train<- read.csv("train.csv")

head(Titanic.train)
str(Titanic.train)
dim(Titanic.train)

Titanic.test<- read.csv("test.csv")

head(Titanic.test)
str(Titanic.test)
dim(Titanic.test)

#Joining train and test togather
Titanic.test$Survived<- NA
Titanic.all<-rbind(Titanic.train, Titanic.test)
str(Titanic.all)
Titanic.all$Pclass<- as.factor(Titanic.all$Pclass)


##################Feature engieering##############################################
Titanic.all$IsChild[grepl(" Master. ", Titanic.all$Name)]<- 'yes'
Titanic.all$IsChild[is.na(Titanic.all$IsChild)]<- 'no'
Titanic.all$Is_Married[grepl(" Mrs. ", Titanic.all$Name)]<- 'married'
Titanic.all$Is_Married[grepl(" Miss. ", Titanic.all$Name)| Titanic.all$IsChild == 'yes']<-'unmarried' 
Titanic.all$Is_Married[grepl(" Mr. ", Titanic.all$Name)]<- 'cant_say'
Titanic.all$Is_Married[is.na(Titanic.all$Is_Married)]<- 'cant_say'
Titanic.all$Is_Alone[Titanic.all$SibSp+ Titanic.all$Parch == 0]<- "yes"
Titanic.all$Is_Alone[is.na(Titanic.all$Is_Alone)]<- "no"


###############-----------missing values treatment ------################################
library(dplyr)
attach(Titanic.all)

colSums(is.na(Titanic.all))
Titanic.all[Titanic.all==""]<- NA

# Filling Na's in embarked
summary(Titanic.all$Embarked)
Titanic.all$Embarked[is.na(Titanic.all$Embarked)]<- "S"

# Filling NA in Ages with category means
library(zoo)
Titanic.all$Age <- with(Titanic.all, ave(Age,Sex,IsChild,Is_Married, FUN = na.aggregate)) # Stackoverflow zindabad

#Filling Na for fare
Titanic.all$Fare[is.na(Titanic.all$Fare)]<- mean(Titanic.all$Fare[Pclass==3&IsChild=='no'], na.rm = TRUE)

##########-------------GETTING THE NECESSARY RELEVANT FEATURES---------###########
colnames(Titanic.all)
Titanic.all<- Titanic.all[, -c(1,4,9,11)]

##########----------------SEPERATING tRAIN AND TEST SETS -----------####################

Titanic.test<- Titanic.all[is.na(Titanic.all$Survived),-c(1)]
Titanic.train<- Titanic.all[!is.na(Titanic.all$Survived),]

#Writing to csv
getwd()



# 
# ########################## DATA VISUALISATION #######################################
# library(ggplot2)
# ggplot(data = Titanic.train, aes(x= factor(Survived), fill = Sex))+ geom_bar()+facet_grid(.~Embarked)  
# ggplot(data = Titanic.train, aes(x= Age, fill = factor(Survived)))+ geom_histogram(bins = 10) 
# ggplot(data = Titanic.train, aes(x= factor(Parch), fill = factor(Survived)))+ geom_bar() 
# ggplot(data = Titanic.train, aes(x= Age, fill = factor(Survived)))+ geom_histogram(bins = 20)-> obj1 
# ggplot(data = Titanic.train, aes(x= Embarked, fill = factor(Survived)))+ geom_bar() 
# 
# ggplot(data = Titanic.train, aes(x= Fare, col = factor(Survived)))+ geom_freqpoly(bins=15)
# 
# obj1<-obj1+labs(title = "Age vs Survival", y= "No. of passangers survived", fill = "Survived")+theme(panel.background = element_rect(fill = "palegreen1"),plot.title = element_text(hjust = 0.5,face = "bold",color = "cadetblue"))
# 
