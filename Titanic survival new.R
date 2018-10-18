#######################TItanic Dataset##################################################

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
path<- "C:/Users/Shashank/Downloads/Titanic dataset"
setwd(path)

Titanic<- read.csv("train.csv")

head(Titanic)
str(Titanic)
dim(Titanic)

#------------------------------------------------------------------------------#

Titanic$Pclass<- as.factor(Titanic$Pclass)
colnames(Titanic)
colSums(is.na(Titanic))
Titanic[Titanic ==""]<- NA

#Removing outliers
boxplot(Titanic$Fare)
Titanic<-Titanic[Titanic$Fare<=100,]

#Feature engieering
Titanic$IsChild[grepl(" Master. ", Titanic$Name)]<- 'yes'
Titanic$IsChild[is.na(Titanic$IsChild)]<- 'no'
Titanic$Is_Married[grepl(" Mrs. ", Titanic$Name)]<- 'married'
Titanic$Is_Married[grepl(" Miss. ", Titanic$Name)| IsChild==TRUE]<-'unmarried' 
Titanic$Is_Married[grepl(" Mr. ", Titanic$Name)]<- 'cant_say'
Titanic$Is_Married[is.na(Titanic$Is_Married)]<- 'cant_say'

#------Filling NA in Ages with category means----------------------#
library(dplyr)
attach(Titanic)

#LOng way to do this :(

#mean_male_adult<- mean(Titanic$Age[Sex=='male'& IsChild==FALSE], na.rm = TRUE)
#mean_male_child<- mean(Titanic$Age[Sex=='male'& IsChild==TRUE], na.rm = TRUE)
#mean_female_adult<-mean(Titanic$Age[Sex=='female'&IsChild==FALSE], na.rm = TRUE)
#mean_female_child<-  mean(Titanic$Age[Sex=='female'&IsChild==TRUE], na.rm = TRUE)# no female child exists

#Titanic$Age[is.na(Age)&Sex=='male'& IsChild==FALSE] <- mean_male_adult
#Titanic$Age[is.na(Age)&Sex=='male'& IsChild==TRUE]<- mean_male_child 
#Titanic$Age[is.na(Age)&Sex=='female'& IsChild==FALSE]<- mean_female_adult 


library(zoo)
Titanic$Age <- with(Titanic, ave(Age,Sex,IsChild, FUN = na.aggregate)) # Stackoverflow zindabad

#Filling Na's in embarked
summary(Titanic$Embarked)
Titanic$Embarked[is.na(Embarked)]<- "S"


#Getting relavant features
Titanic_new<- Titanic[,-c(1,4,9,11)]
colSums(is.na(Titanic_new))

write.csv(Titanic_new, "Titanic_train_Cleaned.csv")

attach(Titanic_new)
str(Titanic_new)

library(fastDummies)

Titanic_new <- dummy_cols(Titanic_new,remove_first_dummy = TRUE)

#######################################################################################

#Data visualisation

library(ggplot2)
corr_matrix<- cor(Titanic_new)



corrplot::corrplot(corr_matrix,method = "number")





