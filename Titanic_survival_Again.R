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

str(Titanic.all)

Titanic.all$IsChild <- as.factor(Titanic.all$IsChild)
Titanic.all$Is_Married <- as.factor(Titanic.all$Is_Married)
Titanic.all$Is_Alone <- as.factor(Titanic.all$Is_Alone)

##########----------------SEPERATING tRAIN AND TEST SETS -----------####################

Titanic.test<- Titanic.all[is.na(Titanic.all$Survived),-c(1)]
Titanic.train<- Titanic.all[!is.na(Titanic.all$Survived),]

dim(Titanic.test)
dim(Titanic.train)


#Writing to csv
getwd()
write.csv(Titanic.train, "train_cleaned.csv",row.names = FALSE)
write.csv(Titanic.test, "test_cleaned.csv", row.names = FALSE)


# ########################## DATA VISUALISATION ################################################################
library(ggplot2)

sex_survival_plot<-ggplot(data = Titanic.train, aes(x= factor(Survived), fill = Sex))+ geom_bar() + labs(title = "Sex vs Survival", x= "Survived", y = "No. of Passengers", fill ="Survived")
sex_survival_plot<- sex_survival_plot+ theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "lemonchiffon2" ))
sex_survival_plot

Age_survival_plot<-ggplot(data = Titanic.train, aes(x= Age, fill = factor(Survived)))+ geom_histogram(bins = 10)
Age_survival_plot<- Age_survival_plot + labs(title = "Age vs Survival", x= "Age", y = "No. of Passengers", fill= "Survived")+ theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "lemonchiffon2" ))
Age_survival_plot


Pclass_Survival_plot<- ggplot(data = Titanic.train, aes(x= Pclass, fill = factor(Survived)))+ geom_bar()
Pclass_Survival_plot<-Pclass_Survival_plot+ labs(title = "class vs Survival", x= "Pclass", y = "No. of Passengers", fill ="Survived")+ theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "lemonchiffon2" ))
Pclass_Survival_plot


Embarked_survived_plot<-ggplot(data = Titanic.train, aes(x= Embarked, fill = factor(Survived)))+ geom_bar()
Embarked_survived_plot


Fare_survived_plot<-ggplot(data = Titanic.train, aes(x= Fare, fill = factor(Survived)))+ geom_histogram(bins = 5)
Fare_survived_plot


#############################################################################################





