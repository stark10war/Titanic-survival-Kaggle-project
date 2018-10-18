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


##################Feature engieering##############################################
Titanic$IsChild[grepl(" Master. ", Titanic$Name)]<- 'yes'
Titanic$IsChild[is.na(Titanic$IsChild)]<- 'no'
Titanic$Is_Married[grepl(" Mrs. ", Titanic$Name)]<- 'married'
Titanic$Is_Married[grepl(" Miss. ", Titanic$Name)| IsChild== 'yes']<-'unmarried' 
Titanic$Is_Married[grepl(" Mr. ", Titanic$Name)]<- 'cant_say'
Titanic$Is_Married[is.na(Titanic$Is_Married)]<- 'cant_say'

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
#Checking correlation

library(ggplot2)
corr_matrix<- cor(Titanic_new)
corrplot::corrplot(corr_matrix,method = "number")

library(randomForest)
library(MASS)
str(Titanic_new)

Titanic_new$Survived<- as.factor(Titanic_new$Survived)
Titanic_new$Pclass_1<- as.factor(Titanic_new$Pclass_1)
Titanic_new$Pclass_2<- as.factor(Titanic_new$Pclass_2)
Titanic_new$Sex_female<- as.factor(Titanic_new$Sex_female)
Titanic_new$Embarked_C<- as.factor(Titanic_new$Embarked_C)
Titanic_new$Embarked_Q<- as.factor(Titanic_new$Embarked_Q)
Titanic_new$IsChild_yes<- as.factor(Titanic_new$IsChild_yes)
Titanic_new$Is_Married_married<- as.factor(Titanic_new$Is_Married_married)
Titanic_new$Is_Married_unmarried<- as.factor(Titanic_new$Is_Married_unmarried)


#-------------Splitting into train and test set-------------------------------------------
set.seed(999)
library(caTools)

spl<- sample.split(Titanic_new$Survived, 0.7)

titanic_train<- Titanic_new[spl == TRUE, ]
titanic_test<-  Titanic_new[spl == FALSE, ]

###################DECISION TREE#################################################

library(rpart.plot)
set.seed(999)
tree<-rpart(Survived~., data = titanic_train)
prp(tree)

pred_tree<-predict(tree, newdata = titanic_test, type = 'class')
confusionMatrix(data = pred_tree,reference = titanic_test$Survived)

tree$variable.importance
attach(titani)
##################RANDOM FOREST#################################################

set.seed(999)
fit<- randomForest(formula = Survived~.,data = titanic_train, ntree = 82, mtry = 12)
plot(fit)

which.min(fit$err.rate)
fit$err.rate
summary(fit)

pred_fit<-predict(fit, newdata = titanic_test,type = 'class')
confusionMatrix(pred_fit, titanic_test$Survived)











