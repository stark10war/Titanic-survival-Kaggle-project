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
colnames(Titanic_new)
Titanic_new<- Titanic_new[,-c(6,12,13)]
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
##################RANDOM FOREST#################################################


set.seed(999)
fit<- randomForest(formula = Survived~.,data = titanic_train, ntree = 60, mtry = 8)
plot(fit)

summary(fit)

#Train accuracy of Random forest
predTrain_fit<-predict(fit, newdata = titanic_train,type = 'class')
confusionMatrix(predTrain_fit, titanic_train$Survived)

#Test accuracy of Random Forest
predTest_fit<-predict(fit, newdata = titanic_test,type = 'class')
confusionMatrix(predTest_fit, titanic_test$Survived)

#Roc curve 
library(pROC)
prob <- predict(fit, titanic_test, type="prob") # Prediction
roc_curve <- roc(titanic_test$Survived, prob[,2]) # Draw ROC curve.
plot(roc_curve, print.thres="best", print.thres.best.method="closest.topleft")
result.coords <- coords(roc_curve, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)#

#AUC
auc(titanic_test$Survived, prob[,2])


#####################################################################################
#Using full data set for model
set.seed(999)
fit_final<- randomForest(formula = Survived~.,data = Titanic_new, ntree = 1000, mtry = 12)
plot(fit_final)

pred_all<-predict(fit_final, newdata = Titanic_new,type = 'class')
confusionMatrix(pred_all, Titanic_new$Survived)

prob <- predict(fit_final, Titanic_new, type="prob") # Prediction
roc_curve <- roc(Titanic_new$Survived, prob[,2]) # Draw ROC curve.
plot(roc_curve, print.thres="best", print.thres.best.method="closest.topleft")
result.coords <- coords(roc_curve, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)#

#AUC
auc(Titanic_new$Survived, prob[,2])


#################### Prediction on the unseen data###############################
#importing dataset

path<- "C:/Users/Shashank/Downloads/Titanic dataset"
setwd(path)

data.test<- read.csv("test.csv")

head(data.test)
str(data.test)
dim(data.test)



#------------------------------------------------------------------------------#

data.test$Pclass<- as.factor(data.test$Pclass)
colnames(data.test)
colSums(is.na(data.test))
data.test[data.test ==""]<- NA

#------Filling NA in Ages with category means----------------------#
library(dplyr)
attach(data.test)

#LOng way to do this :(

#mean_male_adult<- mean(data.test$Age[Sex=='male'& IsChild==FALSE], na.rm = TRUE)
#mean_male_child<- mean(data.test$Age[Sex=='male'& IsChild==TRUE], na.rm = TRUE)
#mean_female_adult<-mean(data.test$Age[Sex=='female'&IsChild==FALSE], na.rm = TRUE)
#mean_female_child<-  mean(data.test$Age[Sex=='female'&IsChild==TRUE], na.rm = TRUE)# no female child exists

#data.test$Age[is.na(Age)&Sex=='male'& IsChild==FALSE] <- mean_male_adult
#data.test$Age[is.na(Age)&Sex=='male'& IsChild==TRUE]<- mean_male_child 
#data.test$Age[is.na(Age)&Sex=='female'& IsChild==FALSE]<- mean_female_adult 


library(zoo)
data.test$Age <- with(data.test, ave(Age,Sex,IsChild, FUN = na.aggregate)) # Stackoverflow zindabad

#Filling Na's in embarked
summary(data.test$Embarked)
data.test$Embarked[is.na(Embarked)]<- "S"

attach(data.test)
##################Feature engieering##############################################
data.test$IsChild[grepl(" Master. ", data.test$Name)]<- 'yes'
data.test$IsChild[is.na(data.test$IsChild)]<- 'no'
data.test$Is_Married[grepl(" Mrs. ", data.test$Name)]<- 'married'
data.test$Is_Married[grepl(" Miss. ", data.test$Name)| IsChild == 'yes']<-'unmarried' 
data.test$Is_Married[grepl(" Mr. ", data.test$Name)]<- 'cant_say'
data.test$Is_Married[is.na(data.test$Is_Married)]<- 'cant_say'


#Getting relavant features
colnames(data.test)
data.test2<- data.test[,-c(1,3,8,10)]
colSums(is.na(data.test2))
data.test2$Fare[is.na(data.test2$Fare)]<- mean(data.test2$Fare[Embarked=='S'& Sex=='male'], na.rm = TRUE)

write.csv(data.test2, "Titanic_test_Cleaned.csv")

attach(data.test2)
str(data.test2)
data.test2$IsChild<- as.factor(data.test2$IsChild)
data.test2$Is_Married <- as.factor(data.test2$Is_Married)

library(fastDummies)

data.test2 <- dummy_cols(data.test2,remove_original = TRUE,remove_first_dummy = FALSE)
colnames(Titanic_new)
colnames(data.test2)
data.test2<- data.test2[,-c(5,8,11,13,15)]
data.test2<- data.test2[,-c(6,12)]

#######################################################################################

#Data visualisation
#Checking correlation

library(ggplot2)
corr_matrix<- cor(data.test2)
corrplot::corrplot(corr_matrix,method = "number")

library(randomForest)
library(MASS)
str(data.test2)

data.test2$Pclass_2<- as.factor(data.test2$Pclass_2)
data.test2$Sex_female<- as.factor(data.test2$Sex_female)
data.test2$Embarked_C<- as.factor(data.test2$Embarked_C)
data.test2$Embarked_Q<- as.factor(data.test2$Embarked_Q)
data.test2$IsChild_yes<- as.factor(data.test2$IsChild_yes)
data.test2$Is_Married_married<- as.factor(data.test2$Is_Married_married)

colSums(is.na(data.test2))


pred_test<-predict(fit_final, newdata = data.test2,type = 'class')
pred_test<- as.character(pred_test)
results<- data.frame(data.test$PassengerId, pred_test,row.names = NULL)
colnames(results)<- c("PassengerId", "Survived")

write.csv(results,"Results.csv",row.names = FALSE)

########################################################################################################








