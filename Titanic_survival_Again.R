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
library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(sqldf)
library(Hmisc)
library(aod)
library(BaylorEdPsych)
library(ResourceSelection)
library(pROC)
library(ROCR)
library(caTools)
library(DescTools)
library(Metrics)
#importing dataset
path<- "D:/Shashank R files/Titanic dataset/Titanic-survival-Kaggle-project"
setwd(path)

Titanic.train<- read.csv("train.csv")
Titanic.train1<- Titanic.train #Backup file

head(Titanic.train)
str(Titanic.train)
dim(Titanic.train)

Titanic.test<- read.csv("test.csv")
Titanic.test1<- Titanic.test #Backup file
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
Titanic.all$Survived<- as.factor(Titanic.all$Survived)
######-----------Outlier treatment-------------###########################
ggplot(data = Titanic.all, aes(x= Pclass, y= Fare, fill = Pclass))+ geom_boxplot()
boxplot(Titanic.all$Fare)$out


#Replacing values of outlier by 99th percentile
H<-quantile(Titanic.all$Fare, probs = 0.99)
Titanic.all$Fare[Titanic.all$Fare>H]<- H

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

######-------------Spliting Titanic.train into train and validation set----------############
set.seed(24)
spl<- sample.split(Titanic.train$Survived, 0.7)

data.train<- Titanic.train[spl==TRUE, ]
data.test<- Titanic.train[spl==FALSE,]


summary(data.train$Survived)
summary(data.test$Survived)


##########-----------Model building-----------------------#########################

################---------LOGISTIC REGRESSION-------------------####################
library(lmtest)
library(caTools)

str(Titanic.train)

#Getting formula
predictors<- paste(colnames(Titanic.train)[-1], sep = "+", collapse = '+')


model1<- glm(Survived~Pclass+Sex+Age+SibSp+Parch+I(Embarked=='S')+IsChild+ I(Is_Married=="unmarried"),
             data = data.train, family = 'binomial')
summary(model1)

#Checking multicolinearity
vif(model1)

waldtest(model1)#Overall significance test


###################### Lagrange Multiplier or Score test ###########
modelchi<- model1$null.deviance - model1$deviance

chidf <- model1$df.null -model1$df.residual
chisq.prob <- 1 - pchisq(modelchi, chidf)
format(round(chisq.prob, 2), nsmall = 5)


########### GOODNESS OF FIT A.k.a R square ##########
library(DescTools)
PseudoR2(model1)

############## Accuracy on train set #################

pred_train<- predict(model1, newdata = data.train, type = 'response')

roc_curve<- roc(response= data.train$Survived, predictor = pred_train, levels= rev(levels(data.train$Survived)))
plot(roc_curve)

coords(roc_curve, 'best')

predclass_train <-ifelse(pred_train>coords(roc_curve,"best")[1],1,0)
confusionMatrix(data = predclass_train, reference = data.train$Survived)



############Accuracy on test set #################################
pred_test<- predict(model1, newdata = data.test, type = 'response')


roc_curve<- roc(response= data.test$Survived, predictor = pred_test, levels= rev(levels(data.test$Survived)))
plot(roc_curve)



predclass_test <-ifelse(pred_test>coords(roc_curve,"best")[1],1,0)
confusionMatrix(data = predclass_test, reference = data.test$Survived)


###################----Building model on full data--------#####################3


final_model<- glm(Survived~Pclass+Sex+Age+SibSp+Parch+I(Embarked=='S')+IsChild+ I(Is_Married=="unmarried"),
             data = Titanic.train, family = 'binomial')
summary(final_model)


pred_all_train<- predict(final_model, newdata = Titanic.train, type = 'response')

roc_curve<- roc(response= Titanic.train$Survived, predictor = pred_all_train, levels= rev(levels(Titanic.train$Survived)))
plot(roc_curve)

coords(roc_curve, 'best')

predclass_all_train <-ifelse(pred_all_train>coords(roc_curve,"best")[1],1,0)
confusionMatrix(data = predclass_all_train, reference = Titanic.train$Survived)

threshold<- coords(roc_curve,"best")[1]





################### Predicting on Final test set (Unseen data) #####################

Predictions_final<- predict(final_model, newdata = Titanic.test, type = 'response')

predclass_final<- ifelse(Predictions_final>threshold,1,0)

#---------Storing the results-----------#
getwd()
results_final<-data.frame(Titanic.test1$PassengerId, predclass_final)
results_final
colnames(results_final)<- c("PassengerId","Survived")

#write.csv(results_final, "submission_Logistic_final_model.csv", row.names = FALSE)


Final_formula<- as.Formula(Survived~Pclass+Sex+Age+SibSp+Parch+I(Embarked=='S')+IsChild+ I(Is_Married=="unmarried"))

############# Decision tree v 2.0 ######################################

str(data.train)
tree2<-rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked+IsChild+Is_Married+Fare,data = data.train)
prp(tree2)

pred_tree2<- predict(tree2, newdata = data.test, type = 'class')
confusionMatrix(pred_tree2, data.test$Survived)

##############  Random forest ############################################

#---- Using for loop to get the score matrix
#-----for 100 trees and and 9 mtry to find appropriate parameters -----

scores<- matrix(nrow = 100, ncol = 9)
for (i in 1:9) {
for (j in 1:100) {
  set.seed(24)
  forest1<- randomForest(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked+IsChild+Is_Married+Fare, data = data.train, ntree = j, mtry = i)
  pred_forest<-predict(forest1,newdata = data.test, type = 'class')
  acc<- accuracy(data.test$Survived, pred_forest)
  scores[j,i]<- acc
  scores
  }  
}

max(scores) # this is the max score given by the random forest

which(scores==max(scores),arr.ind = TRUE) # the row  gives the no of trees and col give the no of mtry



#We are using 43 trees with 1 mtry as we get the max accuracy on the test set
scores[43,1]

############ BUlding random forest on full training data set ########################

set.seed(24)
final_forest<- randomForest(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked+IsChild+Is_Married+Fare, data = Titanic.train, ntree = 43, mtry = 1)
plot(final_forest)

pred_final_forest<-predict(final_forest,newdata = Titanic.train, type = 'class')
confusionMatrix(pred_final_forest, Titanic.train$Survived) # ACCuracy on full training set doesnt change so much hence the model doesnt seem to be overfitting

############ PRedicting the results of the final test data set ##############

pred_final_test<- predict(final_forest, newdata = Titanic.test, type = 'class')

results_final_forest<-data.frame(Titanic.test1$PassengerId, pred_final_test)
results_final_forest
colnames(results_final_forest)<- c("PassengerId","Survived")

#------saving the output on disk
#write.csv(results_final_forest, "submission_forest_model.csv", row.names = FALSE)




################# Neural network ###############################################

library(dummies)
colnames(Titanic.train)
Titanic.train_dum<-dummy.data.frame(Titanic.train,names = c("Pclass","Sex","IsChild", "Is_Married", "Is_Alone","Embarked"),sep="_" , fun = as.factor)
str(Titanic.train_dum)
colnames(Titanic.train_dum)
Titanic.train_dum<- Titanic.train_dum[,-c(2,6,11,14,16,19)]


Titanic.test_dum<-dummy.data.frame(Titanic.test,names = c("Pclass","Sex","IsChild", "Is_Married", "Is_Alone","Embarked"),sep="_" , fun = as.factor)
str(Titanic.test_dum)


Titanic.test_dum<- subset(Titanic.test_dum, select = colnames(Titanic.train_dum[,-1]))
str(Titanic.train_dum)
str(Titanic.test_dum)






