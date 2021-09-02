setwd("C:/Users/ssharma/Desktop/Kaggle/Titanic")
titanic.train<- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test<- read.csv(file = "test.csv",stringsAsFactors = FALSE, header = TRUE)



#Creating a new coulmn in train and test set to identify them before combining these data sets. 
titanic.train$IsTrainSet<- TRUE
titanic.test$IsTrainSet<- FALSE

# creating a coulmn named survived in test dataset with all the values as NA.
titanic.test$Survived<- NA

#combining train and test datasets
titanic.full<- rbind(titanic.train, titanic.test)

# Cleaning the data
# Cleaning the missing values in "Embarked" 

titanic.full[titanic.full$Embarked=="","Embarked"]<- "S"

# Cleaning the missing values in "Fare"

upper.fixer<- boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter<- titanic.full$Fare < upper.fixer
titanic.full[outlier.filter,]
fare.equation = "Fare ~ Parch + Embarked + Sex + SibSp + Pclass + Age"
fare.model<- lm(
  formula = fare.equation,
  data= titanic.full[outlier.filter,]
  
)

fare.row<- titanic.full[is.na(titanic.full$Fare), c("Parch", "Age", "Pclass", "Sex", "SibSp","Embarked")]

fare.predictions<- predict(fare.model, newdata = fare.row)

titanic.full[is.na(titanic.full$Fare),"Fare"] <- fare.predictions

#Cleaning the missing values in "Age"

titanic.full[is.na(titanic.full$Age),"Age"]
boxplot(titanic.full$Age)
boxplot.stats(titanic.full$Age)$stats[5]
upperwhixer_age<- boxplot.stats(titanic.full$Age)$stats[5]
 outlierfilter_age<- titanic.full$Age< upperwhixer_age
 age.equation<- "Age~ Parch+ Embarked +Sex + SibSp + Pclass + Fare"
lm(formula = age.equation, data= titanic.full[outlierfilter_age,])
age.model<- lm(formula = age.equation, data= titanic.full[outlierfilter_age,])
 age.row <- titanic.full[is.na(titanic.full$Age),c("Parch", "Fare", "Pclass","Sex","SibSp", "Embarked")]
 age.predictions<- predict(age.model, newdata = age.row)
age.predictions
titanic.full[is.na(titanic.full$Age),"Age"]<- age.predictions

# Categorical Casting

summary(titanic.full)
sapply(titanic.full,class)
titanic.full<- transform(titanic.full,
                         Pclass= as.factor(Pclass),
                         Sex=as.factor(Sex),
                         Embarked=as.factor(Embarked)
                         )
# Bringing the data back to Train and Test data

titanic.train<- titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test<- titanic.full[titanic.full$IsTrainSet==FALSE,]

#converting "Survived" to a factor 

titanic.train$Survived<- as.factor(titanic.train$Survived)

#Set a random seed
set.seed(1984)

# Training using "random forest" algorithm,
titanic.model<- train(Survived~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
              data= titanic.train, method= 'rf',
              trControl= trainControl(method = 'cv', number = 5))

titanic.model


#predictions on the test data set

Survived<- predict(titanic.model, newdata = titanic.test)

PassengerId <- titanic.test$PassengerId

output.df <- as.data.frame(PassengerId)

output.df$Survived <- Survived

write.csv(output.df, file = "kaggle_submission.csv", row.names = FALSE)
