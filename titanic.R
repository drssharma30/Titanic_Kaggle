setwd("C:/Users/ssharma/Desktop/Kaggle/Titanic")
read.csv(file = "train.csv",stringsAsFactors = FALSE, header = TRUE)
titanic.train<- read.csv(file = "train.csv",stringsAsFactors = FALSE, header = TRUE)
titanic.test<- read.csv(file = "test.csv",stringsAsFactors = FALSE, header = TRUE)

titanic.train$Istrainset<- TRUE
titanic.test$Istrainset<- FALSE
titanic.test$Survived <- NA
titanic.full <- rbind(titanic.train,titanic.test)
titanic.full[titanic.full$Embarked=='',"Embarked"]<-'S'
is.na(titanic.full$Age)
table(is.na(titanic.full$Age))
Age.median<- median(titanic.full$Age,na.rm = TRUE)
titanic.full[is.na(titanic.full$Age),"Age"]<- Age.median

Fare.median<- median(titanic.full$Fare,na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare),"Fare"] <- Fare.median




