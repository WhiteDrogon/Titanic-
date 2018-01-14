getwd()
setwd("/home/hemanth/Documents/titanic")
train <- read.csv("train (1).csv")
test <- read.csv("test (1).csv")
head(train)
str(train)

table(train$Sex,train$Survived)
prop.table(table(train$Sex,train$Survived),1)
test$Survived <- 0
test$Survived[test$Sex == "female"] <-1
#almost all the female passengers survived the incident 

test$child[test$Age < 18] <- 1
test$child[test$Age >=18] <- 0
train$child[train$Age < 18] <- 1
train$child[train$Age >= 18] <- 0

prop.table(table(train$child,train$Survived),1)
summary(train)
summary(test)
#aggregate function to include more than one two variables to display in the table 
aggregate(Survived~child+Sex+Survived,data=train,FUN=function(x){ sum(x)/length(x)})

train$tick_fare[train$Fare < 10] <- '<10'
train$tick_fare[train$Fare <20 & train$Fare>=10] <- '10-20'
train$tick_fare[train$Fare <30 & train$Fare>=20] <- '20-30'
train$tick_fare[train$Fare >30] <- '>30'

test$tick_fare[test$Fare < 10] <- '<10'
test$tick_fare[test$Fare <20 & test$Fare>=10] <- '10-20'
test$tick_fare[test$Fare <30 & test$Fare>=20] <- '20-30'
test$tick_fare[test$Fare >30] <- '>30'

aggregate(Survived~Pclass+tick_fare+Sex,data = train,FUN = function(x){sum(x)/length(x)})
#females beloning to 3rd class and having ticket fare greater than 20 have more deaths
#males have very less survival percentage irrespective of their class or ticket fare
test$Survived[test$Fare >=20 &test$Age == "female" & test$Pclass == 3] <- 0

#using decsicion trees
library(rpart)
fit <- rpart(Survived~Age+Sex+Fare+SibSp+Parch+Pclass,data = train,method = "class")
plot(fit)
text(fit)
#depict using fancyrplot
library(rattle)
library(RColorBrewer)
library(rpart.plot)
fancyRpartPlot(fit)


#feature engineering 
train$Name[1]
test$Survived <- NA
combination <- rbind(train,test)
combination$Name <- as.character(combination$Name)
combination$Name[1]
strsplit(combination$Name[1],split='[,.]')
strsplit(combination$Name[1],split='[,.]')[[1]][[2]]
combination$title <- sapply(combination$Name , FUN = function(x){strsplit(x,split='[,.]')[[1]][[2]]})
combination$title <- sub(' ','',combination$title)
table(combination$title)
combination$title[combination$title %in% c("Mlle","Mme")] <- "Mlle"
combination$title[combination$title %in% c("Capt","Major","Don","Sir")] <- "Sir"
combination$title[combination$title %in% c("Dona","Jonkheer","Lady","the Countess")] <- "Lady"

#calculate the size of the family 
combination$familysize <- combination$SibSp + combination$Parch +1
combination$Surname <- sapply(combination$Name , FUN = function(x){strsplit(x,split='[,.]'[[1]][[1]])})
combination$familyID <- paste(as.character(combination$familysize),combination$Surname,sep="")
famIDs <- data.frame(table(combination$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
train <- combination[1:891,]
test <- combination[892:1309,]
summary(combination$Age)
fit <- rpart(Age~Pclass+Fare+Sex+Embarked+Parch+SibSp+title+familysize,data = combination[!is.na(combination$Age),],method = "anova")
combination$Age[is.na(combination$Age)] <- predict(fit,combination[is.na(combination$Age),])
summary(combination)

summary(combination$Embarked)
which(combination$Embarked == '')
combination$Embarked[c(62,830)] <- "S"
summary(combination$Embarked)
train <- combination[1:891,]
test <- combination[892:1309,]
combination$Embarked <- as.factor(combination$Embarked)
summary(combination$Fare)
which(is.na(combination$Fare))
combination$Fare[1044] <- median(combination$Fare,na.rm=TRUE)      
combination$Fare[1044]
combination$familyID
str(combination$familyID)
install.packages("randomForest")
library(randomForest)
set.seed(400)
library(dplyr)
train <- train%>%mutate_if(is.character,as.factor)
test <- test%>% mutate_if(is.character,as.factor)
model <- randomForest(Survived~Age+Sex+Parch+SibSp+familysize+Fare+Pclass+Embarked+title,data=train,importance=TRUE,ntree=2000)
varImpPlot(model)
prediction <- predict(model,test)
