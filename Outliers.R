library(randomForest)
library(dplyr)
library(caTools)
library(caret)
library(dplyr)
library(VIM)

train <- read.csv("train.csv")
test <- read.csv("test.csv")


train$Istrain <- TRUE
test$Istrain <- FALSE
test$Survived <- NA



############################################################################### TRATANDO TRAIN

head(train)

colSums(is.na(train))

colnames(train)

######################################################## Outlier Age

boxplot(train$Age)
unique(boxplot.stats(train$Age)$out)
summary(train$Age)
hist(train$Age)

trainAge <- train %>% filter(train$Age < 66.0)

mean(trainAge$Age)

train$Age[train$Age==66.0] <- 29.22475
train$Age[train$Age==71.0] <- 29.22475
train$Age[train$Age==70.5] <- 29.22475
train$Age[train$Age==80.0] <- 29.22475
train$Age[train$Age==70.0] <- 29.22475
train$Age[train$Age==74.0] <- 29.22475

######################################################## Outlier SibSP
boxplot(train$SibSp)
unique(boxplot.stats(train$SibSp)$out)
summary(trainSibSp$SibSp)
hist(train$SibSp)

unique(boxplot.stats(train$SibSp)$out)

trainSibSp <- train %>% filter(train$SibSp < 5)

mean(trainSibSp$SibSp)

train$SibSp[train$SibSp==8] <- 0.4379977

####################################################### Outlier Parch
boxplot(train$Parch)
unique(boxplot.stats(train$Parch)$out)
summary(train$Parch)
hist(train$Parch)

trainParch <- train %>% filter(train$Parch <=3)

mean(trainParch$Parch)

train$Parch[train$Parch>3] <- 0.3325766


####################################################### Outlier Fare

boxplot(train$Fare)
unique(boxplot.stats(train$Fare)$out)
summary(train$Fare)
hist(train$Fare)

trainFare <- train %>% filter(train$Fare <200)

mean(trainFare$Fare)

train$Fare[train$Fare>200] <- 26.54408

############################################################################### TRATANDO TESTE

head(test)

colSums(is.na(test))

colnames(test)

####################################################### Test Age

boxplot(test$Age)
unique(boxplot.stats(test$Age)$out)
summary(test$Age)
hist(test$Age)

testAge <- test %>% filter(test$Age < 67)

mean(testAge$Age)

test$Age[test$Age>67] <- 30.02273

####################################################### Test Parch
boxplot(test$Parch)
unique(boxplot.stats(test$Parch)$out)
summary(test$Parch)
hist(test$Parch)

testParch <- test %>% filter(test$Parch <=2)

mean(testParch$Parch)

test$Parch[test$Parch>2] <- 0.2888725


####################################################### Test Fare

boxplot(test$Fare)
unique(boxplot.stats(test$Fare)$out)
summary(test$Fare)
hist(test$Fare)

testFare <- test%>% filter(test$Fare <300)

mean(testFare$Fare)

test$Fare[test$Fare>300] <- 34.48127



############################################################################### DATASET INTEIRO

df <- rbind(train, test)

#Excluindo colunas desnecessárias
df$Name <- NULL
df$Ticket <- NULL
df$Cabin <- NULL
df$PassengerId <- NULL

colSums(is.na(df))

df$Sex[df$Sex=="male"] <- 0
df$Sex[df$Sex=="female"] <- 1

summary(df$Fare)
dfFare <- df%>% filter(df$Fare >= 0)
mean(dfFare$Fare)

df$Fare[which(is.na(df$Fare))] <- 29.07451

colSums(is.na(df))

df <- kNN(df, variable = "Age", k = 70)

df$Age_imp <- NULL

df$Embarked[df$Embarked=="S"] <- 1
df$Embarked[df$Embarked=="Q"] <- 2
df$Embarked[df$Embarked=="C"] <- 3
df$Embarked[df$Embarked==""] <- 1

unique(df$Embarked)

colSums(is.na(df))

#escalonamento de Fare e Pclass
df$Fare <- scale(df$Fare)
df$Pclass <- scale(df$Pclass)
df$Age <- scale(df$Age)
df$SibSp <- scale(df$SibSp)
df$Sex <- as.numeric(df$Sex)
df$Sex <- scale(df$Sex)
df$Embarked <- as.numeric(df$Embarked)
df$Embarked <- scale(df$Embarked)


#Alterando a posição da coluna Age
df <- df %>%
  relocate(Survived, .after = Istrain)

colSums(is.na(df))
#Criando a equação e fórmula para rodar no randomForest
#equacao <- "Survived ~ Pclass + Sex + Age + SibSp + Fare + Embarked"
#formula <- as.formula(equacao)

df$Survived <- as.factor(df$Survived)

####################### Cross validation

df_train <- df[df$Istrain == TRUE,]
df_test <- df[df$Istrain == FALSE,]

df$Istrain <- NULL


set.seed(10)
modelo1 <- randomForest(x = df_train[1:7], 
                        y = df_train$Survived, 
                        ntree = 120, 
                        na.action=na.omit,
                        n_jobs = -1,
                        maxnodes = 110
                        )

modelo1

#modelo

plot(modelo1)


#Fazendo a previsão do modelo

previsao <- predict(modelo, newdata = df_test)





library(klaR)

controle <- trainControl(method = 'cv',  number = 10)

modelocc <- train(Survived ~., data = df_train, trControl = controle, method = 'rf')

modelocc





#Gerando matriz de importância
importancia <- importance(modelo1, type = 1)

importancia




submission <- data.frame(PassengerId = 892:1309,
                         Survived = previsao)


#Finalizando o trabalho

write.csv(submission, file = "submission.csv", row.names = FALSE)
