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

df <- rbind(train, test)

#Excluindo colunas desnecessárias
df$Name <- NULL
df$Ticket <- NULL
df$Cabin <- NULL

colSums(is.na(df))

df$Sex[df$Sex=="male"] <- 0
df$Sex[df$Sex=="female"] <- 1

# Preenchendo missing values de Fare com KNN

df <- kNN(df, variable = "Fare", k = 3)

df$Fare_imp <- NULL

colSums(is.na(df))

df <- kNN(df, variable = "Age", k = 2)

df$Age_imp <- NULL

colSums(is.na(df))

df$Embarked[df$Embarked=="S"] <- 1
df$Embarked[df$Embarked=="Q"] <- 2
df$Embarked[df$Embarked=="C"] <- 3

df$Embarked[which(is.na(df$Embarked))] <- mean(!is.na(df$Embarked))

df$Embarked <- is.numeric(df$Embarked)

unique(df$Embarked)

colSums(is.na(df))

#escalonamento de Fare e Pclass
df$Fare <- scale(df$Fare)
df$PassengerId <- scale(df$PassengerId)
df$Pclass <- scale(df$Pclass)
df$Age <- scale(df$Age)
df$SibSp <- scale(df$SibSp)
df$Parch <- scale(df$Parch)
df$Sex <- as.numeric(df$Sex)
df$Sex <- scale(df$Sex)
df$Embarked <- as.numeric(df$Embarked)
df$Embarked <- scale(df$Embarked)


#Alterando a posição da coluna Age
df <- df %>%
  relocate(Survived, .after = Istrain)

colnames(df)
#Criando a equação e fórmula para rodar no randomForest
equacao <- "Survived ~ Pclass + Sex + Age+ SibSp + Parch + Fare + Embarked"
formula <- as.formula(equacao)

df$Survived <- as.factor(df$Survived)

df_train <- df[df$Istrain == TRUE,]
df_test <- df[df$Istrain == FALSE,]

set.seed(10)
modelo <- randomForest(formula = formula, 
                       data = df_train,
                       ntree = 175,
                       mtry = 3,
                       importance = TRUE,
                       nodesize = 0.01 * nrow(df_test))

modelo

plot(modelo)


#Gerando matriz de importância
importancia <- importance(modelo, type = 1)

importancia


#Fazendo a previsão do modelo

previsao <- predict(modelo, newdata = df_test)

submission <- data.frame(PassengerId = 892:1309,
                         Survived = previsao)


#Finalizando o trabalho

write.csv(submission, file = "submission.csv", row.names = FALSE)
