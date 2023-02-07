################################################################################
###                                                                          ###
###                           Base de dados TITANIC                          ###
###                              Random Forest                               ###
################################################################################

library(randomForest)
library(dplyr)
library(caTools)
library(caret)
library(dplyr)
library(VIM)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

colnames(train)
colnames(test)

summary(train)
summary(test)

train$Istrain <- TRUE
test$Istrain <- FALSE
test$Survived <- NA

df <- rbind(train, test)


#Excluindo colunas desnecessárias
df$Name <- NULL
df$Cabin <- NULL
df$Ticket <- NULL
df$PassengerId <- NULL


#Transformando os dados de sexo em binário.
df$Sex[df$Sex=="male"] <- 0
df$Sex[df$Sex=="female"] <- 1

df$Sex <- as.numeric(df$Sex)

#Verificando se tem valores vazios no dataset
colSums(df == "")
unique(df$Embarked)
median(df$Embarked)
df$Embarked[df$Embarked==""] <- "S"


# Mediana para Fare

df$Fare[which(is.na(df$Fare))] <- median(!is.na(df$Fare))

colSums(is.na(df))

# Média para Age

df$Age[which(is.na(df$Age))] <- mean(!is.na(df$Age))

colSums(is.na(df))

str(df)


#############################################################################################################


df <- df %>%
  relocate(Survived, .after = Embarked)

df$Survived <- as.factor(df$Survived)

df_train <- df[df$Istrain == TRUE,]
df_test <- df[df$Istrain == FALSE,]

#Criando a equação e fórmula para rodar no randomForest

formula <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

formulatitanic <- as.formula(formula)

formulatitanic

library(caret)

set.seed(10)
modelo <- randomForest(formula = formulatitanic, 
                       data = df_train,
                       ntree = 180,
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
