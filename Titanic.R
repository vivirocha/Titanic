################################################################################
###                                                                          ###
###                           Base de dados TITANIC                          ###
###                              Random Forest                               ###
################################################################################

install.packages("randomForest")
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

train$Survived<- NULL

train$Istrain <- TRUE
test$Istrain <- FALSE

df <- rbind(train, test)


#Excluindo colunas desnecessárias
df$Name <- NULL
df$Cabin <- NULL
df$Ticket <- NULL
df$PassengerId <- NULL

#Transformando os dados de sexo em binário.
df$Sex[df$Sex=="male"] <- 0
df$Sex[df$Sex=="female"] <- 1


#Verificando se tem valores vazios no dataset
colSums(df == "")
unique(df$Embarked)
median(df$Embarked)
df$Embarked[df$Embarked==""] <- "S"

#Criando variáveis dummy para Embarked
df$EmbarkedS = as.numeric(df$Embarked == "S")
df$EmbarkedC = as.numeric(df$Embarked == "C")
df$EmbarkedQ = as.numeric(df$Embarked == "Q")
df$Embarked <- NULL
colSums(is.na(df)) 

# Preenchendo missing values de Fare com KNN

df <- kNN(df, variable = "Fare", k = 8)

df$Fare_imp <- NULL

colSums(is.na(df))


#Transformando as variáveis em fator
df$Age <- as.factor(df$Age)
df$Pclass <- as.numeric(df$Pclass)
df$SibSp <- as.factor(df$SibSp)
df$Parch <- as.factor(df$Parch)
df$EmbarkedS <- as.factor(df$EmbarkedS)
df$EmbarkedC <- as.factor(df$EmbarkedC)
df$EmbarkedQ <- as.factor(df$EmbarkedQ)

#escalonamento de Fare e Pclass
df$Fare <- scale(df$Fare)
df$Pclass <- scale(df$Pclass)

#Alterando a posição da coluna Age
df <- df %>%
  relocate(Age, .after = EmbarkedQ)

  
#Criando a equação e fórmula para rodar no randomForest
equacaoAge <- "Age ~ Pclass + Sex + SibSp + Parch + Fare + EmbarkedS + EmbarkedC + EmbarkedQ"
formulaAge <- as.formula(equacaoAge)


#Dividindo os dados entre treinamento e teste


set.seed(1523)
dftreinoAge <- df[!is.na(df$Age), ]
dftesteAge <- df[is.na(df$Age), ]

dftreinoAge$Age <- as.factor(dftreinoAge$Age)


#Criando o modelo para previsão de Age

modeloAge <- randomForest(formula = formulaAge, 
                       data = dftreinoAge,
                       ntree = 80,
                       mtry = 3,
                       importance = TRUE,
                       nodesize = 0.01 * nrow(dftesteAge))

modeloAge <- randomForest(x = dftreinoAge[-10], y = dftreinoAge$Age, ntree = 10)

modeloAge

plot(modeloAge)

#Fazendo a previsão do modelo Age
previsaoAge <- predict(modeloAge, newdata = dftesteAge[-10])

dftesteAge$Age <- as.integer(previsaoAge)

dfAge <- rbind(dftesteAge, dftreinoAge)

#############################################################################################################
#
#
#
df_Treino <- read.csv("train.csv")
df_Teste <- read.csv("test.csv")

df_Teste$Survived <- NA

df_Treino$Istrain <- TRUE
df_Teste$Istrain <- FALSE

dfT <- rbind(df_T1,df_T2)
dfT$Survived <- as.factor(dfT$Survived)

#Transformando os dados de sexo em binário.
dfT$Sex[dfT$Sex=="male"] <- 0
dfT$Sex[dfT$Sex=="female"] <- 1

#Verificando se tem valores vazios no dataset
colSums(dfT == "")
unique(dfT$Embarked)
median(dfT$Embarked)
dfT$Embarked[dfT$Embarked==""] <- "S"

#Criando variáveis dummy para Embarked
dfT$EmbarkedS = as.numeric(dfT$Embarked == "S")
dfT$EmbarkedC = as.numeric(dfT$Embarked == "C")
dfT$EmbarkedQ = as.numeric(dfT$Embarked == "Q")

#Deletando a variável Embarked original 
dfT$Embarked <- NULL

colSums(is.na(dfT)) #Podemos ver 418 NA's em Survived (que nós inserimos), 263 em Age e 1 em Fare.Iremos tratar estes dados faltantes.

# Preenchendo missing values de Fare com KNN

dfT <- kNN(dfT, variable = "Fare", k = 5)

dfT$Fare_imp <- NULL

colSums(is.na(dfT))

dfT$Age[which(is.na(dfT$Age))] <- dftesteAge$Age


dfT <- dfT %>%
  relocate(Survived, .after = EmbarkedQ)

dfT$PassengerId <- NULL
dfT$Name <- NULL
dfT$Ticket <- NULL
dfT$Cabin <- NULL


#Dividindo os dados entre treinamento e teste TITANIC

dfT_train <- dfT[dfT$Istrain == TRUE,]
dfT_test <- dfT[dfT$Istrain == FALSE,]

#Criando a equação e fórmula para rodar no randomForest

equacaoT <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + EmbarkedS + EmbarkedC + EmbarkedQ"

formulatitanic <- as.formula(equacaoT)

formulatitanic

library(caret)

#Criando o modelo randomForest


set.seed(10)
modelo <- randomForest(formula = formulatitanic, 
                       data = dfT_train,
                       ntree = 180,
                       mtry = 3,
                       importance = TRUE)

modelo

plot(modelo)


#Gerando matriz de importância
importancia <- importance(modelo, type = 1)

importancia


#Fazendo a previsão do modelo

previsao <- predict(modelo, newdata = dfT_test)

submission <- data.frame(PassengerId = 892:1309,
                         Survived = previsao)


#Finalizando o trabalho

write.csv(submission, file = "submission.csv", row.names = FALSE)
