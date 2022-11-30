################################################################################
###                                                                          ###
###                           Base de dados TITANIC                          ###
###                                Naive Bayes                               ###
################################################################################

install.packages("randomForest")
library(e1071)
library(dplyr)
library(caret)

train <- read.csv("train.csv")
test <- read.csv("test.csv")


#Descobrindo o nome das variáveis
colnames(train)
colnames(test)

#Apresentando um resumo dos dados
summary(train)
summary(test)

#Criando a coluna "faltante" no dataset test
test$Survived<- NA

#Identificando se os dados são de treino ou de teste, para isso criaremos outra coluna, desta forma os dois datasetes terão 13 colunas.
train$Istrain <- TRUE
test$Istrain <- FALSE

#Unindo os dois datasets que conterão as 1309 observações.
df <- rbind(train, test)

#Apresentando o tipo de dados.
str(df)

#Transformando os dados de sexo em binário.
df$Sex[df$Sex=="male"] <- 0
df$Sex[df$Sex=="female"] <- 1

#Transformando os dados da variável Sex em inteiro.
df$Sex <- as.integer(df$Sex)
str(df)

#verificando se existe valores nulos no nosso banco de dados - Como já tínhamos visto ao usar o summary.
sum(is.null(df))
sum(is.na(df))

#Verificando se tem valores vazios no dataset e preencheno=do com a mediana do Embarked
colSums(df == "")
unique(df$Embarked)
median(df$Embarked)
df$Embarked[df$Embarked==""] <- "S"

colSums(is.na(df)) #Podemos ver 418 NA's em Survived (que nós inserimos), 263 em Age e 1 em Fare.Iremos tratar estes dados faltantes.

#Inserindo a média das idades nos dados faltantes de Age
mean(df$Age, na.rm = TRUE)
df$Age = ifelse(is.na(df$Age), mean(df$Age, na.rm = TRUE), df$Age)

#Inserindo a moda nos dados faltantes de Fare, como é apenas um dado faltante, irei utilizar a frequência para completar este dado.
frequency(df$Fare, na.rm = TRUE)
df$Fare = ifelse(is.na(df$Fare), frequency(df$Fare, na.rm = TRUE), df$Fare)

summary(df)

#Transformando as variáveis em fator
str(df)
df$Survived <- as.factor(df$Survived)
df$Pclass <- as.factor(df$Pclass)
df$Sex <- as.factor(df$Sex)
df$SibSp <- as.factor(df$SibSp)
df$Parch <- as.factor(df$Parch)
df$Embarked <- as.factor(df$Embarked)
df$Parch <- as.factor(df$Parch)
str(df)

#Dividindo os dados entre treinamento e teste

df_train <- df[df$Istrain == TRUE,]
df_test <- df[df$Istrain == FALSE,]

df_train$Istrain <- NULL
df_test$Istrain <- NULL
#df_test$Survived <- NULL
View(df_test)

#vamos ver se meu pensamento está correto...para fazer o NBAYES, 
#eu preciso deletar as colunas criadas pq não faz sentido mantê-las 
#e o que eu faço com a coluna da previsão?

#alterando a posição da coluna "Classe" - Survived
df_train <- df_train %>% relocate(Survived, .after = everything())
View(df_train)

classificador <- naiveBayes(x = df_train [- 12], y = df_train$Survived) 
classificador

df_test <- df_test %>% relocate(Survived, .after = everything())
View(df_test)

previsao = predict(classificador, newdata = df_test [-12])
matriz_confusao = table(df_test[,12], previsao)
matriz_confusao

