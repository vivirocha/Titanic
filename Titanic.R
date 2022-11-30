################################################################################
###                                                                          ###
###                           Base de dados TITANIC                          ###
###                                                                          ###
################################################################################

install.packages("randomForest")
library(randomForest)
library(dplyr)

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

#Verificando se tem valores vazios no dataset
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


#Criando a fórmula para rodar no randomForest

form_df <- as.formula("Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked")

#Criando o modelo randomForest

modelo_df <- randomForest(formula = form_df,
                          data = df_train,
                          ntree = 30,
                          importance = TRUE)

modelo_df

importancia <- importance(modelo_df, type = 1)

importancia

