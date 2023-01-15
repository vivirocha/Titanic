################################################################################
###                                                                          ###
###                           Base de dados TITANIC                          ###
###                              Random Forest                               ###
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
df$Sex <- as.factor(df$Sex)
str(df)

#verificando se existe valores nulos no nosso banco de dados - Como já tínhamos visto ao usar o summary.
sum(is.null(df))
sum(is.na(df))

#Verificando se tem valores vazios no dataset
colSums(df == "")
unique(df$Embarked)
median(df$Embarked)
df$Embarked[df$Embarked==""] <- "S"

#Criando variáveis dummy para Embarked
df$EmbarkedS = as.numeric(df$Embarked == "S")
df$EmbarkedC = as.numeric(df$Embarked == "C")
df$EmbarkedQ = as.numeric(df$Embarked == "Q")

#Deletando a variável Embarked original 
df$Embarked <- NULL

colSums(is.na(df)) #Podemos ver 418 NA's em Survived (que nós inserimos), 263 em Age e 1 em Fare.Iremos tratar estes dados faltantes.

#Inserindo a moda nos dados faltantes de Fare, como é apenas um dado faltante, irei utilizar a frequência para completar este dado.
frequency(df$Fare, na.rm = TRUE)
df$Fare = ifelse(is.na(df$Fare), frequency(df$Fare, na.rm = TRUE), df$Fare)

summary(df)

#Transformando as variáveis em fator
str(df)
df$Survived <- as.factor(df$Survived)
df$Pclass <- as.factor(df$Pclass)
df$SibSp <- as.factor(df$SibSp)
df$Parch <- as.factor(df$Parch)
df$EmbarkedS <- as.factor(df$EmbarkedS)
df$EmbarkedC <- as.factor(df$EmbarkedC)
df$EmbarkedQ <- as.factor(df$EmbarkedQ)
str(df)

#Excluindo colunas desnecessárias
df$Name <- NULL
df$Cabin <- NULL

#Alterando a posição da coluna Survived
library(dplyr)
df <- df %>%
  relocate(Survived, .after = EmbarkedQ)

View(df)

# Preenchendo missing values com KNN

install.packages("VIM")
library(VIM)

df <- kNN(df, variable = "Age", k = 5)

View(df)

df$Age_imp <- NULL

colSums(is.na(df))


#Verificando a existência outliers
summary(df)
boxplot(df$SibSp, outline = TRUE)
boxplot.stats(df$SibSp)

boxplot(df$Age, outline = TRUE)

min(df$Age)
max(df$Age)

boxplot.stats(df$Age)

?boxplot.stats

boxplot(df$Parch, outline = TRUE)
boxplot(df$Fare, outline = TRUE)

outlier(df)


#Dividindo os dados entre treinamento e teste

df_train <- df[df$Istrain == TRUE,]
df_test <- df[df$Istrain == FALSE,]

#Criando a equação e fórmula para rodar no randomForest

equacao <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + EmbarkedS + EmbarkedC + EmbarkedQ"

formulatitanic <- as.formula(equacao)

formulatitanic

library(caret)

#Criando o modelo randomForest

set.seed(10)
modelo <- randomForest(formula = formulatitanic, 
                       data = df_train,
                       ntree = 180,
                       mtry = 3,
                       importance = TRUE,
                       nodesize = 0.01 * nrow(df_test)
)

modelo

plot(modelo)

#Fazendo a previsão do modelo
previsao <- predict(modelo, newdata = df_test[-13])

#Gerando matriz de importância
importancia <- importance(modelo, type = 1)

importancia


#Avaliando o modelo
matriz <- table(df_test[,13], previsao)
confusionMatrix(matriz)

#Finalizando o trabalho
PassengerId <- df_test$PassengerId
output <- as.data.frame(PassengerId)
output$Survived <- previsao

write.csv(output, file = "submission.csv", row.names = FALSE)
