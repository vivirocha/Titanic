library(pacman)

pacman::p_load(dplyr, caret, tidyverse, rpart, randomForest, VIM)

treino <- read.csv('train.csv')
teste <- read.csv('test.csv')

colnames(treino)
colnames(teste)

teste$Survived <- NA
treino$Istrain <- TRUE
teste$Istrain <- FALSE

df <- rbind(treino, teste)

colSums(is.na(df))

prop.table(table(treino$Survived))

table(treino$Sex, treino$Survived )

prop.table(table(treino$Sex, treino$Survived),margin = 1)


#Alteração de Sex para numérico
df$Sex[df$Sex=='female'] <- 1
df$Sex[df$Sex=='male'] <- 0


#Alteração de Embarked para numérico
df$Embarked[df$Embarked=='S'] <- 0
df$Embarked[df$Embarked=='C'] <- 1
df$Embarked[df$Embarked=='Q'] <- 2
df$Embarked[df$Embarked==""] <- 0

#Completando Fare com a média
df$Fare <- as.numeric(df$Fare)
mean(!is.na(df$Fare))

df$Fare[is.na(df$Fare)]<-mean(df$Fare,na.rm=TRUE)

df$Fare <- round(df$Fare)

#Tratando Cabin
colSums(is.na(df))

df$CabinLetter <- substr(df$Cabin, 1, 1)
unique(df$CabinLetter)

df$CabinNumber <- substr(df$Cabin, 2, 4)
unique(df$CabinNumber)

df$CabinLetter[df$CabinLetter=='A'] <- 0
df$CabinLetter[df$CabinLetter=='B'] <- 1
df$CabinLetter[df$CabinLetter=='C'] <- 2
df$CabinLetter[df$CabinLetter=='D'] <- 3
df$CabinLetter[df$CabinLetter=='E'] <- 4
df$CabinLetter[df$CabinLetter=='F'] <- 5
df$CabinLetter[df$CabinLetter=='G'] <- 6
df$CabinLetter[df$CabinLetter=='T'] <- 7

df$CabinLetter <- as.numeric(df$CabinLetter)

summary(df$CabinNumber)
unique(df$CabinNumber)
df$CabinNumber <- as.numeric(df$CabinNumber)

df <- kNN(df, variable = "CabinLetter", k = 5)

df$CabinLetter_imp <- NULL

df <- kNN(df, variable = "CabinNumber", k = 5)

df$CabinNumber_imp <- NULL

colSums(is.na(df))

df <- kNN(df, variable = "Age", k = 2)

df$Age_imp <- NULL

colSums(is.na(df))
#Prevendo as idades faltantes
previsaoidade <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                       data = df[!is.na(df$Age),], method = "anova")

df$Age[is.na(df$Age)] <- predict(previsaoidade, df[is.na(df$Age),])

#Arredondamento idade
df$Age <- round(df$Age)

### Tratamento nome
df$Titulo1 <- strsplit((df$Name), " ")
df$Titulo <- sapply(df$Titulo1, "[",2)

unique(df$Titulo)

df %>% filter(str_detect(Titulo, "Brito,"))

df$Titulo[df$PassengerId==19] <- "Mrs."
df$Titulo[df$PassengerId==39] <- "Miss."
df$Titulo[df$PassengerId==334] <- "Mr."
df$Titulo[df$PassengerId==1037] <- "Mr."

df$Titulo[df$PassengerId==154] <- "Mr."
df$Titulo[df$PassengerId==1084] <- "Master."
df$Titulo[df$PassengerId==1236] <- "Master."

df$Titulo[df$PassengerId==171] <- "Mr."
df$Titulo[df$PassengerId==287] <- "Mr."

df$Titulo[df$PassengerId==283] <- "Mr."

df$Titulo[df$PassengerId==308] <- "Mrs."
df$Titulo[df$PassengerId==506] <- "Mr."
df$Titulo[df$PassengerId==548] <- "Mr."
df$Titulo[df$PassengerId==867] <- "Miss."
df$Titulo[df$PassengerId==874] <- "Mr."
df$Titulo[df$PassengerId==965] <- "Mr."
df$Titulo[df$PassengerId==1112] <- "Miss."
df$Titulo[df$PassengerId==1261] <- "Mr."
df$Titulo[df$PassengerId==1306] <- "Dona."

df$Titulo[df$PassengerId==356] <- "Mr."

df$Titulo[df$PassengerId==362] <- "Mr."
df$Titulo[df$PassengerId==907] <- "Mrs."

df$Titulo[df$PassengerId==420] <- "Miss."
df$Titulo[df$PassengerId==596] <- "Mr."
df$Titulo[df$PassengerId==800] <- "Mrs."

df$Titulo[df$PassengerId==201] <- "Mr."

df$Titulo[df$PassengerId==557] <- "Lady."
df$Titulo[df$PassengerId==600] <- "Sir."

df$Titulo[df$PassengerId==753] <- "Mr."

df$Titulo[df$PassengerId==560] <- "Mrs."
df$Titulo[df$PassengerId==1152] <- "Mr."

df$Titulo[df$PassengerId==760] <- "Countess."

df$Titulo[df$PassengerId==799] <- "Mr."

df$Titulo[df$PassengerId==823] <- "Mr."

df$Titulo[df$PassengerId==869] <- "Mr."

df$Titulo[df$PassengerId==911] <- "Mrs."

df$Titulo[df$PassengerId==995] <- "Mr."

df$Titulo[df$PassengerId==1228] <- "Mr."

df$Titulo [df$Titulo =='Mr.'] <- 0
df$Titulo [df$Titulo =='Mrs.'] <- 1
df$Titulo [df$Titulo =='Miss.'] <- 2
df$Titulo [df$Titulo =='Master.'] <- 3
df$Titulo [df$Titulo =='Dr.'] <- 4
df$Titulo [df$Titulo =='Capt.'] <- 5
df$Titulo [df$Titulo =='Ms.'] <- 6
df$Titulo [df$Titulo =='Don.'] <- 7
df$Titulo [df$Titulo =='Major.'] <- 8
df$Titulo [df$Titulo =='Rev.'] <- 9
df$Titulo [df$Titulo =='Col.'] <- 10
df$Titulo [df$Titulo =='Mme.'] <- 11
df$Titulo [df$Titulo =='Mlle.'] <- 12
df$Titulo [df$Titulo =='Lady.'] <- 13
df$Titulo [df$Titulo =='Sir.'] <- 14
df$Titulo [df$Titulo =='Countess.'] <- 15
df$Titulo [df$Titulo =='Dona.'] <- 16

unique(df$Titulo)

df$Name <- NULL
df$PCID <- NULL
df$Cabin <- NULL

#Separado os dados
treino2 <- df[1:891,]
teste2 <- df[892:1309,]

#Criando o modelo
modelo <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
                data = treino2, 
                method = "class")
previsaomodelo <- predict(modelo, newdata = teste2, type = "class")

submission <- data.frame(PassengerId = 892:1309,
                         Survived = previsaomodelo)


#Finalizando o trabalho

write.csv(submission, file = "submission.csv", row.names = FALSE)

