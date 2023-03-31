################## ANÁLISE E MANIPULAÇÃO TITANIC

library(pacman)

pacman::p_load(dplyr, raster, tmap, plotly, ggplot2, caTools, caret, tidyverse, sf, foreign, caret, randomForest, VIM, mice, ggthemes)

treino <- read.csv('train.csv')
teste <- read.csv('test.csv')

colnames(treino)
colnames(teste)

teste$Survived <- NA
treino$Istrain <- TRUE
teste$Istrain <- FALSE

df <- rbind(treino, teste)

df$PCID <- 1

head(df)

###Legenda das variáveis

#Pclass: 1st = Upper | 2nd = Middle | 3rd = Lower
#Name com título: Mr. | Mrs. | ...
#SibSp: 

options (scipen=999)

#Crianco tabelas para vermos os sobreviventes

table(df$Survived, df$Sex)
#Total de passageiros: 891 - Total de Sobreviventes: 342 | Total de mortos: 549
#Total de mulheres: 314 - Total de Sobreviventes: 233 | Total de mortos: 81
#Total de homens: 577 - Total de Sobreviventes: 109 | Total de mortos: 468

table(df$Survived, df$Pclass, df$Sex)
#Total de mulheres: 314 - Total de Sobreviventes: 233 | Total de mortos: 81
#Total de homens: 577 - Total de Sobreviventes: 109 | Total de mortos: 468

table(df$Survived, df$SibSp)
table(df$Survived, df$Parch)
table(df$Survived, df$Fare)
table(df$Survived, df$Cabin)
table(df$Survived, df$Embarked)

################################################################## GRÁFICO PASSAGEIROS
dfPassageiros <- group_by(df, Sex) %>% 
  summarise(sum(PCID)) 

dfPassageiros$TOTAL <- as.factor(dfPassageiros$TOTAL)

dfPassageiros$TOTAL <- dfPassageiros$`sum(PCID)` 

factor(dfPassageiros$Sex)

GPassageiros <- ggplot(dfPassageiros, aes(x = Sex, y = TOTAL)) +
                geom_bar(stat = "identity", position = "dodge", aes(fill = Sex))+
                geom_label(aes(label = TOTAL))+
                scale_fill_manual(values = c("#f0abc1",
                                             "#94ddde"))+
                                               labs(title="TOTAL PASSAGEIROS",
                                                    x = "Sexo",
                                                    y = "Quantidade de passageiros")+
                theme_hc() +
                theme(legend.position="none")+
                theme(plot.title = element_text(hjust=0.5)+
                theme(axis.text.x = element_text(angle = 0, hjust = 20))+
                theme(legend.position = "bottom"))
GPassageiros
ggplotly(GPassageiros)

################################################################## GRÁFICO PASSAGEIROS - MAIORES DE 18
dfAdults <- filter(df, Age>18)

dfPassAdults <- group_by(dfAdults , Sex, Survived) %>% 
  summarise(sum(PCID))

dfPassAdults$Sex <- factor(dfPassAdults$Sex)

dfPassAdults$TOTAL <- dfPassAdults$`sum(PCID)` 

dfPassAdults$Survived <- factor(dfPassAdults$Survived)

dfPassAdults$TOTAL <- factor(dfPassAdults$TOTAL)

GPassAdults <- ggplot(dfPassAdults, aes( x = Survived, y = TOTAL, fill = Sex, label = TOTAL))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_label(position = position_dodge(width = 1))+
  scale_fill_manual(values = c("#f0abc1",
                                        "#94ddde"))+
                                          labs(title="PASSAGEIROS MAIORES DE 18 ANOS",
                                               x = "Sobreviveu? 0 -> MORREU ou 1 -> SOBREVIVEU",
                                               y = "Quantidade de passageiros")+
  theme_hc() +
  theme(plot.title = element_text(hjust=0.5)+
          theme(axis.text.x = element_text(angle = 0, hjust = 1))+
          theme(legend.position = "bottom"))
GPassAdults

################################################################## GRÁFICO PASSAGEIROS - MENORES DE 18
dfKids <- filter(df, Age<18)

dfPassKids <- group_by(dfKids , Sex, Survived) %>% 
  summarise(sum(PCID))

dfPassKids$Sex <- factor(dfPassKids$Sex)

dfPassKids$TOTAL <- dfPassKids$`sum(PCID)` 

dfPassKids$Survived <- factor(dfPassKids$Survived)

dfPassKids$TOTAL <- factor(dfPassKids$TOTAL)

GPassKids <- ggplot(dfPassKids, aes( x = Survived, y = TOTAL, fill = Sex, label = TOTAL))+
  geom_bar(stat = "identity", position = "dodge")+
    geom_label(position = position_dodge(width = 1))+
  scale_fill_manual(values = c("#f0abc1",
                               "#94ddde"))+
  labs(title="PASSAGEIROS MENORES DE 18 ANOS",
          x = "Sobreviveu? 0 -> MORREU ou 1 -> SOBREVIVEU",
          y = "Quantidade de passageiros")+
  theme_hc() +
  theme(plot.title = element_text(hjust=0.5)+
          theme(axis.text.x = element_text(angle = 0, hjust = 1))+
          theme(legend.position = "bottom"))
GPassKids

################################################################## GRÁFICO CLASSES
unique(df$Pclass)

dfPassPclass <- group_by(df , Sex, Survived, Pclass) %>% 
  summarise(sum(PCID))

dfPassPclass$Sex <- factor(dfPassPclass$Sex)

dfPassPclass$TOTAL <- dfPassPclass$`sum(PCID)` 

dfPassPclass$Survived <- factor(dfPassPclass$Survived)

dfPassPclass$TOTAL <- factor(dfPassPclass$TOTAL)
dfPassPclass$Pclass <- factor(dfPassPclass$Pclass)

GPassPclass <- ggplot(dfPassPclass, aes( x = Survived, y = TOTAL, fill = Sex, label = TOTAL))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~ Pclass)+
  geom_label(position = position_dodge(width = 1))+
  scale_fill_manual(values = c("#f0abc1",
                               "#94ddde"))+
                             labs(title="CLASSES",
                                     x = "1st - 2st - 3st ",
                                     y = "Quantidade de passageiros")+
  theme_hc() +
  theme(plot.title = element_text(hjust=0.5)+
          theme(axis.text.x = element_text(angle = 0, hjust = 1))+
          theme(legend.position = "bottom"))
GPassPclass
ggplotly(GPassPclass)
str(dfPassPclass$Pclass)


################################################################## GRÁFICO PARCH TOTAL

unique(df$Parch)

dfPassParch <- group_by(df , Sex, Survived, Parch) %>% 
  summarise(sum(PCID))

dfPassParch$Sex <- factor(dfPassParch$Sex)

dfPassParch$TOTAL <- dfPassParch$`sum(PCID)` 

dfPassParch$Survived <- factor(dfPassParch$Survived)

dfPassParch$TOTAL <- factor(dfPassParch$TOTAL)
dfPassParch$Parch <- factor(dfPassParch$Parch)

GPassParch <- ggplot(dfPassParch, aes( x = Survived, y = TOTAL, fill = Sex, label = TOTAL))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~ Parch)+
  geom_label(position = position_dodge(width = 1))+
  scale_fill_manual(values = c("#f0abc1",
                                        "#94ddde"))+
                                          labs(title="Nº de pais/filhos a bordo do Titanic",
                                               x = "Nº de pais/filhos a bordo do Titanic",
                                               y = "Quantidade de passageiros")+
  theme_hc() +
  theme(plot.title = element_text(hjust=0.5)+
          theme(axis.text.x = element_text(angle = 0, hjust = 1))+
          theme(legend.position = "bottom"))
GPassParch
ggplotly(GPassParch)
str(dfPassParch$Parch)

################################################################## GRÁFICO PARCH MENORES DE 18

unique(df$Parch)

dfParchKids <- filter(df, Age<18)

dfParchK <- group_by(dfParchKids, Sex, Survived, Parch) %>% 
  summarise(sum(PCID))

dfParchK$Sex <- factor(dfParchK$Sex)

dfParchK$TOTAL <- dfParchK$`sum(PCID)` 

dfParchK$Survived <- factor(dfParchK$Survived)

dfParchK$TOTAL <- factor(dfParchK$TOTAL)
dfParchK$Parch <- factor(dfParchK$Parch)

GParchK <- ggplot(dfParchK, aes( x = Survived, y = TOTAL, fill = Sex, label = TOTAL))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~ Parch)+
  geom_label(position = position_dodge(width = 1))+
  scale_fill_manual(values = c("#d01c8b",
                                        "#4dac26",
                                        "#fdae61"))+
                                          labs(title="MENORES DE 18 ANOS - COM QUEM VIAJAM?",
                                               x = "ACOMPANHANTES",
                                               y = "Quantidade de passageiros")+
  theme_hc() +
  theme(plot.title = element_text(hjust=0.5)+
          theme(axis.text.x = element_text(angle = 0, hjust = 1))+
          theme(legend.position = "bottom"))
GParchK
ggplotly(GParchK)
str(dfParchK$Parch)





################################################################## GRÁFICO SibSp
unique(df$SibSp)

dfSibSp <- group_by(df, Sex, Survived, SibSp) %>% 
  summarise(sum(PCID))

dfSibSp$Sex <- factor(dfSibSp$Sex)

dfSibSp$TOTAL <- dfSibSp$`sum(PCID)` 

dfSibSp$Survived <- factor(dfSibSp$Survived)

dfSibSp$TOTAL <- factor(dfSibSpTOTAL)
dfSibSp$SibSp <- factor(dfSibSp$SibSp)

GSibSp <- ggplot(dfSibSp, aes( x = Survived, y = TOTAL, fill = Sex, label = TOTAL))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~ SibSp)+
  geom_label(position = position_dodge(width = 1))+
  scale_fill_manual(values = c("#f0abc1",
                                        "#94ddde"))+
                                          labs(title="Nº de irmãos/cônjuges a bordo do Titanic",
                                               x = "ACOMPANHANTES",
                                               y = "Quantidade de passageiros")+
  theme_hc() +
  theme(plot.title = element_text(hjust=0.5)+
          theme(axis.text.x = element_text(angle = 0, hjust = 1))+
          theme(legend.position = "bottom"))
GSibSp
ggplotly(GSibSp)
str(dfSibSp$SibSp)



################################################################## GRÁFICO Cabin
unique(df$Cabin)

summary(df$Cabin)

df$CabinLetter <- substr(df$Cabin, 1, 1)
unique(df$CabinLetter)
df$CabinNumber <- substr(df$Cabin, 2, 4)

summary(df$Fare)

cabinesA <- filter(df, CabinLetter == "A")
cabinesB <- filter(df, CabinLetter == "B")
cabinesC <- filter(df, CabinLetter == "C")
cabinesD <- filter(df, CabinLetter == "D")
cabinesE <- filter(df, CabinLetter == "E")
cabinesF <- filter(df, CabinLetter == "F")
cabinesG <- filter(df, CabinLetter == "G")
cabinesT <- filter(df, CabinLetter == "T")

pclassA <- cabinesA[cabinesA$Pclass==1,]
unique(cabinesA$Fare)
cabinesA$Fare <- round(cabinesA$Fare)
summary(cabinesA$Fare)

pclassB <- cabinesB[cabinesB$Pclass==1,]
unique(cabinesB$Fare)
cabinesB$Fare <- round(cabinesB$Fare)
summary(cabinesB$Fare)

pclassC <- cabinesC[cabinesC$Pclass==1,]
unique(cabinesC$Fare)
cabinesC$Fare <- round(cabinesC$Fare)
summary(cabinesC$Fare)

pclassD1 <- cabinesD[cabinesD$Pclass==1,]
unique(cabinesD$Fare)
cabinesD$Fare <- round(cabinesD$Fare)
summary(cabinesD$Fare)

pclassD2 <- cabinesD[cabinesD$Pclass==2,]
unique(cabinesD$Fare)
summary(cabinesD$Fare)

pclassE1 <- cabinesE[cabinesE$Pclass==1,]
unique(cabinesE$Fare)
cabinesE$Fare <- round(cabinesE$Fare)
summary(cabinesE$Fare)

pclassE2 <- cabinesE[cabinesE$Pclass==2,]
unique(cabinesE$Fare)
summary(cabinesE$Fare)

pclassE3 <- cabinesE[cabinesE$Pclass==3,]
unique(cabinesE$Fare)
summary(cabinesE$Fare)

pclassF2 <- cabinesF[cabinesF$Pclass==2,]
unique(cabinesF$Fare)
cabinesF$Fare <- round(cabinesF$Fare)
summary(cabinesF$Fare)

pclassF3 <- cabinesF[cabinesF$Pclass==3,]
unique(cabinesF$Fare)
summary(cabinesF$Fare)

pclassG <- cabinesG[cabinesG$Pclass==3,]
unique(cabinesG$Fare)
cabinesG$Fare <- round(cabinesG$Fare)
summary(cabinesG$Fare)

pclassT <- cabinesT[cabinesT$Pclass==1,]
unique(cabinesT$Fare)
cabinesT$Fare <- round(cabinesT$Fare)
summary(cabinesT$Fare)

pclass1 <- df[df$Pclass==1,]
unique(pclass1$Fare)
summary(pclass1$Fare)

pclass2 <- df[df$Pclass==2,]
unique(pclass2$Fare)
summary(pclass2$Fare)

pclass3 <- df[df$Pclass==3,]
unique(pclass3$Fare)
summary(pclass3$Fare)

#######################################################

###################### PREPARAÇÃO DOS DADOS

View(df)

unique(df$Sex)

df$Sex[df$Age<=18] <- 2
df$Sex[df$Sex=='female'] <- 1
df$Sex[df$Sex=='male'] <- 0

unique(df$Embarked)
df$Embarked[df$Embarked=='S'] <- 0
df$Embarked[df$Embarked=='C'] <- 1
df$Embarked[df$Embarked=='Q'] <- 2
df$Embarked[df$Embarked==""] <- 0

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

#Excluindo colunas desnecessárias
df$Titulo1 <- NULL
df$Name <- NULL
df$Cabin <- NULL
df$Ticket <- NULL

unique(df$CabinLetter)
df$CabinNumber <- substr(df$Cabin, 2, 4)

colSums(is.na(df))


# Preenchendo missing values de Fare com KNN
#df$Fare <- mean(df$Fare)

df$Fare <- as.numeric(df$Fare)
mean(!is.na(df$Fare))

df$Fare[is.na(df$Fare)]<-mean(df$Fare,na.rm=TRUE)

summary(df$Fare)

df$Cabin_imp <- NULL

summary(df$Cabin)
unique(df$Cabin)
summary(df$CabinLetter)
unique(df$CabinLetter)
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

df$Cabin <- NULL
df$PassengerId <- NULL

##################  VERIFICANDO OUTLIER

boxplot(df$Fare)

summary(df$Fare)
unique(boxplot.stats(df$Fare)$out)
unique(df$Fare)
df$Fare <- round(df$Fare)


#########################################Escalonamento 

#df$Pclass <- scale(df$Pclass)
df$Sex <- as.numeric(df$Sex)
#df$Sex <- scale(df$Sex)
#df$Age <- scale(df$Age)
#df$SibSp <- scale(df$SibSp)
#df$Parch <- scale(df$Parch)
#df$Fare <- scale(df$Fare)
df$Embarked <- as.numeric(df$Embarked)
#df$Embarked <- scale(df$Embarked)
df$CabinLetter <- scale(df$CabinLetter)
df$CabinNumber <- scale(df$CabinNumber)
#df$Titulo <- as.numeric(df$Titulo)
#df$Titulo <- scale(df$Titulo)

df$PCID <- NULL
#Alterando a posição da coluna Age
df <- df %>%
  relocate(Survived, .after = CabinNumber)

colnames(df)
#Criando a equação e fórmula para rodar no randomForest
equacao <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + CabinLetter + CabinNumber + Titulo"
formula <- as.formula(equacao)

df$Survived <- as.factor(df$Survived)

df_train <- df[df$Istrain == TRUE,]
df_test <- df[df$Istrain == FALSE,]

set.seed(10)
modelo <- randomForest(formula = formula, 
                       data = df_train,
                       ntree = 120,
                       maxnodes = 105,
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
























