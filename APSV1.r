#Library

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(readr)

##Importing data
##If you import the data from V2 ignore this next few steps
data <- read_delim("C:\\Users\\anton\\Desktop\\5o\\elet2\\MICRODADOS_ENEM_2022.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

##Selecting only the columns that we will use
#
data <- data %>% select(NU_INSCRICAO, TP_SEXO, TP_FAIXA_ETARIA, SG_UF_ESC, TP_ESCOLA, Q006, TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT, TP_STATUS_REDACAO, NU_NOTA_CN, NU_NOTA_CH, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO,IN_TREINEIRO, Q006,Q019,Q022,Q024,Q025,Q001,Q002)
#
#write.csv(data, "C:\\Users\\anton\\Desktop\\5o\\elet2\\MICRODADOS_ENEM_2022_V2.csv")
#

#Data aux
dataAux <- data
data <- dataAux

#Filtering data by age (1 Maior our igual a 17 anos, 10 menor ou igual a 25 anos)

data <- data %>%
 filter(SG_UF_ESC == "SP",
 TP_FAIXA_ETARIA >= 1 & TP_FAIXA_ETARIA <= 10,
 IN_TREINEIRO == 0,
 TP_ESCOLA != 1,
 TP_STATUS_REDACAO == 1,
 TP_PRESENCA_CN == 1 & TP_PRESENCA_CH == 1 & TP_PRESENCA_LC == 1 & TP_PRESENCA_MT == 1)


#Relative age plot with mutation to correct the x axis
dataAux <- data

data$TP_FAIXA_ETARIA <- as.factor(data$TP_FAIXA_ETARIA)

data %>% 
  ggplot(aes(x = TP_FAIXA_ETARIA)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(x = "Idade", y = "Frequência relativa", title = "Frequência relativa por idade") +
  scale_x_discrete(labels = c("17", "18", "19", "20", "21", "22", "23", "24", "25")) +
  theme_bw()

data <- dataAux

#Relative frequency plot for UF, only avalible for V0
#
#dataAux <- data
#
#data$SG_UF_ESC <- as.factor(data$SG_UF_ESC)
##Omiting NA Values

#data <- data %>% filter(!is.na(SG_UF_ESC))
#
#data %>% 
#  ggplot(aes(x = SG_UF_ESC)) +
#  geom_bar(aes(y = (..count..)/sum(..count..))) +
#  labs(x = "UF", y = "Frequência relativa", title = "Frequência relativa por UF") +
#  theme_bw()
##Pie plot with top 10 states and put the rest under Others, only avalible for V0
#
#dataAux <- data
#
#data$SG_UF_ESC <- as.factor(data$SG_UF_ESC)
#
#data <- data %>% filter(SG_UF_ESC != "NA") %>% 
#    group_by(SG_UF_ESC) %>%
#    summarise(n = n()) %>%
#    arrange(desc(n)) %>%
#    slice(1:10) %>%
#    mutate(SG_UF_ESC = fct_lump(SG_UF_ESC, n = 10))
#
#data %>% 
#  ggplot(aes(x = "", y = n, fill = SG_UF_ESC)) +
#  geom_bar(stat = "identity") +
#  coord_polar("y", start = 0) +
#  labs(x = "", y = "", fill = "UF") +
#  theme_bw() +
#  theme(legend.position = "bottom")
#
#Ploting type of school 

data <- dataAux

data$TP_ESCOLA <- as.factor(data$TP_ESCOLA)

data %>% 
  ggplot(aes(x = TP_ESCOLA)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(x = "Tipo de escola", y = "Frequência relativa", title = "Frequência relativa por tipo de escola") +
  scale_x_discrete(labels = c('Publica', 'Privada'))+
  theme_bw()

data <- dataAux

#Ploting the relative wealth of the candidate Q006

data$Q006 <- as.factor(data$Q006)

data %>% 
  ggplot(aes(x = Q006)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(x = "Renda familiar", y = "Frequência relativa", title = "Frequência relativa por renda familiar") +
  #scale_x_discrete(labels = c('Nenhuma Renda', "Até R$1212", 'De R$1212,01 até R$1818,00',
  #   'Até R$2424,00', 'Até R$3030,00', 'Até R$3636,00', 'Até R$4848,00', 'Até R$6060,00', 'Até R$7272,00',
  #   'Até R$8484,00','Até R$9696,00','Até R$10908,00', 'Até R$12120,00','Até R$ 14544,00','Até R$18180,00', 'Até R$24240,00', 'Acima de 24240,01' ))  +
  theme_bw()

data <- dataAux
#Ploting based on sex

data$TP_SEXO <- as.factor(data$TP_SEXO)

data %>% 
  ggplot(aes(x = TP_SEXO)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(x = "Sexo", y = "Frequência relativa", title = "Frequência relativa por sexo") +
  scale_x_discrete(labels = c('Masculino', 'Feminino'))+
  theme_bw()

#Table with the descriptive variables using the skim() function

library(skimr)

dataSkim <- data %>% select(TP_SEXO, TP_FAIXA_ETARIA, TP_ESCOLA, Q006, TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT, TP_STATUS_REDACAO, NU_NOTA_CN, NU_NOTA_CH, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO)

tableSkim <- skim(dataSkim)
View(tableSkim)

##Description analisys for the grades

#CN

data$NU_NOTA_CN <- as.numeric(data$NU_NOTA_CN)

data %>% 
  ggplot(aes(x = NU_NOTA_CN)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), bins = 30) +
  labs(x = "Nota", y = "Frequência relativa", title = "Frequência relativa por nota CN") +
  theme_bw()


#CH

data$NU_NOTA_CH <- as.numeric(data$NU_NOTA_CH)

data %>% 
  ggplot(aes(x = NU_NOTA_CH)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), bins = 30) +
  labs(x = "Nota", y = "Frequência relativa", title = "Frequência relativa por nota CH") +
  theme_bw()

#LC

data$NU_NOTA_LC <- as.numeric(data$NU_NOTA_LC)

data %>% 
  ggplot(aes(x = NU_NOTA_LC)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), bins = 30) +
  labs(x = "Nota", y = "Frequência relativa", title = "Frequência relativa por nota LC") +
  theme_bw()

#MT

data$NU_NOTA_MT <- as.numeric(data$NU_NOTA_MT)

data %>% 
  ggplot(aes(x = NU_NOTA_MT)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), bins = 30) +
  labs(x = "Nota", y = "Frequência relativa", title = "Frequência relativa por nota MT") +
  theme_bw()


#Redacao

data$NU_NOTA_REDACAO <- as.numeric(data$NU_NOTA_REDACAO)

data %>% 
  ggplot(aes(x = NU_NOTA_REDACAO)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), bins = 30) +
  labs(x = "Nota", y = "Frequência relativa", title = "Frequência relativa por nota Redacao") +
  theme_bw()



#AvgPerformace data frame for future analysis
avgPerformance <- data.frame(data,
     ID = data$NU_INSCRICAO,
     NotaCN = data$NU_NOTA_CN ,
     NotaCH = data$NU_NOTA_CH,
     NotaLC = data$NU_NOTA_LC,
     NotaMT = data$NU_NOTA_MT, 
     NotaRedacao = data$NU_NOTA_REDACAO)

View(avgPerformance)

#Creating a pontuation variable

avgPerformance$Pontuacao <- (avgPerformance$NotaCN + avgPerformance$NotaCH + avgPerformance$NotaLC + avgPerformance$NotaMT + avgPerformance$NotaRedacao)/5

#Dropping SG_UF_ESC, TP_TREINEIRO and NU_INSCRICAO  columns

avgPerformance <- avgPerformance %>% select(-c(SG_UF_ESC,TP_TREINEIRO,NU_INSCRICAO))

#Description analysis for the avgPerformance$Pontuacao

avgPerformance$Pontuacao <- as.numeric(avgPerformance$Pontuacao)

avgPerformance %>% 
  ggplot(aes(x = Pontuacao)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), bins = 30) +
  labs(x = "Pontuação", y = "Frequência relativa", title = "Frequência relativa por pontuação") +
  theme_bw()

#Creating a regression model for predicting the pontuation

model_Descriptive <- lm(Pontuacao ~ TP_SEXO + TP_FAIXA_ETARIA + TP_ESCOLA + Q006, data = avgPerformance)

library(data.table)

model_Descriptive.summary <- as.data.frame(coef(summary(model_Descriptive)), check.names = FALSE)


model_Descriptive.summary$Var <- c("Intercept", "Sexo M", "Idade", "Tipo de escola", "Até R$1212", 'Até R$1818,00',
  'Até R$2424,00', 'Até R$3030,00', 'Até R$3636,00', 'Até R$4848,00', 'Até R$6060,00', 'Até R$7272,00',
  'Até R$8484,00','Até R$9696,00','Até R$10908,00', 'Até R$12120,00','Até R$ 14544,00','Até R$18180,00', 'Até R$24240,00', 'Acima de 24240,01')

View(model_Descriptive.summary)

#Model based on acess to information

model_Information <- lm(Pontuacao ~ Q019+Q022+Q024+Q025, data = avgPerformance)

model_Information.summary <- as.data.frame(coef(summary(model_Information)), check.names = FALSE)

model_Information.summary$Var <- c("Intercept", "Uma TV","Duas TV","Três TV","Quatro ou mais TV", "Um cell", "Dois cell", "Tres cell", "Quatro Cell ou mais",  "Um PC", "Dois PC", "Tres PC", "Quatrou ou mais PC", "Sim")

View(model_Information.summary)

#Model based on the parents education

model_Education <- lm(Pontuacao ~ Q001+Q002, data = avgPerformance)

model_Education.summary <- as.data.frame(coef(summary(model_Education)), check.names = FALSE)

model_Education.summary$Var <- c("Intercept", "Nunca estudou", "Fundamental incompleto", "Fundamental II incompleto", "Medio incompleto", "Medio completo, Faculdade Incompleto", "Superior completo, sem pós", "Completou pós", "Nunca estudou", "Fundamental incompleto", "Fundamental II incompleto", "Medio incompleto", "Medio completo, Faculdade Incompleto", "Superior completo, sem pós", "Completou pós")
View(model_Education.summary)

#Checking if gender has a significant impact on the pontuation

model_Gender <- lm(Pontuacao ~ TP_SEXO, data = avgPerformance)

model_Gender.summary <- as.data.frame(coef(summary(model_Descriptive)), check.names = FALSE)

model_Gender.summary$Var <- c("Intercept", "Sexo M")

view(model_Gender.summary)

#Checking if age has a significant impact on the pontuation

model_Age <- lm(Pontuacao ~ TP_FAIXA_ETARIA, data = avgPerformance)

model_Age.summary <- as.data.frame(coef(summary(model_Age)), check.names = FALSE)

View(model_Age.summary)

#Checking if type of school has a significant impact on the pontuation

model_School <- lm(Pontuacao ~ factor(TP_ESCOLA), data = avgPerformance)

model_School.summary <- as.data.frame(coef(summary(model_School)), check.names = FALSE)

model_School.summary$var <- c("Intercept", "Privada")

View(model_School.summary)
#Checking with man goes better in individual tests

model_CN <- lm(NU_NOTA_CN ~ TP_SEXO, data = avgPerformance)

model_CN.summary <- as.data.frame(coef(summary(model_CN)), check.names = FALSE)

model_CN.summary$var <- c("Intercept CN", "Sexo M")

View(model_CN.summary)

#MT

model_MT <- lm(NU_NOTA_MT ~ TP_SEXO, data = avgPerformance)

model_MT.summary <- as.data.frame(coef(summary(model_MT)), check.names = FALSE)

model_MT.summary$var <- c("Intercept MT", "Sexo M")

View(model_MT.summary)

#CH

model_CH <- lm(NU_NOTA_CH ~ TP_SEXO, data = avgPerformance)

model_CH.summary <- as.data.frame(coef(summary(model_CH)), check.names = FALSE)

model_CH.summary$var <- c("Intercept CH", "Sexo M")

View(model_CH.summary)

#LC

model_LC <- lm(NU_NOTA_LC ~ TP_SEXO, data = avgPerformance)

model_LC.summary <- as.data.frame(coef(summary(model_LC)), check.names = FALSE)

model_LC.summary$var <- c("Intercept LC", "Sexo M")

View(model_LC.summary)

#Redacao

model_Redacao <- lm(NU_NOTA_REDACAO ~ TP_SEXO, data = avgPerformance)

model_Redacao.summary <- as.data.frame(coef(summary(model_Redacao)), check.names = FALSE)

model_Redacao.summary$var <- c("Intercept Redacao", "Sexo M")

View(model_Redacao.summary)
