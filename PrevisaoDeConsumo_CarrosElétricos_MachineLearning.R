setwd("C:/Users/Blazi/Documents/Data Science Academy DSA/Formação Cientista de Dados/Big Data Analytics com R e Microsoft Azure Machine Learning/ProjetoFeedback/Projeto1")
getwd()

#install.packages("readxl")
require("readxl")
original <- read_excel("FEV-data-Excel.xlsx")

#install.packages("writexl")
require("writexl")

View(original)
str(original)

#######
####### Limpeza/Transformação dos dados pt.1
#######

# - Decisão 1: Descartar variáveis 'Car full name', 'Make' e 'Model': Não são categorias, não serão úteis para machine learning

#install.packages('dplyr')
require(dplyr)
dados<- original %>% select(-c('Car full name' , 'Make', 'Model'))
View(dados)

#Renomear colunas

names(dados) <- c("price", "engine", "torque", "brakes", "drive", "battery", "range", 
                  "wheelbase", "length", "width", "height", "empty", "weight", "load", 
                  "seats", "doors", "tire", "speed", "boot", "acceleration", "charging", "consumption")

# Mudar tipo das variáveis
str(dados)

dados$brakes <- as.factor(dados$brakes)
dados$drive <- as.factor(dados$drive)

#####
##### Teste/Modelo 1 - Modelo de Regressão Linear
#####

## Na primeira tentativa vamos somente descartar os casos em branco para teste

colSums(is.na(dados))
df1 <- na.omit(dados)
View(df1) #Dataset df1 ficou com 42 linas e 22 colunas

## Split dados treino e teste

#install.packages('caret')
require(caret)
split1 <- createDataPartition(y = df1$consumption, p = 0.7, list = FALSE)

treino1 <- df1[split1,]
teste1 <- df1[-split1,]

## Regressão linear

mod1 <- lm(treino1$consumption ~ ., treino1)
mod1

pred_mod1 <- predict(mod1, teste1)
pred_mod1

#Avaliação

summary(mod1)
#Somente o intercepto e variável 'tire' mostraram significância: ‘*’ 0.05
#Residual standard error: 0.8856
#Multiple R-squared:  0.9887
# Adjusted R-squared:  0.9577
#p-value: 1.48e-05
#F-statistic: 31.87

aval_mod1 <- cbind.data.frame(teste1$consumption, pred_mod1)
aval_mod1$error <- teste1$consumption - pred_mod1
aval_mod1

#######
####### Limpeza/Transformação dos dados pt.2
#######

#Objetivo: Aproveitar melhor os casos originais tentando manter casos com a variável alvo

str(dados)
colSums(is.na(dados))

#- Decisão 2: Descartar variável 'brakes' = Categórica com 1 valor missing, não pode ser estimada
#- Decisão 3: Descartar variável 'boot' = Com 1 valor missing, não pode ser estimada
# O caso faltando de ambas tem o 'consumption' listado, necessário preservar caso

#- Decisão 4: Descartar variável 'acceleration' = 3 casos faltando, não podem ser estimados
# Sendo que dois dos casos faltando tem o 'consumption' listado, necessário preservar casos

df2<- dados %>% select(-c('brakes' , 'boot' , 'acceleration'))
str(df2)

# Descartar casos faltando da variável target

df2 <- na.omit(df2)
colSums(is.na(df2))

View(df2) # Dataframe ficou com 44 casos e 19 variáveis

#####
##### Modelo 2 - Modelo de Regressão Linear
#####

## Split dados treino e teste

split2 <- createDataPartition(y = df2$consumption, p = 0.7, list = FALSE)

treino2 <- df2[split2,]
teste2 <- df2[-split2,]

## Regressão linear

mod2 <- lm(treino2$consumption ~ ., treino2)
mod2

pred_mod2 <- predict(mod2, teste2)
pred_mod2

#Avaliação

summary(mod2)
#Variáveis que mostraram significância: 
# 'battery' e 'range' == 0.001 ‘**’
# 'height' == 0.01 ‘*’
#Residual standard error: 0.9582
#Multiple R-squared:  0.9818
#Adjusted R-squared:  0.9529 
#p-value: 1.249e-07
#F-statistic: 34.03

aval_mod2 <- cbind.data.frame(teste2$consumption, pred_mod2)
aval_mod2$error <- teste2$consumption - pred_mod2
aval_mod2

#######
####### Limpeza/Transformação dos dados pt.3
#######

# Solução/tentativa: Selecionar variáveis mais importantes e normalizar dados

## Testar associação entre variáveis 'empty' e 'weight' x 'load'

#teste pearson
cor.test(df2$weight, df2$load, method = "pearson")
# p-value = 1.365e-08 ; corr = 0.734

cor.test(df2$empty, df2$load, method = "pearson")
# p-value = 4.548e-06 ; corr = 0.630

# novo dataset com a diferença entre 'weight' - 'empty' para teste
data_teste <- as.data.frame(df2$load)
data_teste$diferença <- df2$weight - df2$empty
data_teste

cor.test(data_teste[,1], data_teste[,2], method = "pearson")
# p-value = 1.872e-11 ; corr = 0.813

# Resultado: Alta associação (0.81) entre 'load' e a diferença entre 'weight' - 'empty'

#- Decisão 5: Descartar variável 'load' devido a associação com outras

df3 <- df2[,-13]
str(df3)

## Normalizar variáveis quantitativas

cols <- sapply(df3, is.numeric)
df3 <- log(df3[cols])
df3$drive <- df2$drive

View(df3) #Dataframe com 44 casos, 18 variáveis e normalizado

#####
##### Modelo 3 - Modelo de Regressão Linear
#####

## Split dados treino e teste

split3 <- createDataPartition(y = df3$consumption, p = 0.7, list = FALSE)

treino3 <- df3[split3,]
teste3 <- df3[-split3,]

## Regressão linear

mod3 <- lm(treino3$consumption ~ ., treino3)
mod3

pred_mod3 <- predict(mod3, teste3)
pred_mod3

#Avaliação

summary(mod3)
#Variáveis que mostraram significância: 
# 'battery' e 'range' == 0 ‘***’
# 'height' e 'drive4WD' (um fator) == 0.001 ‘**’
# 'empty' == 0.01 ‘*’
# 'weight', 'doors' e 'tire' == 0.05 ‘.’
#Residual standard error: 0.04975
#Multiple R-squared:  0.9773
#Adjusted R-squared:  0.9459
#p-value: p-value: 7.8e-08
#F-statistic:  31.1

aval_mod3 <- cbind.data.frame(teste3$consumption, pred_mod3)
aval_mod3$error <- teste3$consumption - pred_mod3
aval_mod3

# Desnormalização do df3 para comparar resultados

erro_mod3 <- cbind.data.frame(exp(teste3$consumption), exp(pred_mod3))
erro_mod3$error <- exp(teste3$consumption) - exp(pred_mod3)
erro_mod3

#####
##### Modelo 4 - Modelo de Regressão Linear com variáveis selecionadas
#####

# Modelo será refeito com somente as variáveis que apresentaram significância do anterior

# Novo dataframe

df4 <- df3 %>% select(c('battery' , 'range', 'height' , 'drive' , 'empty' , 'weight', 'doors' , 'tire' , 'consumption'))
View(df4) # 44 casos, 9 variáveis e normalizado

# Split dos dados

split4 <- createDataPartition(y = df4$consumption, p = 0.7, list = FALSE)

treino4 <- df4[split4,]
teste4 <- df4[-split4,]

## Regressão linear

mod4 <- lm(treino4$consumption ~ ., treino4)
mod4

pred_mod4 <- predict(mod4, teste4)
pred_mod4

#Avaliação

summary(mod4)
#Variáveis que mostraram significância: 
# 'battery', 'range', 'drive', 'doors' == 0 ‘***’
# 'height' == 0.001 ‘**’
# 'tire' == 0.01 ‘*’
#Residual standard error: 0.06308
#Multiple R-squared:  0.9439 
#Adjusted R-squared:  0.921 
#p-value: 1.034e-11
#F-statistic: 41.15

aval_mod4 <- cbind.data.frame(exp(teste4$consumption), exp(pred_mod4))
aval_mod4$error <- exp(teste4$consumption) - exp(pred_mod4)
aval_mod4 #Já desnormalizado

#####
##### Modelo 5 - Modelo de Regressão Linear com variáveis selecionadas
#####

# Modelo será refeito com somente as variáveis que apresentaram significância do anterior

## Vatagem Importante: Essas variáveis constam para todos os casos do dataset original, 
#possível prever mesmo valores para casos que não tinham a variável target listada

# Novo dataframe

df5 <- df4 %>% select(c('battery' , 'range', 'height' , 'drive' , 'doors' , 'tire' , 'consumption'))
View(df5) # 44 casos, 7 variáveis e normalizado

# Split dos dados

split5 <- createDataPartition(y = df5$consumption, p = 0.7, list = FALSE)

treino5 <- df5[split5,]
teste5 <- df5[-split5,]

## Regressão linear

mod5 <- lm(treino5$consumption ~ ., treino5)
mod5

pred_mod5 <- predict(mod5, teste5)
pred_mod5

#Avaliação

summary(mod5)
#Variáveis que mostraram significância: 
# 'battery', 'range' == 0 ‘***’
# 'height', 'drive4WD' (um fator), 'doors' , 'tire' == 0.001 ‘**’
# Intercepto == 0.01 ‘*’
#Residual standard error: 0.06044
#Multiple R-squared:  0.9392 
#Adjusted R-squared:  0.9215  
#p-value: 4.616e-13
#F-statistic: 52.99

aval_mod5 <- cbind.data.frame(exp(teste5$consumption), exp(pred_mod5))
aval_mod5$error <- exp(teste5$consumption) - exp(pred_mod5)
aval_mod5 #Já desnormalizado

# Calculando o erro médio
# Quão distantes seus valores previstos estão dos valores observados
# MSE
mse <- mean((exp(teste5$consumption) - exp(pred_mod5))^2)
mse

# RMSE
rmse <-mse^0.5
rmse

# Calculando R Squared
SSE = sum((exp(teste5$consumption) - exp(pred_mod5))^2)
SST = sum((mean(exp(teste5$consumption)) - exp(pred_mod5))^2)

# R-Squared
# Ajuda a avaliar o nível de precisão do nosso modelo. Quanto maior, melhor, sendo 1 o valor ideal.
R2 = 1 - (SSE/SST)
R2

#####
##### Modelo 6 - Modelo SVM (Support Vector Machines) com variáveis selecionadas(df5)
#####

#install.packages('e1071')
require(e1071)

# Será usado novamente o split5

svm6 <- svm(treino5$consumption ~ . , data=treino5, kernel= "linear", cost=10, scale=FALSE)
svm6

pred_svm6 <- predict(svm6, teste5)
pred_svm6

#Avaliação

summary(svm6)
# gamma:  0.125 
# epsilon:  0.1

aval_mod6 <- cbind.data.frame(exp(teste5$consumption), exp(pred_svm6))
aval_mod6$error <- exp(teste5$consumption) - exp(pred_svm6)
aval_mod6 #Já desnormalizado

mean(aval_mod5$error)
mean(aval_mod6$error)

# Calculando o erro médio
# Quão distantes seus valores previstos estão dos valores observados
# MSE
mse <- mean((exp(teste5$consumption) - exp(pred_svm6))^2)
mse

# RMSE
rmse <-mse^0.5
rmse

# Calculando R Squared
SSE = sum((exp(teste5$consumption) - exp(pred_svm6))^2)
SST = sum((mean(exp(teste5$consumption)) - exp(pred_svm6))^2)

# R-Squared
# Ajuda a avaliar o nível de precisão do nosso modelo. Quanto maior, melhor, sendo 1 o valor ideal.
R2 = 1 - (SSE/SST)
R2

#####
##### Modelo 7 - Modelo gbm com variáveis selecionadas(df5)
#####

install.packages('gbm')
require(gbm)

# Será usado novamente o split5

gbm7 = gbm(treino5$consumption ~.,
                data = treino5,
                distribution = "gaussian",
                cv.folds = 10,
                shrinkage = .01,
                n.minobsinnode = 5,
                n.trees = 1000)
gbm7

pred_gbm7 <- predict(gbm7, teste5)
pred_gbm7

#Avaliação

summary(gbm7)
# 'doors' sem relevância

aval_mod7 <- cbind.data.frame(exp(teste5$consumption), exp(pred_gbm7))
aval_mod7$error <- exp(teste5$consumption) - exp(pred_gbm7)
aval_mod7 #Já desnormalizado

# Calculando o erro médio
# Quão distantes seus valores previstos estão dos valores observados
# MSE
mse <- mean((exp(teste5$consumption) - exp(pred_gbm7)^2))
mse

# RMSE
rmse <-mse^0.5
rmse # NaN (??)

# Calculando R Squared
SSE = sum((exp(teste5$consumption) - exp(pred_gbm7))^2)
SST = sum((mean(exp(teste5$consumption)) - exp(pred_gbm7))^2)

# R-Squared
# Ajuda a avaliar o nível de precisão do nosso modelo. Quanto maior, melhor, sendo 1 o valor ideal.
R2 = 1 - (SSE/SST)
R2

#####
##### Teste/Modelo 8 - Refazer modelos excluindo variável 'doors', que não demonstrou significância segundo o modelo gbm
#####

# Novo dataframe

df8 <- df5 %>% select(c('battery' , 'range', 'height' , 'drive' , 'tire' , 'consumption'))
View(df8) # 44 casos, 6 variáveis e normalizado

# Split dos dados

split8 <- createDataPartition(y = df8$consumption, p = 0.7, list = FALSE)

treino8 <- df8[split8,]
teste8 <- df8[-split8,]

#------------------
## Regressão linear (modelo8)
#------------------

mod8 <- lm(treino8$consumption ~ ., treino8)
mod8

pred_mod8 <- predict(mod8, teste8)
pred_mod8

#Avaliação

summary(mod8)
#Residual standard error: 0.08336
#Multiple R-squared:  0.878 
#Adjusted R-squared:  0.8487  
#p-value: 2.926e-10
#F-statistic: 29.99

aval_mod8 <- cbind.data.frame(exp(teste8$consumption), exp(pred_mod8))
aval_mod8$error <- exp(teste8$consumption) - exp(pred_mod8)
aval_mod8

# Calculando o erro médio
# Quão distantes seus valores previstos estão dos valores observados
# MSE
mse <- mean((exp(teste8$consumption) - exp(pred_mod8)^2))
mse 

# RMSE
rmse <-mse^0.5
rmse # NaN (??)

# Calculando R Squared
SSE = sum((exp(teste8$consumption) - exp(pred_mod8))^2)
SST = sum((mean(exp(teste8$consumption)) - exp(pred_mod8))^2)

# R-Squared
# Ajuda a avaliar o nível de precisão do nosso modelo. Quanto maior, melhor, sendo 1 o valor ideal.
R2 = 1 - (SSE/SST)
R2

#####
##### Deploy - Modelos 5, 6 e 8
#####

View(dados)
str(dados)

#Normalizar dataset com todos casos originais

cols_num <- sapply(dados, is.numeric)
dados_norm <- log(dados[cols_num])
dados_norm$drive <- dados$drive
dados_norm$brakes <- dados$brakes
head(dados_norm)

#------------------------------------
###Deploy modelo 5 - Regressão Linear
#------------------------------------

dep_mod5 <- predict(mod5, dados_norm)
dep_mod5

#Avaliação Deploy Mod5

summary(dep_mod5)

aval_dep_mod5 <- cbind.data.frame(exp(dados_norm$consumption), exp(dep_mod5))
aval_dep_mod5$error <- exp(dados_norm$consumption) - exp(dep_mod5)
aval_dep_mod5 #Já desnormalizado

#------------------------------------
###Deploy modelo 6 - SVM
#------------------------------------

dep_svm6 <- predict(svm6, dados_norm)
dep_svm6

#Avaliação Deploy svm6

summary(dep_svm6)
View(dep_svm6) ## Erro! Previu somente 51 casos, e objeto gerado consta com ainda menos: 42 casos(?)

# Não foi possível consertar o erro

#------------------------------------
###Deploy modelo 8 - Regressão Linear
#------------------------------------

dep_mod8 <- predict(mod8, dados_norm)
dep_mod8

#Avaliação Deploy Mod8

summary(dep_mod8)

aval_dep_mod8 <- cbind.data.frame(exp(dados_norm$consumption), exp(dep_mod8))
aval_dep_mod8$error <- exp(dados_norm$consumption) - exp(dep_mod8)
aval_dep_mod8 #Já desnormalizado

############# Comparação

aval_final <- cbind.data.frame(exp(dep_mod5), exp(dep_mod8), (exp(dados_norm$consumption)))
aval_final #Já desnormalizado

# O modelo 5 foi o melhor

#####
##### Resultado
#####

#O modelo 5 (Regressão Linear) foi escolhido por ser o mais generalizável, podendo ser aplicado no 
#dataset original mesmo para prever os casos onde existiam casos faltando, foi melhor 
#que os modelos 7 e 8, e também a impossibilidade do deploy do modelo 6 (svm) devido ao erro.

#####
##### Dataset final já com as previsões
#####

final <- cbind.data.frame(original, round(exp(dep_mod5), 2))
#renomear coluna da previsão
final <- rename(final, 'Previsao_mean_Energy_consumption' = 'round(exp(dep_mod5), 2)')
View(final)
#exportar
write_xlsx(final, 'Previsão.xlsx')