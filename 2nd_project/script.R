# Trabalho pr�tico 2
#
# Comando para limpar o ambiente do RStudio

rm(list=ls())

# Comando geral para importar packages

if(!require(pacman)) install.packages("pacman") ; library(pacman)

pacman::p_load(PerformanceAnalytics, corrplot, rpart, rpart.plot, neuralnet, class, FNN, stats, mltest)


#-----------------------------------------------------------------------------------------------------------------
# Regress�o
#
# 1) Comece por carregar o ficheiro ("countryagregatedata.xlsx") para o ambiente do R,
# verifique a sua dimens�o e obtenha um sum�rio dos dados.

# Comando geral com cria��o de estrutura a ser usada por todos os exerc�cios

dados <- read.csv("~/countryagregatedata.csv",sep=",")

# Fun��o usada para mostrar as primeiras x linhas da estrutura (sendo x o segundo argumento)

head(dados, 4)

# Elimina��o da coluna Row

dados$row <- NULL

# Fun��o usada para verificar o n�mero de linhas e de colunas da estrutura

dim(dados)

# Fun��o utilizada para obter o sum�rio dos dados da estrutura

summary(dados)

# Para as colunas cujos valores s�o num�ricos, podemos constatar o m�nimo, m�ximo,
# 1� quartil, mediana (ou 2� quartil), 3� quartil e m�dia. J� para as colunas com valores n�o num�ricos,
# podemos ter acesso ao tamanho, � classe e ao modo.

#--------------------------------------------------------------------------------------------------------------
# 2) Crie um diagrama de correla��o entre todos os atributos e comente o que se observa.

# Exclus�o de dados n�o num�ricos dos dados originais

dados.numericos <- Filter(is.numeric,dados)

# Obten��o do diagrama de correla��o

dados.auxiliares <- cor(dados.numericos)
round(dados.auxiliares,3)
corrplot(dados.auxiliares)

#--------------------------------------------------------------------------------------------------------------
# 3) Obtenha um modelo de regress�o linear simples para a vari�vel objetivo para
# determinar o "total_deaths" usando o n�mero de novos casos ("new_cases")

# Holdout: 70% para treino e 30% teste

set.seed(123)

index <- sample(1:nrow(dados),0.7*nrow(dados))

dados.treino <- dados[index,]
dados.teste <- dados[-index, ]

# Cria��o do modelo

modelo.regressao.simples <- lm(total_deaths~new_cases, data=dados.treino)

# a) Apresente a fun��o linear resultante

summary(modelo.regressao.simples)

# A partir dos dados obtidos atrav�s da fun��o, conseguimos obter os coeficientes da reta de regress�o
# ??0 + ??1X.

# A fun��o linear resultante � a seguinte: total_deaths = 639,97 + 3,18 * new_cases

# b) Visualize a reta correspondente ao modelo de regress�o linear simples e o
# respetivo diagrama de dispers�o.

attach(dados.treino)

plot(new_cases,total_deaths, pch=20)
abline(modelo.regressao.simples, col = "blue")

detach(dados.treino)

# c) Calcule o erro m�dio absoluto (MAE) e raiz quadrada do erro m�dio (RMSE) do
# modelo sobre os 30% casos de teste.

# Previs�o do total_deaths a partir do modelo criado e dos dados de teste

previsoes.modelo <- predict(modelo.regressao.simples, dados.teste)

# Fun��o criada para calcular o erro m�dio absoluto

mae <- function(test, predicted) {
  mean(abs(test - predicted))
}

# Fun��o criada para calcular a raiz quadrada do erro m�dio

rmse <- function(test, predicted) {
  sqrt(mean((test-predicted)^2))
}

# Calcular o MAE e o RMSE dos dados de teste

attach(dados.teste)

mae(total_deaths, previsoes.modelo)
rmse(total_deaths, previsoes.modelo)

detach(dados.teste)

#-------------------------------------------------------------------------------------------------------------
# 4) Tendo em conta o conjunto de dados apresentado, pretende-se prever a esperan�a
# de vida ('life_expectancy')

# Efetuar normaliza��o minimax

minmaxnorm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dados.norm <- as.data.frame(lapply(dados.numericos, minmaxnorm))

# Holdout

set.seed(123)

index <- sample(1:nrow(dados.norm),0.7*nrow(dados.norm))

dados.treino <- dados.norm[index,]
dados.teste <- dados.norm[-index, ]

# a) Regress�o linear m�ltipla

# Cria��o do modelo

modelo.regressao.multipla <- lm(life_expectancy~., data=dados.treino)

summary(modelo.regressao.multipla)

# Fazer previs�o da vari�vel life_expectancy

previsoes.modelo.reg.multipla <- predict(modelo.regressao.multipla, dados.teste)

# Desnormalizar os dados

minmaxdesnorm <- function(x,goal.attrib) {
  return (x*(max(goal.attrib)-
               min(goal.attrib))+min(goal.attrib))
}

previsoes.modelo.reg.multipla <- minmaxdesnorm(previsoes.modelo.reg.multipla, dados$life_expectancy)
teste.life_expectancy <- minmaxdesnorm(dados.teste$life_expectancy, dados$life_expectancy)

# Calcular o MAE e o RMSE dos dados de teste

mae.regressao.multipla <- c(mae(teste.life_expectancy, previsoes.modelo.reg.multipla))
rmse.regressao.multipla <- c(rmse(teste.life_expectancy, previsoes.modelo.reg.multipla))

# b) �rvore de regress�o, usando a fun��o rpart. Apresente a �rvore de regress�o
# obtida.

# rpart()pode ser utilizada na Regress�o e
# Classifica��o:
# "class" �rvore de classifica��o
# "anova" �rvore de regress�o

# Obter a �rvore de regress�o

arvore.regressao <- rpart(life_expectancy~., method="anova", data=dados.treino)

# Visualiza��o da �rvore de regress�o

rpart.plot(arvore.regressao, digits=3)

# Fazer previs�o da vari�vel life_expectancy

previsoes.arvore.regressao <- predict(arvore.regressao, dados.teste)

# Desnormalizar os dados

previsoes.arvore.regressao <- minmaxdesnorm(previsoes.arvore.regressao, dados$life_expectancy)
teste.life_expectancy <- minmaxdesnorm(dados.teste$life_expectancy, dados$life_expectancy)

# Calcular o MAE e o RMSE dos dados de teste

mae.arvore.regressao <- c(mae(teste.life_expectancy, previsoes.arvore.regressao))
rmse.arvore.regressao <- c(rmse(teste.life_expectancy, previsoes.arvore.regressao))

# c) Rede neuronal usando a fun��o neuralnet, fazendo variar os par�metros.
# Apresente a rede obtida.

# Cria��o da rede neuronal

# Rede com 1 n� no n�vel interno
numnodes <- 1

# Rede com 3 n�s no n�vel interno
#numnodes <- 3

# Rede com 2 n�veis internos: 6, 2 n�s
#numnodes <- c(6,2)

modelo.rede.neuronal <- neuralnet(life_expectancy ~ population + population_density + median_age +
                        aged_65_older + aged_70_older + gdp_per_capita + extreme_poverty + cardiovasc_death_rate +
                         diabetes_prevalence + female_smokers + male_smokers + hospital_beds_per_thousand +
                         human_development_index + total_cases + new_cases + total_deaths + positive_rate +
                         stringency_index + reproduction_rate + Tot_dead_pop + incidence, data = dados.treino,
                      hidden=numnodes)

# Visualiza��o da topologia da rede

plot(modelo.rede.neuronal)

# Avalia��o as previs�es da rede

previsoes.rede.neuronal <- compute(modelo.rede.neuronal, dados.teste)

# Desnormalizar os dados

previsoes.rede.neuronal <- minmaxdesnorm(previsoes.rede.neuronal$net.result,dados$life_expectancy)
teste.life_expectancy <- minmaxdesnorm(dados.teste$life_expectancy,dados$life_expectancy)

# Calcular o MAE e o RMSE dos dados de teste

mae.rede.neuronal <- mae(teste.life_expectancy, previsoes.rede.neuronal)
rmse.rede.neuronal <- rmse(teste.life_expectancy, previsoes.rede.neuronal)

# Comparar valores do MAE e RMSE obtidos

data.frame(Modelo=c("Regressao linear m�ltipla", "�rvore de regress�o", "Rede neuronal"),
           MAE=c(mae.regressao.multipla,mae.arvore.regressao,mae.rede.neuronal),
           RMSE=c(rmse.regressao.multipla,rmse.arvore.regressao, rmse.rede.neuronal))

# De acordo com os resultados, podemos concluir que o modelo com pior desempenho � a regress�o linear m�ltipla.

#---------------------------------------------------------------------------------------------------------------
# Compara��o dos 2 melhores modelos

set.seed(123)

k <- 10
folds <- sample(1:k, nrow(dados.norm), replace = TRUE)

#Tamanho de cada fold

table(folds)

matrix_mae <- matrix(nrow = k, ncol = 2)
matrix_rmse <- matrix(nrow = k, ncol = 2)

for (i in 1:k){
  
  dados.treino <- dados.norm[folds != i,]
  dados.teste <- dados.norm[folds == i,]
  
  # �rvore de regress�o
  
  arvore.regressao <- rpart(life_expectancy~., method="anova", data=dados.treino)
  
  # Fazer previs�o da vari�vel life_expectancy
  
  previsoes.arvore.regressao <- predict(arvore.regressao, dados.teste)
  
  # Desnormalizar os dados
  
  previsoes.arvore.regressao <- minmaxdesnorm(previsoes.arvore.regressao, dados$life_expectancy)
  teste.life_expectancy <- minmaxdesnorm(dados.teste$life_expectancy, dados$life_expectancy)
  
  # Calcular o MAE e o RMSE dos dados de teste
  
  mae.arvore.regressao <- c(mae(teste.life_expectancy, previsoes.arvore.regressao))
  rmse.arvore.regressao <- c(rmse(teste.life_expectancy, previsoes.arvore.regressao))
  
  # Rede neuronal
  
  modelo.rede.neuronal <- neuralnet(life_expectancy ~ population + population_density + median_age +
                                      aged_65_older + aged_70_older + gdp_per_capita + extreme_poverty + cardiovasc_death_rate +
                                      diabetes_prevalence + female_smokers + male_smokers + hospital_beds_per_thousand +
                                      human_development_index + total_cases + new_cases + total_deaths + positive_rate +
                                      stringency_index + reproduction_rate + Tot_dead_pop + incidence, data = dados.treino,
                                    hidden=numnodes)
  
  # Avalia��o as previs�es da rede
  
  previsoes.rede.neuronal <- compute(modelo.rede.neuronal, dados.teste)
  
  # Desnormalizar os dados
  
  previsoes.rede.neuronal <- minmaxdesnorm(previsoes.rede.neuronal$net.result,dados$life_expectancy)
  teste.life_expectancy <- minmaxdesnorm(dados.teste$life_expectancy,dados$life_expectancy)
  
  # Calcular o MAE e o RMSE dos dados de teste
  
  mae.rede.neuronal <- mae(teste.life_expectancy, previsoes.rede.neuronal)
  rmse.rede.neuronal <- rmse(teste.life_expectancy, previsoes.rede.neuronal)
  
  # Adcionar valores do MAE e RMSE �s matrizes
  
  matrix_mae[i,] <- c(mae.arvore.regressao, mae.rede.neuronal)
  matrix_rmse[i,] <- c(rmse.arvore.regressao, rmse.rede.neuronal)
  
}

matrix_mae
matrix_rmse

apply(matrix_mae, 2, mean)
apply(matrix_rmse, 2, mean)

# Teste de m�dias para os 2 melhores modelos

# Hip�teses:

#H0: N�o existem diferen�as significativas no desempenho dos 2 modelos
#H1: Existem diferen�as significativas no desempenho dos 2 modelos

# Trata-se de um teste bilateral

t.test(matrix_mae[,1], matrix_mae[,2])
t.test(matrix_rmse[,1], matrix_rmse[,2])

# De acordo com os resultados do teste, obtivemos um p-value de 0.2256 (MAE) e 0.5118 (RMSE). Como ambos os 
# valores s�o superiores a 0.05, n�o se rejeita H0. Logo, podem existir ou n�o diferen�as significativas 
# no desempenho dos modelos.

#------------------------------------------------------------------------------------------------------------

# Classifica��o

# 5) Derive um novo atributo NiveldeRisco, discretizando o atributo "stringency_index"
# em 2 classes: "low" e "high" usando como valor de corte a m�dia do atributo.

# Cria��o de nova coluna NiveldeRisco, com os valores "high" e "low". Se o valor do stringency_index da linha
# for maior que a m�dia, o valor na coluna NiveldeRisco ser� "high", sen�o "low".

attach(dados)
niveldeRisco <- as.factor(ifelse(stringency_index > mean(stringency_index), "high", "low"))
detach(dados)

dados$NiveldeRisco <- niveldeRisco
dados.norm$NiveldeRisco <- niveldeRisco

# Visualiza��o do n� de ocorr�ncias dos valores "high" e "low"

table(dados$NiveldeRisco)

# Remo��o do atributo stringency_index

dados$stringency_index <- NULL
dados.norm$stringency_index <- NULL

# --------------------------------------------------------------------------------------------------------
# 6) 

# Holdout: 70% para treino e 30% para teste

set.seed(123)

index <-sample(1:nrow(dados.norm), 0.7*nrow(dados.norm))

dados.treino <- dados.norm[index, -22]
dados.teste <- dados.norm[-index, -22]

treino.nivelRisco <- dados.norm[index,22]
teste.nivelRisco <- dados.norm[-index,22]

# Determina��o do k que maximiza a taxa de acerto

set.seed(123)

k <- c()
taxa.acerto <- c()

for (i in seq(1, 50, 2)){
  
  previsao.modelo.knn <- knn(train=dados.treino, test=dados.teste, cl= treino.nivelRisco, k=i)
  m.conf <- table(teste.nivelRisco,previsao.modelo.knn)
  taxa.acerto <- c(taxa.acerto, sum(diag(m.conf))/sum(m.conf))
  k <- c(k,i)
  
}

valor.k <- k[which.max(taxa.acerto)]

# Utiliza��o do m�todo k-fold cross validation

set.seed(123)
numnodes<-1

k <- 10
folds <- sample(1:k, nrow(dados.norm), replace = TRUE)

#Tamanho de cada fold

table(folds)

cv.error <- matrix(nrow = k, ncol = 3)

for(i in 1:k) {
  
  train.cv <- dados.norm[folds != 1,]
  test.cv <- dados.norm[folds == 1,]
  
  train.nivelRisco <- dados[folds != 1, 24]
  tst.nivelRisco <- dados[folds == 1, 24]
  
  # a) �rvore de Decis�o
  
  arvore.decisao <- rpart(NiveldeRisco ~.,data=train.cv, method="class")
  
  previsao.modelo <- predict(arvore.decisao, test.cv, type="class")
  
  # Matrix de confus�o
  
  matrix.conf.arvore.decisao <- table(test.cv$NiveldeRisco,previsao.modelo)
  
  # Obten��o da taxa de acerto de previs�o
  
  taxa.acerto.arvore.decisao <- (matrix.conf.arvore.decisao[1,1]+matrix.conf.arvore.decisao[2,2])/sum(matrix.conf.arvore.decisao)
  
  # b) Rede neuronal
  
  modelo.rede.neuronal <- neuralnet(NiveldeRisco ~ population + population_density + median_age +
                           aged_65_older + aged_70_older + gdp_per_capita + extreme_poverty + cardiovasc_death_rate +
                           diabetes_prevalence + female_smokers + male_smokers + hospital_beds_per_thousand +
                           life_expectancy + human_development_index + total_cases + new_cases + total_deaths + positive_rate +
                           reproduction_rate + Tot_dead_pop + incidence, data = train.cv, hidden=numnodes, linear.output = F)
  
  # Avalia��o as previs�es da rede
  
  modelo.previsao.rede.neuronal <- compute(modelo.rede.neuronal, test.cv[,1:21])
  idx <- apply(modelo.previsao.rede.neuronal$net.result, 1, which.max)
  modelo <- as.factor(c('high', 'low')[idx])
  
  # Matrix de confus�o
  
  matrix.conf.rede.neuronal <- table(tst.nivelRisco,modelo)
  
  # Obten��o da taxa de acerto de previs�o
  
  taxa.acerto.rede.neuronal <- (matrix.conf.rede.neuronal[1,1]+matrix.conf.rede.neuronal[2,2])/sum(matrix.conf.rede.neuronal)
  
  # c) K-vizinhos-mais-pr�ximos
  
  modelo.previsao.knn <- knn(train=train.cv[,-22], test=test.cv[,-22], cl= train.nivelRisco, k=valor.k)
  matrix.conf.knn <- table(tst.nivelRisco,modelo.previsao.knn)
  
  # Obten��o da taxa de acerto de previs�o
  
  taxa.acerto.knn <- (matrix.conf.knn[1,1]+matrix.conf.knn[2,2])/sum(matrix.conf.knn)
  
  # Guardar taxas de acerto
  
  cv.error[i,] <- c(taxa.acerto.arvore.decisao, taxa.acerto.rede.neuronal, taxa.acerto.knn)
  
}

cv.error

# Determinar a m�dia das taxas de acerto

apply(cv.error,2,mean)

# Determinar o desvio padr�o das taxas de acerto

apply(cv.error,2,sd)

# Ao observamos os resultados, podemos concluir que o modelo com pior desempenho � a �rvore de decis�o, com
# o valor m�dio das taxas de acerto da previs�o do atributo mais baixo.

# Teste de m�dias para os 2 melhores modelos (rede neuronal e k-vizinhos-mais-pr�ximos)

# Hip�teses:

#H0: N�o existem diferen�as significativas no desempenho dos 2 modelos
#H1: Existem diferen�as significativas no desempenho dos 2 modelos

# Trata-se de um teste bilateral

t.test(cv.error[,1], cv.error[,3])

# De acordo com os resultados do teste, obtivemos um p-value de 0.3908. Como o valor � superior a 0.05, n�o se
# rejeita H0. Logo, podem existir ou n�o diferen�as significativas no desempenho dos modelos.

# ---------------------------------------------------------------------------------------------------------------
# 7) Derive um novo atributo ClassedeRisco, discretizando o atributo
# "reproduction_rate" - Taxa de Transmissibilidade R(t) e o atributo "incidence" -
# Incid�ncia em 3 classes (conforme Figura 1): "Vermelho", "Amarelo" e "Verde". Por
# simplifica��o considera-se que a Incid�ncia corresponde � raz�o entre o atributo
# "total_cases" e "population" multiplicado por 100.000 habitantes.

# Cria��o de nova coluna ClassedeRisco, com os valores "Vermelho", "Amarelo" e "Verde".

# Para ser verde: reproduction_rate deve ser inferior a 1 & Incid�ncia deve ser inferior a 120
# Para ser vermelho: reproduction_rate deve ser superior a 1 & Incid�ncia deve ser superior a 120
# Para ser amarelo: resto


attach(dados)
classedeRisco <- as.factor(ifelse(reproduction_rate < 1 & ((total_cases/population) * 100000) < 120, "Verde", 
                                  ifelse(reproduction_rate > 1 & ((total_cases/population) * 100000) > 120, "Vermelho", "Amarelo")))
detach(dados)

dados$ClassedeRisco <- classedeRisco
dados.norm$ClassedeRisco <- classedeRisco

# Visualiza��o do n� de ocorr�ncias dos valores "Vermelho", "Verde" e "Amarelo"

table(dados$ClassedeRisco)

# Remo��o do atributo reproduction_rate

dados$reproduction_rate <- NULL
dados.norm$reproduction_rate <- NULL

# Remo��o do atributo incidence

dados$incidence <- NULL
dados.norm$incidence <- NULL

#-----------------------------------------------------------------------------------------------------------
# 8) 

# Converter coluna "NiveldeRisco" dos dados normalizados para num�rica

dados.norm$NiveldeRisco <- as.numeric(dados.norm$NiveldeRisco)

# Holdout

set.seed(123)

index <-sample(1:nrow(dados.norm), 0.7*nrow(dados.norm))

dados.treino <- dados.norm[index, -21]
dados.teste <- dados.norm[-index, -21]

train.classeRisco <- dados.norm[index,21]
tst.classeRisco <- dados.norm[-index,21]

# Determina��o do k que maximiza a taxa de acerto

set.seed(123)

k <- c()
taxa.acerto <- c()

for (i in seq(1, 50, 2)){
  
  modelo.previsao.knn <- knn(train=dados.treino, test=dados.teste, cl= train.classeRisco, k=i)
  m.conf <- table(tst.classeRisco,modelo.previsao.knn)
  taxa.acerto <- c(taxa.acerto, sum(diag(m.conf))/sum(m.conf))
  k <- c(k,i)
  
}

valor.k <- k[which.max(taxa.acerto)]

# Utiliza��o do m�todo k-fold cross validation

set.seed(123)
numnodes<-c(6,2)

k <- 10
folds <- sample(1:k, nrow(dados.norm), replace = TRUE)

#Tamanho de cada fold

table(folds)

matrix_accuracy <- matrix(nrow = k, ncol = 3)
matrix_sensitivity <- matrix(nrow = k, ncol = 3)
matrix_specificity <- matrix(nrow = k, ncol = 3)
matrix_f1 <- matrix(nrow = k, ncol = 3)

for(i in 1:k) {
  
  train.cv <- dados.norm[folds != i,]
  test.cv <- dados.norm[folds == i,]
  
  train.classeRisco <- dados[folds != i, 23]
  tst.classeRisco <- dados[folds == i, 23]
  
  # a) �rvore de Decis�o
  
  arvore.decisao <- rpart(ClassedeRisco ~.,data=train.cv, method="class")
  
  modelo.previsao <- predict(arvore.decisao, test.cv, type="class")
  
  # M�tricas da matriz de confus�o

  metricas.arvore.decisao <- ml_test(tst.classeRisco, modelo.previsao)
  
  accuracy.arvore.decisao <- metricas.arvore.decisao$accuracy
  
  sensitivity.arvore.decisao <- mean(metricas.arvore.decisao$recall)
  
  specificty.arvore.decisao <- mean(metricas.arvore.decisao$specificity)
  
  F1.arvore.decisao <- mean(metricas.arvore.decisao$F1)
  
  # b) Rede neuronal
  
  modelo.rede.neuronal <- neuralnet(ClassedeRisco ~ population + population_density + median_age +
                                      aged_65_older + aged_70_older + gdp_per_capita + extreme_poverty + cardiovasc_death_rate +
                                      diabetes_prevalence + female_smokers + male_smokers + hospital_beds_per_thousand +
                                      human_development_index + total_cases + new_cases + total_deaths + positive_rate +
                                      Tot_dead_pop + NiveldeRisco, data = train.cv,
                                    hidden=numnodes, linear.output = F)
  
  # Avalia��o as previs�es da rede
  
  modelo.previsao.rede.neuronal <- compute(modelo.rede.neuronal, test.cv[,-21])
  idx <- apply(modelo.previsao.rede.neuronal$net.result, 1, which.max)
  modelo <- as.factor(c('Amarelo', 'Verde', 'Vermelho')[idx])
  
  # M�tricas da matriz de confus�o
  
  metricas.rede.neuronal <- ml_test(tst.classeRisco, modelo)
  
  accuracy.rede.neuronal <- metricas.rede.neuronal$accuracy
  
  sensitivity.rede.neuronal <- mean(metricas.rede.neuronal$recall)
  
  specificty.rede.neuronal <- mean(metricas.rede.neuronal$specificity)
  
  F1.rede.neuronal <- mean(metricas.rede.neuronal$F1)
  
  # c) K-vizinhos-mais-pr�ximos
  
  modelo.previsao.knn <- knn(train=train.cv[,-21], test=test.cv[,-21], cl= train.classeRisco, k=valor.k)

  # M�tricas da matriz de confus�o
  
  metricas.knn <- ml_test(tst.classeRisco, modelo.previsao.knn)
  
  accuracy.knn <- metricas.knn$accuracy
  
  sensitivity.knn <- mean(metricas.knn$recall)
  
  specificty.knn <- mean(metricas.knn$specificity)
  
  F1.knn <- mean(metricas.knn$F1)
  
  # Guardar m�tricas de cada modelo
  
  matrix_accuracy <- c(accuracy.arvore.decisao, accuracy.rede.neuronal, accuracy.knn)
  matrix_sensitivity <- c(sensitivity.arvore.decisao, sensitivity.rede.neuronal, sensitivity.knn)
  matrix_specificity <- c(specificty.arvore.decisao, specificty.rede.neuronal, specificty.knn)
  matrix_f1 <- c(F1.arvore.decisao, F1.rede.neuronal, F1.knn)
  
}

# Visualiza��o das matrizes

matrix_accuracy
matrix_sensitivity
matrix_specificity
matrix_f1

# Devido � exist�ncia de um erro que n�o se conseguiu resolver, optou-se por comparar apenas os valores 
# da 1� itera��o


