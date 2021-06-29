# Trabalho prático 2
#
# Comando para limpar o ambiente do RStudio

rm(list=ls())

# Comando geral para importar packages

if(!require(pacman)) install.packages("pacman") ; library(pacman)

pacman::p_load(PerformanceAnalytics, corrplot, rpart, rpart.plot, neuralnet, class, FNN, stats, mltest)


#-----------------------------------------------------------------------------------------------------------------
# Regressão
#
# 1) Comece por carregar o ficheiro ("countryagregatedata.xlsx") para o ambiente do R,
# verifique a sua dimensão e obtenha um sumário dos dados.

# Comando geral com criação de estrutura a ser usada por todos os exercícios

dados <- read.csv("~/countryagregatedata.csv",sep=",")

# Função usada para mostrar as primeiras x linhas da estrutura (sendo x o segundo argumento)

head(dados, 4)

# Eliminação da coluna Row

dados$row <- NULL

# Função usada para verificar o número de linhas e de colunas da estrutura

dim(dados)

# Função utilizada para obter o sumário dos dados da estrutura

summary(dados)

# Para as colunas cujos valores são numéricos, podemos constatar o mínimo, máximo,
# 1º quartil, mediana (ou 2º quartil), 3º quartil e média. Já para as colunas com valores não numéricos,
# podemos ter acesso ao tamanho, à classe e ao modo.

#--------------------------------------------------------------------------------------------------------------
# 2) Crie um diagrama de correlação entre todos os atributos e comente o que se observa.

# Exclusão de dados não numéricos dos dados originais

dados.numericos <- Filter(is.numeric,dados)

# Obtenção do diagrama de correlação

dados.auxiliares <- cor(dados.numericos)
round(dados.auxiliares,3)
corrplot(dados.auxiliares)

#--------------------------------------------------------------------------------------------------------------
# 3) Obtenha um modelo de regressão linear simples para a variável objetivo para
# determinar o "total_deaths" usando o número de novos casos ("new_cases")

# Holdout: 70% para treino e 30% teste

set.seed(123)

index <- sample(1:nrow(dados),0.7*nrow(dados))

dados.treino <- dados[index,]
dados.teste <- dados[-index, ]

# Criação do modelo

modelo.regressao.simples <- lm(total_deaths~new_cases, data=dados.treino)

# a) Apresente a função linear resultante

summary(modelo.regressao.simples)

# A partir dos dados obtidos através da função, conseguimos obter os coeficientes da reta de regressão
# ??0 + ??1X.

# A função linear resultante é a seguinte: total_deaths = 639,97 + 3,18 * new_cases

# b) Visualize a reta correspondente ao modelo de regressão linear simples e o
# respetivo diagrama de dispersão.

attach(dados.treino)

plot(new_cases,total_deaths, pch=20)
abline(modelo.regressao.simples, col = "blue")

detach(dados.treino)

# c) Calcule o erro médio absoluto (MAE) e raiz quadrada do erro médio (RMSE) do
# modelo sobre os 30% casos de teste.

# Previsão do total_deaths a partir do modelo criado e dos dados de teste

previsoes.modelo <- predict(modelo.regressao.simples, dados.teste)

# Função criada para calcular o erro médio absoluto

mae <- function(test, predicted) {
  mean(abs(test - predicted))
}

# Função criada para calcular a raiz quadrada do erro médio

rmse <- function(test, predicted) {
  sqrt(mean((test-predicted)^2))
}

# Calcular o MAE e o RMSE dos dados de teste

attach(dados.teste)

mae(total_deaths, previsoes.modelo)
rmse(total_deaths, previsoes.modelo)

detach(dados.teste)

#-------------------------------------------------------------------------------------------------------------
# 4) Tendo em conta o conjunto de dados apresentado, pretende-se prever a esperança
# de vida ('life_expectancy')

# Efetuar normalização minimax

minmaxnorm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dados.norm <- as.data.frame(lapply(dados.numericos, minmaxnorm))

# Holdout

set.seed(123)

index <- sample(1:nrow(dados.norm),0.7*nrow(dados.norm))

dados.treino <- dados.norm[index,]
dados.teste <- dados.norm[-index, ]

# a) Regressão linear múltipla

# Criação do modelo

modelo.regressao.multipla <- lm(life_expectancy~., data=dados.treino)

summary(modelo.regressao.multipla)

# Fazer previsão da variável life_expectancy

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

# b) Árvore de regressão, usando a função rpart. Apresente a árvore de regressão
# obtida.

# rpart()pode ser utilizada na Regressão e
# Classificação:
# "class" Árvore de classificação
# "anova" Árvore de regressão

# Obter a árvore de regressão

arvore.regressao <- rpart(life_expectancy~., method="anova", data=dados.treino)

# Visualização da árvore de regressão

rpart.plot(arvore.regressao, digits=3)

# Fazer previsão da variável life_expectancy

previsoes.arvore.regressao <- predict(arvore.regressao, dados.teste)

# Desnormalizar os dados

previsoes.arvore.regressao <- minmaxdesnorm(previsoes.arvore.regressao, dados$life_expectancy)
teste.life_expectancy <- minmaxdesnorm(dados.teste$life_expectancy, dados$life_expectancy)

# Calcular o MAE e o RMSE dos dados de teste

mae.arvore.regressao <- c(mae(teste.life_expectancy, previsoes.arvore.regressao))
rmse.arvore.regressao <- c(rmse(teste.life_expectancy, previsoes.arvore.regressao))

# c) Rede neuronal usando a função neuralnet, fazendo variar os parâmetros.
# Apresente a rede obtida.

# Criação da rede neuronal

# Rede com 1 nó no nível interno
numnodes <- 1

# Rede com 3 nós no nível interno
#numnodes <- 3

# Rede com 2 níveis internos: 6, 2 nós
#numnodes <- c(6,2)

modelo.rede.neuronal <- neuralnet(life_expectancy ~ population + population_density + median_age +
                        aged_65_older + aged_70_older + gdp_per_capita + extreme_poverty + cardiovasc_death_rate +
                         diabetes_prevalence + female_smokers + male_smokers + hospital_beds_per_thousand +
                         human_development_index + total_cases + new_cases + total_deaths + positive_rate +
                         stringency_index + reproduction_rate + Tot_dead_pop + incidence, data = dados.treino,
                      hidden=numnodes)

# Visualização da topologia da rede

plot(modelo.rede.neuronal)

# Avaliação as previsões da rede

previsoes.rede.neuronal <- compute(modelo.rede.neuronal, dados.teste)

# Desnormalizar os dados

previsoes.rede.neuronal <- minmaxdesnorm(previsoes.rede.neuronal$net.result,dados$life_expectancy)
teste.life_expectancy <- minmaxdesnorm(dados.teste$life_expectancy,dados$life_expectancy)

# Calcular o MAE e o RMSE dos dados de teste

mae.rede.neuronal <- mae(teste.life_expectancy, previsoes.rede.neuronal)
rmse.rede.neuronal <- rmse(teste.life_expectancy, previsoes.rede.neuronal)

# Comparar valores do MAE e RMSE obtidos

data.frame(Modelo=c("Regressao linear múltipla", "Árvore de regressão", "Rede neuronal"),
           MAE=c(mae.regressao.multipla,mae.arvore.regressao,mae.rede.neuronal),
           RMSE=c(rmse.regressao.multipla,rmse.arvore.regressao, rmse.rede.neuronal))

# De acordo com os resultados, podemos concluir que o modelo com pior desempenho é a regressão linear múltipla.

#---------------------------------------------------------------------------------------------------------------
# Comparação dos 2 melhores modelos

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
  
  # Árvore de regressão
  
  arvore.regressao <- rpart(life_expectancy~., method="anova", data=dados.treino)
  
  # Fazer previsão da variável life_expectancy
  
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
  
  # Avaliação as previsões da rede
  
  previsoes.rede.neuronal <- compute(modelo.rede.neuronal, dados.teste)
  
  # Desnormalizar os dados
  
  previsoes.rede.neuronal <- minmaxdesnorm(previsoes.rede.neuronal$net.result,dados$life_expectancy)
  teste.life_expectancy <- minmaxdesnorm(dados.teste$life_expectancy,dados$life_expectancy)
  
  # Calcular o MAE e o RMSE dos dados de teste
  
  mae.rede.neuronal <- mae(teste.life_expectancy, previsoes.rede.neuronal)
  rmse.rede.neuronal <- rmse(teste.life_expectancy, previsoes.rede.neuronal)
  
  # Adcionar valores do MAE e RMSE às matrizes
  
  matrix_mae[i,] <- c(mae.arvore.regressao, mae.rede.neuronal)
  matrix_rmse[i,] <- c(rmse.arvore.regressao, rmse.rede.neuronal)
  
}

matrix_mae
matrix_rmse

apply(matrix_mae, 2, mean)
apply(matrix_rmse, 2, mean)

# Teste de médias para os 2 melhores modelos

# Hipóteses:

#H0: Não existem diferenças significativas no desempenho dos 2 modelos
#H1: Existem diferenças significativas no desempenho dos 2 modelos

# Trata-se de um teste bilateral

t.test(matrix_mae[,1], matrix_mae[,2])
t.test(matrix_rmse[,1], matrix_rmse[,2])

# De acordo com os resultados do teste, obtivemos um p-value de 0.2256 (MAE) e 0.5118 (RMSE). Como ambos os 
# valores são superiores a 0.05, não se rejeita H0. Logo, podem existir ou não diferenças significativas 
# no desempenho dos modelos.

#------------------------------------------------------------------------------------------------------------

# Classificação

# 5) Derive um novo atributo NiveldeRisco, discretizando o atributo "stringency_index"
# em 2 classes: "low" e "high" usando como valor de corte a média do atributo.

# Criação de nova coluna NiveldeRisco, com os valores "high" e "low". Se o valor do stringency_index da linha
# for maior que a média, o valor na coluna NiveldeRisco será "high", senão "low".

attach(dados)
niveldeRisco <- as.factor(ifelse(stringency_index > mean(stringency_index), "high", "low"))
detach(dados)

dados$NiveldeRisco <- niveldeRisco
dados.norm$NiveldeRisco <- niveldeRisco

# Visualização do nº de ocorrências dos valores "high" e "low"

table(dados$NiveldeRisco)

# Remoção do atributo stringency_index

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

# Determinação do k que maximiza a taxa de acerto

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

# Utilização do método k-fold cross validation

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
  
  # a) Árvore de Decisão
  
  arvore.decisao <- rpart(NiveldeRisco ~.,data=train.cv, method="class")
  
  previsao.modelo <- predict(arvore.decisao, test.cv, type="class")
  
  # Matrix de confusão
  
  matrix.conf.arvore.decisao <- table(test.cv$NiveldeRisco,previsao.modelo)
  
  # Obtenção da taxa de acerto de previsão
  
  taxa.acerto.arvore.decisao <- (matrix.conf.arvore.decisao[1,1]+matrix.conf.arvore.decisao[2,2])/sum(matrix.conf.arvore.decisao)
  
  # b) Rede neuronal
  
  modelo.rede.neuronal <- neuralnet(NiveldeRisco ~ population + population_density + median_age +
                           aged_65_older + aged_70_older + gdp_per_capita + extreme_poverty + cardiovasc_death_rate +
                           diabetes_prevalence + female_smokers + male_smokers + hospital_beds_per_thousand +
                           life_expectancy + human_development_index + total_cases + new_cases + total_deaths + positive_rate +
                           reproduction_rate + Tot_dead_pop + incidence, data = train.cv, hidden=numnodes, linear.output = F)
  
  # Avaliação as previsões da rede
  
  modelo.previsao.rede.neuronal <- compute(modelo.rede.neuronal, test.cv[,1:21])
  idx <- apply(modelo.previsao.rede.neuronal$net.result, 1, which.max)
  modelo <- as.factor(c('high', 'low')[idx])
  
  # Matrix de confusão
  
  matrix.conf.rede.neuronal <- table(tst.nivelRisco,modelo)
  
  # Obtenção da taxa de acerto de previsão
  
  taxa.acerto.rede.neuronal <- (matrix.conf.rede.neuronal[1,1]+matrix.conf.rede.neuronal[2,2])/sum(matrix.conf.rede.neuronal)
  
  # c) K-vizinhos-mais-próximos
  
  modelo.previsao.knn <- knn(train=train.cv[,-22], test=test.cv[,-22], cl= train.nivelRisco, k=valor.k)
  matrix.conf.knn <- table(tst.nivelRisco,modelo.previsao.knn)
  
  # Obtenção da taxa de acerto de previsão
  
  taxa.acerto.knn <- (matrix.conf.knn[1,1]+matrix.conf.knn[2,2])/sum(matrix.conf.knn)
  
  # Guardar taxas de acerto
  
  cv.error[i,] <- c(taxa.acerto.arvore.decisao, taxa.acerto.rede.neuronal, taxa.acerto.knn)
  
}

cv.error

# Determinar a média das taxas de acerto

apply(cv.error,2,mean)

# Determinar o desvio padrão das taxas de acerto

apply(cv.error,2,sd)

# Ao observamos os resultados, podemos concluir que o modelo com pior desempenho é a árvore de decisão, com
# o valor médio das taxas de acerto da previsão do atributo mais baixo.

# Teste de médias para os 2 melhores modelos (rede neuronal e k-vizinhos-mais-próximos)

# Hipóteses:

#H0: Não existem diferenças significativas no desempenho dos 2 modelos
#H1: Existem diferenças significativas no desempenho dos 2 modelos

# Trata-se de um teste bilateral

t.test(cv.error[,1], cv.error[,3])

# De acordo com os resultados do teste, obtivemos um p-value de 0.3908. Como o valor é superior a 0.05, não se
# rejeita H0. Logo, podem existir ou não diferenças significativas no desempenho dos modelos.

# ---------------------------------------------------------------------------------------------------------------
# 7) Derive um novo atributo ClassedeRisco, discretizando o atributo
# "reproduction_rate" - Taxa de Transmissibilidade R(t) e o atributo "incidence" -
# Incidência em 3 classes (conforme Figura 1): "Vermelho", "Amarelo" e "Verde". Por
# simplificação considera-se que a Incidência corresponde à razão entre o atributo
# "total_cases" e "population" multiplicado por 100.000 habitantes.

# Criação de nova coluna ClassedeRisco, com os valores "Vermelho", "Amarelo" e "Verde".

# Para ser verde: reproduction_rate deve ser inferior a 1 & Incidência deve ser inferior a 120
# Para ser vermelho: reproduction_rate deve ser superior a 1 & Incidência deve ser superior a 120
# Para ser amarelo: resto


attach(dados)
classedeRisco <- as.factor(ifelse(reproduction_rate < 1 & ((total_cases/population) * 100000) < 120, "Verde", 
                                  ifelse(reproduction_rate > 1 & ((total_cases/population) * 100000) > 120, "Vermelho", "Amarelo")))
detach(dados)

dados$ClassedeRisco <- classedeRisco
dados.norm$ClassedeRisco <- classedeRisco

# Visualização do nº de ocorrências dos valores "Vermelho", "Verde" e "Amarelo"

table(dados$ClassedeRisco)

# Remoção do atributo reproduction_rate

dados$reproduction_rate <- NULL
dados.norm$reproduction_rate <- NULL

# Remoção do atributo incidence

dados$incidence <- NULL
dados.norm$incidence <- NULL

#-----------------------------------------------------------------------------------------------------------
# 8) 

# Converter coluna "NiveldeRisco" dos dados normalizados para numérica

dados.norm$NiveldeRisco <- as.numeric(dados.norm$NiveldeRisco)

# Holdout

set.seed(123)

index <-sample(1:nrow(dados.norm), 0.7*nrow(dados.norm))

dados.treino <- dados.norm[index, -21]
dados.teste <- dados.norm[-index, -21]

train.classeRisco <- dados.norm[index,21]
tst.classeRisco <- dados.norm[-index,21]

# Determinação do k que maximiza a taxa de acerto

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

# Utilização do método k-fold cross validation

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
  
  # a) Árvore de Decisão
  
  arvore.decisao <- rpart(ClassedeRisco ~.,data=train.cv, method="class")
  
  modelo.previsao <- predict(arvore.decisao, test.cv, type="class")
  
  # Métricas da matriz de confusão

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
  
  # Avaliação as previsões da rede
  
  modelo.previsao.rede.neuronal <- compute(modelo.rede.neuronal, test.cv[,-21])
  idx <- apply(modelo.previsao.rede.neuronal$net.result, 1, which.max)
  modelo <- as.factor(c('Amarelo', 'Verde', 'Vermelho')[idx])
  
  # Métricas da matriz de confusão
  
  metricas.rede.neuronal <- ml_test(tst.classeRisco, modelo)
  
  accuracy.rede.neuronal <- metricas.rede.neuronal$accuracy
  
  sensitivity.rede.neuronal <- mean(metricas.rede.neuronal$recall)
  
  specificty.rede.neuronal <- mean(metricas.rede.neuronal$specificity)
  
  F1.rede.neuronal <- mean(metricas.rede.neuronal$F1)
  
  # c) K-vizinhos-mais-próximos
  
  modelo.previsao.knn <- knn(train=train.cv[,-21], test=test.cv[,-21], cl= train.classeRisco, k=valor.k)

  # Métricas da matriz de confusão
  
  metricas.knn <- ml_test(tst.classeRisco, modelo.previsao.knn)
  
  accuracy.knn <- metricas.knn$accuracy
  
  sensitivity.knn <- mean(metricas.knn$recall)
  
  specificty.knn <- mean(metricas.knn$specificity)
  
  F1.knn <- mean(metricas.knn$F1)
  
  # Guardar métricas de cada modelo
  
  matrix_accuracy <- c(accuracy.arvore.decisao, accuracy.rede.neuronal, accuracy.knn)
  matrix_sensitivity <- c(sensitivity.arvore.decisao, sensitivity.rede.neuronal, sensitivity.knn)
  matrix_specificity <- c(specificty.arvore.decisao, specificty.rede.neuronal, specificty.knn)
  matrix_f1 <- c(F1.arvore.decisao, F1.rede.neuronal, F1.knn)
  
}

# Visualização das matrizes

matrix_accuracy
matrix_sensitivity
matrix_specificity
matrix_f1

# Devido à existência de um erro que não se conseguiu resolver, optou-se por comparar apenas os valores 
# da 1º iteração


