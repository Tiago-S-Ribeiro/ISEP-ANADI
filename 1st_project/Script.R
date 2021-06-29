# Trabalho prático 1
#
# Comando geral com criação de estrutura a ser usada por todos os exercícios

dados <- read.csv("~/owid-covid-data.csv",sep=",")

# Biblioteca necessária para o uso da função ggpplot()

if(!require(ggplot2)) install.packages("ggplot2") ; library(ggplot2)

# ------------------------------------------------------------------------------

# 4.1. Análise de Dados
#
# a) Gráfico que mostra o número total de infetados ao longo do período de tempo (indicado no ficheiro
# de dados), por continente.

# Criação de uma estrutura de dados com as colunas LOCATION, TOTAL_CASES e DATE

attach(dados)
novos_dados <- na.omit(data.frame(location,total_cases,date))
detach(dados)

# Criação de estruturas com os dados extraídos referentes a cada continente

dados_Africa <- subset(novos_dados,location == "Africa")
dados_Asia <- subset(novos_dados,location == "Asia")
dados_Europa <- subset(novos_dados,location == "Europe")
dados_America_Norte <- subset(novos_dados,location == "North America")
dados_Oceania <- subset(novos_dados,location == "Oceania")
dados_America_Sul <- subset(novos_dados,location == "South America")

# Criação do gráfico

gráfico_1a <- ggplot() + 
  geom_line(data=dados_Asia, aes(x=as.Date(date),y=total_cases,colour="Asia")) +
  geom_line(data=dados_Africa, aes(x=as.Date(date),y=total_cases,colour="África")) +
  geom_line(data=dados_Europa, aes(x=as.Date(date),y=total_cases,colour="Europa")) +
  geom_line(data=dados_America_Norte, aes(x=as.Date(date),y=total_cases,colour="América do Norte")) +
  geom_line(data=dados_Oceania, aes(x=as.Date(date),y=total_cases,colour="Oceania")) +
  geom_line(data=dados_America_Sul, aes(x=as.Date(date),y=total_cases,colour="América do Sul")) +
  xlab("Tempo") + ylab("Total Infetados") + labs(title="Gráfico") +
  scale_colour_manual("",values=c("Asia"="black","África"="red","Europa"="orange","América do Norte"="purple",
                                  "Oceania"="blue","América do Sul"="green")) +
  theme_bw()

# b) Gráfico do total de infetados por milhão de habitantes, ao longo do período de tempo, por continente. 

# Criação de uma estrutura de dados com as colunas LOCATION, TOTAL_CASES_PER_MILLION e DATE

attach(dados)
novos_dados <- na.omit(data.frame(location,total_cases_per_million,date))
detach(dados)

# Criação de estruturas com os dados extraídos referentes a cada continente

dados_Africa <- subset(novos_dados,location == "Africa")
dados_Asia <- subset(novos_dados,location == "Asia")
dados_Europa <- subset(novos_dados,location == "Europe")
dados_America_Norte <- subset(novos_dados,location == "North America")
dados_Oceania <- subset(novos_dados,location == "Oceania")
dados_America_Sul <- subset(novos_dados,location == "South America")

# Criação do gráfico

gráfico_1b <- ggplot() + 
  geom_line(data=dados_Asia, aes(x=as.Date(date),y=total_cases_per_million,colour="Asia")) +
  geom_line(data=dados_Africa, aes(x=as.Date(date),y=total_cases_per_million,colour="África")) +
  geom_line(data=dados_Europa, aes(x=as.Date(date),y=total_cases_per_million,colour="Europa")) +
  geom_line(data=dados_America_Norte, aes(x=as.Date(date),y=total_cases_per_million,colour="América do Norte")) +
  geom_line(data=dados_Oceania, aes(x=as.Date(date),y=total_cases_per_million,colour="Oceania")) +
  geom_line(data=dados_America_Sul, aes(x=as.Date(date),y=total_cases_per_million,colour="América do Sul")) +
  xlab("Tempo") + ylab("Total Infetados por milhão de habitantes") + labs(title="Gráfico") +
  scale_colour_manual("",values=c("Asia"="black","África"="red","Europa"="orange","América do Norte"="purple",
                                  "Oceania"="blue","América do Sul"="green")) +
  theme_bw()

# c) Um boxplot do número de mortos diários por milhão de habitantes para cada um dos seguintes países:
# Portugal, Espanha, Itália e Reino Unido. Remova os outliers.

# https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/

# Criação de uma estrutura de dados com as colunas LOCATION e NEW_DEATHS_PER_MILLION

attach(dados)
novos_dados <- na.omit(data.frame(location,new_deaths_per_million))
detach(dados)

# Criação de um vetor com os valores de mortes diárias por milhão para Portugal

subset_Portugal <- subset(novos_dados, location == "Portugal")
dados_Portugal <- c(subset_Portugal$new_deaths_per_million)

# Determinação dos quartis (1º e 3º), da amplitude interquartil e dos limites inferior e superior, respetivamente

Quartis <- quantile(dados_Portugal,probs=c(.25, .75))
AIQ <- IQR(dados_Portugal)
limite_superior <- Quartis[2]+1.5*AIQ
limite_inferior <- Quartis[1]-1.5*AIQ

# Remoção dos outliers

dados_Portugal <- dados_Portugal[dados_Portugal > limite_inferior & dados_Portugal < limite_superior]

# Criação de um vetor com os valores de mortes diárias por milhão para Espanha

subset_Espanha <- subset(novos_dados, location == "Spain")
dados_Espanha <- c(subset_Espanha$new_deaths_per_million)

# Determinação dos quartis (1º e 3º), da amplitude interquartil e dos limites inferior e superior, respetivamente

Quartis <- quantile(dados_Espanha,probs=c(.25, .75))
AIQ <- IQR(dados_Espanha)
limite_superior <- Quartis[2]+1.5*AIQ
limite_inferior <- Quartis[1]-1.5*AIQ

# Remoção dos outliers

dados_Espanha <- dados_Espanha[dados_Espanha > limite_inferior & dados_Espanha < limite_superior]

# Criação de um vetor com os valores de mortes diárias por milhão para Itália

subset_Italia <- subset(novos_dados, location == "Italy")
dados_Italia <- c(subset_Italia$new_deaths_per_million)

# Determinação dos quartis (1º e 3º), da amplitude interquartil e dos limites inferior e superior, respetivamente

Quartis <- quantile(dados_Italia,probs=c(.25, .75))
AIQ <- IQR(dados_Italia)
limite_superior <- Quartis[2]+1.5*AIQ
limite_inferior <- Quartis[1]-1.5*AIQ

# Remoção dos outliers

dados_Italia <- dados_Italia[dados_Italia > limite_inferior & dados_Italia < limite_superior]

# Criação de um vetor com os valores de mortes diárias por milhão para Reino Unido

subset_Reino_Unido <- subset(novos_dados, location == "United Kingdom")
dados_Reino_Unido <- c(subset_Reino_Unido$new_deaths_per_million)

# Determinação dos quartis (1º e 3º), da amplitude interquartil e dos limites inferior e superior, respetivamente

Quartis <- quantile(dados_Reino_Unido,probs=c(.25, .75))
AIQ <- IQR(dados_Reino_Unido)
limite_superior <- Quartis[2]+1.5*AIQ
limite_inferior <- Quartis[1]-1.5*AIQ

# Remoção dos outliers

dados_Reino_Unido <- dados_Reino_Unido[dados_Reino_Unido > limite_inferior & dados_Reino_Unido < limite_superior]

# Criação de variáveis a serem usadas no boxplot

nomes <- c("Portugal", "Espanha", "Itália", "Reino Unido")
titulo <- "Boxplot do número de mortes diárias por milhão de habitantes"

# Criação do boxplot

boxplot_1c <- boxplot(dados_Portugal, dados_Espanha, dados_Italia, dados_Reino_Unido, names=nomes, main=titulo, 
                      ylab="Número mortos diários")

# d) Um gráfico de barras com o número total de mortos, por milhão de habitantes, e o nº de testes diários
# por milhar de habitantes, para os países: Albânia, Dinamarca, Alemanha e Rússia.

# Criação de uma estrutura de dados com as colunas LOCATION e TOTAL_DEATHS_PER_MILLION

attach(dados)
novos_dados <- na.omit(data.frame(location,total_deaths_per_million))
detach(dados)

# Criação de uma estrutura de dados com as colunas LOCATION e TOTAL_TESTS_PER_THOUSAND

attach(dados)
novos_dados_2 <- na.omit(data.frame(location,total_tests_per_thousand))
detach(dados)

# Criação de um vetor com o total de mortes por milhão de habitantes para cada país

subset_paises <- subset(novos_dados,location == "Albania" | location == "Denmark" | location == "Germany" |
                                  location == "Russia")
subset_total_mortes <- aggregate(subset_paises$total_deaths_per_million, by=list(subset_paises$location),max)
vetor_total_mortes <- c(subset_total_mortes$x)

# Criação de um vetor com o número total de testes por milhar de habitantes para cada país

subset_paises_2 <- subset(novos_dados_2,location == "Albania" | location == "Denmark" | location == "Germany" |
                                  location == "Russia")
subset_total_testes <- aggregate(subset_paises_2$total_tests_per_thousand, by=list(subset_paises_2$location),max)
vetor_total_testes <- c(subset_total_testes$x)

# Criação de uma estrutura com 2 colunas para cada país: uma com o total de mortos por milhão e outra com total
# de testes por milhar de habitantes

dados_finais <- data.frame(vetor_total_mortes,vetor_total_testes)

# Conversão da estrutura numa matrix e transposição da mesma, para efeitos visuais de barplot

matrix_dados_finais <- as.matrix(dados_finais)
matrix_transposta_dados_finais <- t(matrix_dados_finais)

# Criação do barplot

barplot_obtido <- barplot(matrix_transposta_dados_finais,beside=TRUE, legend=c("Número total de mortos","Número total de testes"), 
        names.arg=c("Albânia","Dinamarca", "Alemanha", "Rússia"), col=c("black","blue"))

# e) Indique qual o país europeu que teve o maior número de infetados, por milhão de habitantes, num só dia.

# Criação de uma estrutura com os dados dos campos LOCATION e NEW_CASES_PER_MILLION relativos à Europa

subset_Europa <- subset(dados, continent == "Europe")
novos_dados <- na.omit(data.frame(subset_Europa$location, subset_Europa$new_cases_per_million))

# Determinação do país com maior número de infetados num dia

index <- which.max(novos_dados$subset_Europa.new_cases_per_million)
pais <- novos_dados[index,]$subset_Europa.location

# Visualização do país

cat("País europeu: ", pais)

# f) Indique em que dia, e em que pais, se registou a maior taxa de transmissibilidade do vírus.

# Criação de uma estrutura com os dados dos campos LOCATION, DATE e REPRODUCTION_RATE

novos_dados <- na.omit(data.frame(dados$location, dados$date, dados$reproduction_rate))

# Determinação do país e dia com maior taxa de transmissibilidade do vírus

index <- which.max(novos_dados$dados.reproduction_rate)
pais <- novos_dados[index,]$dados.location
dia <- novos_dados[index,]$dados.date

# Visualização do país e o dia

cat("A maior taxa foi registada no país",pais,",no dia",dia)

# g) Efetue um boxplot do nº de mortos diários por milhão de habitantes, em cada continente. Remova os outliers.

# https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/

# Criação de uma estrutura de dados com as colunas LOCATION e NEW_DEATHS_PER_MILLION

attach(dados)
novos_dados <- na.omit(data.frame(location,new_deaths_per_million))
detach(dados)

# Criação de um vetor com os valores de mortes diárias por milhão para África

subset_Africa <- subset(novos_dados, location == "Africa")
dados_Africa <- c(subset_Africa$new_deaths_per_million)

# Determinação dos quartis (1º e 3º), da amplitude interquartil e dos limites inferior e superior, respetivamente

Quartis <- quantile(dados_Africa,probs=c(.25, .75))
AIQ <- IQR(dados_Africa)
limite_superior <- Quartis[2]+1.5*AIQ
limite_inferior <- Quartis[1]-1.5*AIQ

# Remoção dos outliers

dados_Africa <- dados_Africa[dados_Africa > limite_inferior & dados_Africa < limite_superior]

# Criação de um vetor com os valores de mortes diárias por milhão para Ásia

subset_Asia <- subset(novos_dados, location == "Asia")
dados_Asia <- c(subset_Asia$new_deaths_per_million)

# Determinação dos quartis (1º e 3º), da amplitude interquartil e dos limites inferior e superior, respetivamente

Quartis <- quantile(dados_Asia,probs=c(.25, .75))
AIQ <- IQR(dados_Asia)
limite_superior <- Quartis[2]+1.5*AIQ
limite_inferior <- Quartis[1]-1.5*AIQ

# Remoção dos outliers

dados_Asia <- dados_Asia[dados_Asia > limite_inferior & dados_Asia < limite_superior]

# Criação de um vetor com os valores de mortes diárias por milhão para Europa

subset_Europa <- subset(novos_dados, location == "Europe")
dados_Europa <- c(subset_Europa$new_deaths_per_million)

# Determinação dos quartis (1º e 3º), da amplitude interquartil e dos limites inferior e superior, respetivamente

Quartis <- quantile(dados_Europa,probs=c(.25, .75))
AIQ <- IQR(dados_Europa)
limite_superior <- Quartis[2]+1.5*AIQ
limite_inferior <- Quartis[1]-1.5*AIQ

# Remoção dos outliers

dados_Europa <- dados_Europa[dados_Europa > limite_inferior & dados_Europa < limite_superior]

# Criação de um vetor com os valores de mortes diárias por milhão para América do Norte

subset_America_Norte <- subset(novos_dados, location == "North America")
dados_America_Norte <- c(subset_America_Norte$new_deaths_per_million)

# Determinação dos quartis (1º e 3º), da amplitude interquartil e dos limites inferior e superior, respetivamente

Quartis <- quantile(dados_America_Norte,probs=c(.25, .75))
AIQ <- IQR(dados_America_Norte)
limite_superior <- Quartis[2]+1.5*AIQ
limite_inferior <- Quartis[1]-1.5*AIQ

# Remoção dos outliers

dados_America_Norte <- dados_America_Norte[dados_America_Norte > limite_inferior & 
                                             dados_America_Norte < limite_superior]

# Criação de um vetor com os valores de mortes diárias por milhão para Oceania

subset_Oceania <- subset(novos_dados, location == "Oceania")
dados_Oceania <- c(subset_Oceania$new_deaths_per_million)

# Determinação dos quartis (1º e 3º), da amplitude interquartil e dos limites inferior e superior, respetivamente

Quartis <- quantile(dados_Oceania,probs=c(.25, .75))
AIQ <- IQR(dados_Oceania)
limite_superior <- Quartis[2]+1.5*AIQ
limite_inferior <- Quartis[1]-1.5*AIQ

# Remoção dos outliers

dados_Oceania <- dados_Oceania[dados_Oceania > limite_inferior & dados_Oceania < limite_superior]

# Criação de um vetor com os valores de mortes diárias por milhão para América do Sul

subset_America_Sul <- subset(novos_dados, location == "South America")
dados_America_Sul <- c(subset_America_Sul$new_deaths_per_million)

# Determinação dos quartis (1º e 3º), da amplitude interquartil e dos limites inferior e superior, respetivamente

Quartis <- quantile(dados_America_Sul,probs=c(.25, .75))
AIQ <- IQR(dados_America_Sul)
limite_superior <- Quartis[2]+1.5*AIQ
limite_inferior <- Quartis[1]-1.5*AIQ

# Remoção dos outliers

dados_America_Sul <- dados_America_Sul[dados_America_Sul > limite_inferior & dados_America_Sul < limite_superior]

# Criação de variáveis a serem usadas no boxplot

nomes <- c("África", "Ásia", "Europa", "América do Norte", "Oceania", "América do Sul")
titulo <- "Boxplot do número de mortes diárias por milhão de habitantes"

# Criação do boxplot

boxplot_1g <- boxplot(dados_Africa, dados_Asia, dados_Europa, dados_America_Norte, dados_Oceania, dados_America_Sul, 
                      names=nomes, main=titulo, ylab="Número mortos diários")

# ------------------------------------------------------------------------------
#
# 4.2. Inferência Estatística
#
# Para a geração de amostras pseudoaleatórias, pertencentes ao período 2020-04-01 até 2021-02-27,
# considere o uso da função set.seed():
#
# a) Considerando apenas os dados relativos, a 30 dias, da amostra pseudoaleatória (usando o valor 118
# no parâmetro da função set.seed()) que obteve, verifique se a média da taxa transmissibilidade no
# Reino Unido é superior à média da taxa de transmissibilidade em Portugal.

# Criação de uma estrutura de dados com as colunas LOCATION, DATE e REPRODUCTION_RATE

attach(dados)
novos_dados <- na.omit(data.frame(location,date,reproduction_rate))
detach(dados)

# Criação de estruturas com dados para Portugal e Reino Unido, com a restrição do período

subset_Portugal <- na.omit(subset(novos_dados,location == "Portugal" & date >= "2020-04-01" & date <= "2021-02-27"))
subset_Reino_Unido <- na.omit(subset(novos_dados,location == "United Kingdom" & date >= "2020-04-01" & 
                                       date <= "2021-02-27"))

# Criação de amostra pseudoaleatória de 30 dias para Portugal

tamanho_subset_Portugal <- nrow(subset_Portugal)
set.seed(118)
amostra_Portugal <- subset_Portugal[sample(1:tamanho_subset_Portugal,size=30),]

# Criação de amostra pseudoaleatória de 30 dias para Reino Unido

tamanho_subset_Reino_Unido <- nrow(subset_Reino_Unido)
set.seed(118)
amostra_Reino_Unido <- subset_Reino_Unido[sample(1:tamanho_subset_Reino_Unido,size=30),]

# Criação de vetores com os valores da taxa de transmissibilidade, para Portugal e Reino Unido

dados_transmissibilidade_amostra_Portugal <- c(amostra_Portugal$reproduction_rate)
dados_transmissibilidade_amostra_Reino_Unido <- c(amostra_Reino_Unido$reproduction_rate)

# Para realização de testes de médias entre 2 amostras independentes (considera-se que as variancias são
# desconhecidas e diferentes), recorre-se ao teste de Welch, até porque estamos perante amostras grandes
# (com tamanho igual)

# Consideramos as seguintes hipóteses:

# H0: média taxa Reino Unido <= média taxa Portugal
# H1: média taxa Reino Unido > média taxa Portugal

# Trata-se de um teste unilaterial à direita

resultado_teste <-t.test(dados_transmissibilidade_amostra_Reino_Unido,dados_transmissibilidade_amostra_Portugal,
                         alternative ="greater")
resultado_teste

# Como p-value > 0.05, então não se rejeita H0. Logo, não existem evidências de que a média da taxa de transmissibilidade
# no Reino Unido seja superior à media da taxa de transmissibilidade em Portugal.

# b) Considerando apenas os dados relativos, a 15 dias, da amostra pseudoaleatória (usando o valor 115
# no parâmetro da função set.seed()) que obteve, verifique se há diferenças significativas entre o nº de 
# mortes diárias, por milhão de habitantes, em Espanha, França, Portugal e Itália. No caso de haver,
# efetue uma análise post-hoc.

# Criação de uma estrutura de dados com as colunas LOCATION, DATE e NEW_DEATHS_PER_MILLION

attach(dados)
novos_dados <- na.omit(data.frame(location,date,new_deaths_per_million))
detach(dados)

# Criação de estruturas para Espanha, França, Portugal e Itália, com a restrição do período

subset_Espanha <- na.omit(subset(novos_dados,location == "Spain" & date >= "2020-04-01" & date <= "2021-02-27"))
subset_Franca <- na.omit(subset(novos_dados,location == "France" & date >= "2020-04-01" & date <= "2021-02-27"))
subset_Portugal <- na.omit(subset(novos_dados,location == "Portugal" & date >= "2020-04-01" & date <= "2021-02-27"))
subset_Italia <- na.omit(subset(novos_dados,location == "Italy" & date >= "2020-04-01" & date <= "2021-02-27"))

# Criação de amostra pseudoaleatória de 15 dias para Espanha

tamanho_subset_Espanha <- nrow(subset_Espanha)
set.seed(115)
amostra_Espanha <- subset_Espanha[sample(1:tamanho_subset_Espanha,size=15),]

# Criação de amostra pseudoaleatória de 15 dias para França

tamanho_subset_Franca <- nrow(subset_Franca)
set.seed(115)
amostra_Franca <- subset_Franca[sample(1:tamanho_subset_Franca,size=15),]

# Criação de amostra pseudoaleatória de 15 dias para Portugal

tamanho_subset_Portugal <- nrow(subset_Portugal)
set.seed(115)
amostra_Portugal <- subset_Portugal[sample(1:tamanho_subset_Portugal,size=15),]

# Criação de amostra pseudoaleatória de 15 dias para Itália

tamanho_subset_Italia <- nrow(subset_Italia)
set.seed(115)
amostra_Italia <- subset_Italia[sample(1:tamanho_subset_Italia,size=15),]

# Criação de vetores com os valores do número de mortes diárias, por milhão de habitantes, para Espanha, França
# Portugal e Itália

dados_numero_mortes_amostra_Espanha <- c(amostra_Espanha$new_deaths_per_million)
dados_numero_mortes_amostra_Franca <- c(amostra_Franca$new_deaths_per_million)
dados_numero_mortes_amostra_Portugal <- c(amostra_Portugal$new_deaths_per_million)
dados_numero_mortes_amostra_Italia <- c(amostra_Italia$new_deaths_per_million)

# Primeiro, é necessário realizar o teste de Shapiro, para verificar se cada amostra tem distribuição normal.

shapiro.test(dados_numero_mortes_amostra_Espanha)
shapiro.test(dados_numero_mortes_amostra_Franca)
shapiro.test(dados_numero_mortes_amostra_Portugal)
shapiro.test(dados_numero_mortes_amostra_Italia)

# Como p_value é menor que 0.05, nenhuma da amostra tem distribuição normal e, 
# em vez de usarmos ANOVA, iremos recorrer ao teste de Kruskall-Wallis, que é uma
# alternativa não paramétrica para o ANOVA

# Consideramos as seguintes hipóteses:

# H0: nº de mortes diárias, por milhão de habitantes, em Espanha 
# = nº de mortes diárias, por milhão de habitantes, em França
# = nº de mortes diárias, por milhão de habitantes, em Portugal 
# = nº de mortes diárias, por milhão de habitantes, na Itália
#
# H1: nº de mortes diárias, por milhão de habitantes, em Espanha
# != nº de mortes diárias, por milhão de habitantes, em França
# != nº de mortes diárias, por milhão de habitantes, em Portugal
# != nº de mortes diárias, por milhão de habitantes, na Itália

amostra_completa <- data.frame(cbind(dados_numero_mortes_amostra_Espanha,dados_numero_mortes_amostra_Franca,
                                     dados_numero_mortes_amostra_Portugal,dados_numero_mortes_amostra_Italia))

grupos <- factor(c(rep ("Espanha", length(dados_numero_mortes_amostra_Espanha)),
                   rep("França", length(dados_numero_mortes_amostra_Franca)),
                   rep ("Portugal", length(dados_numero_mortes_amostra_Portugal)),
                   rep ("Itália", length(dados_numero_mortes_amostra_Italia))))
kruskal.test(amostra_completa,grupos)

# O facto de p-value ser superior a 0.05 leva-nos a não rejeitar H0. Não existem diferenças significativas entre os grupos. Post-hoc não é 
# necessário

# c) Para cada Continente gere uma amostra pseudoaleatória, de 30 dias. Para a África use a seed 100,
# para a Ásia use a seed 101, para a Europa use a seed 102, para a América do Norte use a seed 103, e
# para a América do Sul use a seed 104. Verifique se existe diferença significativa entre os números
# médios diários de mortes, por milhão de habitantes, entre os continentes. No caso de haver, efetue
# uma análise post-hoc.

# Criação de uma estrutura de dados com as colunas CONTINENT, DATE e NEW_DEATHS_PER_MILLION

attach(dados)
novos_dados <- na.omit(data.frame(continent,date,new_deaths_per_million))
detach(dados)

# Criação de estruturas para cada continente (exceto Oceania), com a restrição do período

subset_Africa <- na.omit(subset(novos_dados,continent == "Africa" & date >= "2020-04-01" & date <= "2021-02-27"))
subset_Asia <- na.omit(subset(novos_dados,continent == "Asia" & date >= "2020-04-01" & date <= "2021-02-27"))
subset_Europa <- na.omit(subset(novos_dados,continent == "Europe" & date >= "2020-04-01" & date <= "2021-02-27"))
subset_America_Norte <- na.omit(subset(novos_dados,continent == "North America" & date >= "2020-04-01" & 
                                         date <= "2021-02-27"))
subset_America_Sul <- na.omit(subset(novos_dados,continent == "South America" & date >= "2020-04-01" 
                                     & date <= "2021-02-27"))

# Criação de amostra pseudoaleatória de 15 dias para cada continente (exceto Oceania)

tamanho_subset_Africa <- nrow(subset_Africa)
set.seed(100)
amostra_Africa <- subset_Africa[sample(1:tamanho_subset_Africa,size=30),]

tamanho_subset_Asia <- nrow(subset_Asia)
set.seed(101)
amostra_Asia <- subset_Asia[sample(1:tamanho_subset_Asia,size=30),]

tamanho_subset_Europa <- nrow(subset_Europa)
set.seed(102)
amostra_Europa <- subset_Europa[sample(1:tamanho_subset_Europa,size=30),]

tamanho_subset_America_Norte <- nrow(subset_America_Norte)
set.seed(103)
amostra_America_Norte <- subset_America_Norte[sample(1:tamanho_subset_America_Norte,size=30),]

tamanho_subset_America_Sul <- nrow(subset_America_Sul)
set.seed(104)
amostra_America_Sul <- subset_America_Sul[sample(1:tamanho_subset_America_Sul,size=30),]

# Criação de vetores com os valores do número de mortes diárias, por milhão de habitantes, para cada continente
# (exceto Oceania)

dados_numero_mortes_amostra_Africa <- c(amostra_Africa$new_deaths_per_million)
dados_numero_mortes_amostra_Asia <- c(amostra_Asia$new_deaths_per_million)
dados_numero_mortes_amostra_Europa <- c(amostra_Europa$new_deaths_per_million)
dados_numero_mortes_amostra_America_Norte <- c(amostra_America_Norte$new_deaths_per_million)
dados_numero_mortes_amostra_America_Sul <- c(amostra_America_Sul$new_deaths_per_million)


# Biblioteca necessária para o uso da função lillie.test()

if(!require(nortest)) install.packages("nortest") ; library(nortest)

# Primeiro, é necessário realizar o teste de Lilliefors, para verificar se cada amostra tem distribuição normal.
# É usado este teste porque a amostra é grande, ou seja, tamanho igual a 30

lillie.test(dados_numero_mortes_amostra_Africa)
lillie.test(dados_numero_mortes_amostra_Asia)
lillie.test(dados_numero_mortes_amostra_Europa)
lillie.test(dados_numero_mortes_amostra_America_Norte)
lillie.test(dados_numero_mortes_amostra_America_Sul)

# Como p_value é menor que 0.05, nenhuma da amostra tem distribuição normal e, 
# em vez de usarmos ANOVA, iremos recorrer ao teste de Kruskall-Wallis, que é uma
# alternativa não paramétrica para o ANOVA

# Consideramos as seguintes hipóteses:

# H0: nº médio diário mortes, por milhão de habitantes, na África 
# = nº médio de mortes diárias, por milhão de habitantes, na Ásia
# = nº médio de mortes diárias, por milhão de habitantes, na Europa
# = nº médio de mortes diárias, por milhão de habitantes, na América do Norte
# = nº médio de mortes diárias, por milhão de habitantes, na América do Sul
#
# H1: nº médio diário mortes, por milhão de habitantes, na África 
# != nº médio de mortes diárias, por milhão de habitantes, na Ásia
# != nº médio de mortes diárias, por milhão de habitantes, na Europa
# != nº médio de mortes diárias, por milhão de habitantes, na América do Norte
# != nº médio de mortes diárias, por milhão de habitantes, na América do Sul

amostra_completa <- data.frame(cbind(dados_numero_mortes_amostra_Africa,dados_numero_mortes_amostra_Asia,
                                     dados_numero_mortes_amostra_Europa,dados_numero_mortes_amostra_America_Norte,
                                     dados_numero_mortes_amostra_America_Sul))

grupos <- factor(c(rep ("África", length(dados_numero_mortes_amostra_Africa)),
                   rep("Asia", length(dados_numero_mortes_amostra_Asia)),
                   rep ("Europa", length(dados_numero_mortes_amostra_Europa)),
                   rep ("América do Norte", length(dados_numero_mortes_amostra_America_Norte)),
                   rep ("América do Sul", length(dados_numero_mortes_amostra_America_Sul))))
kruskal.test(amostra_completa,grupos)

# O facto de p-value ser inferior a 0.05 leva-nos a rejeitar H0. Ou seja, existem diferenças significativas entre 
# os grupos. Post-hoc é necessário

# Biblioteca necessária para o uso da função kruskalmc()

if(!require(pgirmess)) install.packages("pgirmess") ; library(pgirmess)

# Realização de análise Post-hoc

stack <- stack(amostra_completa)
kruskalmc(stack$values,grupos)

# ------------------------------------------------------------------------------
# 4.3 Correlação
#
# Averigue se existe correlação, em 2021, entre:
#
# a) o valor máximo da taxa diária de transmissibilidade e a densidade populacional de todos os países da
# Europa com mais de 10 milhões de habitantes.

# Biblioteca necessária para o uso de algumas funções

if(!require(dplyr)) install.packages("dplyr") ; library(dplyr)

subset_paises <- subset(dados, continent == "Europe" & population > 10000000 & reproduction_rate !="NA" & 
                          "2020-12-31" < date)

novos_dados <- subset_paises %>%
  group_by(location) %>%
  summarise(max_rep_rate = max(reproduction_rate), pop = max(population_density))

attach(novos_dados)
cor.test(max_rep_rate, pop, alternative = "t", method = "pearson")
detach(novos_dados)

# b) o total de mortos por milhão de habitantes e a percentagem da população com 65 anos ou mais em
# todos os países da Europa com mais de 10 milhões de habitantes.

novos_dados <- subset_paises %>%
  group_by(location) %>%
  summarise(age65 = max(aged_65_older), deathsM = max(total_deaths_per_million))


attach(novos_dados)
cor.test(age65, deathsM, alternative = "t", method = "pearson")
detach(novos_dados)

# ------------------------------------------------------------------------------
# 4.4. Regressão
#
# Considere o "Índice de rigor" médio mensal (Ir, Stringency Index) em Portugal, a variável dependente e as
# variáveis independentes: média de mortes diárias por milhão de habitantes Dm, média de casos diários por
# milhão de habitantes Cm e a média mensal da taxa de transmissibilidade Rm. Considere os dados no período
# 2020-04-01 até 2021-02-27.
#
# Variável Dependente (contínua, aleatória):
# Ir - Índice de Rigor em Portugal 
# Variáveis independentes (não aleatórias):
# Dm - Média de mortes diárias por milhão de habitantes
# Cm - Média de casos diários por milhão de habitantes
# Rm - Média mensal da taxa de transmissibilidade.

# 2020-04-01 <-> 2021-02-27

if(!require(pacman)) install.packages("pacman") ; library(pacman)

pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr, QuantPsyc, psych, scatterplot3d)

# a) Construa o modelo de regressão linear múltipla.

## Modelo de regressão linear múltipla

sublistaPortugal <- subset(dados, location == "Portugal" & "2020-03-31" < date & date < "2021-02-28")
datesPortugal <- as.Date(sublistaPortugal$date)
datesPortugalMeses <- months(datesPortugal)
datesPortugalAnos <- format(datesPortugal,format="%y")
mediaMensalCasos <- aggregate(new_cases_per_million ~ datesPortugalMeses + datesPortugalAnos, sublistaPortugal, mean)
mediaMensalTaxa <- aggregate(reproduction_rate ~ datesPortugalMeses + datesPortugalAnos, sublistaPortugal, mean)
mediaMensalMortes <- aggregate(new_deaths_per_million ~ datesPortugalMeses + datesPortugalAnos, sublistaPortugal, mean)
mediaMensalIndice <- aggregate(stringency_index ~ datesPortugalMeses + datesPortugalAnos, sublistaPortugal, mean)

tabela <- data.frame(mediaMensalCasos$datesPortugalMeses,mediaMensalIndice$stringency_index, mediaMensalCasos$new_cases_per_million, mediaMensalMortes$new_deaths_per_million, mediaMensalTaxa$reproduction_rate)
attach(tabela)
modelo <- lm(mediaMensalIndice.stringency_index~mediaMensalCasos.new_cases_per_million+mediaMensalMortes.new_deaths_per_million+mediaMensalTaxa.reproduction_rate)
detach(tabela)

# Gráfico
par(mfrow=c(2,2))
plot(modelo)

# Principais valores
summary(modelo)

# b) Verifique se as condições de: Homocedasticidade, Autocorrelação nula e de Multicolinearidade são
# satisfeitas.

## Resumo dos residuos normalizados
summary(rstandard(modelo))

## Normalidade dos residuos (Teste de Shapiro-Wilk)
# Distruibuicao Normal: p > 0.05.
# No nosso caso, p menor que 0.05. Logo aponta para a nao normalidade.
shapiro.test(residuals(modelo))

## Independencia dos residuos (Durbin-Watson)
# Usa-se quando temos valores iguais. Isto é, para verificar a correlação entre os resíduos.
# Podemos assumir que os residuos não s~ao independente.
durbinWatsonTest(modelo)

## Homocedasticidade (Breusch-Pagan)
# Existe homocedasticidade para p > 0.05.
# No nosso caso, podemos afirmar que existe homocedasticidade.
bptest(modelo)

## Ausência de Multicolinearidade
# Se vif < 3, considerar ausencia de multicolinariedade.
# No nosso caso, podemos afirmar que existe multicolinearidade.
vif(modelo)

# c) Estime o valor de Ir para os valores Dm = 10, Cm = 460 e Rm = 1.1

## confint(modelo,level=0.99)
attach(tabela)
previsao <- data.frame(mediaMensalMortes.new_deaths_per_million=10,mediaMensalCasos.new_cases_per_million=460,mediaMensalTaxa.reproduction_rate=1.1)
detach(tabela)
predict(modelo, previsao)
