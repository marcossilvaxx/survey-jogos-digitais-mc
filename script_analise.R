if(!require(dplyr)) install.packages("dplyr") # Instalacao do pacote caso nao esteja instalado
library(dplyr)                                # Carregamento do pacote

if(!require(epiDisplay)) install.packages('epiDisplay')
library(epiDisplay)

if(!require(tidyr)) install.packages('tidyr')
library(tidyr)

if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

## Leitura dos dados

dados <- read.csv('dados_colunas_resumidas.csv', sep = ',', dec = '.')
dados_formatados <- read.csv('dados_numericos_escala.csv', sep = ',', dec = '.', fileEncoding = "Latin1")


## Visualizando os dados

View(dados)
glimpse(dados)
View(dados_formatados)
glimpse(dados_formatados)


## Visualização do histograma de uma das perguntas do questionário

ggplot(dados_formatados, mapping = aes(x=Importância.na.aprendizagem.de.algum.idioma)) +
  geom_histogram(
    fill="lightblue",
    col="black",
    alpha=0.5,
    bins=20,
    aes(y=after_stat(density))
  ) +
  xlab("Importância na aprendizagem de algum idioma") +
  ylab("Densidade") +
  labs(title = "Distribuição da importância na aprendizagem de algum idioma")


## Tabela de frequência para a variável Idade

tab1(sort(dados$Idade), cum.percent = TRUE, main = "Distribuição de Idade")


## Tabela de frequência para a variável Frequência de uso de jogos digitais

tab1(dados$Frequência.de.uso.de.jogos.digitais, sort.group = "decreasing", cum.percent = TRUE, main = "Distribuição de frequência de uso de jogos digitais")


# Transformando dados para nova estrutura

new_sample <- dados_formatados[,-c(1)] %>% 
  pivot_longer(
    cols = Frequência.de.uso.de.jogos.digitais:Frequência.de.uso.de.idiomas.estrangeiros.em.jogos.digitais, # as colunas desse intervalo
    names_to = "Questões", # terão seus nomes armazenados nessa nova coluna
    values_to = "Resposta")

View(new_sample)


## Realizando Análise Inferencial (teste de mediana)

tabela_analise_descritiva <- new_sample %>%
  group_by(Questões) %>%
  summarize(
    Mediana = median(Resposta),
    IQR = IQR(Resposta)
  )


## Ordenando tabela em ordem decrescente pela mediana

tabela_analise_descritiva <- arrange(tabela_analise_descritiva, desc(tabela_analise_descritiva$Mediana))

tabela_analise_descritiva

View(tabela_analise_descritiva)


## Realizando Análise Inferencial (teste de mediana)

tabela_analise_inferencial <- new_sample %>%
  group_by(Questões) %>%
  summarize(
    "p-valor" = wilcox.test(
      Resposta,
      alternative = c("greater"),
      mu = 3,
      conf.level = 0.95
    )$p.value
  )


## Ordenando tabela em ordem crescente pelo p-valor

tabela_analise_inferencial <- arrange(tabela_analise_inferencial, tabela_analise_inferencial$`p-valor`)

tabela_analise_inferencial

View(tabela_analise_inferencial)