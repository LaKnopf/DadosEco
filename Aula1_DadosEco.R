# AULA 1 DADOS ECONÔMICOS 

# TASK ####
# BUSQUE DUAS VARIÁVEIS DE SEU INTERESSE NO BANCO MUNDIAL
# CRIE UM ARQUIVO DE DADOS DE TODOS OS PAÍSES PARA 2022, POR EXEMPLO
# UM ARQUIVO DE DADOS DO BRASIL (TODO PERÍODO)

# TIRAR NOTAÇÃO CIENTÍFICA ####
options(scipen = 999)

#install.packages('WDI')
library(WDI)

# VARIÁVEIS ####

# TAXA DE DESEMPREGO TOTAL (% DA POPULAÇÃO ATIVA): SL.UEM.TOTL.ZS
# TAXA DE INFLAÇÃO (ÍNDICES DE PREÇO AO CONSUMIDOR): FP.CPI.TOTL.ZG

VARIAVEIS <- c('DESEMPREGO'='SL.UEM.TOTL.ZS',
               'INFLAÇÃO'='FP.CPI.TOTL.ZG')

# BASE DE DADOS DE CORTE TRANSVERSAL

dados_todos_paises_2022 <- WDI(indicator = VARIAVEIS,
                               country = 'all', # PARA TODOS OS PAÍSES
                               start = 2022, end = 2022)

# LIMPAR OS DADOS (REMOVE NA)

#install.packages('tidyverse')
library(tidyverse)
dados_todos_paises_2022_ok <- WDI(indicator = VARIAVEIS,
                                  country = 'all',
                                  start = 2022, end = 2022) %>% 
  na.omit()


# CRIAR BASE DE DADOS BRASIL TODO O PERÍODO

dados_brasil_todo_periodo <- WDI(indicator = VARIAVEIS,
                                 country = 'BR', #Código BR
                                 start = 1961, end = 2022)

# LIMPAR OS DADOS BR (REMOVE NA)

dados_brasil_todo_periodo_ok <- WDI(indicator = VARIAVEIS,
                                    country = 'BR',
                                    start = 1961, end= 2022) %>%
  na.omit()


# FAZER GRÁFICOS

# HÁ DUAS BASES

# 1. "dadoS_todos_paises_2022" (CORTE TRANSVERSAL)
# Vários países em 2022

# 2. "dados_brasil_todo_periodo" (SÉRIE TEMPORAL)
# Brasil, 1961-2018


# REGRESSÃO

#'ct' = corte transversal
modelo_ct <- lm(DESEMPREGO ~ INFLAÇÃO, data = dados_todos_paises_2022_ok)
summary(modelo_ct) # VISUALIZAR OS RESULTADOS

#'st' = série temporal
modelo_st <- lm(INFLAÇÃO ~ DESEMPREGO, data = dados_brasil_todo_periodo_ok)
summary(modelo_st)

head(dados_todos_paises_2022_ok)


# GRÁFICO PONTO_PARA CORTE TRANSVERSAL
grafico_ponto <- 
  ggplot(data = dados_todos_paises_2022_ok,
         mapping = aes(x = INFLAÇÃO, y = DESEMPREGO)) +
  geom_point()

print(grafico_ponto)

# PROMPT CHAT GPT

# Carregar pacotes necessários
#library(ggplot2)
l#ibrary(dplyr)

# Supondo que sua base de dados tenha uma coluna indicando o continente de cada país
# Se não tiver, você pode criar manualmente um vetor para adicionar essa informação
# Exemplo:
dados_todos_paises_2022_ok <- dados_todos_paises_2022_ok %>%
  mutate(Continente = case_when(
    iso3c %in% c("BRA", "ARG", "USA", "CAN", "MEX", "CHL", "COL") ~ "América",
    iso3c %in% c("NGA", "ZAF", "EGY", "KEN", "DZA", "AGO") ~ "África",
    iso3c %in% c("FRA", "DEU", "ITA", "ESP", "GBR", "ALB") ~ "Europa",
    iso3c %in% c("CHN", "JPN", "IND", "IDN", "KOR", "THA") ~ "Ásia",
    iso3c %in% c("AUS", "NZL", "FJI", "PNG") ~ "Oceania",
    TRUE ~ "Outros"
  ))

# Definição das cores por continente
cores_continentes <- c("América" = "green", "África" = "orange", "Europa" = "red",
                       "Ásia" = "blue", "Oceania" = "purple")

# Criar o gráfico
grafico_ponto <- ggplot(data = dados_todos_paises_2022_ok, 
                        aes(x = DESEMPREGO, y = INFLAÇÃO, color = Continente)) +
  geom_point(size = 3, alpha = 0.8) +  # Pontos maiores e semi-transparentes
  scale_color_manual(values = cores_continentes) + # Aplicando cores personalizadas
  labs(x = "Desemprego (%)", 
       y = "Inflação (%)", 
       color = "Continente",
       title = "Relação entre Desemprego e Inflação por Países em 2022") +
  theme_minimal(base_size = 14) +  # Estilo limpo e profissional
  theme(legend.position = "bottom")  # Posicionando a legenda abaixo

# Adicionar o nome do Brasil acima do ponto correspondente
grafico_ponto <- grafico_ponto +
  geom_text(data = dados_todos_paises_2022_ok %>% filter(iso3c == "BRA"),
            aes(label = country), vjust = -1, fontface = "bold", color = "black")

# Exibir o gráfico
print(grafico_ponto)


# CORREÇÃO DAS CORES DOS PONTOS

# Criar uma coluna para indicar o continente de cada país
dados_todos_paises_2022_ok <- dados_todos_paises_2022_ok %>%
  mutate(Continente = case_when(
    country %in% c("Brazil", "Argentina", "United States", "Canada", "Mexico", "Chile", "Colombia", "Peru", "Venezuela") ~ "América",
    country %in% c("Nigeria", "South Africa", "Egypt", "Kenya", "Algeria", "Angola", "Ghana", "Ethiopia", "Morocco") ~ "África",
    country %in% c("France", "Germany", "Italy", "Spain", "United Kingdom", "Albania", "Portugal", "Netherlands", "Belgium") ~ "Europa",
    country %in% c("China", "Japan", "India", "Indonesia", "South Korea", "Thailand", "Vietnam", "Malaysia", "Philippines") ~ "Ásia",
    country %in% c("Australia", "New Zealand", "Fiji", "Papua New Guinea") ~ "Oceania",
    TRUE ~ "Outros"  # Para garantir que nenhum país fique sem categoria
  ))

# Remover a categoria "Outros", pois queremos apenas países corretamente categorizados
dados_todos_paises_2022_ok <- dados_todos_paises_2022_ok %>% filter(Continente != "Outros")

# Definição das cores por continente
cores_continentes <- c("América" = "green", "África" = "orange", "Europa" = "red",
                       "Ásia" = "blue", "Oceania" = "purple")

# Criar o gráfico
grafico_ponto <- ggplot(data = dados_todos_paises_2022_ok, 
                        aes(x = DESEMPREGO, y = INFLAÇÃO, color = Continente)) +
  geom_point(size = 3, alpha = 0.8) +  # Pontos com tamanho maior e transparência
  scale_color_manual(values = cores_continentes) + # Aplicando cores personalizadas
  labs(x = "Desemprego (%)", 
       y = "Inflação (%)", 
       color = "Continente",
       title = "Relação entre Desemprego e Inflação por Países em 2022") +
  theme_minimal(base_size = 14) +  # Estilo mais limpo e moderno
  theme(legend.position = "bottom")  # Legenda abaixo para melhor visualização

# Adicionar o nome do Brasil acima do ponto correspondente
grafico_ponto <- grafico_ponto +
  geom_text(data = dados_todos_paises_2022_ok %>% filter(country == "Brazil"),
            aes(label = country), vjust = -1, fontface = "bold", color = "black")

# Exibir o gráfico
print(grafico_ponto)


# GRÁFICO EM LINHA_PARA SÉRIE TEMPORAL 
grafico_linha_br <- 
  ggplot(data = dados_brasil_todo_periodo_ok,
         mapping = aes(x = year, y = DESEMPREGO)) +
  geom_line()

print(grafico_linha_br)

head(dados_brasil_todo_periodo_ok)

# PROMPT CHAT GPT

# Carregar pacotes necessários
install.packages("ggthemes")
library(ggplot2)
library(ggthemes)  # Pacote para temas personalizados como o The Economist

# Criar o gráfico de linha
grafico_linha_br <- ggplot(data = dados_brasil_todo_periodo_ok, 
                           aes(x = year, y = DESEMPREGO)) +
  geom_line(color = "blue", size = 1.2) +  # Linha azul mais espessa
  geom_point(color = "blue", size = 2) +  # Adiciona pontos para melhor visualização
  scale_x_continuous(breaks = seq(1990, 2025, by = 5)) +  # Eixo X com intervalos de 5 anos
  labs(title = "Evolução da Taxa de Desemprego no Brasil",
       x = "Ano",
       y = "Desemprego (%)") +
  theme_economist() +  # Aplicando o tema The Economist
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"))

# Exibir o gráfico
print(grafico_linha_br)
