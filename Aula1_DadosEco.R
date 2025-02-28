# AULA 1 DADOS ECONÔMICOS 

# TASK ####
# BUSQUE DUAS VARIÁVEIS DE SEU INTERESSE NO BANCO MUNDIAL
# CRIE UM ARQUIVO DE DADOS DE TODOS OS PAÍSES PARA 2022, POR EXEMPLO
# UM ARQUIVO DE DADOS DO BRASIL (TODO PERÍODO)

# TIRAR NOTAÇÃO CIENTÍFICA ####
options(scipen = 999)

# install.packages('WDI')
library(WDI)

# VARIÁVEIS ####

# TAXA DE DESEMPREGO TOTAL (% DA POPULAÇÃO ATIVA): SL.UEM.TOTL.ZS
# TAXA DE INFLAÇÃO (ÍNDICES DE PREÇO AO CONSUMIDOR): FP.CPI.TOTL.ZG

VARIAVEIS <- c('SL.UEM.TOTL.ZS',
               'FP.CPI.TOTL.ZG')

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

# SALVAR DADOS DOS PAÍSES EM CSV

write.csv(dados_todos_paises_2022_ok,"dados_todos_paises_2022.csv",row.names = FALSE)

# CRIAR BASE DE DADOS BRASIL TODO O PERÍODO

dados_brasil_todo_periodo <- WDI(indicator = VARIAVEIS,
                                 country = 'BR', #Código BR
                                 start = 1961, end = 2022)

# LIMPAR OS DADOS BR (REMOVE NA)

dados_brasil_todo_periodo_ok <- WDI(indicator = VARIAVEIS,
                                    country = 'BR',
                                    start = 1961, end= 2022) %>%
  na.omit()

# SALVAR DADOS BR EM CSV
write.csv(dados_brasil_todo_periodo_ok, "dados_brasil_todo_periodo.csv", row.names = FALSE)
