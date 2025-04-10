#AULA2

# TIRAR NOTAÇÃO CIENTÍFICA ####
options(scipen = 999)

# EXCLUIR UMA BASE DE DADOS ####
# nomedoobjeto <- NULL

##ATALHOS DE TECLADO####
# "<- " "ALT" + "-"
#PARA EXECUTAR A LINHA: "ALT" + "ENTER"
#PARA EXECUTAR TODO O SCRIPT: "ALT"+"SHIFT"+"ENTER"

##BIBLIOTECA PARA ACESSAR DADOS DO BANCO MUNDIAL####

#install.packages('WDI')
library(WDI)

##VARIÁVEIS####

#PIB A PREÇOS CORRENTES(US$):NY.GDP.MKTP.CD
#DESPESAS DE CONSUMO PESSOAL,PC/US$:NE.CON.PRVT.CD

#Infos: http://vincentarelbundock.github.io/WDI/

PIB_vars <- WDIsearch('GDP')#GDP=PIB(todos)
TODO_BM <- WDIsearch('')#TODAS AS VAR. DO BM

VARIAVEIS <- c('NY.GDP.MKTP.CD',
               'NE.CON.PRVT.CD')


#BASE DE DADOS DE CORTE TRANSVERSAL

dados <- WDI(indicator = VARIAVEIS,
             country = 'all', #TODOS = "all"
             start = 2022, end = 2022) #PROBLEMAS DE NA

#install.packages("tidyverse")
library(tidyverse)
dadosok <- WDI(indicator = VARIAVEIS,
               country = 'all', #TODOS = "all"
               start = 2022, end = 2022) %>% 
  na.omit()


#BASE DE DAOS DA APRESENTAÇÃO(BRASIL)

dadosbr <- WDI(indicator = VARIAVEIS,
             country = 'BR', # CÓDIGO Iso2C
             start = 1961, end = 2018)

# BASE COMPLETA DO BRASIL

dadosbrcomp <- WDI(indicator = VARIAVEIS,
               country = 'BR')
