# LAB DADOS ECON_2

## CORTE TRANSVERSAL
### COLETAR DADOS DE DUAS VARIÁVEIS
### FAZER GRÁFICO DE DISPERSÃO
#### UMA VARIÁVEL EM CADA EIXO (Y E X)

## SÉRIE TEMPORAL
### UMA SÉRIE PARA UM PAÍS ESPECÍFICO

# TIRAR NOTAÇÃO CIENTÍFICA ####
options(scipen = 999)

#install.packages('WDI')
library(WDI)

# VARIÁVEIS ####
# TAXA DE POBREZA (PERCENTUAL DA POPULAÇÃO ABAIXO DA LINHA DE POBREZA): SI.POV.DDAY
# EXPECTATIVA DE VIDA AO NASCER (ANOS): SP.DYN.LE00.IN

VARIAVEIS <- c('POBREZA' = 'SI.POV.DDAY',
               'EXPECTATIVA_VIDA' = 'SP.DYN.LE00.IN')

# BASE DE DADOS DE CORTE TRANSVERSAL

dados_countries <- WDI(indicator = VARIAVEIS,
                               country = 'all', # PARA TODOS OS PAÍSES
                               start = 2022, end = 2022)

# LIMPAR OS DADOS (REMOVE NA)

#install.packages('tidyverse')
library(tidyverse)
dados_countries <- WDI(indicator = VARIAVEIS,
                                  country = 'all',
                                  start = 2022, end = 2022) %>% 
  na.omit()

# REGRESSÃO

#'ct' = corte transversal
modelo_ct <- lm(POBREZA ~ EXPECTATIVA_VIDA, data = dados_countries)
summary(modelo_ct) # VISUALIZAR OS RESULTADOS

# GRÁFICO PONTO PARA CORTE TRANSVERSAL
grafico_ponto <- 
  ggplot(data = dados_countries,
         mapping = aes(x = EXPECTATIVA_VIDA, y = POBREZA)) +
  geom_point()

print(grafico_ponto)


# PROMPT GPT

# GRÁFICO PONTO PARA CORTE TRANSVERSAL
grafico_ponto <- 
  ggplot(data = dados_countries,
         mapping = aes(x = EXPECTATIVA_VIDA, y = POBREZA, color = continente)) +
  geom_point(size = 3) +  # Ajusta o tamanho dos pontos
  scale_color_manual(values = c("Americas" = "purple", 
                                "Asia" = "blue", 
                                "Africa" = "yellow", 
                                "Europe" = "green", 
                                "Oceania" = "grey")) +  # Define as cores conforme o continente
  labs(title = "Relação entre Pobreza e Expectativa de Vida em 2022",
       x = "Expectativa de Vida em Anos", 
       y = "Pobreza (% da população)",
       color = "Continente") +  # Adiciona o título e as legendas
  theme_minimal(base_size = 15) +  # Tema minimalista com tamanho de texto maior
  theme(
    plot.background = element_rect(fill = "black", color = NA),  # Fundo escuro
    panel.background = element_rect(fill = "black"),  # Fundo do painel escuro
    axis.text = element_text(color = "white"),  # Texto dos eixos em branco
    axis.title = element_text(color = "white"),  # Título dos eixos em branco
    plot.title = element_text(color = "white", hjust = 0.5),  # Título centralizado e branco
    legend.background = element_rect(fill = "black"),  # Fundo da legenda escuro
    legend.text = element_text(color = "white"),  # Texto da legenda em branco
    legend.title = element_text(color = "white")  # Título da legenda em branco
  )

print(grafico_ponto)

# Verifique se a coluna 'iso2c' existe
if (!"iso2c" %in% colnames(dados_countries)) {
  stop("A coluna 'iso2c' não foi encontrada. Certifique-se de que os dados possuem esse campo.")
}

# Adicionando a variável 'continente' com base no código de país (iso2c)
install.packages("countrycode")
library(countrycode)

# Criar a coluna de continente
dados_countries$continente <- countrycode(dados_countries$iso2c, "iso2c", "continent")

# Verifique se a coluna 'continente' foi criada corretamente
if (!"continente" %in% colnames(dados_countries)) {
  stop("A variável 'continente' não foi criada corretamente.")
}

# REGRESSÃO

#'ct' = corte transversal
modelo_ct <- lm(POBREZA ~ EXPECTATIVA_VIDA, data = dados_countries)
summary(modelo_ct) # VISUALIZAR OS RESULTADOS

# GRÁFICO PONTO PARA CORTE TRANSVERSAL
grafico_ponto <- 
  ggplot(data = dados_countries,
         mapping = aes(x = EXPECTATIVA_VIDA, y = POBREZA, color = continente)) +
  geom_point(size = 3) +  # Ajusta o tamanho dos pontos
  scale_color_manual(values = c("Americas" = "purple", 
                                "Asia" = "blue", 
                                "Africa" = "yellow", 
                                "Europe" = "green", 
                                "Oceania" = "grey")) +  # Define as cores conforme o continente
  labs(title = "Relação entre Pobreza e Expectativa de Vida em 2022",
       x = "Expectativa de Vida em Anos", 
       y = "Pobreza (% da população)",
       color = "Continente") +  # Adiciona o título e as legendas
  theme_minimal(base_size = 15) +  # Tema minimalista com tamanho de texto maior
  theme(
    plot.background = element_rect(fill = "black", color = NA),  # Fundo escuro
    panel.background = element_rect(fill = "black"),  # Fundo do painel escuro
    axis.text = element_text(color = "white"),  # Texto dos eixos em branco
    axis.title = element_text(color = "white"),  # Título dos eixos em branco
    plot.title = element_text(color = "white", hjust = 0.5),  # Título centralizado e branco
    legend.background = element_rect(fill = "black"),  # Fundo da legenda escuro
    legend.text = element_text(color = "white"),  # Texto da legenda em branco
    legend.title = element_text(color = "white")  # Título da legenda em branco
  )

print(grafico_ponto)

# GRÁFICO PONTO PARA CORTE TRANSVERSAL
grafico_ponto <- 
  ggplot(data = dados_countries,
         mapping = aes(x = EXPECTATIVA_VIDA, y = POBREZA, color = continente)) +
  geom_point(size = 3) +  # Ajusta o tamanho dos pontos
  scale_color_manual(values = c("Americas" = "purple", 
                                "Asia" = "blue", 
                                "Africa" = "yellow", 
                                "Europe" = "green", 
                                "Oceania" = "grey")) +  # Define as cores conforme o continente
  labs(title = "Relação entre Pobreza e Expectativa de Vida em 2022",
       x = "Expectativa de Vida em Anos", 
       y = "Pobreza (% da população)",
       color = "Continente") +  # Adiciona o título e as legendas
  theme_minimal(base_size = 15) +  # Tema minimalista com tamanho de texto maior
  theme(
    plot.background = element_rect(fill = "black", color = NA),  # Fundo escuro
    panel.background = element_rect(fill = "black"),  # Fundo do painel escuro
    axis.text = element_text(color = "white"),  # Texto dos eixos em branco
    axis.title = element_text(color = "white"),  # Título dos eixos em branco
    plot.title = element_text(color = "white", hjust = 0.5),  # Título centralizado e branco
    legend.background = element_rect(fill = "black"),  # Fundo da legenda escuro
    legend.text = element_text(color = "white"),  # Texto da legenda em branco
    legend.title = element_text(color = "white")  # Título da legenda em branco
  ) +
  # Adicionar rótulos apenas para o Brasil e EUA
  geom_text(data = dados_countries %>% filter(iso2c %in% c("BR", "US")), 
            aes(x = EXPECTATIVA_VIDA, y = POBREZA, label = country), 
            color = "white", vjust = -1, hjust = 0.5, size = 4)  # Rótulos para o Brasil e EUA

print(grafico_ponto)





# SÉRIE TEMPORAL

# Instalar e carregar pacotes necessários
# install.packages('WDI')
install.packages('ggplot2')
library(WDI)
library(ggplot2)

# Definir as variáveis de interesse (taxa de crescimento do PIB e taxa de pobreza)
VARIAVEIS <- c('PIB_CRESCIMENTO' = 'NY.GDP.MKTP.KD.ZG',
               'POBREZA' = 'SI.POV.DDAY')

# CRIAR BASE DE DADOS EUA TODO O PERÍODO
dados_eua_todo_periodo <- WDI(indicator = VARIAVEIS,
                              country = 'US',  # Código dos EUA
                              start = 1961, end = 2022)

# LIMPAR OS DADOS EUA (REMOVE NA)
dados_eua_todo_periodo_ok <- dados_eua_todo_periodo %>%
  na.omit()

# ANÁLISE DE REGRESSÃO (Exemplo: crescimento do PIB ~ taxa de pobreza)
modelo_st <- lm(PIB_CRESCIMENTO ~ POBREZA, data = dados_eua_todo_periodo_ok)
summary(modelo_st)

# GRÁFICO EM LINHA PARA SÉRIE TEMPORAL (Crescimento do PIB e Pobreza)
grafico_linha_eua <- ggplot(data = dados_eua_todo_periodo_ok) +
  geom_line(aes(x = year, y = PIB_CRESCIMENTO, color = "Crescimento do PIB"), size = 1) +
  geom_line(aes(x = year, y = POBREZA, color = "Taxa de Pobreza"), size = 1) +
  labs(title = "Crescimento do PIB e Taxa de Pobreza nos EUA (1961-2022)",
       x = "Ano", 
       y = "Valor (%)") +
  scale_color_manual(name = "Variáveis", values = c("Crescimento do PIB" = "blue", "Taxa de Pobreza" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Exibir o gráfico
print(grafico_linha_eua)

# GRÁFICO EM LINHA PARA SÉRIE TEMPORAL (Crescimento do PIB e Pobreza)
grafico_linha_eua <- ggplot(data = dados_eua_todo_periodo_ok) +
  geom_line(aes(x = year, y = PIB_CRESCIMENTO, color = "Crescimento do PIB"), size = 1) +
  geom_line(aes(x = year, y = POBREZA, color = "Taxa de Pobreza"), size = 1) +
  labs(title = "Crescimento do PIB e Taxa de Pobreza nos EUA (1961-2022)",
       x = "Ano", 
       y = "Valor (%)") +
  scale_color_manual(name = "Variáveis", values = c("Crescimento do PIB" = "blue", "Taxa de Pobreza" = "red")) +
  theme_dark() +  # Fundo escuro
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1960, 2022, by = 5))  # Adiciona mais anos ao eixo X

# Exibir o gráfico
print(grafico_linha_eua)
