# CÓDIGO OFICIAL DO GRÁFICO DE SÉRIE TEMPORAL
# CRESCIMENTO DO PIB E TAXA DE POBREZA NOS EUA (1961-2022)

# TIRAR NOTAÇÃO CIENTÍFICA ####
options(scipen = 999)

# INSTALAR E CARREGAR PACOTES NECESSÁRIOS ####
# Verificar e instalar pacotes, se necessário
if (!requireNamespace("WDI", quietly = TRUE)) {
  install.packages("WDI")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

# Carregar os pacotes
library(WDI)
library(ggplot2)
library(dplyr)

# Definir as variáveis de interesse (taxa de crescimento do PIB e taxa de pobreza) ####
VARIAVEIS <- c('PIB_CRESCIMENTO' = 'NY.GDP.MKTP.KD.ZG',
               'POBREZA' = 'SI.POV.DDAY')

# CRIAR BASE DE DADOS EUA TODO O PERÍODO ####
dados_eua_todo_periodo <- WDI(indicator = VARIAVEIS,
                              country = 'US',  # Código dos EUA
                              start = 1961, end = 2022)

# LIMPAR OS DADOS EUA (REMOVE NA) ####
dados_eua_todo_periodo_ok <- dados_eua_todo_periodo %>%
  na.omit()

# VISUALIZAR OS DADOS (OPCIONAL) ####
head(dados_eua_todo_periodo_ok)

# ANÁLISE DE REGRESSÃO (Exemplo: crescimento do PIB ~ taxa de pobreza) ####
modelo_st <- lm(PIB_CRESCIMENTO ~ POBREZA, data = dados_eua_todo_periodo_ok)
summary(modelo_st)

# GRÁFICO EM LINHA PARA SÉRIE TEMPORAL (Crescimento do PIB e Pobreza) ####
grafico_linha_eua <- ggplot(data = dados_eua_todo_periodo_ok) +
  geom_line(aes(x = year, y = PIB_CRESCIMENTO, color = "Crescimento do PIB"), size = 1) +
  geom_line(aes(x = year, y = POBREZA, color = "Taxa de Pobreza"), size = 1) +
  labs(title = "Crescimento do PIB e Taxa de Pobreza nos EUA (1961-2022)",
       x = "Ano", 
       y = "Crescimento do PIB e Pobreza (%)") +
  scale_color_manual(name = "Variáveis", values = c("Crescimento do PIB" = "blue", "Taxa de Pobreza" = "red")) +
  theme_dark() +  # Fundo escuro
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1960, 2022, by = 5))  # Adiciona mais anos ao eixo X

# Exibir o gráfico ####
print(grafico_linha_eua)