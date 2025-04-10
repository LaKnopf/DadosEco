# CÓDIGO OFICIAL DO GRÁFICO DE CORTE TRANSVERSAL
# RELAÇÃO ENTRE POBREZA E EXPECTATIVA DE VIDA EM 2022

# TIRAR NOTAÇÃO CIENTÍFICA ####
options(scipen = 999)

# CARREGAR PACOTES NECESSÁRIOS ####
# install.packages('WDI')
# install.packages('tidyverse')
# install.packages('countrycode')
library(WDI)
library(tidyverse)
library(countrycode)

# VARIÁVEIS ####
# TAXA DE POBREZA (PERCENTUAL DA POPULAÇÃO ABAIXO DA LINHA DE POBREZA): SI.POV.DDAY
# EXPECTATIVA DE VIDA AO NASCER (ANOS): SP.DYN.LE00.IN
VARIAVEIS <- c('POBREZA' = 'SI.POV.DDAY',
               'EXPECTATIVA_VIDA' = 'SP.DYN.LE00.IN')

# BASE DE DADOS DE CORTE TRANSVERSAL ####
dados_countries <- WDI(indicator = VARIAVEIS,
                       country = 'all', # PARA TODOS OS PAÍSES
                       start = 2022, end = 2022)

# ADICIONAR A VARIÁVEL 'CONTINENTE' ####
dados_countries$continente <- countrycode(dados_countries$iso2c, "iso2c", "continent")

# LIMPAR OS DADOS (REMOVE NA E PAÍSES NÃO CLASSIFICADOS) ####
dados_countries_ok <- dados_countries %>%
  na.omit() %>%  # Remove linhas com NA nas variáveis POBREZA e EXPECTATIVA_VIDA
  filter(!is.na(continente)) %>%  # Remove países sem classificação de continente
  mutate(continente = recode(continente,  # Corrige a acentuação dos continentes
                             "Africa" = "África",
                             "Americas" = "Américas",
                             "Asia" = "Ásia",
                             "Europe" = "Europa",
                             "Oceania" = "Oceania"))

# REGRESSÃO ####
#'ct' = corte transversal
modelo_ct <- lm(POBREZA ~ EXPECTATIVA_VIDA, data = dados_countries_ok)
summary(modelo_ct) # VISUALIZAR OS RESULTADOS

# GRÁFICO DE PONTOS PARA CORTE TRANSVERSAL ####
grafico_ponto <- 
  ggplot(data = dados_countries_ok,
         mapping = aes(x = EXPECTATIVA_VIDA, y = POBREZA, color = continente)) +
  geom_point(size = 3) +  # Ajusta o tamanho dos pontos
  scale_color_manual(values = c("África" = "yellow", 
                                "Américas" = "purple", 
                                "Ásia" = "blue", 
                                "Europa" = "green", 
                                "Oceania" = "red")) +  # Define as cores conforme o continente
  labs(title = "Relação entre Pobreza e Expectativa de Vida em 2022",
       x = "Expectativa de Vida em Anos", 
       y = "Pobreza (% da população)",
       color = "Continente") +  # Adiciona o título e as legendas
  theme_minimal(base_size = 15) +  # Tema minimalista com tamanho de texto base
  theme(
    plot.background = element_rect(fill = "black", color = NA),  # Fundo escuro
    panel.background = element_rect(fill = "black"),  # Fundo do painel escuro
    axis.text = element_text(color = "white"),  # Texto dos eixos em branco
    axis.title = element_text(color = "white"),  # Título dos eixos em branco
    plot.title = element_text(color = "white", hjust = 0.5, size = 14, face = "bold"),  # Título centralizado, branco e ajustado
    legend.background = element_rect(fill = "black"),  # Fundo da legenda escuro
    legend.text = element_text(color = "white", size = 8),  # Texto da legenda em branco e menor
    legend.title = element_text(color = "white", size = 10),  # Título da legenda em branco e menor
    legend.position = "bottom"  # Posiciona a legenda na parte inferior
  ) +
  # Adicionar rótulos apenas para o Brasil e EUA
  geom_text(data = dados_countries_ok %>% filter(iso2c %in% c("BR", "US")), 
            aes(x = EXPECTATIVA_VIDA, y = POBREZA, label = country), 
            color = "white", vjust = -1, hjust = 0.5, size = 4)  # Rótulos para o Brasil e EUA

# EXIBIR O GRÁFICO ####
print(grafico_ponto)