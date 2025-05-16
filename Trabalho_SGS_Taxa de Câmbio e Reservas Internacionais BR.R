# TIRAR NOTAÇÃO CIENTÍFICA
options(scipen = 999)

# DESCARREGAR PACOTES PARA EVITAR CONFLITOS
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)

# INSTALAR PACOTES NECESSÁRIOS
install.packages(c("rbcb", "ggplot2", "dplyr", "scales", "lubridate", "ggthemes"), dependencies = TRUE)
install.packages("GetBCBData")
install.packages("tidyr")

# CARREGAR BIBLIOTECAS
library(scales) 
library(ggplot2) 
library(dplyr) 
library(lubridate) 
library(rbcb) 
library(ggthemes)
library(GetBCBData)
library(tidyr)

# DEFINIR PERÍODO DE ANÁLISE
start_date <- "2010-01-01"
end_date <- "2025-05-16"

# COLETAR DADOS DO SGS
my_ids <- c(cambio = 1, reservas = 13982)
dados <- tryCatch(
  {
    gbcbd_get_series(
      id = my_ids,
      first.date = start_date,
      last.date = end_date,
      format.data = "long",
      use.memoise = TRUE,
      cache.path = tempdir(),
      do.parallel = FALSE
    )
  },
  error = function(e) {
    message("Erro ao coletar dados com SGS 13982. Usando SGS 3546 (Reservas Internacionais, Mensal)...")
    my_ids_alt <- c(cambio = 1, reservas = 3546)
    gbcbd_get_series(
      id = my_ids_alt,
      first.date = start_date,
      last.date = end_date,
      format.data = "long",
      use.memoise = TRUE,
      cache.path = tempdir(),
      do.parallel = FALSE
    )
  }
)

# PIVOTAR DADOS PARA FORMATO WIDE
print("Estrutura dos dados após coleta:")
glimpse(dados)
dados <- dados %>%
  mutate(month = floor_date(ref.date, "month")) %>%
  group_by(month, series.name) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = series.name, values_from = value) %>%
  rename(
    ref.date = month,
    cambio = "cambio",
    reservas = "reservas"
  )

print("Estrutura dos dados após pivot_wider e rename:")
glimpse(dados)

if (!all(c("cambio", "reservas") %in% colnames(dados))) {
  stop("Erro: As colunas 'cambio' e/ou 'reservas' não foram encontradas no dataframe. Verifique a saída de glimpse(dados).")
}


# CONVERTER RESERVAS PARA BILHÕES DE US$
  dados <- dados %>% mutate(reservas = reservas / 1000)

# CRIAR GRÁFICO NO ESTILO THE ECONOMIST
  p <- ggplot(dados, aes(x = ref.date)) +
    geom_line(aes(y = cambio, color = "Taxa de Câmbio (US$)"), size = 1.2) +
    geom_line(aes(y = reservas / 100, color = "Reservas Internacionais (US$ bi)"), size = 1.2) +
    scale_y_continuous(
      name = "Taxa de Câmbio (US$)",
      labels = number_format(accuracy = 0.01),
      sec.axis = sec_axis(~ . * 100, name = "Reservas Internacionais (US$ bi)",
                          labels = number_format(accuracy = 1))
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_color_manual(
      values = c("Taxa de Câmbio (US$)" = "#A12830", "Reservas Internacionais (US$ bi)" = "#005B82"),
      name = ""
    ) +
    labs(
      title = "Taxa de Câmbio e Reservas Internacionais no Brasil (Mensal)",
      subtitle = paste0(format(min(dados$ref.date), "%Y"), " a ", format(max(dados$ref.date), "%b %Y")),
      x = "",
      caption = "Fonte: Banco Central do Brasil (SGS). Elaboração própria."
    ) +
    theme_economist(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14, margin = margin(t = 10, b = 10)),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      plot.caption = element_text(hjust = 0, size = 8),
      axis.title.y = element_text(size = 10, vjust = 1),
      axis.title.y.right = element_text(size = 10, vjust = 1),
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

# EXIBIR GRÁFICO
print(p)
