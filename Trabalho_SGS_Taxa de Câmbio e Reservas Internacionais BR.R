# TIRAR NOTAÇÃO CIENTÍFICA
options(scipen = 999)

# DEFINIR REPOSITÓRIO
options(repos = "https://cran.r-project.org")

# INSTALAR PACOTES NECESSÁRIOS
required_packages <- c("rbcb", "ggplot2", "dplyr", "scales", "lubridate", "ggthemes", "GetBCBData", "tidyr", "gganimate", "gifski")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages)
}

# CARREGAR BIBLIOTECAS
library(scales)
library(ggplot2)
library(dplyr)
library(lubridate)
library(rbcb)
library(ggthemes)
library(GetBCBData)
library(tidyr)
library(gganimate)
library(gifski)

# DEFINIR PERÍODO
start_date <- "2010-01-01"
end_date <- "2025-05-16"

# COLETAR DADOS
my_ids <- c(cambio = 1, reservas = 13982)
dados <- tryCatch({
  gbcbd_get_series(id = my_ids, first.date = start_date, last.date = end_date,
                   format.data = "long", use.memoise = TRUE,
                   cache.path = tempdir(), do.parallel = FALSE)
}, error = function(e) {
  message("Erro ao coletar dados com SGS 13982. Usando SGS 3546...")
  my_ids_alt <- c(cambio = 1, reservas = 3546)
  gbcbd_get_series(id = my_ids_alt, first.date = start_date, last.date = end_date,
                   format.data = "long", use.memoise = TRUE,
                   cache.path = tempdir(), do.parallel = FALSE)
})

# TRANSFORMAR DADOS
dados <- dados %>%
  mutate(month = floor_date(ref.date, "month")) %>%
  group_by(month, series.name) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = series.name, values_from = value) %>%
  rename(ref.date = month, cambio = "cambio", reservas = "reservas") %>%
  mutate(reservas = reservas / 1000)

stopifnot(all(c("cambio", "reservas") %in% names(dados)))

# CRIAR GRÁFICO
p_anim <- ggplot(dados, aes(x = ref.date)) +
  geom_line(aes(y = cambio, color = "Taxa de Câmbio (US$)"), size = 1.2) +
  geom_line(aes(y = reservas / 100, color = "Reservas Internacionais (US$ bi)"), size = 1.2) +
  scale_y_continuous(
    name = "Taxa de Câmbio (US$)",
    labels = number_format(accuracy = 0.01),
    sec.axis = sec_axis(~ . * 100, name = "Reservas Internacionais (US$ bi)", labels = number_format(accuracy = 1))
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
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0, size = 9),
    axis.title.y = element_text(size = 12),
    axis.title.y.right = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) +
  transition_reveal(ref.date)

# ANIMAR E SALVAR
anim <- animate(p_anim, renderer = gifski_renderer(loop = TRUE), fps = 10, duration = 15, width = 800, height = 600)
anim_save("grafico_animated.gif", animation = anim)
