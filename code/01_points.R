library(tidyverse)
library(sf)
library(geobr)
library(tidygeocoder)
library(janitor)
library(cartogram)
library(ggiraph)
library(patchwork)
library(googlesheets4)
library(janitor)
library(shadowtext)

sheet_url <- "https://docs.google.com/spreadsheets/d/1tgWGanTiCr9tgHTd34MnDQ8gt0yDATghi-OoemokdJY/edit#gid=0"
df_raw <- read_sheet(sheet_url) |> clean_names()
df_raw <- df_raw |>
  filter(
    !local %in% c(
      "Zilker Park",
      "SESC Jundiaí",
      "Aldeia Jundiaí",
      "Yellow Pubmarine Jundiaí",
      "Teatro Polytheama Jundiaí"
    )
  ) |>
  mutate(local = ifelse(local == "Centro Cultural São Paulo", "CCSP", local)) |>
  mutate(local = ifelse(local == "Vale do Anhangabaú", "Vl. Anhangabaú", local)) |> 
  mutate(local = ifelse(local == "Autódromo de Interlagos", "Autódromo", local))
df <- df_raw |>
  mutate(across(where(is.character), str_trim)) |> 
  count(local, latitude, longitude, name = "frequencia") |> 
  filter(!is.na(latitude) &
           !is.na(longitude))

df <- df |>
  mutate(tooltip_text = sprintf(": já vi %d show(s) aqui!", frequencia))

df_sf <- st_as_sf(df,
                  coords = c("longitude", "latitude"),
                  crs = "EPSG:4326") |>
  st_transform(crs = 3857)

df_sf_dorling <- cartogram_dorling(df_sf, weight = "frequencia", k = 1)

sp_shapefile <- read_municipality(code_muni = 3550308, year = 2020) |>
  st_transform(crs = 3857)

p_map <- ggplot() +
  geom_sf(
    data = sp_shapefile,
    fill = "#F8EDD1",
    color = "#003f5c",
    linewidth = 0.2
  ) +
  geom_sf_interactive(
    data = df_sf_dorling,
    aes(
      size = frequencia,
      tooltip = paste0(local, tooltip_text),
      data_id = local
    ),
    shape = 21,
    color = "#003f5c",
    fill = "#D88A8A",
    alpha = 0.7
  ) +
  labs(title = "Onde vi shows em São Paulo",
       subtitle = "O mapa mostra casas de show, bares e teatros, centros culturais onde vi shows em desde 2013. 
    Círculos indicam cada espaço, e barras destacam os lugares com mais de cinco apresentações.
    Quanto maior o círculo ou a barra, maior a quantidade de shows. 
    Passe o mouse para mais detalhes."
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

top10_locais <- df |>
  arrange(desc(frequencia)) |>
  filter(frequencia > 4)

p_top10 <- top10_locais |>
  mutate(local = fct_reorder(local, frequencia)) |>
  ggplot(aes(y = local, x = frequencia)) +
  geom_col_interactive(aes(data_id = local),
                       fill = "#D88A8A",
                       width = 0.4) +
  geom_text(
    aes(label = frequencia),
    hjust = -0.3,
    vjust = 0.5,
    color = "white"
  ) +
  labs(x = "Frequência", y = NULL) +
  theme_minimal() +
  geom_shadowtext(
    data = subset(top10_locais, frequencia > 4),
    aes(frequencia, y = local, label = frequencia),
    hjust = 0,
    nudge_x = 0.3,
    colour = "black",
    bg.colour = "white",
    bg.r = 0.2,
    family = "Raleway",
    size = 3
  ) +
  geom_text(
    data = subset(top10_locais, frequencia > 4),
    aes(0, y = local, label = local),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    family = "Raleway",
    size = 2.5
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_blank()
  )

title <- ggplot() +
  annotate(
    "text",
    x = 1,
    y = 1,
    label = " ",
    size = 5,
    family = "Bitter",
    fontface = "bold",
    hjust = 0.75
  ) +
  theme_void() +
  theme(plot.margin = margin(b = 10, l = 10))

credits <- ggplot() + 
  annotate("text", x = 1, y = 1, 
           label = "Dados e visualização: Bianca Muniz (@biancmuniz)", 
           size = 2.5, hjust = 0.2, family="Raleway") +
  theme_void() +
  theme(
    plot.margin = margin(t = 10, l = 10)
  )

p_combined <- title / (p_map + p_top10 + plot_layout(
  ncol = 2,
  widths = c(2, 2),
  guides = "collect"
)) / credits +
  plot_layout(heights = c(0.1, 1, 0.05)) &
  theme(
    plot.margin = margin(
      t = 0,
      b = 0,
      l = 0,
      r = 0
    ),
    plot.subtitle = element_text(family = "Raleway", size = 9, hjust = 0),
    plot.caption = element_text(family = "Bitter"),
    plot.title = element_text(family = "Bitter", face = "bold", size = 20)
  )

interactive_chart <- girafe(
  ggobj = p_combined,
  options = list(
    opts_selection(type = "single", only_shiny = FALSE),
    opts_hover_inv(css = "opacity:0.1;"),
    opts_hover(css = "stroke-width:2px;")
  )
)
htmltools::save_html(interactive_chart, file.path("/Users/biancamuniz/Documents/repos/30DayMapChallenge-2024/plots/01-points.html"))
