# Carregar pacotes necessários
pacman::p_load(tidyverse, grid, png, osrm, sf, rnaturalearth, rnaturalearthdata)

# Carregar imagem da rosa dos ventos (ajuste o caminho para o arquivo)
rosa_dos_ventos <- readPNG("/Users/biancamuniz/Documents/repos/30DayMapChallenge-2024/rosa.png")
rosa_grob <- rasterGrob(
  rosa_dos_ventos,
  width = unit(4, "cm"),
  height = unit(4, "cm"),
  hjust = 0.8
)

# Definir coordenadas das cidades e converter para objeto sf
oeiras <- st_as_sf(
  data.frame(lon = -42.1286, lat = -7.0194),
  coords = c("lon", "lat"),
  crs = 4326
)
sertaneja <- st_as_sf(
  data.frame(lon = -50.8311, lat = -23.0361),
  coords = c("lon", "lat"),
  crs = 4326
)
cajamar <- st_as_sf(
  data.frame(lon = -46.8687, lat = -23.3517),
  coords = c("lon", "lat"),
  crs = 4326
)

# Obter rotas usando OpenStreetMap
rota1 <- osrmRoute(src = oeiras,
                   dst = cajamar,
                   overview = "full")
rota2 <- osrmRoute(src = sertaneja,
                   dst = cajamar,
                   overview = "full")

# Obter mapa do Brasil e rodovias principais
brasil <- ne_countries(scale = "medium",
                       country = "Brazil",
                       returnclass = "sf")
rodovias <- ne_download(
  scale = "large",
  type = "roads",
  category = "cultural",
  returnclass = "sf"
)

# Converter rotas para objetos sf
rota_sf1 <- st_as_sf(rota1,
                     coords = c("lon", "lat"),
                     crs = 4326,
                     agr = "constant")
rota_sf2 <- st_as_sf(rota2,
                     coords = c("lon", "lat"),
                     crs = 4326,
                     agr = "constant")

# Obter cidades brasileiras e filtrar capitais
cidades <- ne_download(
  scale = "large",
  type = "populated_places",
  category = "cultural",
  returnclass = "sf"
)
# Filtrar apenas cidades brasileiras e capitais de estado
capitais <- cidades %>%
  filter(ADM0NAME == "Brazil" & FEATURECLA == "Admin-1 capital")

# Extrair as coordenadas de 'capitais' e adicioná-las como colunas 'lon' e 'lat'
capitais <- capitais %>%
  mutate(lon = st_coordinates(capitais)[, 1], lat = st_coordinates(capitais)[, 2])
# Converter cidades para data.frame para extrair coordenadas
oeiras_df <- as.data.frame(st_coordinates(oeiras)) |> mutate(label = "Oeiras, PI")
sertaneja_df <- as.data.frame(st_coordinates(sertaneja)) |> mutate(label = "Sertaneja, PR")
cajamar_df <- as.data.frame(st_coordinates(cajamar)) |> mutate(label = "Cajamar, SP")

# Criar o gráfico
ggplot(data = brasil) +
  geom_sf(fill = "antiquewhite", color = "black") +
  geom_sf(
    data = rodovias,
    color = "gray70",
    size = 0.3,
    linetype = "dotted"
  ) +
  geom_sf(data = rota_sf1,
          color = "#74641A",
          size = 2) +
  geom_sf(data = rota_sf2,
          color = "maroon",
          size = 2) +
  geom_point(
    data = oeiras_df,
    aes(x = X, y = Y),
    color = "#74641A",
    size = 3
  ) +
  geom_point(data = sertaneja_df,
             aes(x = X, y = Y),
             color = "maroon",
             size = 3) +
  geom_point(
    data = cajamar_df,
    aes(x = X, y = Y),
    color = "darkred",
    size = 6,
    shape = 18
  ) +
  coord_sf(xlim = c(-75, -35),
           ylim = c(-35, 5),
           expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "antiquewhite", color = NA),
    plot.background = element_rect(
      fill = "antiquewhite",
      color = "NA",
      size = 2
    ),
    legend.position = "none",
    plot.title = element_text(
      family = "serif",
      size = 22,
      face = "bold",
      color = "saddlebrown",
      hjust = 0.5
    ),
    plot.caption = element_text(
      hjust = 1,
      color = "saddlebrown",
      size = 15,
      margin = margin(b = 10, r = 10),
      family = "serif"
    ),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(title = "Rotas que conectam as cidades de \norigem dos meus pais até Cajamar, SP", caption = "Bianca Muniz\n@biancmuniz") +
  geom_text(
    data = capitais,
    aes(x = lon, y = lat, label = NAME),
    color = "gray50",
    family = "serif",
    size = 2.5,
    hjust = 1,
    vjust = 1,
    alpha = 0.5
  ) +
  geom_text(
    data = oeiras_df,
    aes(x = X, y = Y, label = "OEIRAS (PI)"),
    color = "#74641A",
    size = 4.5,
    hjust = -0.1,
    vjust = -0.5,
    family = "serif"
  ) +
  geom_text(
    data = sertaneja_df,
    aes(x = X, y = Y, label = "SERTANEJA (PR)"),
    color = "maroon",
    size = 4.5,
    hjust = 1,
    vjust = -0.5,
    family = "serif"
  ) +
  geom_text(
    data = cajamar_df,
    aes(x = X, y = Y, label = "CAJAMAR (SP)"),
    color = "darkred",
    size = 4.5,
    hjust = -0.1,
    vjust = 2,
    family = "serif"
  ) +
  annotate(
    "rect",
    xmin = -72,
    xmax = -54,
    ymin = -21,
    ymax = -15,
    fill = "antiquewhite",
    color = "saddlebrown",
    size = 0.5
  ) +
  annotate(
    "text",
    x = -63,
    y = -18,
    label = "Minha mãe nasceu em Oeiras (PI).\nUma semana depois e mais de 2 mil km\nlonge dali, nascia meu pai, em Sertaneja (PR).\nVinte anos mais tarde, se encontraram\nem Cajamar (SP).",
    color = "saddlebrown",
    size = 4.8,
    family = "serif",
    lineheight = 1.1
  ) +
  annotation_custom(
    rosa_grob,
    xmin = -40,
    xmax = -34,
    ymin = -34,
    ymax = -28
  )

# Definir proporção quadrada ao exportar
ggsave("05_a_journey.png",
       width = 10,
       height = 10,
       dpi = 800)
