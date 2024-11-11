# Carregar pacotes necessários
pacman::p_load(tidyverse, grid, png, osrm, sf, rnaturalearth, rnaturalearthdata)

# Definindo textos e imagens
label_title <- glue::glue("O MAPA DE UM ENCONTRO")
label_subtitle <- glue::glue("Rotas sugeridas entre as cidades de origem do <b><span style = 'color:maroon;'>meu pai</span></b><br>e da <b><span style = 'color:#74641A;'>minha mãe</span></b> até a <b><span style = 'color:darkred;'>cidade onde se conheceram</span></b>, via rodovias")
label_caption <- "Bianca Muniz\n(biancamuniz.bsky.social)"
point_oeiras <- "OEIRAS (PI)"
point_sertaneja <- "SERTANEJA (PR)"
point_cajamar <- "CAJAMAR (SP)"
label_cajamar <- "foi aqui que meus \npais se conheceram"
label_distancia <- glue::glue("A distância entre <b><span style = 'color:darkred;'>Cajamar (SP)</span></b> e a cidade natal da minha mãe,<b><span style = 'color:#74641A;'> Oeiras (PI)</span></b>, é de <b>2.485 km</b>.<br><br><br>Até <b><span style = 'color:maroon;'>Sertaneja (PR)</span></b>, onde meu pai nasceu, a distância é de <b>509 km</b>.")
rosa_dos_ventos <- readPNG("/Users/biancamuniz/Documents/repos/30DayMapChallenge-2024/code/assets/rosa.png")
rosa_grob <- rasterGrob(rosa_dos_ventos, width = unit(4, "cm"), height = unit(4, "cm"), hjust = 0.8)

# Obtendo os shapes
brasil <- ne_countries(scale = "medium", country = "Brazil", returnclass = "sf")
rodovias_path <- "/Users/biancamuniz/Downloads/ne_10m_roads"
rodovias <- st_read(rodovias_path)
cidades_path <- "/Users/biancamuniz/Documents/repos/30DayMapChallenge-2024/code/assets/ne_10m_populated_places/ne_10m_populated_places.shp"
cidades <- st_read(cidades_path)
capitais <- cidades %>% filter(ADM0NAME == "Brazil" & FEATURECLA == "Admin-1 capital")
capitais <- capitais %>% mutate(lon = st_coordinates(capitais)[, 1], lat = st_coordinates(capitais)[, 2])

# Definir coordenadas das cidades e converter para objeto sf
oeiras <- st_as_sf(data.frame(lon = -42.1286, lat = -7.0194), coords = c("lon", "lat"), crs = 4326)
sertaneja <- st_as_sf(data.frame(lon = -50.8311, lat = -23.0361), coords = c("lon", "lat"), crs = 4326)
cajamar <- st_as_sf(data.frame(lon = -46.8687, lat = -23.3517), coords = c("lon", "lat"), crs = 4326)

# Obter rotas usando OpenStreetMap
rota1 <- osrmRoute(src = oeiras, dst = cajamar, overview = "full")
rota2 <- osrmRoute(src = sertaneja, dst = cajamar, overview = "full")

# Converter rotas para objetos sf
rota_sf1 <- st_as_sf(rota1, coords = c("lon", "lat"), crs = 4326, agr = "constant")
rota_sf2 <- st_as_sf(rota2, coords = c("lon", "lat"), crs = 4326, agr = "constant")

# Converter cidades para data.frame para extrair coordenadas
oeiras_df <- as.data.frame(st_coordinates(oeiras))
sertaneja_df <- as.data.frame(st_coordinates(sertaneja))
cajamar_df <- as.data.frame(st_coordinates(cajamar))

# Criar o mapa
ggplot(data = brasil) +
  geom_sf(fill = "antiquewhite", color = "black") +
  geom_sf(data = rodovias, color = "gray70", size = 0.3, linetype = "dotted") +
  geom_sf(data = rota_sf1, color = "#74641A", size = 2) +
  geom_sf(data = rota_sf2, color = "maroon", size = 2) +
  geom_point(data = oeiras_df, aes(x = X, y = Y), color = "#74641A", size = 3) +
  geom_point(data = sertaneja_df, aes(x = X, y = Y), color = "maroon", size = 3) +
  geom_point(data = cajamar_df, aes(x = X, y = Y), color = "darkred", size = 6, shape = 18) +
  coord_sf(xlim = c(-60, -20), ylim = c(-35, 5), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "antiquewhite", color = NA),
    plot.background = element_rect(fill = "antiquewhite", color = "NA", size = 2),
    legend.position = "none",
    plot.title = element_markdown(family = "serif", size = 26, color = "saddlebrown", face = "bold", hjust = 0.5, margin = margin(t = 5, b = 5), padding = unit(c(5, 5, 5, 5), "pt"), box.color = "saddlebrown"),
    plot.subtitle = element_markdown(family = "serif", size = 20, color = "saddlebrown", hjust = 0.5, margin = margin(t = 5, b = 5), padding = unit(c(5, 5, 5, 5), "pt"), box.color = "saddlebrown"),
    plot.caption = element_text(hjust = 1, color = "saddlebrown", size = 15, margin = margin(b = 10, r = 10), family = "serif"),
    plot.margin = margin(10, 10, 10, 10)
  ) + 
  labs(
    title = label_title, 
    subtitle = label_subtitle, 
    caption = label_caption
  ) +
  geom_text(data = capitais, aes(x = lon, y = lat, label = NAME), color = "gray50", family = "serif", size = 2.5, hjust = 1, vjust = 1, alpha = 0.5) +
  geom_text(data = oeiras_df, aes(x = X, y = Y, label = point_oeiras), color = "#74641A", size = 4.5, hjust = -0.1, vjust = -0.5, family = "serif") +
  geom_text(data = sertaneja_df, aes(x = -50, y = -22, label = point_sertaneja), color = "maroon", size = 4.5, hjust = 1, vjust = -0.5, family = "serif") +
  geom_text(data = cajamar_df, aes(x = X, y = Y, label = point_cajamar), color = "darkred", size = 4.5, hjust = -0.1, vjust = 2, family = "serif") +
  geom_label(aes(x = -42.5, y = -26.5, label = label_cajamar), color = "darkred", fill = "antiquewhite", label.r = unit(0.15, "cm"), size = 5, family = "serif", hjust = 0.5, lineheight = 1.1)  +
  geom_textbox(
    aes(x = -29, y = -16, label = label_distancia),
    color = "saddlebrown",
    fill = NA,
    box.color = NA,
    size = 5.5,
    family = "serif",
    width = unit(9.5, "cm"),
    halign = 0.5  # Centraliza o texto
  )+ annotation_custom(rosa_grob, xmin = -24, xmax = -18, ymin = -34, ymax = -28)

# Definir proporção quadrada ao exportar
ggsave("/Users/biancamuniz/Documents/repos/30DayMapChallenge-2024/social/bluesky/day_05_a_bsky.png", width = 10, height = 10, dpi = 800)

# Criar o mapa
ggplot(data = brasil) +
  geom_sf(fill = "antiquewhite", color = "black") +
  geom_sf(data = rodovias, color = "gray70", size = 0.3, linetype = "dotted") +
  geom_sf(data = rota_sf1, color = "#74641A", size = 2) +
  geom_sf(data = rota_sf2, color = "maroon", size = 2) +
  geom_point(data = oeiras_df, aes(x = X, y = Y), color = "#74641A", size = 3) +
  geom_point(data = sertaneja_df, aes(x = X, y = Y), color = "maroon", size = 3) +
  geom_point(data = cajamar_df, aes(x = X, y = Y), color = "darkred", size = 6, shape = 18) +
  coord_sf(xlim = c(-75, -34), ylim = c(-35, 5), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "antiquewhite", color = NA),
    plot.background = element_rect(fill = "antiquewhite", color = "NA", size = 2),
    legend.position = "none",
    plot.caption = element_text(hjust = 1, color = "saddlebrown", size = 15, margin = margin(b = 10, r = 10), family = "serif"),
    plot.margin = margin(10, 10, 10, 10)
  ) + 
  labs(
    caption = label_caption
  ) +
  geom_text(data = capitais, aes(x = lon, y = lat, label = NAME), color = "gray50", family = "serif", size = 2.5, hjust = 1, vjust = 1, alpha = 0.5) +
  geom_text(data = oeiras_df, aes(x = X, y = Y, label = point_oeiras), color = "#74641A", size = 6, hjust = -0.1, vjust = -0.5, family = "serif") +
  geom_text(data = sertaneja_df, aes(x = -50, y = -22, label = point_sertaneja), color = "maroon", size = 6, hjust = 1, vjust = -0.5, family = "serif") +
  geom_text(data = cajamar_df, aes(x = X, y = Y, label = point_cajamar), color = "darkred", size = 6, hjust = -0.1, vjust = 2, family = "serif") +
  annotation_custom(rosa_grob,
                    xmin = -37,
                    xmax = -33,
                    ymin = -34,
                    ymax = -28)
ggsave("/Users/biancamuniz/Documents/repos/30DayMapChallenge-2024/social/bluesky/day_05_b_bsky.png", width = 10, height = 10, dpi = 800)

