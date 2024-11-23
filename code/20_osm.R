# Packages loading ---- 
library(osmdata)
library(sf)
library(tidyverse)

# Baixar os dados do polígono do Centro Cultural Vergueiro ----
# Coordenadas aproximadas do Centro Cultural Vergueiro ----
long <- -46.641099
lat <- -23.574317

# Definir um bounding box manual em torno do CCSP ----
bbox_ccsp <- c(
  minx = long - 0.01, # Reduz a longitude
  miny = lat - 0.01,  # Reduz a latitude
  maxx = long + 0.01, # Aumenta a longitude
  maxy = lat + 0.01   # Aumenta a latitude
)

# Baixar os dados do polígono do Centro Cultural Vergueiro ----
ccsp_data <- opq(bbox = bbox_ccsp) %>%
  add_osm_feature(key = "alt_name", value = "Centro Cultural Vergueiro") %>%
  osmdata_sf()

# Extrair o polígono do Centro Cultural Vergueiro ----
ccsp_building <- ccsp_data$osm_polygons

# Validar o resultado
if (nrow(ccsp_building) == 0) {
  stop("Nenhum polígono encontrado para 'Centro Cultural Vergueiro' no bounding box fornecido.")
}

# Obter o centroide do polígono para definir o centro da visualização ----
ccsp_center <- st_centroid(ccsp_building) %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(long = X, lat = Y)

# Configuração do círculo ao redor do polígono central ----
dist <- 3500 # Ajuste o raio do círculo conforme necessário
circle <- ccsp_center %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = 6384) %>%
  st_buffer(dist = dist) %>%
  st_transform(crs = 4326)

# Ajustar o bounding box ao círculo ----
bbx <- st_bbox(circle)

# Baixar dados adicionais para o contexto da visualização ----
# Get buildings ---- 
buildings <- opq(bbox = bbx) %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf() %>%
  .$osm_polygons

highways <- bbx %>%
  opq() %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "motorway",
      "trunk",
      "primary",
      "secondary",
      "tertiary",
      "motorway_link",
      "trunk_link",
      "primary_link",
      "secondary_link",
      "tertiary_link"
    )
  ) %>%
  osmdata_sf()

# Get small streets, pedestrian paths, living streets ----
streets <- bbx %>%
  opq() %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "residential",
      "living_street",
      "service",
      "unclassified",
      "pedestrian",
      "footway",
      "track",
      "path"
    )
  ) %>%
  osmdata_sf()

# Garantir que os CRS sejam compatíveis ----
circle <- st_transform(circle, st_crs(buildings))
streets_lines <- st_intersection(circle, st_make_valid(streets$osm_lines))
highways_lines <- st_intersection(circle, st_make_valid(highways$osm_lines))
buildings_polygons <- st_intersection(circle, st_make_valid(buildings))

# Graphic -----------------------------------------------------------------
ggplot() +
  # Edifícios em geral
  geom_sf(
    data = buildings_polygons, 
    size = .05,
    fill = "#456990", 
    color = "#456990", 
    alpha = .75
  ) + 
  geom_sf(
    data = streets_lines,
    col = "#6b2737",
    size = .4,
    alpha = .65
  ) +
  geom_sf(
    data = highways_lines,
    col = "#114b5f",
    size = .6,
    alpha = .8
  ) + 
  # Prédio do CCSP destacado
  geom_sf(
    data = ccsp_building, 
    size = .5,
    fill = "#db504a", 
    color = "#db504a", 
    alpha = 1
  ) +
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "beige", color = NA),
    plot.margin = margin(b = 10)
  )

# Saving ------------------------------------------------------------------
path <- here::here("plots/day20", "day20_2024")
ggsave(glue::glue("{path}_bsky.png"), width = 10, height = 10, device = ragg::agg_png, dpi = 216)
