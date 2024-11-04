# Carregar pacotes
library(sf)
library(terra)
library(elevatr)
library(tidyverse)
library(ggridges)
library(geobr)
library(units)

# Carregar e transformar dados de região metropolitana
region_raw <- read_metro_area(year = 2018)
region <- region_raw |> 
  filter(name_metro == "RM São Paulo") |> 
  st_transform(crs = 31984)

# Plotar geometria da região
plot(st_geometry(region), main = "Região Metropolitana de São Paulo")

# Obter e processar dados de elevação
dem <- get_elev_raster(region, z = 10, clip = "bbox", expand = 10000) %>%
  rast() %>%
  mask(vect(region))
names(dem) <- "elev"

# Agrupar dados de elevação
factor <- round(nrow(dem) / 120)
dem_agg <- aggregate(dem, factor)
dem_agg[dem_agg < 0] <- 0
dem_agg[is.na(dem_agg)] <- 0

# Converter dados para data frame
dem_df <- as.data.frame(dem_agg, xy = TRUE, na.rm = FALSE)
dem_miles <- dem_df %>%
  mutate(
    x = set_units(x, "m"),
    x_mile = set_units(x, "mi"),
    y = set_units(y, "m"),
    y_mile = set_units(y, "mi"),
    elev = set_units(elev, "m"),
    elev_mile = set_units(elev, "mi")
  )

# Gráfico de densidade por elevação
ggplot() +
  geom_sf(data = region, color = NA, fill = NA) +
  geom_ridgeline(
    data = dem_df, aes(x = x, y = y, group = y, height = elev),
    scale = 15, fill = "black", color = "white", size = 0.25
  ) +
  theme_void() +
  theme(plot.background = element_rect(fill = "black"))

# Versão estilizada do gráfico com título e anotação
ggplot() +
  geom_sf(data = region, color = NA, fill = NA) +
  geom_density_ridges(
    data = dem_df, aes(x = x, y = y, group = y, height = elev),
    stat = "identity", scale = 15, fill = "black", color = "white", size = 0.25
  ) +
  labs(
    title = "RMSP",
    caption = "UNKNOWN ELEVATIONS"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(family = "Helvetica", color = "white", size = 50, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica", color = "white", size = 40, hjust = 0.5)
  )


# Salvar o gráfico em SVG e PNG
ggsave("/Users/biancamuniz/Documents/repos/30DayMapChallenge-2024/plots/02_lines.svg", device = "svg")
ggsave("/Users/biancamuniz/Documents/repos/30DayMapChallenge-2024/plots/02_lines.png", device = "png")