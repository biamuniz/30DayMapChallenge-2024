library(elevatr)
library(rayshader)
library(ggplot2)

# Definir o local (exemplo usando coordenadas para São Paulo)
local <- data.frame(x = -46.765278, y = -23.4583326)
"/Users/biancamuniz/Downloads/geological_map_of_south_america_2019.tiff"
# Baixar dados de elevação
elev_data <- get_elev_raster(local, z = 6, prj = "+proj=longlat +datum=WGS84")

# Converter o raster para matriz, necessária para rayshader
elev_matrix <- matrix(
  raster::extract(elev_data, raster::extent(elev_data), buffer = 1000),
  nrow = ncol(elev_data),
  ncol = nrow(elev_data)
)
elev_matrix %>%
  sphere_shade(texture = "imhof3") %>%
  plot_3d(elev_matrix, windowsize = c(1200, 1200),
          zscale = 20, zoom = 0.75, phi = 89, theta = 0, fov = 0, background = "black")

# Aplicar elevação e renderizar em 3D com rayshader
elev_matrix %>%
  height_shade() %>%  # Paleta de cores 'desert'
  add_shadow(ray_shade(elev_matrix,zscale=1),0.3) %>%
  add_overlay(sphere_shade(elev_matrix, texture = "imhof1", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(elev_matrix,zscale = 10),0) %>%
  plot_3d(elev_matrix, zscale = 100, fov = 0, theta = 75, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
Sys.sleep(0.2)
render_snapshot()
  plot_map()

# Salvar o modelo renderizado como imagem
render_snapshot("mapa_3d.png")
