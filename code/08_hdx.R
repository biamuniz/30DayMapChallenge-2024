# Brazil Population Density Map - by @canaytore - data from Kontur

library(sf)
library(tidyverse)
library(tigris)
library(ggplot2)
library(stars)
library(MetBrewer)
library(colorspace)
library(rayshader)
library(scales)

# source -> https://data.humdata.org/dataset/kontur-population-brazil
brazil <- st_read("/Users/biancamuniz/Downloads/kontur_population_BR_20231101.gpkg")

bb <- st_bbox(brazil)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) %>%
  st_sfc(crs = st_crs(brazil))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) %>%
  st_sfc(crs = st_crs(brazil))

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) %>%
  st_sfc(crs = st_crs(brazil))

height <- st_distance(bottom_left, top_left)

# handle conditions of width or height being the longer side
if(width > height) {
  w_ratio <- 1
  h_ratio <- as.numeric(height / width)
} else {
  h_ratio <- 1
  w_ratio <- as.numeric(width / height)
} 

# convert to raster so we can then convert to matrix
size <- 5000

brazil_rast <- st_rasterize(brazil, nx = floor(size * w_ratio), ny = floor(size * h_ratio))

mat <- matrix(brazil_rast$population, nrow = floor(size * w_ratio), ncol = floor(size * h_ratio))

# create color pallette
colors <- MetBrewer::met.brewer(name="Morgenstern")
tx <- grDevices::colorRampPalette(colors, bias = 1.5)(256)
swatchplot(colors)
swatchplot(tx)

# initial graphic
mat %>%
  height_shade(texture = tx) %>%
  add_overlay(sphere_shade(mat, texture = "desert", 
                           zscale=0.5, colorintensity=4), alphalayer=0.5) %>%
  plot_3d(heightmap = mat,
          windowsize = c(1920, 1080),
          background = "#ffffff",
          zscale = 30,
          solid = FALSE,
          fov = 0,
          theta = -16, zoom = 0.5, phi = 30,
          shadowdepth = -30,
          shadowcolor = colors[7],
          shadow_darkness = 2)


outfile <- "brazil_population_density.png"

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if(!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  
  render_highquality (
    filename = outfile,
    interactive = FALSE,
    parallel = TRUE,
    lightdirection = 350,
    lightaltitude = c(20,80),
    lightcolor = c(colors[5], "white"),
    lightintensity = c(800,200),
    samples = 200,
    width = 1920, 
    height = 1080
  )
  
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}


# generate frames
angles = seq(0, 360, length.out = 51)[-1]
for(i in 1:50) {
  render_camera(theta=-45+angles[i])
  render_snapshot(filename = sprintf("br%i.png", i), 
                  title_text = "Brasil - Densidade Populacional",
                  title_bar_color = "#ffffff", title_color = colors[1], title_position = "north", title_bar_alpha = 1)
  Sys.sleep(0.1)
}

rgl::rgl.close()
