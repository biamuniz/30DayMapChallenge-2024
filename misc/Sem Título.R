# Instalar e carregar pacote
library(raster)
library(ggplot2)

# Carregar o arquivo raster
raster_file <- raster("/Users/biancamuniz/Downloads/grmsa2022.tif")
# Converter o raster para data frame
raster_df <- as.data.frame(raster_file, xy = TRUE)

# Criar o gráfico com ggplot2
ggplot(data = raster_df, aes(x = x, y = y, fill = grmsa2022_1)) + geom_raster() + scale_fill_viridis_c() +  labs(title = "Visualização do Raster") + theme_minimal()
