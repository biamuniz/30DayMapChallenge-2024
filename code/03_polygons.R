# install.packages("remotes") # Uncomment if you do not have the 'remotes' package installed
# remotes::install_github("koenderks/rcityviews", dependencies = TRUE) # Uncomment if you do not have the 'rcityviews' package installed
library(rcityviews)
atx <- new_city(name = "Austin, TX", country = "USA", lat = 30.2875389, long = -97.7241645)
cityview(name = atx, zoom = 5, theme = "original") 