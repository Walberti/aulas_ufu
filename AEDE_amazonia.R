rm(list=ls())
library(spdep)
library(gridExtra)
library(ggspatial)
library(sf)
library(classInt)
library(RColorBrewer)
library(ggspatial)
library(rgeoda) # geoda to do cluster map
options(scipen = 9999)

setwd("~/pacotes_r/04/dados_amazonia")
amz <- readRDS("dados.rds")


brks <- classIntervals(amz$desm, n = 4, style = "kmeans")
amz$den <- cut(amz$desm, breaks = brks$brks, include.lowest = T, dig.lab = 5)

# Criando labels para os intervalos
labels <- paste0(
  format(brks$brks[-length(brks$brks)], digits = 3), 
  " - ", 
  format(brks$brks[-1], digits = 3))

# Atribuindo os labels à variável 'den'
amz$den <- factor(amz$den, labels = labels)

ggplot() +
  geom_sf(data = amz, aes(fill = den), color = "gray80")+
  scale_fill_manual(values = colorRampPalette(brewer.pal(4, "OrRd"))(4), 
                    name = "Desmatamento (km2)") +
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme_void()+
  theme(text = element_text(size=10, face="bold", color="black"),
        legend.position = "right",
        strip.text = element_text(size = 14),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12))





#================================ weight matrix ================================
# queen matrix
queen_p <- poly2nb(amz, queen=T)
queen_b <- nb2listw(queen_p, style="W", zero.policy = TRUE)

# rock matrix
rook_p <- poly2nb(amz, queen=F)
rook_b <- nb2listw(rook_p, style="W", zero.policy = TRUE)

# k-nearest neighbors, coordinates in a matrix
coords <- st_centroid(st_geometry(amz), of_largest_polygon=TRUE)

k5nn <- knearneigh(coords, k=5)
k5nn_nb <- knn2nb(k5nn)
k5 <- nb2listw(k5nn_nb)

# Distance
knn1 <- knearneigh(coords) #  k-nearest neighbors for k = 1
k1 <- knn2nb(knn1) # convert to nb.

# get the distances between each point and it’s closest neighbor
critical_threshold <- max(unlist(nbdists(k1,coords))) 
critical_threshold # in km 
nb_dist_band <- dnearneigh(coords, 0, critical_threshold)
dist <- nb2listw(nb_dist_band)


moran.test(amz$desm, queen_b)









plot(amz$geometry, border = 'lightgrey')
plot(rook_b, coords, add = TRUE, col = 'blue')