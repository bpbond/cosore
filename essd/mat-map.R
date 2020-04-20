library(raster)
library(sp)
library(dplyr)
library(cosore)
library(tidyr)
library(ggplot2)

#download worldclim data for precip and tmean
precip <- getData("worldclim", path = "essd/", var="prec", res = 10)
tmean <- getData("worldclim", path = "essd/", var = "tmean", res = 10)

#pull out cosore lat and lons
csr_table(table = "description") %>%
  select(CSR_LONGITUDE, CSR_LATITUDE) -> cosore_coords

#extract cosore location data
raster::extract(precip, cosore_coords) -> precip_coords
apply(precip_coords, 1, mean) -> map_cosore
cbind(cosore_coords, map_cosore) -> map_coords

raster::extract(tmean, cosore_coords) -> tmean_vals
apply(tmean_vals, 1, mean) -> mat_cosore
cbind(map_coords, mat_cosore) -> mat_coords

left_join(map_coords, mat_coords) -> cosore_points

#extract climate space data
raster::as.data.frame(precip, xy = TRUE) %>%
  drop_na() -> precip_global

precip_global %>%
  select(-x, -y) %>%
  apply(1, mean) -> map_global

#mat
raster::as.data.frame(tmean, xy = TRUE) %>%
  drop_na() -> tmean_global

tmean_global %>%
  select(-x, -y) %>%
  apply(1, mean) -> mat_global

tibble(x = tmean_global$x, y = tmean_global$y, mat = as.vector(mat_global)) -> mat
tibble(x = precip_global$x, y = precip_global$y, mat = as.vector(map_global)) -> map
left_join(map, mat, by = c("x", "y")) -> map_mat_global

ggplot() +
  geom_hex(data = map_mat_global,
             aes(x = mat.y/10, y = mat.x), bins = 100) +
#  scale_color_continuous(pal) +
  geom_point(data = cosore_points, aes(x = mat_cosore/10, y = map_cosore),
             color = "white", shape = 4) +
  theme_minimal() +
  labs(x = "MAT", y = "MAP")



