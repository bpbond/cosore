# Script to extract terrestrial MAT and MAP data from WorldClim
# Created April 2020 | Stephanie Pennington

# Load libraries
library(raster)
library(sp)
library(dplyr)
library(cosore)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

# Download worldclim data for precip and tmean, store in essd/ folder
if(!file.exists("wc10/prec_10m_bil.zip")) {
  precip <- getData("worldclim", path = "essd/", var = "prec", res = 10)
  tmean <- getData("worldclim", path = "essd/", var = "tmean", res = 10)
}

# Pull out cosore dataset latitudes and longitudes
csr_table(table = "description") %>%
  select(CSR_LONGITUDE, CSR_LATITUDE) -> cosore_coords

# Extract cosore location data from worldclim data for precip...
raster::extract(precip, cosore_coords) -> precip_coords
apply(precip_coords, 1, mean) -> map_cosore
cbind(cosore_coords, map_cosore) -> map_coords

# ...and tmean
raster::extract(tmean, cosore_coords) -> tmean_vals
apply(tmean_vals, 1, mean) -> mat_cosore
cbind(map_coords, mat_cosore) -> mat_coords

left_join(map_coords, mat_coords) %>%
  # Temp data is stored in degC * 10, so we need to divide to get back to degC
  mutate(mat_cosore = mat_cosore/10) -> cosore_points

# Extract global climate space data
raster::as.data.frame(precip, xy = TRUE) %>%
  drop_na() -> precip_global

# Calculate annual mean for precip...
precip_global %>%
  select(-x, -y) %>%
  apply(1, mean) -> map_global

raster::as.data.frame(tmean, xy = TRUE) %>%
  drop_na() -> tmean_global

# ...and tmean
tmean_global %>%
  select(-x, -y) %>%
  apply(1, mean) -> mat_global

# Create tibble with corresponding coordinates
tibble(x = tmean_global$x, y = tmean_global$y, mat = as.vector(mat_global)) -> mat
tibble(x = precip_global$x, y = precip_global$y, map = as.vector(map_global)) -> map

left_join(map, mat, by = c("x", "y")) %>%
  # Temp data is stored in degC * 10, so we need to divide to get back to degC
  mutate(mat = mat/10) -> map_mat_global

