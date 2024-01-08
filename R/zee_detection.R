require(sf)
library(tidyverse)
source("R/convert_dms_to_decimal.R")
sf_path <- "Data/World_EEZ_v12_20231025"

#Open test file 
data <- readxl::read_excel("test/Inventory_MooseGE2023_Sampling.xlsx", skip = 1) |> janitor::clean_names() |> 
  select(longitude_start, latitude_start) |> 
  rename("longitude" = longitude_start,
         "latitude" = latitude_start) |> 
  na.omit()

# Apply the conversion function to the vector
data <- data |> rowwise() |>  mutate(new_lat = convert_dms_to_decimal(latitude),
                                     new_lon = convert_dms_to_decimal(longitude)) |> 
  select(new_lat, new_lon)

names(data) <- c("latitude", "longitude")

eez <- sf::st_read(sf_path, layer="eez_v12")

sf_use_s2(FALSE)

#Transform data to crs 4326
coords_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
#Transform to the system
coords_eez <- coords_sf %>% st_transform(crs = st_crs(eez))
eez %>% st_contains(coords_eez)

finale_data <- coords_eez %>% 
  st_join(eez) |>
  as_tibble() |> 
  mutate(longitude = data$longitude,
                       latitude = data$latitude) |> 
  select(longitude, latitude, TERRITORY1) |> 
  rename("ZEE" = TERRITORY1)

map_data <- map_data("world")
ggplot()+
  geom_point(aes(x = longitude, y = latitude, colour = ZEE), data = finale_data)+
  geom_polygon(aes(x = long, y = lat, group = group), data = map_data)+
  coord_quickmap()+
  xlim(0,15)+
  ylim(39,50)

worldmap <- rnaturalearth::ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')

med_cropped <- st_crop(worldmap, xmin = -5, xmax = 25,
                          ymin = 35, ymax = 48)
ggplot()+
  geom_sf(data = med_cropped)+
  geom_point(aes(x = longitude, y = latitude, fill = ZEE,), shape = 21, colour = "black", data = finale_data, size = 2)+
  scale_fill_brewer(palette = "Pastel1")+
  theme_bw()
