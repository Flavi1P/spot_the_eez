require(sf)
library(tidyverse)
source("R/convert_dms_to_decimal.R")
source("R/detect_zee.R")
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

data_with_zee <- detect_zee(data, sf_path)

worldmap <- rnaturalearth::ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')

med_cropped <- st_crop(worldmap, xmin = -1, xmax = 25,
                          ymin = 35, ymax = 45)
ggplot()+
  geom_sf(data = med_cropped)+
  geom_point(aes(x = longitude, y = latitude, fill = ZEE,), shape = 21, colour = "black", data = data_with_zee, size = 2)+
  scale_fill_brewer(palette = "Pastel1")+
  theme_bw()
