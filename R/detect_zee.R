library(sf)
library(dplyr)

detect_zee <- function(data, sf_path){
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
  return(finale_data)
}

