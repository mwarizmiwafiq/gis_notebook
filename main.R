library(sf)
library(tigris)
library(tidyverse)

data <- st_read("data/kontur_jabodetabek_pop_2022.gpkg")
  
st <- states()

florida <- st |>
  filter(NAME == "Florida")

florida |>
  ggplot() +
  geom_sf()

# define aspect ration based on bounding box

bb <- st_bbox(data)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |>
    st_sfc(crs = st_crs(data))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

# check by plotting points

data |> 
  ggplot() +
  geom_sf() +
  geom_sf(data = bottom_left) +
  geom_sf(data = bottom_right, color = "red")

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |> 
  st_sfc(crs = st_crs(data))

height <- st_distance(bottom_left, top_left)

# handle conditions of width or height being the longer side

if (width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ration <- 1
  w_ratio <- width / height
}

