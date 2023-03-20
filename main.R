library(sf)
library(tigris)
library(tidyverse)
library(stars)
library(units)
library(rayshader)
library(MetBrewer)
library(colorspace)

data <- st_read("data/kontur_jabodetabek_pop_2022.gpkg")
  

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
  geom_sf(data = bottom_left, color = "blue") +
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
  h_ratio <- 1
  w_ratio <- width / height
}


size <- 2000

nx1 <- as.numeric( floor(size * w_ratio))
ny1 <- floor(size * h_ratio)

jbdt_rast <- st_rasterize(data,
                          nx = nx1,
                          ny = ny1)

matr <- matrix(jbdt_rast$population,
               nrow = floor(size * w_ratio),
               ncol = floor(size * h_ratio))

plot(jbdt_rast)

#create color
cl <- met.brewer("Hokusai2")
swatchplot(cl)

texture2 <- grDevices::colorRampPalette(cl)(256)
swatchplot(texture)


#PLOT 3D MAP

matr |>
  height_shade(texture = texture2) |>
  plot_3d(heightmap = matr,
          zscale = 150,
          solid = FALSE,
          shadowdepth = 0)

render_camera(theta = 145, phi = 36, zoom = 0.7)

outfile <- "images/final_plot.png"

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if (!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  render_highquality(
    filename = outfile,
    interactive = FALSE,
    lightdirection = 280,
    lightaltitude = c(20, 80),
    lightintensity = c(600, 100),
    samples = 450,
    width = 6000,
    height = 6000
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}





