# Introduction to mapping with ggplot2, sf, and terra ----------------------

# Goal:
# This script gives a first look at how ggplot2 makes maps.
#
# We will use:
# - ggplot2 for plotting
# - sf for shapefiles, such as country borders
# - terra for raster data, such as gridded PM2.5 air pollution
#
# Open hei-workshop-2026.Rproj before running this script. The repository root
# should be the working directory.


# Step 1: Load the packages ------------------------------------------------

library(ggplot2)
library(sf)
library(terra)
library(tidyterra)


# Step 2: Read the spatial data -------------------------------------------

# Shapefiles store shapes, such as country borders.
country_boundaries <- sf::st_read(
  "workshop/data/SOGA2025_country_boundaries_simplified.shp"
)
country_boundaries
glimpse(country_boundaries)
plot(country_boundaries)

global_boundary <- sf::st_read(
  "workshop/data/SOGA2025_global_boundary.shp"
)

disputed_boundaries <- sf::st_read(
  "workshop/data/SOGA2025_disputed_boundaries.shp"
)

# Raster files store gridded data. Each cell has a value.
pm25 <- terra::rast("workshop/data/pm25_2023.tif")[[2]]


# Step 3: Check the map coordinates ---------------------------------------

# Spatial files need to use the same coordinate system before we map them
# together. A coordinate system tells R how the locations line up on Earth.
#
# For this workshop, we only do a simple check: the files should match.
country_coordinates <- sf::st_crs(country_boundaries)
global_coordinates <- sf::st_crs(global_boundary)
disputed_coordinates <- sf::st_crs(disputed_boundaries)
pm25_coordinates <- sf::st_crs(terra::crs(pm25))

country_coordinates
global_coordinates
disputed_coordinates
pm25_coordinates

country_epsg <- country_coordinates$epsg
global_epsg <- global_coordinates$epsg
disputed_epsg <- disputed_coordinates$epsg
pm25_epsg <- pm25_coordinates$epsg

if (!identical(country_epsg, global_epsg)) {
  stop("The country boundaries and global boundary use different coordinates.")
}

if (!identical(country_epsg, disputed_epsg)) {
  stop("The country boundaries and disputed boundaries use different coordinates.")
}

if (!identical(country_epsg, pm25_epsg)) {
  stop("The country boundaries and PM2.5 raster use different coordinates.")
}


# Step 4: Start with a very simple ggplot ---------------------------------

# ggplot() creates a plot.
# geom_sf() adds a spatial layer from an sf object.
ggplot() +
  geom_sf(data = country_boundaries)


# Step 5: Add style --------------------------------------------------------

# The grammar of graphics idea is:
# start with a plot, then add layers and settings with +.
ggplot() +
  geom_sf(data = country_boundaries, fill = "white", color = "gray50") +
  labs(
    title = "Country boundaries",
    subtitle = "A first map using ggplot2 and sf"
  ) +
  theme_minimal()


# Step 6: Add another spatial layer ---------------------------------------

# A map can have many layers. Here we draw country boundaries first, then draw
# disputed boundaries on top.
ggplot() +
  geom_sf(data = country_boundaries, fill = "white", color = "gray70") +
  geom_sf(data = disputed_boundaries, fill = NA, color = "red") +
  labs(
    title = "Adding layers",
    subtitle = "Country boundaries plus disputed boundaries"
  ) +
  theme_minimal()


# Step 7: Prepare the PM2.5 raster ----------------------------------------

# The PM2.5 file is a grid. To keep the map focused, we crop it to the global
# boundary and remove cells outside the boundary.
pm25 <- rev(t(pm25))
pm25_crop <- terra::crop(pm25, global_boundary)
pm25_clip <- terra::mask(pm25_crop, global_boundary)


# Step 8: Map the raster with country borders -----------------------------

# geom_spatraster() adds a terra raster to a ggplot map.
# geom_sf() adds the country boundary layer on top.
ggplot() +
  tidyterra::geom_spatraster(data = pm25_clip) +
  geom_sf(data = country_boundaries, fill = NA, color = "gray40", linewidth = 0.1) +
  scale_fill_viridis_c(
    option = "magma",
    na.value = NA,
    name = "PM2.5"
  ) +
  labs(
    title = "Global PM2.5 in 2023",
    subtitle = "A raster layer plus an sf boundary layer"
  ) +
  theme_minimal()


# Step 9: Save one example map --------------------------------------------

output_directory <- "workshop/output/intro_mapping"
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = file.path(output_directory, "intro_pm25_map.png"),
  width = 10,
  height = 6,
  dpi = 300
)


# Next:
# Script 03 shows a more repeatable workshop-style way to make the final map.
