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

# Read the coordinate information from the country boundary shapefile.
# We will give this same coordinate system to the PM2.5 raster below.
country_coordinates <- sf::st_crs(country_boundaries)

country_boundaries # could also use glimpse(country_boundaries)
# you can plot these easily, but don't plot the whole object, just plot single columns from it 
plot(country_boundaries |> dplyr::select(loc_id))

# Raster files store gridded data. Each cell has a value.
# We read in the PM2.5 layer with [terra] packageand turn it so it lines up with the map.
pm25 <- terra::rast("workshop/data/pm25_2023.tif")[[2]] |>
  terra::t() |>
  terra::rev()

# IMPORTANT:
# This raster file does not list its coordinate system.
# For this workshop, we know it lines up with the boundary files.
# You can check that by examining the ranges in each object if you want
# So, right after reading the raster, we give it the same coordinate system as
# the country boundaries.
terra::crs(pm25) <- country_coordinates$wkt


# Step 3: Check the map coordinates ---------------------------------------

# Spatial objects need to use the same coordinate system before we map them
# together. A coordinate system tells R how the locations line up on Earth.
#
# For this workshop, we do a simple check that the two shapefiles match.
# The raster already got the country boundary coordinates in Step 2.
country_coordinates <- sf::st_crs(country_boundaries)
pm25_coordinates <- terra::crs(pm25)

# Step 4: Start with a very simple ggplot ---------------------------------

# ggplot() creates a plot.
# geom_sf() adds a spatial layer from an sf object.
ggplot2::ggplot() + 
  ggplot2::geom_sf(data = country_boundaries)


# Step 5: Add style --------------------------------------------------------

# The grammar of graphics idea is:
# start with a plot, then add layers and settings with +.
ggplot2::ggplot() +
  ggplot2::geom_sf(data = country_boundaries, fill = "white", color = "gray50") +
  ggplot2::labs(
    title = "Country boundaries",
    subtitle = "A first map using ggplot2 and sf"
  ) +
  ggplot2::theme_minimal()


# Step 6: Map the raster with country borders -----------------------------

# geom_spatraster() adds a terra raster to a ggplot map.
# geom_sf() adds the country boundary layer on top.
ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = pm25) +
  ggplot2::geom_sf(
    data = country_boundaries,
    fill = NA,
    color = "gray40",
    linewidth = 0.1
  ) +
  ggplot2::scale_fill_viridis_c(
    option = "magma",
    na.value = NA,
    name = "PM2.5"
  ) +
  ggplot2::labs(
    title = "Global PM2.5 in 2023",
    subtitle = "A raster layer plus an sf boundary layer"
  ) +
  ggplot2::theme_minimal()


# Step 7: Save one example map --------------------------------------------

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
