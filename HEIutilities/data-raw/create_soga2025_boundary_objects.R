# Create SOGA 2025 boundary data for the HEIutilities package -------------

# This script shows how to save objects inside an R package so they can be used
# like this:
#
# HEIutilities::SOGA2025_disputed_boundaries


# Step 1: Remember the folder you started in.
starting_folder <- getwd()


# Step 2: Move into the R package folder.
# usethis::use_data() must be run from inside the package, not from the main
# workshop project folder.
setwd("D:/zrsa/Projects/hei-workshop-2026/HEIutilities")


# Step 3: Load the packages we need.
library(sf)
library(usethis)


# Step 4: Tell usethis that HEIutilities is the package we are working on.
usethis::proj_set(".", force = TRUE)


# Step 5: Read the shapefiles into R.
# The names on the left are the names people will use from the package later.
SOGA2025_country_boundaries_simplified <- sf::st_read(
  "../workshop/data/SOGA2025_country_boundaries_simplified.shp"
)

SOGA2025_global_boundary <- sf::st_read(
  "../workshop/data/SOGA2025_global_boundary.shp"
)

SOGA2025_disputed_boundaries <- sf::st_read(
  "../workshop/data/SOGA2025_disputed_boundaries.shp"
)


# Step 6: Save these objects inside the package.
# This creates files in HEIutilities/data/.
usethis::use_data(
  SOGA2025_country_boundaries_simplified,
  SOGA2025_global_boundary,
  SOGA2025_disputed_boundaries,
  overwrite = TRUE
)


# Step 7: Move back to the folder you started in.
setwd(starting_folder)


# After the package is reloaded, these objects can be used like this:
#
# country_boundaries <- HEIutilities::SOGA2025_country_boundaries_simplified
# global_boundary <- HEIutilities::SOGA2025_global_boundary
# disputed_boundaries <- HEIutilities::SOGA2025_disputed_boundaries
