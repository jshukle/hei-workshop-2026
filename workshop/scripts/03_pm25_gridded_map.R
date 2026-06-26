# State of Global Air 2025: gridded PM2.5 map ------------------------------

# Open hei-workshop-2026.Rproj before running this script. The repository root
# will then be the working directory.

library(dplyr)

library(sf)
library(terra)
library(tidyterra)

library(ggplot2)
library(ggtext)
library(ggthemes)

# Load the workshop R package.
pkgload::load_all("HEIutilities")

# Spatial data bundled with HEIutilities.
country_boundaries <- HEIutilities::SOGA2025_country_boundaries_simplified
global_boundary <- HEIutilities::SOGA2025_global_boundary
disputed_boundaries <- HEIutilities::SOGA2025_disputed_boundaries

# Load and prepare the PM2.5 raster
pm25_path <- "workshop/data/pm25_2023.tif"
pm25 <- terra::rast(pm25_path)[[2]]
pm25 <- rev(t(pm25))

pm25_crop <- terra::crop(pm25, global_boundary)
# plot(pm25_crop)
pm25_clip <- terra::mask(pm25_crop, global_boundary)
# plot(pm25_clip)

# Classify concentrations using WHO guideline and interim-target thresholds.
breaks <- c(0, 5, 10, 15, 25, 35, Inf)
breaks_labels <- c(
  "≤ 5 (<WHO Guideline)",
  "5-10 (<WHO IT-4)", # techincally should be "5 to ≤ 10" but we simplify it
  "10-15 (<WHO IT-3)",
  "15-25 (<WHO IT-2)",
  "25-35 (<WHO IT-1)",
  "> 35 (>WHO IT-1)",
  "(IT = Interim Target)"
)

rast_minmax <- terra::minmax(pm25_clip)
map_data <- terra::classify(pm25_clip, breaks, include.lowest = TRUE)
# unique(map_data)

output_directory <- "workshop/output/pm25/"
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

# This is the custom mapping function we bring it with HEIutilities package
# It will do all the hard work for us behind the scenes and output multiple final images
# We also use get_hei_palette() from HEIutilities to get some palettes that we want

# ?create_hei_gridded
# ?get_hei_palette

# NOTE: adding roxygen documenation is perhaps one of the best uses of gen AI, there is no
# excuse not to have fully documented functions these days. :)

create_hei_gridded(
  gridded_data = map_data,
  max_global_cells = 5e6,
  watermark_text = "State of Global Air 2025",
  poly_data = country_boundaries,
  bord_col = "gray40",
  disputed = TRUE,
  disputed_boundaries = disputed_boundaries,
  disp_col = "gray40",
  disp_fil = get_hei_palette("soga_2025_6")[7],
  palette = get_hei_palette("soga_2025_6")[1:6],
  trans = "identity",
  cut_data = TRUE,
  breaks = breaks,
  legend_labels = breaks_labels,
  legend_title = "PM<sub>2.5</sub> (µg/m<sup>3</sup>)",
  legend_blank_col = "white",
  legend_pos_x = 0,
  legend_pos_y = .05,
  plot_width = 2650,
  plot_height = 1500,
  plot_legend_title_size = 10,
  plot_legend_text_size = 7,
  plot_key_size = .5,
  plot_legend_background = "white",
  save_plot = TRUE,
  out_path = output_directory,
  out_file_name = "2023_pm25_gridded_cut",
  out_types = c("png", "jpg", "pdf"),
  # below is the housekeeping and data documentation portion. We do this for ALL static plotting functions. 
  # This lets other analysts compare what is plotted against raw data, say using QGIS, for accuracy checks.
  info_source = pm25_path,
  info_year = "2023",
  info_poll = "pm25",
  info_data = rast_minmax
)

# Why use this package-based workflow?
#
# - renv handles all version control, which makes it safer
#   to reopen the project later (say years later!!) and rebuild the same map with the same tools,
#   or with updated data.
#
# - The package keeps repeated mapping decisions in one reviewed function instead
#   of copying and editing the same plotting code across many scripts.
#
# - HEIutilities bundles shared boundary data and palettes, so scripts are less
#   dependent on one person's local folders or memory of which files/colors to use.
#
# - create_hei_gridded() writes multiple image formats from the same plot object,
#   reducing the chance that PNG, JPG, and PDF outputs drift apart.
#
# - The metadata arguments create a companion text record of the source, year,
#   pollutant, data range, and transformation used for the final image.
#
# - Overall, this is not just "less code"; it is a safer image pipeline because
#   style, inputs, documentation, and outputs are handled consistently each time.
