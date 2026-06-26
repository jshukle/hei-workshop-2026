# Export SOGA 2025 boundaries to shapefiles -------------------------------

# Open hei-workshop-2026.Rproj before running this script. The repository root
# will then be the working directory.

library(sf)

# Load the workshop copy as an R package so the bundled data objects are
# available as HEIutilities::object_name.
pkgload::load_all("HEIutilities")

output_directory <- "workshop/data"
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

soga_boundaries <- list(
  SOGA2025_country_boundaries_simplified =
    HEIutilities::SOGA2025_country_boundaries_simplified,
  SOGA2025_global_boundary =
    HEIutilities::SOGA2025_global_boundary,
  SOGA2025_disputed_boundaries =
    HEIutilities::SOGA2025_disputed_boundaries
)

write_shapefile <- function(data, name, output_directory) {
  shp_path <- file.path(output_directory, paste0(name, ".shp"))

  sf::st_write(
    obj = data,
    dsn = shp_path,
    driver = "ESRI Shapefile",
    delete_dsn = TRUE,
    quiet = TRUE
  )

  message("Wrote ", shp_path)
}

invisible(
  Map(
    f = write_shapefile,
    data = soga_boundaries,
    name = names(soga_boundaries),
    MoreArgs = list(output_directory = output_directory)
  )
)

