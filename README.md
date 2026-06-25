# HEI State of Global Air mapping workshop

This project contains a small, self-contained example of the workflow used to
produce a State of Global Air gridded exposure map. The R package and the
workshop materials are deliberately kept in separate sibling directories.

## Contents

- `HEIutilities/`: the workshop-specific subset of the HEI utilities R package
- `workshop/scripts/01_setup.R`: the `renv` environment setup
- `workshop/scripts/02_pm25_gridded_map.R`: the PM2.5 mapping demonstration
- `workshop/data/pm25_2023.tif`: the source PM2.5 raster
- `workshop/output/`: map files created when the demonstration runs

Open `hei-workshop-2026.Rproj` so the repository root is the working directory.
Run `workshop/scripts/01_setup.R` once to create or restore the private `renv`
library. Then run `workshop/scripts/02_pm25_gridded_map.R`.

The mapping script loads the package with
`pkgload::load_all("HEIutilities")`. Package dependencies are read directly
from `HEIutilities/DESCRIPTION` by the setup script.
