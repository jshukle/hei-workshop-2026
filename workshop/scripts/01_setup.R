# Set up the workshop R environment ---------------------------------------

# Open hei-workshop-2026.Rproj before running this script. The repository
# root must be the working directory.
if (!file.exists("HEIutilities/DESCRIPTION")) {
  stop(
    "Cannot find HEIutilities/DESCRIPTION. ",
    "Open hei-workshop-2026.Rproj and run this script again.",
    call. = FALSE
  )
}

# Use the public CRAN mirror unless the user has already selected another one.
repos <- getOption("repos")
if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

# Bootstrap renv into the user's normal R library. The remaining packages are
# installed into this project's private renv library.
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
lockfile <- file.path(project_root, "renv.lock")

if (file.exists(lockfile)) {
  message("Restoring the workshop environment from renv.lock...")
  renv::load(project = project_root, quiet = TRUE)
  renv::restore(project = project_root, prompt = FALSE)
} else {
  message("Initializing the workshop environment...")
  renv::init(
    project = project_root,
    bare = TRUE,
    load = TRUE,
    restart = FALSE
  )
  renv::load(project = project_root, quiet = TRUE)

  description <- read.dcf("HEIutilities/DESCRIPTION")
  package_imports <- trimws(
    strsplit(description[1, "Imports"], split = ",", fixed = TRUE)[[1]]
  )
  # pkgload provides load_all() directly without devtools' large documentation
  # and website-building dependency chain.
  workshop_packages <- unique(c("pkgload", package_imports))

  message(
    "Installing workshop packages: ",
    paste(workshop_packages, collapse = ", ")
  )
  renv::install(
    workshop_packages,
    project = project_root,
    type = if (.Platform$OS.type == "windows") "binary" else NULL,
    prompt = FALSE,
    exclude = c("HEIutilities")
  )

  # Record the resolved versions so subsequent users restore the same setup.
  renv::snapshot(
    project = project_root,
    packages = c("renv", workshop_packages),
    prompt = FALSE,
    exclude = c("HEIutilities")
  )
}

message("Workshop setup complete. You can now run 02_intro_to_mapping_with_ggplot_sf_terra.R.")
