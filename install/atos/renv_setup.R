#!/usr/bin/env Rscript
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Get location where this script should exist
rwd <- Sys.getenv("RWD")
if (!dir.exists(rwd)) {
  stop("RWD=",rwd,"not found")
} else {
  message("Using R scripts in ",rwd)
}

# Set github token in here if desired
renviron_path <- file.path(rwd,".Renviron")
if (file.exists(renviron_path)) {
  message("Sourcing ",renviron_path)
  readRenviron(renviron_path)
}
if (Sys.getenv("GITHUB_PAT") == "") {
  message("Warning: No GITHUB_PAT set! The harp installation may fail with messages such as 'Failed to install unknown package'!")
  message("Warning: You should consider adding a GITHUB_PAT to",renviron_path)
}

# Install renv if not on the system
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}
if (!requireNamespace("renv", quietly = TRUE)) {
  stop("renv install failed!")
}

# Initialise renv (bare) if required
# Using renv::settings$use.cache(FALSE) to turn of cache linking which makes the renv totally isolated
# Note that turning off the cache linking does slow down installation significantly
if (identical(Sys.getenv("INIT_RENV"),"true")) {
  message("Initialise renv")
  renv::init(bare = TRUE)
  renv::settings$use.cache(FALSE)
  renv::snapshot(prompt = FALSE)
} else {
  message("Using existing renv")
}
message("R_LIBS_USER: ",Sys.getenv("R_LIBS_USER"))

# Then install remotes into the renv
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Then start installation of packages
# First find packages which already exist in the renv and those to install
specific_packages <- file.path(rwd,"specific_packages.R")
latest_packages   <- file.path(rwd,"latest_packages.R")
if ((file.exists(specific_packages)) && (file.exists(latest_packages))) {
  source(specific_packages)
  source(latest_packages)
} else {
  stop("Did not find ",specific_packages," and/or ",latest_packages)
}

stopifnot(
  exists("pkgs_version"),
  exists("harp_version"),
  exists("pkgs"),
  is.data.frame(pkgs_version),
  is.data.frame(harp_version),
  is.character(pkgs)
)

# Checking against requireNamespace is okay here i.e. fine to use the cache for CRAN packages
if (identical(Sys.getenv("INSTALL_CRAN"),"true")) {
  # Then install specific CRAN packages
  for (ii in seq_len(nrow(pkgs_version))) {
    if ((requireNamespace(pkgs_version$package[ii], quietly = TRUE)) && (packageVersion(pkgs_version$package[ii]) == package_version(pkgs_version$version[ii]))) {
      message("Found ",pkgs_version$package[ii]," with version ",pkgs_version$version[ii]," in renv, skipping")
    } else {
      remotes::install_version(pkgs_version$package[ii], version = pkgs_version$version[ii], upgrade = "never",)
    }
    if ((!requireNamespace(pkgs_version$package[ii], quietly = TRUE)) || (packageVersion(pkgs_version$package[ii]) != package_version(pkgs_version$version[ii]))) {
      stop("Looks like the install of ",pkgs_version$package[ii]," failed, stopping!")
    }
  }

  # Then the latest CRAN packages
  for (pkg in pkgs) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      message("Found ",pkg,", in renv, skipping")
    } else {
      install.packages(pkg)
    }
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Looks like the install of ",pkg," failed, stopping!")
    }
  }
}

# Then install harp
if (identical(Sys.getenv("INSTALL_HARP"),"true")) {
  for (ii in seq_len(nrow(harp_version))) {
    cpkg <- harp_version$package[ii]
    cver <- harp_version$version[ii]
    message("Installing ",cpkg," at branch ",cver)
    if (identical(Sys.getenv("FORCE_HARP"),"true")) {
      message("Forcing install of this package over an existing version")
      force_harp=TRUE
    } else {
      force_harp=FALSE
    }
    # Assuming harp dependencies have been listed first in harp_version, see specified_packages.R
    if (basename(cpkg) == "harp") {
      dep_harp=FALSE
    } else {
      dep_harp=TRUE
    }
    remotes::install_github(cpkg, ref = cver, upgrade = "never", force = force_harp, dependencies = dep_harp)

    # Check that this package exist in the renv
    if (!requireNamespace(basename(cpkg), quietly = TRUE)) {
      stop("Looks like the install of ",cpkg," failed, stopping!")
    }
  }
}

renv::snapshot(prompt = FALSE)

message("The renv installation completed!")
