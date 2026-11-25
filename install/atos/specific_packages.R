pkgs_version <- data.frame(
  package = c("arrow","hdf5r","Deriv","sf","terra","shiny","shinyWidgets"),
  version = c("22.0.0","1.3.16","4.1.6","1.1-0","1.9-27","1.13.0","0.9.1"),
  stringsAsFactors = FALSE
)

# Install the actual "harp" package last after all other dependencies 
# Then do not need to install "harp" dependencies a second time, see renv_setup.R
harp_version <- data.frame(
  package = c(
    "harphub/meteogrid",
    "harphub/Rgrib2",
    "harphub/harpCore",
    "harphub/harpIO",
    "harphub/harpPoint",
    "harphub/ggsonde",
    "harphub/harpVis",
    "harphub/harpSpatial",
    "harphub/Rfa",
    "harphub/harp"
  ),
  version = c(
    "master",
    "master",
    "develop",
    "develop",
    "develop",
    "master",
    "develop",
    "develop",
    "master",
    "develop"
  ),
  stringsAsFactors = FALSE
)

