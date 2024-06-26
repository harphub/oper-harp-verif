for smooth installing from github, create a github token and copy the token into
~/.Renviron
like this: GITHUB_PAT=<your_github_token>


module load R
module load hdf5
module load ecmwf-toolbox
module load proj

R

library(renv)
renv::init()

Exit R session

R
install.packages("remotes")
library(remotes)

install_github("harphub/harp")
install_github("pollyaschm/harpIO", "invert-hdf5-data")
install_github("pollyaschm/harpSpatial", "ACCORD_VS_202406")
3

install.packages("shiny")  # Should have come with harp but in my case didn't 

install_github("harphub/Rgrib2")

# to install hdf5 packages 

Exit R session

mkdir ~/.R/Makevars
insert:
PKG_LIBS = $(HDF5_LIB)

R

install.packages("hdf5r")

remove ~/.R/Makevars after successful installation

install.packages("reticulate")

install.packages("here")
install.packages("tidyverse")
install.packages("dplyr")

install.packages("ggpubr")
install.packages("RColorBrewer")
install.packages("ggplotify")
install.packages("patchwork")

outside R

# for grb-reading of non-known parameters ADD ECCODE DEFINITION:
make eccode definitions available by adding this to your ~/.bashrc file:
export ECCODES_DEFINITION_PATH=<path_to_definitions>/definitions:ECCODES_DEFINITION_PATH


