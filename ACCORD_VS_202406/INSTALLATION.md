# Installation instructions to use the R and python libraries

## Steps followed to install R locally using renv in atos
In order to use the harp libraries it is recommended
to use a local environment created with the `renv` library.
The local environment can be created using the `renv.lock` included
here, or created from scratch using the instructions below

### Using `renv.lock` to recreate environment

Use the provided `renv.lock` found in this directory.

```bash
mkdir harp_local_installation
cd harp_local_installation
cp {path_to_oper-harp_repository}/ACCORD_VS_202406/renv.lock .
...

```

### Create renv from scratch
```bash
module load R/4.2.2 # all tests done with this version of R
module load ecmwf-toolbox #this one is necessary to install Rgrib2 dependencies
module load proj
R
library(renv)
renv::init()
Exit R (ctrl-D or exit)

```
Once the renv environment is initiated, enter R again and install
the following libraries

```bash
R
renv::install("remotes")
library(remotes)
install_github("harphub/harp")
library(harp)
install.packages("reticulate")
install_github("harphub/Rgrib2")

```

In order to install the `hdf5r` and `ncdf4` libraries needed to read hdf5 and netcdf
files, follow these steps

#### for netcdf
```bash
module load netcdf
R
install.packages("ncdf4")
```

#### for hdf5r
When working in atos, follow the following two extra steps.
If working any other machine, this step is not needed.

For the `hdf5r` library, R is does not add the correct link flags for setting rpath
when building the library. In order to avoid this issue, first create a file
` ~/.R/Makevars` with this line
```
PKG_LIBS = $(HDF5_LIB)

```
Then install `hdf5r` in R:
```bash
module load hdf5
R
install.packages("hdf5r")
```
When this is done, remove the `Makevars` file, as this might interfere with the 
installation of other packages.

After starting R in this location you should see a message like this, pointing
to a local R installation and not the standard `$HOME/x86_64-pc-linux-gnu-library`:

```
- Project '/etc/ecmwf/nfs/dh1_perm_b/miag/ACCORD_VS/R/harp_local_installation' loaded. [renv 1.0.3]
[Previously saved workspace restored]

```

To update `renv.lock` after installation use:
```
renv::settings$snapshot.type("all")
renv::snapshot()

```

### Installing local python environment
Some of the tests included here use the local python3 module and a local python environment
#### TODO Ask Fabrizio: provided venv file for recreating python environment that includes sat-data processing libraries

