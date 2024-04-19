# Examples of spatial verification

## Danish domain
Example of using the `verifiy_spatial` function over DMI's NEA domain
with DMI's radar data.
Still using the old version of harpIO, and the `master` version of `harpSpatial`

Using R version 4.2.2 in atos.

## Compilation of harpSpatial in atos

```
module load R # (default 4.2.2 at time of writing)
module load hdf5
module load ecmwf-toolbox # (for eccodes)
R
install.packages("remotes")
install.packages("here")

library(remotes)
install_github("harphub/harpSpatial")


```
## to compile the hdf5r library in atos, follow these steps
There is an issue installing hdf5r directly in atos,
and the following steps provided by Xavi need to be followed:


The issue is that R is not adding the correct link flags for setting the rpath when building this module, and expects HDF5 libraries to be available on system paths at runtime. The way you can overcome this limitation is by temporarily creating a file ~/.R/Makevars with the following content: `PKG_LIBS = $(HDF5_LIB)`

You may need create the ~/.R directory if it does not exist. Then make sure you have the hdf5 module loaded before running R
```
module load hdf5
R
> install.packages("hdf5r")
```
After that, you may remove the Makevars file.




## Currently using harpIO version 0.9186
To install harpIO_0.0.0.9186 use this command
```
library(remotes)
install_github("harphub/harpIO",ref="50e0cb4f3ccbf67797f18d29cfd1a506dc949297")
```


## to read DMI's grib codes correctly
Copy over or point to this path after loading eccodes

```
export ECCODES_DEFINITION_PATH=/home/nhd/libs/grib_definitions_dmi:$ECCODES_DEFINITION_PATH
```
