# Installation instructions to use the R and python libraries

## Steps followed to install R locally using renv in atos
In order to use the harp libraries it is recommended
to use a local environment created with the `renv` library.
The local environment can be created using the `renv.lock` included
here, or created from scratch using the instructions below

### Using `renv.lock` to recreate environment
#### TODO (does not always work smoothly, hence including steps below)

### Create renv from scratch
```
module load R/4.2.2 # all tests done with this version of R
module load ecmwf-toolbox #this one is necessary to install Rgrib2 dependencies
R
library(renv)
renv::init()
Exit R (ctrl-D or exit)

```
Once the renv environment is initiated, enter R again and install
the following libraries

```
R
renv::install("remotes")
library(remotes)
install_github("harphub/harp")
library(harp)
install.packages("reticulate")
install_github("harphub/Rgrib2")

```

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

