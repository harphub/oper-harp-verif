# harp installation instructions

The instructions for installing harp can be found [here](https://harphub.github.io/harp/)
and in the [2024 tutorial page](https://harphub.github.io/harp_training_2024/get-started.html#).
This should install fairly easily on the atos supercomputer at ECMWF.

To install packages required by this repo, you can use:
``` r
pkg_list <- c("here","argparse","yaml","dplyr","tidyr",
              "purr","forcats","stringr","RColorBrewer","grid",
              "gridExtra","pracma","RSQlite","scales","pals",
              "shiny","shinyWidgets","lubridate")
for (pkg in pkg_list) {
  install.packages(pkg)
}
```

In order to install the latest version of harp from the main branch in github, you can use:
``` r
install.packages("remotes")
library(remotes)
install_github("harphub/harp")
```
If you want to use a specific release of harp, you can use:
``` r
install_github("harphub/harp@v0.2.2")
```
or a specific branch or a single repository:
``` r
install_github("harphub/harpPoint@develop")
```
All development versions are in the "develop" branch for each of the harp packages. Note that harp installation can take some time, and you will be asked about updating dependencies.

## PAT issue
Occasionally the harp installation can fail with a message like:
``` r
Downloading GitHub repo andrew-MET/harp@master
Error: HTTP error 403.
 API rate limit exceeded for 130.226.71.190. (But here's the good news: Authenticated requests get a hi
gher rate limit. Check out the documentation for more details.)
```
or:
``` r
Error: Failed to install 'unknown package' from GitHub:
```
If this happens, you need to create a personal token. Follow the instructions [here](https://happygitwithr.com/https-pat.html#get-a-pat) to get yourself a token.
Once you do that, follow these instructions
``` r
gitcreds::gitcreds_set()
<Enter token here>
```
Then use:
``` r
usethis::edit_r_environ()
```
This will open an editor to edit the file `.Renviron`, where you can write GITHUB_PAT=thetokenabove.

The system will ask you to restart R to take effect. Then try to install harp again.

## Updating harp version
To update harp (or a single package, like `harpPoint`) to the latest main branch, do the following:
``` r
library("remotes")
install_github("harphub/harpPoint") 
```

You generally have the option to update package dependencies.

