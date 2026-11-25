# harp installation instructions

The general instructions for installing harp can be found [here](https://harphub.github.io/harp/)
and in the [2024 tutorial page](https://harphub.github.io/harp_training_2024/get-started.html#).

## Install on Atos

An install script is provided under `install/atos` to create an isolated R environment (a "renv") which contains harp and a number of related R packages required by the scripts in this repo. It is recommended to install your renv on $PERM or $HPCPERM. The scripting was created for an MQA working week in November 2026. The Atos modules loaded in `renv_setup.R` and R package versions listed in `specific_pacakges.R` were chosen to get things working at that time; they are not necessarily "optimal"!

The instructions below go through the install step-by-step, but you can also do everything in one go. Please first read the instructions before deciding on what approach you would like to follow.

To create a new "renv" under directory "/your/new/renv", do the following from the root of this repo:

```
cd install/atos
./renv_setup.R -r /your/new/renv
```

To install CRAN packages in the renv, use:

```
./renv_setup.R -r /your/new/renv -c
```

This will install the latest version of R packages listed in `latest_packages.R` and some specific R package versions listed in `specific_packages.R` (these packages are generally a bit awkward to install, such as "arrow"). The renv is completely isolated from your system libraries and cache, and as a result all packages must be built from source. This slows things down considerably; the time taken to install all CRAN packages is approx 35 mins. To confirm that your install worked as expected, simply run

```
./renv_setup.R -r /your/new/renv -c
```

once again. If the required package is found in the renv it will be skipped, and hence the command above should complete in a few seconds.

Once you have installed all the CRAN packages, you can then move on to installing harp. The branches of harp (and related repos such as meteogrid etc) are specified in `specific_packages.R`. Before installing harp, it is useful to generate a Github personal acccess token in order to avoid rate-limiting, which can result in frustrating errors during install such as:

```
Error: HTTP error 403.
 API rate limit exceeded for ???
 ....
Error: Failed to install 'unknown package' from GitHub:
```

When creating a [classic personal access token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens#creating-a-personal-access-token-classic), it is sufficent to use:

- Note: "harp install"
- Expiration: "7 days"
- Select scopes: No need to tick anything

Once you have the token, copy and paste it into the `GITHUB_PAT` at `install/atos/.Renviron`. ***WARNING: DO NOT COMMIT THE UPDATED .Renviron FILE WITH YOUR GITHUB_PAT TO GITHUB!***

Then install harp via:

```
./renv_setup.R -r /your/new/renv -H
```

The harp install should take approximately 15 mins. You can confirm that the installation worked correctly by running the same command again, i.e.

```
./renv_setup.R -r /your/new/renv -H
```

where harp packages will be skipped if they are found in the renv. If you need to change the branch of a specific harp package, you can do so by changing the options for `harp_version` in `specific_packages.R`. Note that if you want to re-install harp (e.g. if changing a branch or updating to the latest version), you should use

```
./renv_setup.R -r /your/new/renv -H -f

```

where the `-f` flag forces that the harp packages are installed over an existing installation.  

Finally, you can do all of this in one step by using:

```
./renv_setup.R -r /your/new/renv -c -H
```

but you should configure your Github personal access token before this.

## Using your renv on Atos

When submitting an R job via a shell script, you can use your renv by adding the following to your script:

```
MY_VENV="/your/new/renv/renv/library/linux-rhel-8.10/R-4.4/x86_64-pc-linux-gnu/"
module load R/4.4.3
export R_LIBS_USER="${MY_VENV}"
```

If you want to use rstudio on Atos with your renv, you can do the following. It is highly recommended to be logged into Atos via the Linux VDI, otherwise rstudio is too laggy to use in practice. 

-  Add the following to your .bashrc or .bash_profile:
```
module load R
module load rstudio
module load R/4.4.3
alias rstu="rstudio > /dev/null 2>&1 &"
```
- Make a directory for your R project e.g. /your/R/project.
- Soft link your renv to this R project directory using `install/link_renv.sh` (usage below). This allows for your renv to be easily used in several places without going through the renv installation process again.
```
./link_renv.sh /your/new/renv /your/R/project
```
- Open rstudio (e.g. via `rstu` alias), navigate to /your/R/project, and do "File -> New Project -> Existing Directory -> /your/R/project -> Create Project".
- Rstudio will then restart. Now every time you load the R project via the `.Rproj` file in the directory your renv will be loaded in your Rstudio session. You can confirm that renv is loaded by inspecting `Sys.getenv("R_LIBS_USER")`; it should point to a renv path in /your/R/project, which is just a soft link to /your/new/renv. Note that if you try to install a new package in rstudio via `install.packages` or similar, you will get a message similar to:
```
packages will be installed into "/your/new/renv/renv/..."
```
which is the correct behaviour. You can test that harp is available in your session by using `library(harp)`.

## Install elsewhere

You can follow the Atos installation tempalte when installing on your own system - it can follow the same process but may differ by system modules, specific R pacakge versions, etc.

Note that installation of the package `sf` can sometimes be problematic. These scripts only require `sf` for polygon station filtering and to improve map plotting appearance. If you do not require polygon filtering, then `sf` does not have to be installed. If you try to use polygon filtering and `sf` is not found, polygon filtering will simply be skipped.

Note also that the installation of the `tidyverse` package can sometimes be problematic. It is not included in the scripts for Atos installation detailed above.

## General information related to harp installation 

In order to install the latest version of harp from the main branch in Github, you can use:
``` r
install.packages("remotes")
remotes::install_github("harphub/harp")
```
If you want to use a specific release of harp, you can use:
``` r
remotes::install_github("harphub/harp@v0.2.2")
```
or a specific branch or a single repository:
``` r
remotes:install_github("harphub/harpPoint@develop")
```
All development versions are in the "develop" branch for each of the harp packages. Note that harp installation can take some time, and you will be asked about updating dependencies.

