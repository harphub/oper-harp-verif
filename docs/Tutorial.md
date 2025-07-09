# Overview

This repo allows users to construct a "standard" set of point verification results using [harp](https://harphub.github.io/harp/), a set of R packages developed within the ACCORD consortium for analysis, verification, and visualisation of NWP data. The typical RRJVSP workflow for deterministic or ensemble experiments is the following:
- Convert HARMONIE-AROME vfld and vobs files to sqlite tables.  
- Use the sqlite data to perform point verification using harp over a standard set of groupings (e.g. ``lead_time``, ``SID``) for surface and upper-air parameters.
- The point verification produces harp ``.rds`` files which can be visualised using ``harpVis::shiny_plot_point_verif``.
- If desired, a set of local png files are also produced for a standard set of verification scores. These pngs can be visualised in the simple shiny app provided in the repo.
- If desired. produce a scorecard comparing two models. While this functionality is available in the scripts, some care must be taken in the construction and interpretation of scorecards. [See here for more information](https://harphub.github.io/harp-training-2022/scrcard.html). The default configuration settings outlined in these scripts are only indicative.

Little to no prior knowledge of harp is assumed, although users are encouraged to familiarise themselves with harp using the [2022](https://harphub.github.io/harp-training-2022/index.html) and [2024](https://harphub.github.io/harp_training_2024/) training courses. Note that harp provides many more utilites than just NWP verification.

# harp installation instructions

The instructions for installing harp can be found [here](https://harphub.github.io/harp/)
and in the [2024 tutorial page](https://harphub.github.io/harp_training_2024/get-started.html#).
This should install fairly easily on the atos supercomputer at ECMWF.

To install packages required by this repo, you can use:
``` r
pkg_list <- c("here","argparse","yaml","dplyr","tidyr",
              "purrr","forcats","stringr","RColorBrewer","grid",
              "gridExtra","pracma","RSQLite","scales","pals",
              "shiny","shinyWidgets","lubridate","scico","cowplot","sf")
for (pkg in pkg_list) {
  install.packages(pkg)
}
```

Note that installation of the package `sf` can sometimes be problematic. These scripts only require `sf` for polygon station filtering. If you do not require polygon filtering, then `sf` does not have to be installed. If you try to use polygon filtering and `sf` is not found, polygon filtering will simply be skipped.
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

# Create your config file

Create a configuration file for your project using the structure provided in the example configs. The config file is split into options used for pre-processing (`pre`), verification (`verif`), visualisation/output (`post`), and scorecards (`scorecards`). A description of these options is given below. Note that paths specified should already exist on your system as the scripts typically do not assume permission to generate parent directories.

| Type | Option name | Description | Example (s)   |
| ---  | --- | --- | --- |   
| verif | project_name | A descriptive name for the verification project. This is the same as the the project name variable PROJECT in monitor’s configuration file scr/Env_exp. | project_name: "TEST" |
| verif | fcst_model   | The names of the forecast models to consider. | fcst_model: <br> - model_A  <br> - model_B |
| verif | lead_time    | Lead times to consider in the verification e.g. 0 to 48 in steps of 3 hours. `seq` and `c()` will be parsed and evaluated by R. | lead_time: seq(0,48,3) | 
| verif | lead_time_UA | If you want to use specific lead times for upper-air variables. The default is NULL, which means that upper-air variables will use lead times specfied by `verif:lead_time`. Useful if you are verifying a list of surface and upper-air parameters, e.g. "T2m,T,Pmsl", and want to change the lead times used without having to change the config. | Default: <br> lead_time_UA: NULL <br> <br> Use hours 0-24 in steps of 1 for upper-air variables <br> lead_time_UA: seq(0,24,1) | 
| verif | by_step      | Interval (in hours) between cycles to be used for verification i.e. cycles considered are `start_date` to `end_date` in steps of `verif:by_step`. | by_step: "6h" |
| verif | fcst_type    | Forecast type, either `det` or `eps`. | fcst_type: "det" |
| verif | domains      | What SID lists to verify (equivalent to SURFSELECTION/TEMPSELECTION in monitor, where the station lists are defined in scr/selection.pm). The choice "All" will use all stations available in the data. SID lists, either defined explicitly or based on a geographic area, can be defined in `fn_station_selection` (see the documentation below for more information). Alternatively you can select a single SID by using it's SID number as a domain. | domains: <br> -"All" <br> <br> domains: <br> - "IRL" <br> - "IE_EN" <br> <br> Verify domain "IRL" and a single station (SID=3953): <br> domains: <br> - "IRL" <br> - "3953" |
| verif | domains_UA   | What SID lists to verify for upper-air parameters (following same convention as "domains" above). This may be useful if you want to use only certain domains for upper-air verification e.g. a large region containing more radiosondes. If this option is missing or set to NULL then it will default to "domains". | To use "domains" for "domains_UA": <br> domains_UA: <br> - NULL <br> <br> To verify surface parameters over multiple domains but upper-air parameters over one: <br> domains: <br> - "IRL" <br> - "IE_EN" <br> <br> domains_UA: <br> - "IE_EN" | 
| verif | members      | What ensemble members to use in the verification for each `verif:fcst_model`. Set to NULL to use all the members available in each ensemble. Include a list here if the members to be used differ for each of the `verif:fcst_model`. | For a deterministic model or to read all memeber in an ensemble: <br> members: <br> - NULL <br> <br> Specific members from different ensembles: <br> members: <br> - c(1,2,3) <br> - c(4,5,6) |
| verif | lags         | How to lag the ensemble members for each ensemble. Include a list if the lags differ for each of the `verif:fcst_model`. Set to “0h” for no lagging. The parent cycles for lagging are determined by the `start date`, `end date`, and `verif:by_step`. See `harpIO::read_point_forecast` for more information. | For a deterministic model: <br> lags: <br> - "0h" <br> <br> For 3hr lagging in the first ensemble, no lagging in the second: <br> lags: <br> - c("0h","3h") <br> - "0h" |
| verif | shifts       |  Experimental and for deterministic models only! How to shift forecasts forwards/backards in time for each model. Include a list if the shifts differ for each of the `verif:fcst_model`. Set to NULL to do nothing (i.e. default behaviour, equivalent to shifting by zero hours). Use digits only, where the shift is assumed to be in hours. Positive values shift forward in time, negative values backwards. | Default: <br> shifts: <br> - NULL <br> <br> Do not shift first model, but shift second by 6h forward in time: <br> shifts: <br> - "0" <br> - "6" |
| verif | num_ref_members | Number of reference members to use when computing fair scores for an ensemble (e.g. "Inf"). Not used for deterministic models. See `harpPoint::ens_verify` for more details. | num_ref_memebrs: "Inf" |
| verif | ua_fcst_cycle | For upper air variables, do you want to group by fcst_cycle? Either TRUE or FALSE (default). | ua_fcst_cycle: FALSE |
| verif | ua_restrict_vh | For upper air variables, do you want to restrict to valid hours 00, 06, 12, and 18Z? Either TRUE (default) or FALSE. | ua_restrict_vh: TRUE |
| verif | force_valid_thr | Set to TRUE if you really want threshold scores over valid times/hours. FALSE is generally sufficient. | force_valid_thr: FALSE |
| verif | lt_split | Set to TRUE if you want to split the auxiliary scores (e.g. scatterplots) into "short" (<=24) and "long" (>24) leadtime ranges. Results using all leadtimes will always be generated | lt_split: FALSE |
| verif | fcst_path    | Path to the forecast sqlite tables generated from the vfld. | fcst_path: "/path/to/FCTABLE" |
| verif | obs_path     | Path to the observation sqlite tables generated from the vobs. | obs_path: "/path/to/OBSTBALE" |
| verif | verif_path   | A root directory where the rds verification files are stored. For a given project, these rds files will be located in `verif:verif_path`/`verif:project_name`. | verif_path: "/path/to/rds" |
| pre   | fcst_model   | Carry out the vfld to sqlite conversion for these forecast models. Note that this can differ from `verif:fcst_model` (e.g. the sqlite tables may already exist for some models, and you may want to skip regenerating these). | fcst_model: <br> - model_A  <br> - model_B | 
| pre   | lead_time    | Which lead times to use when converting the vfld. Can differ from `verif:lead_time`. | lead_time: seq(0,48,1) |
| pre   | vfld_path    | Path to where the vfld files are stored (note that the full file names to be read are constructed from `pre:vfld_path` and `pre:vfld_template`). The vfld files for each `pre:fcst_model` should exist in this directory. | vfld_path: "/path/to/vfld" |
| pre   | vfld_by      | Interval (in hours) between cycles to be used when generating the sqlite tables from vfld i.e. read vfld files from `start_date` to `end_date` in steps of `pre:vfld_by`. This can differ from `verif:by_step`. | vfld_by: "3h" |
| pre   | vfld_template | The template used to construct the vfld file names. See `harpIO::show_file_templates`. For deterministic/ensemble experiments, the standard choices are “vfld” and "vfld_eps". If different templates are to be used for different forecast models, then include a `pre:vfld_template` entry corresponding to each forecast model. Custom templates can also be created. | Standard: <br> vfld_template: <br> - "det" <br> <br> Custom: <br> vfld_template: <br> - "{fcst_model}/{YYYY}/{MM}/vfld{fcst_model}{YYYY}{MM}{DD}{HH}{LDT2}" | 
| pre   | vobs_path    | Path to where the vobs files are stored. | vobs_path: "/path/to/vobs" |
| pre   | vobs_by      | Interval (in hours) between the vobs files i.e. read vobs files from `start_date` to `end_date` in steps of `pre:vobs_by`. | vobs_by: "1h" | 
| pre   | custom_asl_path | Path to a specific HARMONIE-AROME allsynop.list to use when converting the vfld data. If not specified, harp's default stationlist is used (`harpCore::station_list`). | custom_asl_path: "/path/to/allsynop.list" | 
| pre   | members      | Which members to read for each ensemble. If only one option is set for members, it will be repeated for each `pre:fcst_model`. Set to NULL for a deterministic experiment. Note: this variable is different from `verif:members` as the two can differ in general. | For deterministic models: <br> members: <br> - NULL <br> <br> For ensembles: <br> - seq(0,6) |
| pre   | lags         | How the ensemble members are lagged for each `pre:fcst_model`. For a given model, `pre:lags` should be the same length as `pre:members`. Include a list if the lags differ for each of the `pre:fcst_model`. Note: this variable is different from `verif:lags` as the notation used for each is different. | For deterministic models: <br> lags: <br> - NULL <br> <br> For ensemble models: <br> lags: <br> - c(0,0,0,0,3,3,3) | 
| pre   | params       | Which parameters to consider when generating the sqlite tables from vfld. See `harpIO::show_harp_parameters` and `harpIO::harp_params` for recognised vfld parameters in harp. Set to NULL to convert everything found. | params: <br> - T2m <br> - Td2m <br> - T <br> <br> To convert everything found in the vfld: <br> params: <br> - NULL |
| post  | plot_output | A root directory indicating where to save the png files generated during the verification process. For a given project, the png files will be stored in `post:plot_output`/`verif:project_name`. Set to "default" to use `verif:verif_path`/archive. | plot_output: "/path/to/png" |
| post  | create_png  | Flag to indicate if png files should be generated. | create_png: TRUE | 
| post  | save_vofp   | Flag to indicate if the verification object used for creating pngs should be saved. Useful for generating images at a later stage. Files will be stored in `post:plot_output`/`verif:project_name`. Default is FALSE. | save_vofp: TRUE |
| post  | save_sidrds | Flag to indicate if the verification object used for SID scores (i.e. maps) should be saved. May be useful for investigating issues with certain stations. Files will be store in `post:plot_output`/`verif:project_name`. Default is FALSE. | save_sidrds: TRUE | 
| post  | cmap        | What palette to use for the line plots. Choose from a palette in "RColorBrewer" or a function from the "pals" package e.g. "trubetskoy". Defaults to "Set2" from RColorBrewer if not specified. | cmap: "trubetskoy" |
| post  | cmap_hex    | What pallete to use for scatterplots. Choose from "paired" (RColorBrewer), "magma", "viridis", or a scico pallette (see scico::scico_pallete_names). Defaults to magma. | cmap_hex: "bukavu" |
| post  | map_cbar_d  | Logcial flag to indicate if a discrete colourbar (with fixed breaks and bounds) should be used in the station map plots as opposed to a continuous one where the bounds change depending on the data. Defaults to "FALSE" if not specified. | map_cbar_discrete: TRUE |
| scorecards | create_scrd | Logical switch to produce scorecard data, save the data, and plot the scorecard. | create_scrd: TRUE | 
| scorecards | ref_model   | The reference model for the scorecards. | ref_model: "old_model" |
| scorecards | fcst_model  | The “new” model for the scorecards (i.e. score differences are `scorecards:fcst_model` - `scorecards:ref_model`). | fcst_model: "new_model" | 
| scorecards | parameters  | Which parameters to plot in the scorecards (parameters appear as ordered here). For upper-air variables, if variable “X” is included in the verification, then you can include individual pressure levels in the scorecard plots by using “X750”, etc. . | parameters: <br> - T2m <br> - Td2m <br> - T925 <br> - T700 |
| scorecards | scores      | Which of the deterministic or ensemble summary scores to plot in the scorecard. These appear from left to right in the scorecard. | Deterministic: <br> scores: <br> - "bias" <br> - "rmse" <br> <br> Ensemble: <br> scores: <br> - "mean_bias" <br> - "spread" |
| scorecards | domains     | Construct scorecards for these domains only (provided they exist in `verif:domains` or `verif:domains_UA`). | domains: <br> - "All" |
| scorecards | pooled_by   | How to pool the data for bootstrapping. See `harpPoint:bootstrap_verify` for more details. | pooled_by: fcst_dttm |
| scorecards | numboot     | Number of bootstrap replicates. See `harpPoint:bootstrap_verify` for more details. Should be “large”. | numboot: 1000 | 
| scorecards | parallel    | Use parallel processing when performing the bootstrapping. | parallel: FALSE | 
| scorecards | num_cores   | Number of cores to use if `parallel=TRUE`. | num_cores: 1 |
| scorecards | plot_signif | As well as the scorecard, plot the actual score difference as a function of leadtime for each parameter and score (provided that `scorecards:create_scrd` is TRUE). Statistical significance of the score difference is also indicated. Uses `fn_plot_signif_diff.R`. | plot_signif: TRUE | 
# Pre-processing

Typically the vfld and vobs files need to be converted to sqlite tables before starting the verification process. 

## vfld2sql.R

The pre-processing of vfld files in particular can take some time, especially for ensemble experiments. However, the tables only need to be generated once. As such, this step can be skipped once the tables are in place. The `vfld2sql.R` script takes the following command line inputs (required arguments in **bold**, optional arguments are in *italics*):

- **-config_file**: The config file in the `config_files` directory (no default).
- **-start_date**: The first forecast cycle to process (in YYYYMM, YYYYMMDD, or YYYYMMDDHH format (preferred), with no default). If YYYYMM is given then the first day and cycle of the month (i.e. 01/00Z) is assumed. If YYYYMMDD is given then the first cycle (i.e. 00Z) of YYYYMMDD is assumed. 
- **-end_date**: The last forecast cycle to process (in YYYYMM, YYYYMMDD, or YYYYMMDDHH format (preferred), with no default). If YYYYMM is given then the last day and cycle of the month (i.e. lastday/23Z) is assumed. If YYYYMMDD is given then the last cycle (i.e. 23Z) of YYYYMMDD is assumed. 
- *-use_custom_asl*: Logical flag to use a custom HARMONIE-AROME allsynop.list (as specified in the config file) when converting the vfld (default=FALSE). This is useful when your vfld files contain more stations than in harp's default list of stations (`harpCore::station_list`). The script will automatically convert the allsynop.list into something readable by harp. If set to `FALSE`, or not specified in the config file, harp's default station list is used. 
- *-remove_m_elev*: Logical flag to remove writing model elevation to the sqlite files (default=FALSE). This may be useful for ensemble experiments when members have different model elevations. 
- *-params_list*: A comma separated list of parameters to convert (default="MISSING"). This will override whatever is set for `pre:params` in your config file, which may be useful in certain circumstances. If this argument is not specified then `pre:params` will be used (i.e. the default behaviour).
- *-use_obs_elev*: Logical flag to use the station elevation from the OBSTABLE (i.e. `verif:obs_path`) when performing the height correction for T2m or Ps (surface pressure). This option is included as the observation station elevation can differ significantly from that in a stations file (e.g. an "allsynop.list" file or harp's default `harpCore::station_list`). Set to FALSE by default i.e. just use the elevation from the stations file. 

Typical usage:
``` 
./vfld2sql.R -config_file config_files/config_det_example.yml -start_date YYYYMMDDHH -end_date YYYYMMDDHH -use_custom_asl TRUE -remove_m_elev TRUE
```
This converts all cycles from `start_date` to `end_date` in steps of `pre:vfld_by` (in the config file). By default the output sqlite files are stored in:
```
{verif:fcst_path}/{pre:fcst_model}/{YYYY}/{MM}/FCTABLE_{parameter}_{YYYY}{MM}_{HH}.sqlite
```
for each {parameter} in `pre:params` (or for every parameter in the vfld if `pre:params = NULL`).

## vobs2sql.R

Similarly, to create the observation sqlite tables run:
``` 
./vobs2sql.R -config_file config_files/config_det_example.yml -start_date YYYYMMDDHH -end_date YYYYMMDDHH 
```
Note that the start and end dates here correspond to valid times (and thus end_date should extend beyond the last forecast cycle converted). By default the output sqlite files are stored in:
```
{verif:obs_path}/OBSTABLE_{YYYY}.sqlite
```
This OBSTABLE will contain all observations available in the vobs files.

# Point verification

Once the configuration file is set and the sqlite tables are created, the point verification can be carried out. 

## set_params.R

This parameter list file is used to specify the parameters considered by the `point_verif.R` script and their associated options, in particular:
- **scale_fcst**: Forecast scaling (e.g. Kelvin to degress). If you only want to apply the forecast scaling to certain models in `verif:fcst_model` for certain paramters, this can be controlled by the flag `models_to_scale` in your parameter list file (e.g. set_params.R, see below).
- **scale_obs**: Observation scaling.
- **thresholds**: Thresholds used when computing threshold skill scores.
- **obsmin/max_val**: Max/min observation values allowed.
- **fctmax_val**: Max forecast values allowed (experimental).
- **error_sd**: Number of standard deviations used in `harpPoint::check_obs_against_fcst`.
- **models_to_scale**: What specific models to scale using `scale_fcst` for this parameter. If missing or NULL for a given parameter, the same scaling will be applied to all models specified by `verif:fcst_model` in the config file if `scale_fcst` is specified. If `models_to_scale` contains a model which is not found in the forecast data, the `point_verif.R` script will abort. For example, suppose you read in two models ("Model_A" and "Model_B") and you only want to scale "Model_A" for T2m and "Model_B" for S10m. Then you should add `models_to_scale = c("Model_A")` under the T2m list and `models_to_scale=c("Model_B")` under the S10m list. For all other parameters where `scale_fcst` is specified, the same scaling will be applied to both "Model_A" and "Model_B". 

Typically this file does not need to be changed. By default `point_verif.R` reads parameter options from this file, but a custom parameter file can also be used by passing the `-params_file` option to `point_verif.R`. 

## point_verif.R

### Inputs and usage 

This script takes the following command line inputs (required arguments in **bold**, optional arguments are in *italics*):

- **-config_file**: The config file in the `config_files` directory (no default).
- **-start_date**: The first forecast cycle to process (in YYYYMM, YYYYMMDD, or YYYYMMDDHH format (preferred), with no default). If YYYYMM is given then the first day and cycle of the month (i.e. 01/00Z) is assumed. If YYYYMMDD is given then the first cycle (i.e. 00Z) of YYYYMMDD is assumed.
- **-end_date**: The last forecast cycle to process (in YYYYMM, YYYYMMDD, or YYYYMMDDHH format (preferred), with no default). If YYYYMM is given then the last day and cycle of the month (i.e. lastday/23Z) is assumed. If YYYYMMDD is given then the last cycle (i.e. 23Z) of YYYYMMDD is assumed.
- *-params_file*: The parameter list file containing parameter scalings, thresholds, etc. (default="verification/set_params.R"). **Note: if you are making use of the `models_to_scale` option in the parameter list file, it is best to create a new parameter list file and explicitly call this when you are running point_verif.R. Sharing a parameter list file with specific model scalings across different projects may be dangerous due to common models e.g. Model A is scaled for T2m in project X, but it is not scaled for T2m in project Y. Unfortunately creating new parameter lists for specific projects does introduce some code duplication.**
- *-params_list*: Which parameters for verify (default="T2m"). This should be a comma separated string of parameters, for example "T2m,S10m,T,S". These parameters should exist in the parameter list file, otherwise they will be skipped. If `params_list` is not specified, it just defaults to "T2m". Note that for `params_list="All"` all parameters in the parameter list file are considered in the verification (this is NOT recommended in general).
- *-dynamic_sid_gen*: A logical flag to generate SID lists corresponding to the `verif:domains` or `verif:doamins_UA` during the verification process (default=TRUE). Different `domains` options are defined in `fn_station_selection.pm`. This flag replaces the old methodology of reading SID lists from a static file (i.e. `verification/sid_lists.rds`). This old (now deprecated) method can be activated by switching this flag to "FALSE".
- *-plot_dynamic_sid*: A logical flag to plot a map of the stations used for each domain and parameter (default=FALSE). This is only relevant when "dynamic_sid_gen=TRUE".
- *-mod_def_rds*: A logical flag to prepend the project name to harp's default rds filenames (default=FALSE). Not generally required.
- *-add_proj_png*: A logical flag to prepend the project name to the default png filenames (default=FALSE). Not generally required. 
- *-rolling_verif*: A logcial flag to indicate "rolling" verification (default=FALSE). If TRUE, rolling verification will produce a reduced set of png files and will not produce rds files or scorecards. Generally rolling verification is restricted to a "short" (e.g. 7 days) near-real time period. This option is not compatible with `gen_sc_only=TRUE`. 
- *-gen_sc_only*: A logical flag to run scorecard generation (and plotting) only (default=FALSE). This may be useful in cases where point verification results have already been generated. This option is not compatible with `rolling_verif=TRUE`. 
- *-use_fixed_dates*: A logical flag to use the input `start_date` and `end_date` when naming the directories and png files associated with this verification (default=TRUE). If set to FALSE, the data generated will use start and end dates corresponding to the first and last `fcst_dttm`, respectively, used in the verification for a given parameter. Therefore if set to FALSE, data may be stored in different directories for different parameters if the first and last `fcst_dttm` differs (this can happen in particular for precipitation). This option does not change the start/end dates in the rds filenames. 
- *-skip_sid_verif*: A logical flag to skip the SID verification used for generating map scores (default=FALSE). Useful for quick runs.
- *-skip_thresh_verif*: A logical flag to skip the threshold verification (default=FALSE). This will override whatever is set for thresholds in your parameters file. Useful for quick runs.

Alternatively, the script can be sourced directly from within R/RStudio. In this case, the arguments will be read from the `verification/source_options.R` file. This interactive mode is useful for interrogating the data. 

Typical usage:
``` 
./point_verif.R -config_file config_files/config_det_example.yml -start_date YYYYMMDDHH -end_date YYYYMMDDHH -params_file verification/set_params.R -params_list "T2m,Q"
```
This will run the verifcation for using cycles from `start_date` to `end_date` in steps of `verif:by_step` for parameters T2m and Q.

### QC

The following QC checks of the forecast and observation data are carried out by the script:

- Forcast values above `fctmax_val` are removed (if this variable is set).
- Observation values above/below the `obsmin_val`/`obsmax_val` are removed (if these variables are set).
- `harpPoint::check_obs_against_fcst` is run to remove observations which are more than `error_sd` standard devations away from the forecast.
- Station report frequency is computed and stations in the bottom 1% are removed. This acts to discard stations with very few observation reports. 

### Verification groups

By default the script assumes the following verificaiton for surface and upper-air variables (see `harpCore::make_verif_groups()` for more information):
- Surface: Data is grouped by `fcst_cycle` and the SID grouping specified by the `verif:domains` option in the config file (this is stored under the variable `station_group` in the scripts). Verification scores are then computed as a function of leadtime, valid date, and valid hour.
- Upper air: Data is group by the SID grouping specified by the `verif:domains_UA` option and scores computed as a function of leadtime and valid hour. If `verif:domains_UA` is missing from the config (or set ot NULL), then `verif:domains` will be used for the SID grouping.

### Output

`point_verif.R` will produce standard harp `.rds` files which contain the full suite of verification scores available in harp by default. These files will be stored in:
```
{verif:verif_path}/{verif:project_name}/harpPointVerif.harp.{parameter}.harp.{start_date}-{end_date}.harp.{forecast_model_1}.model.{forecast_model_2).model...{forecast_model_N}.rds
```
Typically the filenames for the harp rds files should not be changed as the harp shiny app assumes a set format. Note that while the filenames do not contain information about the `verif:domains` or `verif:doamins_UA` considered, the domain selection is included in the rds files under the `station_group` variable. 

If `post:create_png: TRUE`, then a suite of standard verification scores are plotted as png files for local visualisation. These local files will also include plots which are not available in harp's shiny app, such as forecast timeseries and station bias/rmse maps. These files should appear in:
```
{post:plot_output}/{verif:project_name}/{start_date}-{end_date}/long_file_names.png 
```
(the filenames are somewhat convoluted and should not be changed as a strict structure is assumed in the local shiny app). See `fn_png_name` in `fn_plot_helpers.R` for more information on the convention used.

If you are also generating a scorecard (`scorecards:create_scrd: TRUE`), then all the underlying scorecard data (for each domain) will be saved to:
```
{verif:verif_path}/{verif:project_name}/harpScData-{start_date}-{end_date}-{scorecards:ref_model}-{scorecards:fcst_model).rds
```
Scorecard plots will also be available in
```
{post:plot_output}/{verif:project_name}/{start_date}-{end_date}/*scard*.png
```
The scorecard data can be passed to `fn_scorecard_signif.R` for visualisation if desired. If `scorecards:plot_signif: TRUE`, these images will be available in the same directory (search for `*sdiff*.png`).

## Visualisation 

The rds files can be visualised by using harp's built-in shiny app:
```
shiny_plot_point_verif(start_dir={verif:verif_path}/{verif:project_name})
```
For example, when loading in a surface parameter, you should see groups for `station_group` and `fcst_cycle` and different options for the "Time" axis.

To browse the png images easily, a simple shiny app is provided with a similar interface to the "monitor" tool. This shiny app is defined in `visualization/visapp/app.R`. Some small edits to `app.R` may need to be taken in order to run the app locally, as detailed below.

1. The variable `img_dir` is set to NULL by default, which just looks for files in the `sample_images` directory provided in visapp. Set `img_dir <- {plot_output}` instead to point to the images available there after verification.
2. Optional: By default the variable `app_dir` is set to `here("visualization/visapp")` and this should correctly point to the `visapp` directory provided that `here()` starts in this repository. If this is not the case for some reason, alternatively you can hardcode `img_dir` to the location of the `visapp` directory on your system. 
3. Optional: Add your project name to `all_experiment_names.R` in the `visapp` directory, following the convention given, in order to change the display name of the project in the app. If your project is not listed in `all_experiment_names.R`, it will still appear in the app but with a default display name.
4. Optional: There is a `smr_ind` variable which changes the date display in the app to cater for dates categrorsied into "Rolling", "Monthly", and "Seasonal" periods. This assumes that data in `plot_output` is stored under directories following the strcuture `Monthly/project_name/start_date-end_date`, and the same for `Rolling` and `Seasonal`. This switch can generally be left as FALSE. 

Note that you can have images for multiple projects in `plot_output` and switch easily between projects in the app. The app will read all directories which exist in `plot_output`, and therefore you need to remove the `project_name` directory from `plot_output` in order to remove the project from the app. The app can then be launched by opening `app.R` in Rstudio and hitting "Run App". From a terminal, cd into the visualization directory, open R, and use:

``` r
library(shiny)
runApp("visapp")
```

### Fixed colours for certain models

In many cases you may want to associate a given forecast model with a fixed colour in the png plots e.g. "model_A" is always coloured red. To do this, search for `model_names` in `visualization/fn_plot_helpers.R`, which defaults to:


``` r
# Some fixed model names which the verification scripts know about.
# The default below considers 20 known models, corresponding to the number
# available from "trubetskoy" in the pals packages (less last two colours).
# The number of colours available in RColorBrewer is typically less, while
# pals offers more options. 
model_names  <- c("c1",
                  "c2",
                  "c3",
                  "c4",
                  "c5",
                  "c6",
                  "c7",
                  "c8",
                  "c9",
                  "c10",
                  "c11",
                  "c12",
                  "c13",
                  "c14",
                  "c15",
                  "c16",
                  "c17",
                  "c18",
                  "c19",
                  "c20")
```

Then take a look at your chosen colour pallete (as specfied by cmap in your config file) from the [pals](https://cran.r-project.org/web/packages/pals/vignettes/pals_examples.html) or [RColorBrewer](https://r-graph-gallery.com/38-rcolorbrewers-palettes.html) packages and replace "cX" by your model name for the colour you want. Note that the index of your model in `model_names` is assoicated with the colour for that index in the palette. For example, if you use `cmap:trubetskoy` in your config file and set:

``` r
model_names  <- c("model_A",
                  "c2",
                  "c3",
                  "model_B",
...
```


then "model_A" will be always be in red and "model_B" will always be in blue i.e. the 1st and 4th colours of the trubetskoy pallete. If some or all of your forecast models are not listed in `model_names`, the colours assoicated with these models will default to the first ones available in the palette. For example, suppose you have forecast models "model_A", "model_B", and "model_C" and `model_names` is set as per the example above. In this case "model_A" will be in red, "model_B" be in blue", and "model_C" will be in green (i.e. the 2nd colour of the trubetskoy pallete, as the 1st colour is already taken by "model_A"). 

### Visualizing the results direcly in atos
It is possible to run the shiny web server directly on atos and open a local browser
to visualized the results `img_dir` using the `launch_visapp_atos.R` script.

1. Soft link the path with the image results in the `sample_images` directory.
```
cd visapp/sample_images/
ln -sf some_path_somewhere local_name_of_the_path
cd ../..
module load R #mind the version where you installed harp
Rscript launch_visapp_atos.R
```

2. Open a new terminal and ssh to one of the atos login nodes
```
ssh -L 9999:localhost:9999 ac6-102
```

3. Open the browser on `http://127.0.0.1:9999/` that should show up after doing step 1.
 
or simply click on the link displayed in the terminal.

This will open a local browser in your local pc showing the shiny app there
while the png files are in the remote server.

Yo might need to install the `shinyWidgets` app.

The same is valid for visualizing the results interactively using the `rds` files.
In this case use the `launch_dynamicapp_atos.R` instead of `launch_visapp_atos.R` in step 1.
# Station selection

The SID lists used during verification are controlled by the `verif:domains` and `verif:domains_UA` options in the config file (the latter applying to upper-air variables only if specified). These `domains` variables are essentially equivalent to `SURFSELECTION` and `TEMPSELECTION` in monitor. Setting:
```
domains:                                  
  - "All"
```
will use all stations available in your data, and this will generally be sufficient for many cases. However there may be cases where verification over a specific subset of SIDs is desired. In monitor, various SID lists are defined in `scr/selection.pm`, whereas here the SID lists are controlled by `verification/fn_station_selection`. Information regarding the default domains/station lists available, and how to add new ones, is given below. Note that you can verify a single station by simply passing it's SID as an option to `verif:domains` or `verif:domains_UA` e.g. using
```
domains:
  - "3953"
  - "3969"
```
will verify the individual stations 3953 (Valentia) and 3969 (Dublin Airport). **Passing a list of stations e.g. c("3953","3969") directly in `verif:domains` will not work - you should add this SID list to `verification/fn_station_selection` as per the instructions below.**

**Note: A new method to handle station selection has been added in which SID lists are generated on the fly during the verification process. The old methodology read SID lists from a static file, however this method is no longer recommended and is now deprecated. The documentation below cover both methodologies, starting with the new recommended method.**

## Using SID lists generated on the fly (recommended method)

### Default domains/lists

There are a number of default domains/lists already available in `fn_station_selection.R` which can be used for the option `verif:domains` or `verif:domains_UA`. These are categorised as follows:

#### User-defined specific SID lists

These lists are specfified explicitly in `fn_station_selection.R`, e.g.

``` r
DKlist    <- stats::setNames(tibble::as_tibble(
             c(06030,06041,06043,06049,06052,06058,06060,06070,06072,06073,
               06074,06079,06080,06081,06096,06102,06104,06110,06116,06118,
               06119,06120,06123,06126,06135,06138,06141,06149,06151,06154,
               06156,06165,06168,06170,06180,06181,06190,06193)),
             "SID")
```

The current specific lists available are (see the function for SIDs used):
- DKlist
- DKland
- DKcoast
- DKupdated
- Greenland
- NorthSea
- NNS (North NorthSea)
- EstoniaSynop
- LatviaSynop
- LithuaniaSynop
- GreenlandIcecap
- NEUcoast
- EWGLAM
- RussiaCoast
- RussiaInland
- NorthAmericaInland
- AirportSynop
- Coastal_All

To use any of these in your config file, simply use e.g.

``` r
domains:
 - "DKlist"
 - "EWGLAM"
```

**Note:The Coastal_All list is indicative only and should be changed by users based on their needs. At the moment it only contains stations pertinent to UWC-W.**

#### Domains based on lat/lon bounding boxes

Domains based on lat/lon bounding boxes are defined in the variable `latlon_reserved_names`, i.e.:

``` r
latlon_reserved_names <- c("DINI","IE_EN","IS","NL","NL_OP","DK","IRL","SCD",
                             "FR","DE","Alps")
```

The bounding boxes themselves can be found in the function (see therein for north, south, east, and west), e.g.:


``` r
} else if (domain == "IE_EN") {  #Ireland+UK bounding box
        slat <- 50.0
        wlon <- -11.0
        nlat <- 60.0
        elon <- 2.0
```

To use any of these in your config file, simply use e.g.

``` r
domains:
 - "IE_EN"
 - "IRL"
```

#### SID lists based on SYNOP ranges

Various country-specific WMO SID ranges are already defined in the function, e.g. "FranceSynop" and "GermanySynop". These are listed in the variable `synop_reserved_names`, i.e.:

``` r
synop_reserved_names  <- paste0(c("France","Germany","Ireland","Norway",
                                  "Sweden","Finland","Iceland","Greenland",
                                  "Denmark","Netherlands","Spain",
                                  "Switzerland","UnitedKingdom"),
                               "Synop")
```

The SID ranges are defined in the subfunction `filter_stations` as:

``` r
if (grepl("France",domain,fixed = "TRUE")) {
      SID_min <- 7001
      SID_max <- 7998
    } else if (grepl("Germany",domain,fixed = "TRUE")) {
      SID_min <- 10001
      SID_max <- 10998
```

To use any of these in your config file, simply use e.g.

``` r
domains:
 - "IrelandSynop"
 - "SpainSynop"
```

#### Domains based on polygon files

You can also generate SID lists based on polygon files (as in monitor). The polygon files should be stored under directory `verification/poly_files`. To use a polgon file called DOMIAN.poly as your verification domain, you simply need to:
- Ensure `DOMAIN.poly` exists in `verification/poly_files.
- Use the following in your config file:

``` r
domains:
 - "DOMAIN"
```

In certain cases you may encounter a message saying that the specified poly file is not valid, in which case an SID list will not be generated for that domain.

### QC checks

Several quality control checks are carried out to ensure consistency of SID lat/lon in the observations and forecast:
- A given SID is checked to see if it has multiple lat/lon positions in the observation data frame (i.e. the OBSTABLE). If this is the case, and if the lat/lon values differ significantly this station will not be used in the verification.
- A given SID is checked to see if the lat/lon position in the observation data frame (i.e. the OBSTABLE) matches that in the forecast data frame (i.e. the FCTABLE). If the observation and forecast position differ significantly, then this station will not be used in the verificaiton. This is controlled by the logical flag "multlatlon_rmv".

### Stations to remove

You can define SIDs which should always be removed from the verification process by adding it to the `stations_always_rmv` variable in `fn_station_selection.R`. In addition, you can also define stations which should be removed for certain parameters, as controlled by the variable `stations_param_rmv`. For example, if you wan to remove SID=1234 for T2m and SID=5678 for Pmsl, use:
``` r
} else if (param == "T2m") {
  stations_param_rmv <- c(1234)
} else if (param == "Pmsl") {
  stations_param_rmv <- c(5678)
```
Filtering for additional parameters can be added as required. 

### Filtering based on elevation

For a given domain/SID list, denoted as X, you can select stations which have elevation above or below a given height Y by using the following convention:
- "X_ELEVA_Y": This will use all stations in domain/SID list X above (>) elevation Y m.
- "X_ELEVB_Y": This will use all stations in domain/SID list X below (<=) elevation Y m.
Note that stations with missing elevation (typically denoted as -99, which comes from the vobs files) will always be removed in this case. If no stations are found which meet the criteria "X_ELEVA/B_Y" then this domain will be skipped.

### Filtering to coastal and inland stations

For a given domain/SID list, denoted as X, you can filter to coastal and inland stations by using the following convention:
- "X_COASTAL": This will use all stations in domain/SID list X which are found in the "Coastal_All" SID list (see function for SIDs used).
- "X_INLAND": This will use all stations in domain/SID list X which are not found in the "Coastal_All" SID list.
If no stations are found which meet the "X_COASTAL" condition then domain "X_COASTAL" will be skipped. 

### Add a new domain/SID list

You can add a new domain/SID list definition by editing `fn_station_selection.R` as follows. **Note that the name of your new domain should not conflict with any existing domain/list name!**

#### User-defined specific SID lists
 
Simply add it to `fn_station_selection.R` following the same convention used.

#### Domains based on lat/lon bounding boxes

To add a new domain based on a lat/lon bounding box, the domain needs to be added to `fn_station_selection.R` with values for `slat, nlat` and `wlon, elon`, following the convetnion used. The new domain name should also be added to the variable `latlon_reserved_names` (desirable but not strictly required for this methodology).

#### SID lists based on SYNOP ranges

A new SID list based on a SID range can be added by:
- Including "NewList" in `synop_reserved_names` by adding it to the vector concatenated with "Synop" e.g. `paste0(c("France","Germany","Ireland","Norway","NewList"`.
- Add SID_min/max corresponding to "NewList" to `filter_stations` following the convention used.

#### Domains based on polygon files

A new domain based on a polygon file can be used simply by adding the polygon definition to `verification/poly_files`. The new domain name should also be added to the variable `poly_reserved_names` (desirable but not strictly required for this methodology).

### Output

When this methodology is used, the SID lists used for each parameter will be saved to files in the `verification` directory with names "dynamic_param_sid_lists.rds". This allows users to see exactly what stations were used in the verification process. You can also plot a map of the stations used for each variable by using the option "-plot_dynamic_sid TRUE" when running `point_verif.R`. These will also be stored in the `verification` directory under the names "dynamic_param_domain_stationmap.png".

## Read SID lists from a static file (no longer recommended)

**WARNING: ALL INFORMATION HENCEFORTH IS DEPRECATED**

### Default domains/lists

To view the default stationlists available, use:

``` r
source(here(verification/fn_station_selection.R))
sid_lists <- fn_station_selction("all_domains")
```

There are 39 SID lists currently available (names matching `*rmv*` can be ignored here):
``` r
> names(sid_lists)
 [1] "DKlist"             "DKland"             "DKcoast"            "DKupdated"          "Greenland"         
 [6] "NorthSea"           "NNS"                "EstoniaSynop"       "LatviaSynop"        "LithuaniaSynop"    
[11] "mll_rmv_obstable"   "mll_rmv_vfldlist"   "DINI"               "IE_EN"              "IS"                
[16] "NL"                 "NL_OP"              "DK"                 "IRL"                "SCD"               
[21] "FR"                 "DE"                 "Alps"               "FranceSynop"        "GermanySynop"      
[26] "IrelandSynop"       "NorwaySynop"        "SwedenSynop"        "FinlandSynop"       "IcelandSynop"      
[31] "GreenlandSynop"     "DenmarkSynop"       "NetherlandsSynop"   "SpainSynop"         "SwitzerlandSynop"  
[36] "UnitedKingdomSynop" "Baltex"             "Gotaland"           "Latvia_Lithuania"   "Norrland"          
[41] "Sweden"             "stations_to_rmv"   
> sid_lists$DKlist
# A tibble: 38 × 1
     SID
   <dbl>
 1  6030
 2  6041
 3  6043
 4  6049
 5  6052
 6  6058
 7  6060
 8  6070
 9  6072
10  6073
# ℹ 28 more rows
# ℹ Use `print(n = ...)` to see more rows
> sid_lists$IRL
# A tibble: 113 × 5
     SID   lat   lon  elev elevmap    
   <int> <dbl> <dbl> <dbl> <chr>      
 1  3901  54.3 -7.59    72 2: 10-100m 
 2  3904  54.7 -7.58    49 2: 10-100m 
 3  3907  55.2 -6.95     6 1: <10m    
 4  3911  54.7 -6.82   225 3: 100-250m
 5  3915  54.9 -6.46    64 2: 10-100m 
 6  3916  55.2 -6.15   156 3: 100-250m
 7  3917  54.7 -6.23    63 2: 10-100m 
 8  3918  54.5 -6.33    18 2: 10-100m 
 9  3923  54.2 -6.50   161 3: 100-250m
10  3951  51.5 -9.42    21 2: 10-100m 
# ℹ 103 more rows
# ℹ Use `print(n = ...)` to see more rows
```
These names correspond to the possible values for `verif:domains` (along with the "All" option). To visualise the SID lists, you can use:
``` r
fn_station_selction("all_domains",plot_domains = TRUE)
```
This will produce pngs in the `verification` directory of this repo for each SID list.

### Add a specific SID list (as in monitor)

To add a specific list of SIDs to use as a new `verif:domains` option, simply add it to `fn_station_selection.R` following the same convention, e.g.:

``` r
#=================================================#
# USER-DEFINED STATIONLISTS (AS IN MONITOR)
# ADD SPECIFIC LISTS HERE AS REQUIRED
#=================================================#
  
# Denmark lists
DKlist    <- stats::setNames(tibble::as_tibble(
             c(06030,06041,06043,06049,06052,06058,06060,06070,06072,06073,
               06074,06079,06080,06081,06096,06102,06104,06110,06116,06118,
               06119,06120,06123,06126,06135,06138,06141,06149,06151,06154,
               06156,06165,06168,06170,06180,06181,06190,06193)),
             "SID")
# Add this to all_stations_list
all_station_lists[["DKlist"]] <- DKlist

# New list
newlist  <- stats::setNames(tibble::as_tibble(
            c(1234,5678),
            "SID")
all_station_lists[["newlist"]] <- newlist

```
You can then immediately use:
```
domains:                                  
  - "newlist"
```
in your config file. **Note that the name of your new list should not conflict with any existing station list name!**

### Stations to remove

You can define SIDs which should always be removed from the verification process by adding it to the `stations_always_rmv` variable in `fn_station_selection.R`. In addition, you can also define stations which should be removed for certain parameters, as controlled by the variable `stations_param_rmv`. For example, if you wan to remove SID=1234 for T2m and SID=5678 for Pmsl, use:
``` r
} else if (param == "T2m") {
  stations_param_rmv <- c(1234)
} else if (param == "Pmsl") {
  stations_param_rmv <- c(5678)
```
Filtering for additional parameters can be added as required. 

### SID lists based on lat/lon bounding boxes

**Generally the easiest way of updating or adding a SID list based on a geographic region is to raise an issue in the repo. However some details are given below.**

`fn_station_selection.R` can also generate SID lists based on lat/lon bounding boxes, which are contained in `verification/sid_lists.rds`. The lat/lon bounding boxes used can be viewed in `fn_station_selection.R`, e.g.:
``` r
} else if (domain == "IRL") {
      slat <- 51
      wlon <- -11
      nlat <- 55.5
      elon <- -6
```
Generation of these SID lists requires the HARMONIE-AROME `allsynop.list` and `alltemp.list` files used when producing the vfld. The location of these `allsynop/temp.list` files is specified by the `sl_dir` argument to the script. Optionally a user can also pass to the script the OBSTABLE used in the verificaiton, as specified by the argument `sql_file`. If an OBSTABLE is provided, then only common stations in the OBSTABLE and `allsynop/temp.list` will be used when generating the SID lists (which is generally recommended). Finally, an extra quality control check can be carried out by comparing the SID lat/lon in the OBSTABLE against that in the `allsynop/temp.list` for consistency (and omits the SID if the position differs significantly). This is controlled by the `multlatlon_rmv` argument to the script. 

To add a new SID list based on a lat/lon bounding box, the domain first needs to be added to the script with values for `slat, nlat` and `wlon, elon`. The new domain name also needs to be added to the variable `latlon_reserved_names`. Then to generate the list, or indeed to update an existing SID list in `verification/sid_lists.rds`, you can run:
``` r
fn_station_selection("all_domains",
		     generate_domains = TRUE,
                     domains_to_gen = "All",
                     plot_domains = TRUE,
                     sl_dir = "/path/to/allsynop.list",
                     sql_file = "/path/to/OBSTABLE",
                     domain_file = "sid_lists.rds",
                     multlatlon_rmv = TRUE)

```
Providing an OBSTABLE and setting `multlatlon_rmv=TRUE` is generally recommended. The new SID lists will be saved to a temporary file for comparison with the existing `sid_lists.rds`, which can then be overwritten if everything looks fine. Plots of the stations in each domain will also be generated in the `verification` directory. Note that if an OBSTABLE is used in the generation process (i.e. `sl_dir` is specified), the SID lists will only contain stations which are appearing in your observation database. Therefore if new stations are added to your OBSTABLE at a later point, you may need to regenerate the SID lists and update the file `sid_lists.rds`.

### SID lists based on SID ranges

An alternative to using a lat/lon bounding box is to use a country-specific WMO SID range. This is already done for multiple SID lists, e.g. "FranceSynop" and "GermanySynop" are listed in `synop_reserved_names`. These lists are stored in `sid_lists.rds` and the SID ranges are defined in the subfunction `filter_stations` as:

``` r
if (grepl("France",domain,fixed = "TRUE")) {
      SID_min <- 7001
      SID_max <- 7998
    } else if (grepl("Germany",domain,fixed = "TRUE")) {
      SID_min <- 10001
      SID_max <- 10998
```

A new SID list based on a SID range can be added by:
- Include "NewList" in `synop_reserved_names` by adding it to the vector concatenated with "Synop" e.g. `paste0(c("France","Germany","Ireland","Norway","NewList"`.
- Add SID_min/max corresponding to "NewList" to `filter_stations` as per the samples given.
- Regenerate `sid_lists.rds` (see the example for lat/lon bounding boxes above).

### SID lists based on poly files (as in monitor)

You can also generate SID lists based on polygon files. These SID lists are also stored in `sid_lists.rds`, which already contains some defaults (e.g. Baltex). To do so, you need to:
- Create a "NewList.poly" file (e.g. the Baltex.poly in monitor). A simple sample file is given in the repo.
- Specify the path to the poly file via the argument `poly_dir` (which defaults to the `verification` directory).
- Include your "NewList" in `poly_reserved_names` in the script.
- Regenerate `sid_lists.rds`, e.g.

``` r
fn_station_selection("Baltex",
		     generate_domains = TRUE,
                     domains_to_gen = "Baltex",
                     plot_domains = TRUE,
                     sl_dir = "/path/to/allsynop.list",
	             poly_dir = "/path/to/poly_files",
                     sql_file = "/path/to/OBSTABLE",
                     domain_file = "sid_lists.rds",
                     multlatlon_rmv = TRUE)

```
In certain cases you may enctounter a message saying that the specified poly file is not valid, in which case an SID list will not be generated for that domain.

Finally, by default `point_verif.R` does not run SID list generation based on bounding boxes or SID ranges and instead calls `fn_station_selection.R` which reads existing lists in `sid_lists.rds`. If you want to read from a different filename, say `tmp_sid_list.rds`, simply change the function call in `point_verif.R` to:
``` r
# Get domains from station_selection
cs_list <- fn_station_selection(base::setdiff(domains_to_run,"All"),
                                param = prm_name,
				domain_file = "tmp_sid_list.rds")
)
```
