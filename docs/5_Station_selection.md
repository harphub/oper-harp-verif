# Station selection

The SID lists used during verification are controlled by the `verif:domains` option in the config file. This `domains` variable is essentially equivalent to `SURFSELECTION` and `TEMPSELECTION` in monitor. Setting:
```
domains:                                  
  - "All"
```
will use all stations available in your data, and this will generally be sufficient for most cases. However there may be cases where verification over a specific subset of SIDs is desired. In monitor, various SID lists are defined in `scr/selection.pm`, whereas here the SID lists are controlled by `verification/fn_station_selection`. Information regarding the default domains/station lists available, and how to add new ones, is given below.

**Note: A new method to handle station selection has been added in which SID lists are generated on the fly during the verification process. The old methodology read SID lists from a static file, however this method is no longer recommended and is now deprecated. The documentation below cover both methodologies, starting with the new recommended method.**

## Using SID lists generated on the fly (recommended method)

### Default domains/lists

There are a number of default domains/lists already available in `fn_station_selection.R` which can be used for the option `verif:domains`. These are categorised as follows:

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
