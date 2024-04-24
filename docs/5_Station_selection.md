# Station selection

The SID lists used during verification are controlled by the `verif:domains` option in the config file. This `domains` variable is essentially equivalent to `SURFSELECTION` and `TEMPSELECTION` in monitor. Setting:
```
domains:                                  
  - "All"
```
will use all stations available in your data, and this will generally be sufficient for most cases. However there may be cases where verification over a specific subset of SIDs is desired. In monitor, various SID lists are defined in `scr/selection.pm`, whereas here the SID lists are controlled by `verification/fn_station_selection`. Information regarding the default station lists available, and how to add new ones, is given below.

## Default lists

To view the default stationlists available, use:

``` r
source(here(verification/fn_station_selection.R))
sid_lists <- fn_station_selction("all_domains")
```

There are 34 SID lists currently available (names matching `*rmv*` can be ignored here):
``` r
> names(sid_lists)
 [1] "DKlist"             "DKland"             "DKcoast"            "DKupdated"          "Greenland"          "NorthSea"           "NNS"                "EstoniaSynop"       "LatviaSynop"     
[10] "LithuaniaSynop"     "mll_rmv_obstable"   "mll_rmv_vfldlist"   "DINI"               "IE_EN"              "IS"                 "NL"                 "NL_OP"              "DK"              
[19] "IRL"                "SCD"                "FR"                 "DE"                 "Alps"               "FranceSynop"        "GermanySynop"       "IrelandSynop"       "NorwaySynop"     
[28] "SwedenSynop"        "FinlandSynop"       "IcelandSynop"       "GreenlandSynop"     "DenmarkSynop"       "NetherlandsSynop"   "SpainSynop"         "SwitzerlandSynop"   "UnitedKingdomSynop"
[37] "stations_to_rmv"   
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

## Add a specific SID list (as in monitor)

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

## Stations to remove

You can define SIDs which should always be removed from the verification process by adding it to the `stations_always_rmv` variable in `fn_station_selection.R`. In addition, you can also define stations which should be removed for certain parameters, as controlled by the variable `stations_param_rmv`. For example, if you wan to remove SID=1234 for T2m and SID=5678 for Pmsl, use:
``` r
} else if (param == "T2m") {
  stations_param_rmv <- c(1234)
} else if (param == "Pmsl") {
  stations_param_rmv <- c(5678)
```
Filtering for additional parameters can be added as required. 

## SID lists based on lat/lon bounding boxes

**Generally the easiest way of updating or adding a SID list based on a geographic region is to raise an issue in the repo. However some details are given below.**

`fn_station_selection.R` can also generate SID lists based on geographic regions, which are contained in `verification/sid_lists.rds`. The lat/lon bounding boxes used for each SID list can be viewed in `fn_station_selection.R`, e.g.:
``` r
} else if (domain == "IRL") {
      slat <- 51
      wlon <- -11
      nlat <- 55.5
      elon <- -6
```
Generation of these lists requires three files:
- The HARMONIE-AROME `allsynop.list` and `alltemp.list` files used when producing the vfld.
- The OBSTABLE used in the verification.

Paths to these files are specified by `sl_dir` and `sql_file`, respectively, in the script. These three files are requried as the script compares the SID lat/lon in the OBSTABLE against that in the `allsynop/temp.list` for consistency (and omits the SID if the position differs significantly). This is controlled by the `multlatlon_rmv` flag.

To add a new SID list based on a lat/lon bounding box, the domain first needs to be added to the script with values for `slat, nlat` and `wlon, elon`. The new domain name also needs to be added to the variable `all_reserved_names`. Then to generate the list, or indeed to update an existing SID list in `verification/sid_lists.rds`, you can run:
``` r
fn_station_selection("all_domains",
		     generate_domains = TRUE,
                     domains_to_gen = "All",
                     plot_domains = TRUE,
                     sl_dir = "/path/to/allsynop.list",
                     sql_file = "/path/to/OBSTABLE")

```
This process can take some time. The new SID lists will be saved to a temporary file for comparison with the existing `sid_lists.rds`, which can then be overwritten if everything looks fine. Note that as the OBSTABLE is used in the generation process, the SID lists will only contain stations which are appearing in your observation database. Therefore if new stations are added to your OBSTABLE at a later point, you may need to regenerate the SID lists and update the file `sid_lists.rds`.

## SID lists based on SID ranges

An alternative to using a lat/lon bounding box is to use a country-specific WMO SID range. This is already done for multiple SID lists, e.g. "FranceSynop" and "GermanySynop" are listed in `all_reserved_names` and the SID ranges are defined in the subfunction `filter_stations` as:

``` r
if (grepl("France",domain,fixed = "TRUE")) {
      SID_min <- 7001
      SID_max <- 7998
    } else if (grepl("Germany",domain,fixed = "TRUE")) {
      SID_min <- 10001
      SID_max <- 10998
```

A new SID list based on a SID range can be added by:
- Include "NewList" in `all_reserved_names` by adding it to the vector concatenated with "Synop" e.g. `paste0(c("France","Germany","Ireland","Norway","NewList"`.
- Add SID_min/max corresponding to "NewList" to `filter_stations` as per the samples given.
- Regenerate `sid_lists.rds` (see the example for lat/lon bounding boxes above).

Finally, by default `point_verif.R` does not run SID list generation based on bounding boxes or SID ranges and instead calls `fn_station_selection.R` which reads existing lists in `sid_lists.rds`. If you want to read from a different filename, say `tmp_sid_list.rds`, simply change the function call in `point_verif.R` to:
``` r
# Get domains from station_selection
cs_list <- fn_station_selection(base::setdiff(domains_to_run,"All"),
                                param = prm_name,
				domain_file = "tmp_sid_list.rds")
)
```
