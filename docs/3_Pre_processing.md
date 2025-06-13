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

