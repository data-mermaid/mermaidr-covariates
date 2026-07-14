# mermaidrcovariates

The goal of `mermaidrcovariates` is to easily access covariates data and
add it to MERMAID data.

For more information and detailed instructions on usage, please visit
the [package
website](https://data-mermaid.github.io/mermaidr-covariates/).

For more details on using `mermaidr` in general, please see the
[`mermaidr` package website](https://data-mermaid.github.io/mermaidr/).

## Installation

You can install `mermaidrcovariates` from GitHub with:

``` r

# install.packages("remotes")
remotes::install_github("data-mermaid/mermaidr-covariates")
```

## Usage

### `mermaidr` data

Through `mermaidrcovariates`, you can easily add covariates to the
aggregated data from your coral reef surveys, which is already entered
in MERMAID. To do this, first load the `mermaidrcovariates` and
`mermaid` packages, and access the sample events for a given project.
For this example, we will work with sample events available from the
Benthic PIT method.

``` r

library(mermaidrcovariates)
library(mermaidr)

se <- mermaid_search_my_projects("Great Sea Reef 2019") %>%
  mermaid_get_project_data("benthicpit", data = "sampleevents", limit = 10)
```

To get covariates, we need each sample event’s date as well as its
latitude and longitude. We will keep these columns along with the site
name, using the `tidyverse` package for data manipulation.

``` r

library(tidyverse)

se <- se %>%
  select(site, sample_date, latitude, longitude)
```

### Covariates

You can use the [Prescient Browser](https://mermaid.prescient.earth/) to
find which covariates are available. To see this programatically, we use
the
[`list_covariates()`](https://data-mermaid.github.io/mermaidr-covariates/reference/list_covariates.md)
function:

``` r

covariates <- list_covariates()
covariates
#> # A tibble: 19 × 11
#>    title  description start_date end_date   license `sci:doi` keywords providers
#>    <chr>  <chr>       <date>     <date>     <chr>   <chr>     <chr>    <list>   
#>  1 50 Re… This datas… 2026-02-22 2026-02-22 CC-BY-… 10.5281/… conserv… <tibble> 
#>  2 ACA B… The Alan C… 2018-01-01 2021-01-01 propri… <NA>      ACA, Al… <tibble> 
#>  3 ACA R… ACA reef e… 2026-01-01 2026-01-01 CC0 1.… <NA>      reef, r… <tibble> 
#>  4 Count… This datas… 2024-10-04 2024-10-04 other   10.14284… adminis… <tibble> 
#>  5 Daily… NOAA Coral… 1985-03-25 2026-07-12 other   10.3390/… BAA, Bl… <tibble> 
#>  6 Daily… NOAA Coral… 1985-03-25 2026-07-12 other   10.3390/… coral b… <tibble> 
#>  7 Daily… NOAA Coral… 1985-01-01 2026-07-12 other   10.3390/… Coral R… <tibble> 
#>  8 Human… Coastal po… 2021-12-28 2021-12-28 MIT Li… 10.1111/… coastal… <tibble> 
#>  9 Marin… A biogeogr… 2011-11-18 2011-11-18 CC-BY-… 10.1641/… biodive… <tibble> 
#> 10 Marke… From econo… 2021-12-28 2021-12-28 MIT Li… 10.1111/… coral r… <tibble> 
#> 11 Numbe… Port locat… 2021-12-28 2021-12-28 MIT Li… 10.1111/… coral r… <tibble> 
#> 12 Touri… Intensive … 2021-12-28 2021-12-28 MIT Li… 10.1111/… coral r… <tibble> 
#> 13 GPW C… This datas… 2024-10-04 2024-10-04 other   10.14284… adminis… <tibble> 
#> 14 GPW D… Dispersal … 2026-01-01 2026-01-01 CC0 1.… <NA>      dispers… <tibble> 
#> 15 GPW G… Global Sed… 2000-01-01 2020-01-01 propri… <NA>      exposur… <tibble> 
#> 16 GPW G… Global sed… 2000-01-01 2020-01-01 CC0 1.… <NA>      gpw, se… <tibble> 
#> 17 GPW L… Land Use a… 2000-01-01 2020-01-01 CC0-1.0 <NA>      land co… <tibble> 
#> 18 GPW M… Marine Eco… 2026-01-01 2026-01-01 cc-by-… <NA>      biodive… <tibble> 
#> 19 GPW W… Watersheds… 2026-01-01 2026-01-01 CC0 1.… <NA>      gpw, hy… <tibble> 
#> # ℹ 3 more variables: `sci:citation` <chr>, bbox <list>, id <chr>
```

Covariates may contain raster data, vector data, or both. There are
different functions for accessing raster or vector data.

The table below summarises which function to use for the different
covariates available on Prescient:

| Covariate | Type | get_zonal_statistics() | attach_covariate_data() |
|:---|:---|:---|:---|
| 50 Reefs+ prioritization | raster | ✅ |  |
| ACA Benthic Habitat | raster | ✅ |  |
| ACA Reef Extent | raster | ✅ |  |
| Country Boundaries | vector |  | ✅ |
| Daily Global 5km Satellite Coral Bleaching Alert Area | raster | ✅ |  |
| Daily Global 5km Satellite Coral Bleaching Degree Heating Week | raster | ✅ |  |
| Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) | raster | ✅ |  |
| Human population within 5km (coastal population) | vector |  | ✅ |
| Marine Ecoregions of the World | vector |  | ✅ |
| Market gravity (fishing pressure) | vector |  | ✅ |
| Number of ports within 5km (industrial development) | vector |  | ✅ |
| Tourism index | vector |  | ✅ |

You can also use the function
[`covariate_helper()`](https://data-mermaid.github.io/mermaidr-covariates/reference/covariate_helper.md)
which will give you information on the data type and which function to
use, as well as any other specifications that might be needed.

### Raster data

For example, we will look at “Daily Global 5km Satellite Sea Surface
Temperature (CoralTemp)” (SST). Running the helper function tells us to
use
[`get_zonal_statistics()`](https://data-mermaid.github.io/mermaidr-covariates/reference/get_zonal_statistics.md),
and that we do not need to specify the dataset or band, since there are
only one of each.

``` r

covariate_helper("Daily Global 5km Satellite Sea Surface Temperature (CoralTemp)")
#> ℹ Covariate contains *raster* data. Use get_zonal_statistics().
#>   There is only one dataset, so you do not need to specify.
#>   There is only one band of data, so you do not need to specify.
```

If we want the maximum SST at each site over the past 30 days, we first
get the *daily* SST using
[`get_zonal_statistics()`](https://data-mermaid.github.io/mermaidr-covariates/reference/get_zonal_statistics.md),
specifying `n_days = 30`. We will request all data within a `radius` of
1000 metres from the site, and that the function *spatially* aggregates
that data for each of the 30 days.

``` r

daily_sst <- get_zonal_statistics(se,
  covariate = "Daily Global 5km Satellite Sea Surface Temperature (CoralTemp)",
  spatial_stats = "mean", radius = 1000, n_days = 30
)

daily_sst
#> # A tibble: 10 × 5
#>    site  sample_date latitude longitude zonal_statistics 
#>    <chr> <date>         <dbl>     <dbl> <list>           
#>  1 LW04  2019-09-25     -17.6      177. <tibble [30 × 5]>
#>  2 BA09  2019-09-26     -17.4      178. <tibble [30 × 5]>
#>  3 BA16  2019-09-27     -17.2      178. <tibble [30 × 5]>
#>  4 BA15  2019-09-27     -17.2      178. <tibble [30 × 5]>
#>  5 BA11  2019-09-27     -17.3      178. <tibble [30 × 5]>
#>  6 YA02  2019-09-30     -17.0      177. <tibble [30 × 5]>
#>  7 YQ02  2019-10-03     -16.6      179. <tibble [30 × 5]>
#>  8 IP3.5 2019-10-04     -16.4      179. <tibble [30 × 5]>
#>  9 GS03  2019-10-08     -16.4      178. <tibble [30 × 5]>
#> 10 GS05  2019-10-08     -16.4      178. <tibble [30 × 5]>
```

You can see that the results for each sample event contains 30 rows –
one value for each of the 30 days we requested.

To expand the zonal statistics, we use `unnest()`, and are able to see
that we have the mean value for each date:

``` r

daily_sst %>%
  select(site, sample_date, zonal_statistics) %>%
  unnest(zonal_statistics)
#> # A tibble: 300 × 7
#>    site  sample_date
#>    <chr> <date>     
#>  1 LW04  2019-09-25 
#>  2 LW04  2019-09-25 
#>  3 LW04  2019-09-25 
#>  4 LW04  2019-09-25 
#>  5 LW04  2019-09-25 
#>  6 LW04  2019-09-25 
#>  7 LW04  2019-09-25 
#>  8 LW04  2019-09-25 
#>  9 LW04  2019-09-25 
#> 10 LW04  2019-09-25 
#>    covariate                                                      date      
#>    <chr>                                                          <date>    
#>  1 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-09-25
#>  2 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-09-24
#>  3 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-09-23
#>  4 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-09-22
#>  5 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-09-21
#>  6 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-09-20
#>  7 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-09-19
#>  8 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-09-18
#>  9 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-09-17
#> 10 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-09-16
#>     band spatial_stat value
#>    <dbl> <chr>        <dbl>
#>  1     1 mean          26.5
#>  2     1 mean          26.4
#>  3     1 mean          26.5
#>  4     1 mean          26.7
#>  5     1 mean          27.0
#>  6     1 mean          26.9
#>  7     1 mean          26.8
#>  8     1 mean          26.7
#>  9     1 mean          26.7
#> 10     1 mean          26.8
#> # ℹ 290 more rows
```

Since we would like the *maximum* SST for each site over those 30 days,
we then use `aggregate_zonal_statistics()`, specifying that the
*temporal* summary statistic should be max:

``` r

max_sst <- daily_sst %>%
  summarise_zonal_statistics("max")

max_sst
#> # A tibble: 10 × 6
#>    site  sample_date latitude longitude zonal_statistics 
#>    <chr> <date>         <dbl>     <dbl> <list>           
#>  1 LW04  2019-09-25     -17.6      177. <tibble [30 × 5]>
#>  2 BA09  2019-09-26     -17.4      178. <tibble [30 × 5]>
#>  3 BA16  2019-09-27     -17.2      178. <tibble [30 × 5]>
#>  4 BA15  2019-09-27     -17.2      178. <tibble [30 × 5]>
#>  5 BA11  2019-09-27     -17.3      178. <tibble [30 × 5]>
#>  6 YA02  2019-09-30     -17.0      177. <tibble [30 × 5]>
#>  7 YQ02  2019-10-03     -16.6      179. <tibble [30 × 5]>
#>  8 IP3.5 2019-10-04     -16.4      179. <tibble [30 × 5]>
#>  9 GS03  2019-10-08     -16.4      178. <tibble [30 × 5]>
#> 10 GS05  2019-10-08     -16.4      178. <tibble [30 × 5]>
#>    summary_zonal_statistics
#>    <list>                  
#>  1 <tibble [1 × 8]>        
#>  2 <tibble [1 × 8]>        
#>  3 <tibble [1 × 8]>        
#>  4 <tibble [1 × 8]>        
#>  5 <tibble [1 × 8]>        
#>  6 <tibble [1 × 8]>        
#>  7 <tibble [1 × 8]>        
#>  8 <tibble [1 × 8]>        
#>  9 <tibble [1 × 8]>        
#> 10 <tibble [1 × 8]>
```

We retain the original `zonal_statistics` column, which contains the
daily values, but now also have a `summary_zonal_statistics` column,
which contains only *one* row for each sample event.

We expand it and see a summary of the start and end date of the data
used for each sample event, as well as the spatial and temporal
statistics used to summarise, and the actual value:

``` r

max_sst %>%
  select(site, sample_date, summary_zonal_statistics) %>%
  unnest(summary_zonal_statistics)
#> # A tibble: 10 × 10
#>    site  sample_date
#>    <chr> <date>     
#>  1 LW04  2019-09-25 
#>  2 BA09  2019-09-26 
#>  3 BA16  2019-09-27 
#>  4 BA15  2019-09-27 
#>  5 BA11  2019-09-27 
#>  6 YA02  2019-09-30 
#>  7 YQ02  2019-10-03 
#>  8 IP3.5 2019-10-04 
#>  9 GS03  2019-10-08 
#> 10 GS05  2019-10-08 
#>    covariate                                                      start_date
#>    <chr>                                                          <date>    
#>  1 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-08-27
#>  2 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-08-28
#>  3 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-08-29
#>  4 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-08-29
#>  5 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-08-29
#>  6 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-09-01
#>  7 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-09-04
#>  8 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-09-05
#>  9 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-09-09
#> 10 Daily Global 5km Satellite Sea Surface Temperature (CoralTemp) 2019-09-09
#>    end_date   n_dates  band spatial_stat temporal_stat value
#>    <date>       <dbl> <dbl> <chr>        <chr>         <dbl>
#>  1 2019-09-25      30     1 mean         max            27.2
#>  2 2019-09-26      30     1 mean         max            27.0
#>  3 2019-09-27      30     1 mean         max            27.0
#>  4 2019-09-27      30     1 mean         max            27.0
#>  5 2019-09-27      30     1 mean         max            27  
#>  6 2019-09-30      30     1 mean         max            27.0
#>  7 2019-10-03      30     1 mean         max            27.4
#>  8 2019-10-04      30     1 mean         max            27.7
#>  9 2019-10-08      30     1 mean         max            27.5
#> 10 2019-10-08      30     1 mean         max            27.4
```

### Vector data

If we want to attach vector data, such as Country Boundaries, we use
`attach_vector_data()`. Again, the
[`covariate_helper()`](https://data-mermaid.github.io/mermaidr-covariates/reference/covariate_helper.md)
function is useful for determining what to do here:

``` r

covariate_helper("Country Boundaries")
#> ℹ Covariate contains *vector* data. Use attach_covariates().
#>   There is only one dataset, so you do not need to specify.
#>   By default, all columns will be returned. You can limit to specific colunmns in `columns` argument.
#>   Column options: "COUNTRY_ID", "TERRITORY1", "UN_TER1", "geom"
```

This tells us which function to use, and that we do not need to specify
the dataset for the covariate. It also tells us that, by default, all of
the columns in the vector data will be returned, but that we can limit
to specific columns.

We attach the covariate data:

``` r

se_country_all_cols <- se %>%
  attach_covariate_data("Country Boundaries")

se_country_all_cols
#> # A tibble: 10 × 7
#>    site  sample_date latitude longitude COUNTRY_ID TERRITORY1 UN_TER1
#>    <chr> <date>         <dbl>     <dbl>      <int> <chr>        <dbl>
#>  1 BA09  2019-09-26     -17.4      178.         54 Fiji           242
#>  2 BA16  2019-09-27     -17.2      178.         54 Fiji           242
#>  3 GS03  2019-10-08     -16.4      178.         54 Fiji           242
#>  4 BA15  2019-09-27     -17.2      178.         54 Fiji           242
#>  5 YA02  2019-09-30     -17.0      177.         54 Fiji           242
#>  6 LW04  2019-09-25     -17.6      177.         54 Fiji           242
#>  7 IP3.5 2019-10-04     -16.4      179.         54 Fiji           242
#>  8 BA11  2019-09-27     -17.3      178.         54 Fiji           242
#>  9 GS05  2019-10-08     -16.4      178.         54 Fiji           242
#> 10 YQ02  2019-10-03     -16.6      179.         54 Fiji           242
```

In this case, it is *not* a format that needs to be expanded – the
columns are immediately available.

If we only want one column (e.g. the country name), then we can specify
that in the function:

``` r

se_country <- se %>%
  attach_covariate_data("Country Boundaries", columns = "TERRITORY1")

se_country
#> # A tibble: 10 × 5
#>    site  sample_date latitude longitude TERRITORY1
#>    <chr> <date>         <dbl>     <dbl> <chr>     
#>  1 BA09  2019-09-26     -17.4      178. Fiji      
#>  2 BA16  2019-09-27     -17.2      178. Fiji      
#>  3 GS03  2019-10-08     -16.4      178. Fiji      
#>  4 BA15  2019-09-27     -17.2      178. Fiji      
#>  5 YA02  2019-09-30     -17.0      177. Fiji      
#>  6 LW04  2019-09-25     -17.6      177. Fiji      
#>  7 IP3.5 2019-10-04     -16.4      179. Fiji      
#>  8 BA11  2019-09-27     -17.3      178. Fiji      
#>  9 GS05  2019-10-08     -16.4      178. Fiji      
#> 10 YQ02  2019-10-03     -16.6      179. Fiji
```
