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

Through `mermaidrcovariates`, you can easily add covariates to the
aggregated data from your coral reef surveys, which is already entered
in MERMAID. To do this, first load the `mermaidrcovariates` and
`mermaid` packages, and access the sample events for a given project.
For this example, we will work with hard coral cover data, available
from the Benthic PIT method.

``` r
library(mermaidrcovariates)
library(mermaidr)

se <- mermaid_search_my_projects("Great Sea Reef 2019") %>%
  mermaid_get_project_data("benthicpit", data = "sampleevents", limit = 10)
```

To get covariates, we need each sample event’s date as well as its
latitude and longitude. We will keep these columns along with the site
name and average hard coral cover, using the `tidyverse` package for
data manipulation.

``` r
library(tidyverse)

se <- se %>%
  select(site, sample_date, latitude, longitude, hard_coral_cover = percent_cover_benthic_category_avg_hard_coral)
```

You can use the [Prescient browser](https://mermaid.prescient.earth/) to
find which covariates are available. To see this programatically, we use
the
[`list_covariates()`](https://data-mermaid.github.io/mermaidr-covariates/reference/list_covariates.md)
function:

``` r
list_covariates()
#> # A tibble: 10 × 10
#>    id         title description start_date end_date   license keywords providers
#>    <chr>      <chr> <chr>       <date>     <date>     <chr>   <chr>    <list>   
#>  1 10da4b11-… Huma… "This data… 2021-12-28 NA         propri… climate… <tibble> 
#>  2 3e410700-… MEOW… "This data… 2012-01-01 2012-12-31 propri… <NA>     <tibble> 
#>  3 50b810fb-… Dail… "Sea surfa… 1985-01-01 2026-01-13 CC0-1.0 climate… <tibble> 
#>  4 640da5d3-… ACA … "Allen Cor… 2018-01-01 2022-12-31 CC-BY-… allen c… <tibble> 
#>  5 aca_extent ACA … "ACA Reef … 2026-01-01 NA         CC0-1.0 aca, re… <tibble> 
#>  6 countries  Coun… "Country B… 2026-01-01 NA         CC0-1.0 adminis… <tibble> 
#>  7 daily_sst  Dail… "A collect… 1985-01-01 1985-01-03 CC0-1.0 climate… <tibble> 
#>  8 disp_poin… Disp… "Dispersal… 2026-01-01 NA         CC0-1.0 dispers… <tibble> 
#>  9 lulc       Land… "Land Use … 2000-01-01 2020-01-01 CC0-1.0 land co… <tibble> 
#> 10 sediment_… Glob… "Global Se… 2000-01-01 2000-01-01 propri… sedimen… <tibble> 
#> # ℹ 2 more variables: `sci:citation` <chr>, bbox <list>
```

For this example, we will look at maximum “Daily Sea Surface
Temperature” (SST). We can access this data by using the function
[`get_summary_zonal_statistics()`](https://data-mermaid.github.io/mermaidr-covariates/reference/get_summary_zonal_statistics.md),
which takes the site latitude and longitude, as well as the survey date,
to find the data at that site for `n_days` days prior, within the given
`radius`, and *spatially* aggregates it according to the specified
`spatial_stat`. Then, the function *temporally* aggregates it according
to the specified `temporal_stat`.

For example, to get the **maximum** SST over the 20 days prior to (and
including) the sample event, using the *mean* SST within 100m of the
sites:

``` r
max_sst <- se %>%
  get_summary_zonal_statistics("Daily Sea Surface Temperature", n_days = 10, radius = 100, spatial_stats = "mean", temporal_stats = "max")
```

The covariates are returned in a format that need to be expanded. Once
they are, you can see they contain start and end date of the data used
for the covariates, the band, and the summarised value.

``` r
max_sst %>%
  unnest(covariates)
#> # A tibble: 10 × 12
#>    site  latitude longitude sample_date covariate                     start_date
#>    <chr>    <dbl>     <dbl> <date>      <chr>                         <date>    
#>  1 BA09     -17.4      178. 2019-09-26  Daily Sea Surface Temperature 2019-09-17
#>  2 BA11     -17.3      178. 2019-09-27  Daily Sea Surface Temperature 2019-09-18
#>  3 BA15     -17.2      178. 2019-09-27  Daily Sea Surface Temperature 2019-09-18
#>  4 BA16     -17.2      178. 2019-09-27  Daily Sea Surface Temperature 2019-09-18
#>  5 GS03     -16.4      178. 2019-10-08  Daily Sea Surface Temperature 2019-09-29
#>  6 GS05     -16.4      178. 2019-10-08  Daily Sea Surface Temperature 2019-09-29
#>  7 IP3.5    -16.4      179. 2019-10-04  Daily Sea Surface Temperature 2019-09-25
#>  8 LW04     -17.6      177. 2019-09-25  Daily Sea Surface Temperature 2019-09-16
#>  9 YA02     -17.0      177. 2019-09-30  Daily Sea Surface Temperature 2019-09-21
#> 10 YQ02     -16.6      179. 2019-10-03  Daily Sea Surface Temperature 2019-09-24
#>    end_date   n_dates  band temporal_stat spatial_stat value
#>    <date>       <int> <dbl> <chr>         <chr>        <dbl>
#>  1 2019-09-26      23     1 max           mean          26.9
#>  2 2019-09-27      23     1 max           mean          26.9
#>  3 2019-09-27      23     1 max           mean          26.8
#>  4 2019-09-27      23     1 max           mean          26.8
#>  5 2019-10-08      23     1 max           mean          27.4
#>  6 2019-10-08      23     1 max           mean          27.4
#>  7 2019-10-04      23     1 max           mean          27.5
#>  8 2019-09-25      23     1 max           mean          27.0
#>  9 2019-09-30      23     1 max           mean          27.0
#> 10 2019-10-03      23     1 max           mean          27.3
```

If we don’t want the data aggregated over time – for example, if we just
want the value of a covariate the day of the sample event, or if we want
to have the individual dates attached to the covariate data – we use the
non-summary version of the function,
[`get_zonal_statistics()`](https://data-mermaid.github.io/mermaidr-covariates/reference/get_zonal_statistics.md).
In this case, we omit `temporal_stats` and just get the mean SST within
100m of the sites, for all 20 days:

``` r
sst_by_day <- se %>%
  get_zonal_statistics("Daily Sea Surface Temperature", n_days = 10, radius = 100, spatial_stats = "mean")

sst_by_day %>%
  unnest(covariates)
#> # A tibble: 100 × 12
#>    site  latitude longitude sample_date covariate                     start_date
#>    <chr>    <dbl>     <dbl> <date>      <chr>                         <date>    
#>  1 BA09     -17.4      178. 2019-09-26  Daily Sea Surface Temperature 2019-09-17
#>  2 BA09     -17.4      178. 2019-09-26  Daily Sea Surface Temperature 2019-09-17
#>  3 BA09     -17.4      178. 2019-09-26  Daily Sea Surface Temperature 2019-09-17
#>  4 BA09     -17.4      178. 2019-09-26  Daily Sea Surface Temperature 2019-09-17
#>  5 BA09     -17.4      178. 2019-09-26  Daily Sea Surface Temperature 2019-09-17
#>  6 BA09     -17.4      178. 2019-09-26  Daily Sea Surface Temperature 2019-09-17
#>  7 BA09     -17.4      178. 2019-09-26  Daily Sea Surface Temperature 2019-09-17
#>  8 BA09     -17.4      178. 2019-09-26  Daily Sea Surface Temperature 2019-09-17
#>  9 BA09     -17.4      178. 2019-09-26  Daily Sea Surface Temperature 2019-09-17
#> 10 BA09     -17.4      178. 2019-09-26  Daily Sea Surface Temperature 2019-09-17
#>    end_date   n_dates date        band spatial_stat value
#>    <date>       <int> <chr>      <dbl> <chr>        <dbl>
#>  1 2019-09-26      23 2019-09-26     1 mean          26.1
#>  2 2019-09-26      23 2019-09-25     1 mean          26.3
#>  3 2019-09-26      23 2019-09-24     1 mean          26.3
#>  4 2019-09-26      23 2019-09-23     1 mean          26.4
#>  5 2019-09-26      23 2019-09-22     1 mean          26.6
#>  6 2019-09-26      23 2019-09-21     1 mean          26.9
#>  7 2019-09-26      23 2019-09-20     1 mean          26.7
#>  8 2019-09-26      23 2019-09-19     1 mean          26.6
#>  9 2019-09-26      23 2019-09-18     1 mean          26.5
#> 10 2019-09-26      23 2019-09-17     1 mean          26.5
#> # ℹ 90 more rows
```

Here, instead of having the data aggregated over time, there is one row
for each date, along with the value on that date.
