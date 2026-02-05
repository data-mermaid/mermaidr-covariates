
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mermaidrcovariates

<!-- badges: start -->

<!-- badges: end -->

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
  mermaid_get_project_data("benthicpit", data = "sampleevents")
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
the `list_covariates()` function:

``` r
list_covariates()
#> # A tibble: 8 × 10
#>   id          title description start_date end_date   license keywords providers
#>   <chr>       <chr> <chr>       <date>     <date>     <chr>   <chr>    <list>   
#> 1 10da4b11-c… Huma… "This data… 2021-12-28 NA         propri… climate… <tibble> 
#> 2 3e410700-2… MEOW… "This data… 2012-01-01 2012-12-31 propri… <NA>     <tibble> 
#> 3 50b810fb-5… Dail… "Sea surfa… 1985-01-01 2026-01-13 CC0-1.0 climate… <tibble> 
#> 4 640da5d3-5… ACA … "Allen Cor… 2018-01-01 2022-12-31 CC-BY-… allen c… <tibble> 
#> 5 789fc81a-f… Disp… "Dispersal… 2026-01-01 NA         CC0-1.0 dispers… <tibble> 
#> 6 daily_sst   Dail… "A collect… 1985-01-01 1985-01-03 CC0-1.0 climate… <tibble> 
#> 7 e6ca4bbf-1… Glob… "Global Se… 2000-01-01 2020-01-01 CC0-1.0 sedimen… <tibble> 
#> 8 ea07abba-0… Land… "Land Use … 2000-01-01 2020-01-01 CC0-1.0 land co… <tibble> 
#> # ℹ 2 more variables: `sci:citation` <chr>, bbox <list>
```

For this example, we will look at maximum “Daily Sea Surface
Temperature” (SST). We can access this data by using the function
`get_summary_zonal_statistics()`, which takes the site latitude and
longitude, as well as the survey date, to find the data at that site for
`n` days prior, then aggregates it.

We access the maximum SST (of the daily average) for the 10 days prior
to (and including) the survey date, using a radius of 1000 metres around
the survey site. The argument `spatial_stats` describes that we want to
get the **mean** of the data in that 1000 metres, while `temporal_stats`
describes that we want the **max** over the 10 days.

``` r
max_sst <- se %>%
    get_summary_zonal_statistics("Daily Sea Surface Temperature", n_days = 10, radius = 1000, spatial_stats = "mean", temporal_stats = "max")
```

The covariates are returned in a format that need to be expanded. Once
they are, you can see they contain start and end date of the data used
for the covariates, the band, and the summarised value.

``` r
max_sst %>%
  unnest(covariates)
#> # A tibble: 72 × 12
#>    site  latitude longitude sample_date covariate                     start_date
#>    <chr>    <dbl>     <dbl> <date>      <chr>                         <date>    
#>  1 BA02     -17.4      178. 2019-09-26  Daily Sea Surface Temperature 2019-09-17
#>  2 BA03     -17.4      178. 2019-09-28  Daily Sea Surface Temperature 2019-09-19
#>  3 BA04     -17.4      178. 2019-09-28  Daily Sea Surface Temperature 2019-09-19
#>  4 BA05     -17.4      178. 2019-09-26  Daily Sea Surface Temperature 2019-09-17
#>  5 BA07     -17.5      178. 2019-09-26  Daily Sea Surface Temperature 2019-09-17
#>  6 BA08     -17.4      178. 2019-09-28  Daily Sea Surface Temperature 2019-09-19
#>  7 BA09     -17.4      178. 2019-09-26  Daily Sea Surface Temperature 2019-09-17
#>  8 BA10     -17.3      178. 2019-09-28  Daily Sea Surface Temperature 2019-09-19
#>  9 BA11     -17.3      178. 2019-09-27  Daily Sea Surface Temperature 2019-09-18
#> 10 BA12     -17.3      178. 2019-09-27  Daily Sea Surface Temperature 2019-09-18
#>    end_date   n_dates  band temporal_stat spatial_stat value
#>    <date>       <int> <dbl> <chr>         <chr>        <dbl>
#>  1 2019-09-26     720     1 max           mean          26.8
#>  2 2019-09-28     720     1 max           mean          26.9
#>  3 2019-09-28     720     1 max           mean          27.0
#>  4 2019-09-26     720     1 max           mean          27.0
#>  5 2019-09-26     720     1 max           mean          27.0
#>  6 2019-09-28     720     1 max           mean          26.9
#>  7 2019-09-26     720     1 max           mean          26.9
#>  8 2019-09-28     720     1 max           mean          26.8
#>  9 2019-09-27     720     1 max           mean          26.9
#> 10 2019-09-27     720     1 max           mean          26.9
#> # ℹ 62 more rows
```
