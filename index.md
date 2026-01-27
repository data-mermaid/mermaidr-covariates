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
the
[`list_covariates()`](https://data-mermaid.github.io/mermaidr-covariates/reference/list_covariates.md)
function:

``` r
list_covariates()
#> $`3e410700-2e6a-4b44-a2d3-1d829d19acb0`
#> ###Collection
#> - id: 3e410700-2e6a-4b44-a2d3-1d829d19acb0
#> - title: MEOW Boundaries
#> - description: 
#> This dataset combines two separately published datasets: the "Marine Ecoregions Of the World" (MEOW; 2007) and the "Pelagic Provinces Of the World" (PPOW; 2012). These datasets were developed by Mark Spalding and colleagues in The Nature Conservancy. Alongside the individual authors, partners for the MEOW layer included WWF, Ramsar, WCS, and UNEP-WCMC. The ecoregions and pelagic provinces are broadly aligned with each other and are non-overlapping.
#> 
#> The MEOW dataset shows a biogeographic classification of the world's coastal and continental shelf waters, following a nested hierarchy of realms, provinces and ecoregions. It describes 232 ecoregions, which lie within 62 provinces and 12 large realms. The regions aim to capture generic patterns of biodiversity across habitats and taxa, with regions extending from the coast (intertidal zone) to the 200 m depth contour (extended beyond these waters out by a 5 km buffer).
#> 
#> The PPOW dataset shows a biogeographic classification of the surface pelagic (i.e. epipelagic) waters of the world's oceans. It describes 37 pelagic provinces of the world, nested into four broad realms. A system of seven biomes are also identified ecologically, and these are spatially disjoint but united by common abiotic conditions, thereby creating physiognomically similar communities.
#> - field(s): 
#> id, type, links, title, extent, license, keywords, providers, description, sci:citation, stac_version, stac_extensions
#> 
#> $`640da5d3-530f-4b92-bbb8-07e70e386f8b`
#> ###Collection
#> - id: 640da5d3-530f-4b92-bbb8-07e70e386f8b
#> - title: ACA Benthic Habitat
#> - description: 
#> Allen Coral Atlas benthic habitat classification map providing detailed geomorphic and benthic cover classifications for shallow coral reef ecosystems worldwide. The dataset is derived from satellite imagery and machine learning classification methods.
#> - field(s): 
#> id, type, links, title, extent, license, keywords, providers, description, stac_version, stac_extensions
#> 
#> $`50b810fb-5f17-4cdb-b34b-c377837e2a29`
#> ###Collection
#> - id: 50b810fb-5f17-4cdb-b34b-c377837e2a29
#> - title: Daily Sea Surface Temperature
#> - description: Sea surface temperature each day.
#> - field(s): 
#> id, type, links, title, extent, license, keywords, providers, summaries, description, stac_version
#> 
#> $`10da4b11-c79f-4ce7-b359-0d4710a1f0fd`
#> ###Collection
#> - id: 10da4b11-c79f-4ce7-b359-0d4710a1f0fd
#> - title: Human Pressures and Climate Vulnerability
#> - description: 
#> This dataset combines climate vulnerability scores and human pressure indicators for coral reefs worldwide. It integrates data from Beyer et al. (2018) on climate vulnerability and Andrello et al. (2022) on local human pressures.
#> 
#> Climate vulnerability scores include overall vulnerability, current niche, current year, projected future climate, thermal history, and temperature range metrics. Human pressure indicators include sedimentation, nutrients (nitrogen), coastal population within 5 km, market gravity (fishing pressure), number of ports, reef value, and cumulative human pressure index.
#> 
#> The dataset provides both normalized indicator values and raw values for key pressure metrics, along with regional classifications and BCU (Bioclimatic Unit) designations.
#> - field(s): 
#> id, type, links, title, extent, license, keywords, providers, description, sci:citation, stac_version, stac_extensions
#> 
#> $`ea07abba-06cf-41a8-92a3-b20eaf801ea9`
#> ###Collection
#> - id: ea07abba-06cf-41a8-92a3-b20eaf801ea9
#> - title: Land Use and Land Cover (LULC) Collection
#> - description: Land Use and Land Cover dataset collection
#> - field(s): 
#> id, type, links, title, extent, license, keywords, providers, summaries, description, stac_version
```

For this example, we will look at mean Daily Sea Surface Temperature
(SST). We can access this data by using its id —
`50b810fb-5f17-4cdb-b34b-c377837e2a29` — in the function
[`get_zonal_statistics()`](https://data-mermaid.github.io/mermaidr-covariates/reference/get_zonal_statistics.md).
The
[`get_zonal_statistics()`](https://data-mermaid.github.io/mermaidr-covariates/reference/get_zonal_statistics.md)
function takes the site latitude and longitude, as well as the survey
date, to find the data at that site for `n` days prior, then aggregates
it.

We access the maximum SST (of the daily average) for the 60 days prior
to the survey data, using a buffer size of 1000 metres:

``` r
max_sst <- se %>%
  get_zonal_statistics("50b810fb-5f17-4cdb-b34b-c377837e2a29", n_days = 10, buffer = 1000, stats = "max")
```

The covariates are returned in a format that need to be expanded. Once
they are, you can see they contain start and end date of the data used
for the covariates, the band, and the summarised value.

``` r
max_sst %>%
  unnest(covariates)
#> # A tibble: 72 × 11
#>    site  sample_date latitude longitude hard_coral_cover
#>    <chr> <date>         <dbl>     <dbl>            <dbl>
#>  1 BA09  2019-09-26     -17.4      178.             12.3
#>  2 BA16  2019-09-27     -17.2      178.             10.7
#>  3 GS03  2019-10-08     -16.4      178.             52  
#>  4 BA15  2019-09-27     -17.2      178.             25  
#>  5 YA02  2019-09-30     -17.0      177.             26.3
#>  6 LW04  2019-09-25     -17.6      177.             11.7
#>  7 IP3.5 2019-10-04     -16.4      179.             52.3
#>  8 BA11  2019-09-27     -17.3      178.             23  
#>  9 GS05  2019-10-08     -16.4      178.             40.3
#> 10 YQ02  2019-10-03     -16.6      179.             59  
#>    covariate                            start_date end_date    band statistic
#>    <chr>                                <date>     <date>     <dbl> <chr>    
#>  1 50b810fb-5f17-4cdb-b34b-c377837e2a29 2019-07-28 2019-09-27     1 max      
#>  2 50b810fb-5f17-4cdb-b34b-c377837e2a29 2019-07-29 2019-09-28     1 max      
#>  3 50b810fb-5f17-4cdb-b34b-c377837e2a29 2019-08-09 2019-10-09     1 max      
#>  4 50b810fb-5f17-4cdb-b34b-c377837e2a29 2019-07-29 2019-09-28     1 max      
#>  5 50b810fb-5f17-4cdb-b34b-c377837e2a29 2019-08-01 2019-10-01     1 max      
#>  6 50b810fb-5f17-4cdb-b34b-c377837e2a29 2019-07-27 2019-09-26     1 max      
#>  7 50b810fb-5f17-4cdb-b34b-c377837e2a29 2019-08-05 2019-10-05     1 max      
#>  8 50b810fb-5f17-4cdb-b34b-c377837e2a29 2019-07-29 2019-09-28     1 max      
#>  9 50b810fb-5f17-4cdb-b34b-c377837e2a29 2019-08-09 2019-10-09     1 max      
#> 10 50b810fb-5f17-4cdb-b34b-c377837e2a29 2019-08-04 2019-10-04     1 max      
#>    value
#>    <dbl>
#>  1  27.1
#>  2  27.0
#>  3  27.5
#>  4  27.0
#>  5  27.1
#>  6  27.2
#>  7  27.7
#>  8  27.0
#>  9  27.5
#> 10  27.4
#> # ℹ 62 more rows
```
