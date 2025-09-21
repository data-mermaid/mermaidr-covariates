Getting environmental covariates for GFCR locations
================

This analysis extracts covariates for GFCR project sites from MERMAID,
then combines that with coral cover data to plot the relationship
between the two. In this example, the environmental data is maximum
degree heating weeks (DHW) for a given number of days prior to the
survey.

# Load packages

Load libraries to use for analysis.

``` r
# Need to run this to ensure that rstac works, because the API is missing "type" in Items:
# remotes::install_github("sharlagelfand/rstac", ref = "invalid-item")

# remotes::install_github("data-mermaid/mermaidr")
# remotes::install_github("data-mermaid/mermaidr-covariates")

library(mermaidr)
library(mermaidrcovariates)
library(tidyverse)
```

# Get project data from MERMAID

The first step is to get coral cover data for GFCR proejcts from
MERMAID. The following gets summary sample events, and then filters for
projects whose tags contain “GFCR”, and projects that have hard coral
cover data.

``` r
summary_sampleevents <- mermaid_get_summary_sampleevents()

gfcr_summary_sampleevents <- summary_sampleevents %>%
  filter(str_detect(tags, "GFCR")) %>%
  filter(!is.na(`benthicpit_percent_cover_benthic_category_avg_Hard coral`)) %>%
  rename(hard_coral_cover = `benthicpit_percent_cover_benthic_category_avg_Hard coral`)

gfcr_summary_sampleevents
```

    ## # A tibble: 29 × 377
    ##    project_id   project tags  country site_id site  latitude longitude reef_type
    ##    <chr>        <chr>   <chr> <chr>   <chr>   <chr>    <dbl>     <dbl> <chr>    
    ##  1 54d68642-48… SLCRI … Inte… Sri La… 4affc2… Bar …     8.41      79.7 patch    
    ##  2 54d68642-48… SLCRI … Inte… Sri La… e61f1d… Bar …     8.37      79.7 patch    
    ##  3 54d68642-48… SLCRI … Inte… Sri La… 168b07… Hipo…     8.27      79.7 patch    
    ##  4 54d68642-48… SLCRI … Inte… Sri La… 51f953… Kand…     8.25      79.7 patch    
    ##  5 54d68642-48… SLCRI … Inte… Sri La… 630643… Kuda…     8.24      79.7 patch    
    ##  6 54d68642-48… SLCRI … Inte… Sri La… 1363cc… Kuda…     8.24      79.7 patch    
    ##  7 54d68642-48… SLCRI … Inte… Sri La… 3d49dc… Sea …     8.17      79.7 fringing 
    ##  8 54d68642-48… SLCRI … Inte… Sri La… 3d49dc… Sea …     8.17      79.7 fringing 
    ##  9 54d68642-48… SLCRI … Inte… Sri La… 0c41df… St. …     8.14      79.7 patch    
    ## 10 54d68642-48… SLCRI … Inte… Sri La… 74f31a… Thal…     8.12      79.7 patch    
    ## # ℹ 19 more rows
    ## # ℹ 368 more variables: reef_zone <chr>, reef_exposure <chr>,
    ## #   management_id <chr>, management <chr>, management_est_year <int>,
    ## #   management_size <dbl>, management_parties <chr>,
    ## #   management_compliance <chr>, management_rules <chr>, sample_date <date>,
    ## #   data_policy_beltfish <chr>, data_policy_benthiclit <chr>,
    ## #   data_policy_benthicpit <chr>, data_policy_benthicpqt <chr>, …

## Summary of hard coral cover for projects

Summarise projects to show their tags, country, number of sites, and
average hard coral cover.

``` r
gfcr_summary_sampleevents %>%
  group_by(project, tags, country) %>%
  summarise(
    n_sites = length(site),
    average_hard_coral_cover = mean(hard_coral_cover), .groups = "drop"
  )
```

    ## # A tibble: 3 × 5
    ##   project                      tags       country n_sites average_hard_coral_c…¹
    ##   <chr>                        <chr>      <chr>     <int>                  <dbl>
    ## 1 SLCRI Bar Reef Seascape      Internati… Sri La…      11                   9.68
    ## 2 SLCRI Kayankerni Seascape    Global Fu… Sri La…      11                  44.4 
    ## 3 SLCRI Pigeon Island Seascape Global Fu… Sri La…       7                  55.9 
    ## # ℹ abbreviated name: ¹​average_hard_coral_cover

# Get covariates for these sites

Next, get the relevant covariates for these sites.

## List STAC collections

List available STAC collections. We have available the monthly
aggregation of Degree Heating Weeks (DHW).

``` r
list_collections()
```

    ## $`noaa-monthly-max-dhw`
    ## ###Collection
    ## - id: noaa-monthly-max-dhw
    ## - title: NOAA Degree Heating Week (DHW) - Monthly Aggregation
    ## - description: 
    ## The NOAA Coral Reef Watch (CRW) daily global 5km satellite coral bleaching Degree Heating Week (DHW) product shows accumulated heat stress, which can lead to coral bleaching and death. The scale ranges from 0 to 20 °C-weeks. The DHW product accumulates the instantaneous bleaching heat stress, measured by CRW's Coral Bleaching HotSpot, during the most recent 12-week period. It is directly related to the timing and intensity of coral bleaching.
    ## - field(s): 
    ## type, id, stac_version, description, links, stac_extensions, title, extent, license, keywords, providers, summaries

## Get maximum DHW for previous year

Focus on degree heating weeks (DHW), and get the max (of the monhtly
average) for the 365 days prior to the survey data. Review the survey
data:

``` r
gfcr_summary_sampleevents %>%
  distinct(project, site, latitude, longitude, sample_date)
```

    ## # A tibble: 29 × 5
    ##    project                 site                  latitude longitude sample_date
    ##    <chr>                   <chr>                    <dbl>     <dbl> <date>     
    ##  1 SLCRI Bar Reef Seascape Bar Reef Deep             8.41      79.7 2025-03-16 
    ##  2 SLCRI Bar Reef Seascape Bar Reef Shallow          8.37      79.7 2025-03-16 
    ##  3 SLCRI Bar Reef Seascape Hipolis Reef Deep         8.27      79.7 2025-03-20 
    ##  4 SLCRI Bar Reef Seascape KandaKuliya Reef Deep     8.25      79.7 2025-03-19 
    ##  5 SLCRI Bar Reef Seascape Kudawa reef Deep          8.24      79.7 2025-03-17 
    ##  6 SLCRI Bar Reef Seascape Kudawa Reef Shallow       8.24      79.7 2025-03-17 
    ##  7 SLCRI Bar Reef Seascape Sea Guard Reef Deep       8.17      79.7 2024-03-19 
    ##  8 SLCRI Bar Reef Seascape Sea Guard Reef Deep       8.17      79.7 2025-03-19 
    ##  9 SLCRI Bar Reef Seascape St. Annes Reef Deep       8.14      79.7 2025-03-20 
    ## 10 SLCRI Bar Reef Seascape Thalawila Reef Deep       8.12      79.7 2025-03-15 
    ## # ℹ 19 more rows

The `summary_zonal_stats()` function takes the site latitude and
longitude, as well as the survey date, to find the data at that site for
`n` days prior, then aggregates it. The buffer size is set to 1000
metres.

``` r
max_dhw <- gfcr_summary_sampleevents %>%
  summary_zonal_stats("noaa-monthly-max-dhw", n_days = 60, buffer = 1000, stats = "max")
```

``` r
max_dhw <- gfcr_summary_sampleevents %>%
  summary_zonal_stats("noaa-monthly-max-dhw", n_days = 365, buffer = 1000, stats = "max")
```

``` r
saveRDS(max_dhw, here::here("scratch", "max_dhw.rds"))
```

``` r
max_dhw <- readRDS(here::here("scratch", "max_dhw.rds"))
```

Looka the returned data, keeping only the project information, site,
survey date, hard coral cover, and covariates.

``` r
max_dhw <- max_dhw %>%
  select(project, country, site, sample_date, hard_coral_cover, covariates)

max_dhw
```

    ## # A tibble: 29 × 6
    ##    project                 country site  sample_date hard_coral_cover covariates
    ##    <chr>                   <chr>   <chr> <date>                 <dbl> <list>    
    ##  1 SLCRI Bar Reef Seascape Sri La… Bar … 2025-03-16              12.5 <tibble>  
    ##  2 SLCRI Bar Reef Seascape Sri La… Bar … 2025-03-16               0   <tibble>  
    ##  3 SLCRI Bar Reef Seascape Sri La… Hipo… 2025-03-20               2.5 <tibble>  
    ##  4 SLCRI Bar Reef Seascape Sri La… Kand… 2025-03-19              24   <tibble>  
    ##  5 SLCRI Bar Reef Seascape Sri La… Kuda… 2025-03-17               4   <tibble>  
    ##  6 SLCRI Bar Reef Seascape Sri La… Kuda… 2025-03-17               0   <tibble>  
    ##  7 SLCRI Bar Reef Seascape Sri La… Sea … 2024-03-19              10   <tibble>  
    ##  8 SLCRI Bar Reef Seascape Sri La… Sea … 2025-03-19              16   <tibble>  
    ##  9 SLCRI Bar Reef Seascape Sri La… St. … 2025-03-20              13.5 <tibble>  
    ## 10 SLCRI Bar Reef Seascape Sri La… Thal… 2025-03-15              11.5 <tibble>  
    ## # ℹ 19 more rows

## Expand covariates

The covariates are currently in a format that need to be expanded. Once
they are, you can see they contain start and end date of the data used
for the covariates, the band, and the summarised value.

``` r
max_dhw <- max_dhw %>%
  unnest(covariates)

max_dhw %>%
  glimpse()
```

    ## Rows: 29
    ## Columns: 9
    ## $ project               <chr> "SLCRI Bar Reef Seascape", "SLCRI Bar Reef Seasc…
    ## $ country               <chr> "Sri Lanka", "Sri Lanka", "Sri Lanka", "Sri Lank…
    ## $ site                  <chr> "Bar Reef Deep", "Bar Reef Shallow", "Hipolis Re…
    ## $ sample_date           <date> 2025-03-16, 2025-03-16, 2025-03-20, 2025-03-19,…
    ## $ hard_coral_cover      <dbl> 12.50, 0.00, 2.50, 24.00, 4.00, 0.00, 10.00, 16.…
    ## $ covariates_start_date <date> 2024-04-01, 2024-04-01, 2024-04-01, 2024-04-01,…
    ## $ covariates_end_date   <date> 2025-03-01, 2025-03-01, 2025-03-01, 2025-03-01,…
    ## $ band                  <chr> "band_1", "band_1", "band_1", "band_1", "band_1"…
    ## $ max                   <dbl> 8.02, 8.01, 8.05, 8.00, 7.71, 7.71, 0.65, 7.80, …

# Visualize

Finally, visualize coral cover against the maximum mean DHW for the past
365 days.

``` r
ggplot(
  max_dhw,
  aes(
    x = max,
    y = hard_coral_cover
  )
) +
  geom_point(color = "darkblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(
    title = paste("Coral Cover vs. Max. DHW (Previous 365 Days)"),
    x = "Maximum DHW",
    y = "Hard Coral Cover (%)"
  ) +
  theme_minimal()
```

![](analysis_files/figure-gfm/plot-1.png)<!-- -->
