Getting environmental covariates for GFCR locations
================

# Load packages

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

Aggregated data for GFCR projects

``` r
summary_sampleevents <- mermaid_get_summary_sampleevents()

gfcr_summary_sampleevents <- summary_sampleevents %>%
  filter(str_detect(tags, "GFCR"))
```

## Summary of hard coral cover for projects

``` r
gfcr_summary_sampleevents <- gfcr_summary_sampleevents %>%
  filter(!is.na(`benthicpit_percent_cover_benthic_category_avg_Hard coral`)) %>%
  rename(hard_coral_cover = `benthicpit_percent_cover_benthic_category_avg_Hard coral`)

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

## List STAC collections

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

``` r
max_dhw <- gfcr_summary_sampleevents %>%
  summary_zonal_stats("noaa-monthly-max-dhw", n_days = 365, buffer = 100, stats = "max")
```

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
