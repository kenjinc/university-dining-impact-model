Data Cleaning and Aggregation
================

## Required Packages

``` r
library(tidyverse)
library(RColorBrewer)
```

## Loading in the Map Data

Before we can synthesize the relevant variables from our two identified
data sources into a single matrix, we will first need to perform a
spatial join.

To prepare for this step, we will have to read in the shapefile data
local to the `maps` package within `tidyverse.` In particular, are going
to specify `map_data("world")`, which will establish nation-states as
our primary unit of analysis.

``` r
map_data <- map_data("world") 
map_data %>%
  head(5)
```

    ##        long      lat group order region subregion
    ## 1 -69.89912 12.45200     1     1  Aruba      <NA>
    ## 2 -69.89571 12.42300     1     2  Aruba      <NA>
    ## 3 -69.94219 12.43853     1     3  Aruba      <NA>
    ## 4 -70.00415 12.50049     1     4  Aruba      <NA>
    ## 5 -70.06612 12.54697     1     5  Aruba      <NA>

Because the directory of nation-states used in the `maps` package does
not align with the conventions laid out by the ISO, we will need to
perform several updates manually in order to align our shapefile data
with the data provided in our two parent datasaets.

Using the instructions laid out by Thomas Haslam in his 2021 [*RPubs*
entry](https://rpubs.com/Thom_JH/798825), we will begin by addressing
what he refers to as the “easy cases.”

Of the 21 total instances he identified, the “easy cases” refer to the
13 more simple straightforward mismatches, whereby the names of the
`regions` listed in the `maps` dataset and the `countries` listed in the
`gapminder` dataset were inconsistent.

``` r
map_data_iso <- map_data %>% 
  rename(country=region) %>%
  mutate(country=case_when(country=="Macedonia"~"North Macedonia",
                           country=="Ivory Coast"~"Cote d'Ivoire",
                           country=="Democratic Republic of the Congo"~"Congo, Dem. Rep.",
                           country=="Republic of Congo"~"Congo, Rep.",
                           country=="UK"~"United Kingdom",
                           country=="USA"~"United States",
                           country=="Laos"~"Lao",
                           country=="Slovakia"~"Slovak Republic",
                           country=="Saint Lucia"~"St. Lucia",
                           country=="Kyrgyzstan"~"Krygyz Republic",
                           country=="Micronesia"~"Micronesia, Fed. Sts.",
                           country=="Swaziland"~"Eswatini",
                           country=="Virgin Islands"~"Virgin Islands (U.S.)",
                        TRUE~country))
```

The second group of cases, which he refers to as the `island nations`
all involve combining the relevant `region` designations in the `maps`
dataset to the appropriate `country` designation in `gapminder`.

``` r
island_nations <- c("Antigua","Barbuda","Nevis", 
                 "Saint Kitts","Trinidad",
                 "Tobago","Grenadines","Saint Vincent")
island_nations_match <- map_data_iso %>% 
  filter(country %in% island_nations)
island_nations_match %>% distinct(country)
```

    ##         country
    ## 1       Antigua
    ## 2       Barbuda
    ## 3         Nevis
    ## 4   Saint Kitts
    ## 5      Trinidad
    ## 6        Tobago
    ## 7    Grenadines
    ## 8 Saint Vincent

``` r
ant_bar <- c(137,138 )
kit_nev <- c(930,931)
tri_tog <- c(1425,1426)
vin_gre <- c(1575,1576,1577)
island_nation_names <- c("Antigua and Barbuda","St. Kitts and Nevis","Trinidad and Tobago","St. Vincent and the Grenadines")
island_nations_match <- island_nations_match %>% 
  mutate(country=case_when(group %in% ant_bar~"Antigua and Barbuda",
                           group %in% kit_nev~"St. Kitts and Nevis",
                           group %in% tri_tog~"Trinidad and Tobago",
                           group %in% vin_gre~"St. Vincent and the Grenadines")) %>% 
  tibble()
```

``` r
island_nations_match %>%
  distinct(country) %>%
  knitr::kable()
```

| country                        |
|:-------------------------------|
| Antigua and Barbuda            |
| St. Kitts and Nevis            |
| Trinidad and Tobago            |
| St. Vincent and the Grenadines |

``` r
map_data_iso <- map_data_iso %>%
  filter(!country %in% island_nation_names)
map_data_iso <- map_data_iso %>% 
  bind_rows(island_nations_match) %>%
  arrange(country) %>%
  tibble()
```

Now, we move onto what Haslam refers to as the cases of “subregion
promotion”. Here, we take Macao and Hong Kong, both of which are Special
Administrative Regions of China and designated at subregions in the
`maps` dataset, and assign them as countries, as is convention when
conducting global studies research.

``` r
sra_names <- c("Hong Kong","Macao")
hk_mc <- map_data_iso %>% 
  filter(subregion %in% sra_names)
hk_mc <- hk_mc %>%
  mutate(country = case_when(subregion=="Hong Kong"~"Hong Kong, China",
                             subregion=="Macao"~"Macao, China"))
```

``` r
map_data_iso <- map_data_iso %>%
  filter(!subregion %in% sra_names)
map_data_iso <- map_data_iso %>% 
  bind_rows(hk_mc) %>%
  select(-subregion) %>% 
  tibble()
```

With these steps complete, the designated countries within our shapefile
data are now as follows:

``` r
map_data_iso %>% distinct(country)
```

    ## # A tibble: 258 × 1
    ##    country            
    ##    <chr>              
    ##  1 Afghanistan        
    ##  2 Albania            
    ##  3 Algeria            
    ##  4 American Samoa     
    ##  5 Andorra            
    ##  6 Angola             
    ##  7 Anguilla           
    ##  8 Antarctica         
    ##  9 Antigua            
    ## 10 Antigua and Barbuda
    ## # … with 248 more rows

## Spatially Joining Our Dietary Footprint Data

To join the relevant columns from our dietary footprint data to our
shapefile data, we will need to first read in the corresponding data
file from the `parent-datasets` folder of this project’s repository.

``` r
dietary_footprint_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/dietary_footprint_data.csv") %>%
  head(5)
```
