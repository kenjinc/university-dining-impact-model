Data Cleaning and Aggregation
================

## Required Packages

``` r
library(tidyverse)
library(RColorBrewer)
```

## Loading in Our Shapefile Data

Before we can synthesize the relevant variables from our two identified
data sources into a single matrix, we will need to first read in the
nation-state shapefile data local to the `maps` package within
`tidyverse`.

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

Because the directory of nation-states used in this package does not
adhere to the conventions laid out by the International Organization for
Standardization (ISO), we will need to make a series of manual changes
to better align our shapefile data with our university-enrollment and
dietary-footprint data.

Using the instructions laid out by Thomas Haslam in his 2021 [*RPubs*
entry](https://rpubs.com/Thom_JH/798825), we can begin by addressing
what he refers to as the “easy cases.”

In the mentioned [*RPubs* entry](https://rpubs.com/Thom_JH/798825)
entry, Haslam identifies a total of 21 differences between the
nation-states listed in the `maps` package and countries listed by
[Gapminder](gapminder.org).

The “easy cases,” then, refer to the 13 instances where the names of the
`regions` listed in the `maps` package and the countries listed by
Gapminder are incongruent.

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

The second group of instances, which he refers to as the “island
nations”, refer to the 8 cases where discrete island `region` need to be
aggregated to match the related `country` designations provided by
[Gapminder](gapminder.org) and the ISO.

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

Now, we move onto the final 2 cases that we will be addressing. Haslam
refers to these as the instances of “subregion promotion”. Here, we take
Macao and Hong Kong, Special Administrative Regions of China designated
as subregions in the `maps` package, and assign them as countries, as is
convention for global studies research.

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

With these each of these steps complete, the regions accounted for
within our shapefile data are now as follows:

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

## Loading in Our Dietary Footprint Data

Before we can spatially join the relevant columns from our dietary
footprint data with our updated shapefile data, we will need to first
read in the corresponding file from the `parent-datasets` folder in our
repository.

``` r
dietary_footprint_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/dietary_footprint_data.csv")
```

Because this dataset is not currently formatted in a way that aligns
with the demands of our analysis, we will need to make several changes
in preparation for our spatial join.

For each country listed, we need to extract the values corresponding to
the `value`, `centile_up`, and `centile_down` columns for each of the
following attributes:`kg_co2e_excl_luc`, `kg_co2e_total`,
`l_blue_wf_total`, `l_green_wf`, and `l_blue_green_wf.` This needs to be
repeated for each of the 9 dietary conditions of interest: `2/3_vegan`,
`baseline`, `lacto_ovo_vegetarian`, `low_red_meat`, `meatless_day`,
`no_dairy`, `no_red_meat`, and `vegan`.

In addition, we will also need to perform the necessary checks to ensure
that country names are consistent across the map and dietary footprint
data.

## Spatially Joining Our Dietary Footprint Data

## Loading in Our University Enrollment Data

## Spatially Joining Our University Enrollment Data

## Writing the Final Data File
