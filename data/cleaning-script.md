Data Cleaning and Aggregation
================

### Required Packages

``` r
library(tidyverse)
library(RColorBrewer)
```

### Loading in the Map Data

Before we can synthesize the relevant variables from the two parent
datasets into a single matrix (i.e., one that will service both our
analytical and visualtion needs), we will first need to perform a
spatial join.

To prepare for this step, we will begin by reading in the shapefile data
local to the `maps` package within `tidyverse.` In particular, we will
need to specify `map_data("world")`, which will allow us to establish
nation-states as our primary unit of analysis.

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

Because the directory of nation-states native to the `maps` package does
not use ISO country codes, we will need to perform several updates
manually in order to more closely align our shapefile data with the data
provided in our two parent datasaets. Using the steps laid out by Thomas
Haslam in his [2021 *RPubs* entry](https://rpubs.com/Thom_JH/798825), we
can create greater consistency in the countries that are included and
(2) the naming conventions that are used across our three data sources.

### Loading in the Original Datasets

First, we will read in the two original datasets from the
`parent-datasets` folder local to this project repository.

``` r
read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/dietary_footprint_data.csv") %>%
  head(5)
```

    ##   country_code country      diet                 attribute centile_up
    ## 1            1 Armenia 2/3_vegan      kg_co2_luc_feed_palm     0.0000
    ## 2            1 Armenia 2/3_vegan       kg_co2_luc_feed_soy     0.0000
    ## 3            1 Armenia 2/3_vegan kg_co2_luc_human_palm_soy     0.0000
    ## 4            1 Armenia 2/3_vegan        kg_co2_luc_pasture     0.0000
    ## 5            1 Armenia 2/3_vegan          kg_co2e_excl_luc   289.7267
    ##         value centile_down value_baseline diff_baseline X._diff_baseline
    ## 1   0.3180732         0.00      0.9548163 -6.367431e-01    -0.6668749579
    ## 2  12.2508407         0.00     36.7755021 -2.452466e+01    -0.6668749579
    ## 3   0.2436078         0.00      0.2437601 -1.523193e-04    -0.0006248737
    ## 4   1.1439146         0.00      3.4338897 -2.289975e+00    -0.6668749579
    ## 5 784.8360925       136.86   1604.2125238 -8.193764e+02    -0.5107655121
    ##   value_baseline_adj diff_baseline_adj X._diff_baseline_adj
    ## 1          0.9542196        -0.6361464           -0.6666667
    ## 2         36.7525221       -24.5016814           -0.6666667
    ## 3          0.2436078         0.0000000            0.0000000
    ## 4          3.4317439        -2.2878293           -0.6666667
    ## 5       1603.2100936      -818.3740011           -0.5104596
