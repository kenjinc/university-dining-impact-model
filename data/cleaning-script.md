Data Cleaning and Aggregation
================

## Required Packages

``` r
library(tidyverse)
```

## Cleaning Our Shapefile Data

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
  distinct(country) 
```

    ## # A tibble: 4 × 1
    ##   country                       
    ##   <chr>                         
    ## 1 Antigua and Barbuda           
    ## 2 St. Kitts and Nevis           
    ## 3 Trinidad and Tobago           
    ## 4 St. Vincent and the Grenadines

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

## Cleaning Our Dietary Footprint Data

Before we can spatially join the relevant columns from our dietary
footprint data with our updated shapefile data, we will need to first
read in the corresponding file from the `parent-datasets` folder in our
repository.

``` r
dietary_footprint_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/dietary_footprint_data.csv")
dietary_footprint_data %>%
  head(6)
```

    ##   country_code country      diet                 attribute centile_up
    ## 1            1 Armenia 2/3_vegan      kg_co2_luc_feed_palm     0.0000
    ## 2            1 Armenia 2/3_vegan       kg_co2_luc_feed_soy     0.0000
    ## 3            1 Armenia 2/3_vegan kg_co2_luc_human_palm_soy     0.0000
    ## 4            1 Armenia 2/3_vegan        kg_co2_luc_pasture     0.0000
    ## 5            1 Armenia 2/3_vegan          kg_co2e_excl_luc   289.7267
    ## 6            1 Armenia 2/3_vegan             kg_co2e_total   289.7267
    ##         value centile_down value_baseline diff_baseline X._diff_baseline
    ## 1   0.3180732         0.00      0.9548163 -6.367431e-01    -0.6668749579
    ## 2  12.2508407         0.00     36.7755021 -2.452466e+01    -0.6668749579
    ## 3   0.2436078         0.00      0.2437601 -1.523193e-04    -0.0006248737
    ## 4   1.1439146         0.00      3.4338897 -2.289975e+00    -0.6668749579
    ## 5 784.8360925       136.86   1604.2125238 -8.193764e+02    -0.5107655121
    ## 6 798.7925288       136.86   1645.6204919 -8.468280e+02    -0.5145949308
    ##   value_baseline_adj diff_baseline_adj X._diff_baseline_adj
    ## 1          0.9542196        -0.6361464           -0.6666667
    ## 2         36.7525221       -24.5016814           -0.6666667
    ## 3          0.2436078         0.0000000            0.0000000
    ## 4          3.4317439        -2.2878293           -0.6666667
    ## 5       1603.2100936      -818.3740011           -0.5104596
    ## 6       1644.5921870      -845.7996582           -0.5142914

Because this dataset is not currently formatted in a way that aligns
with the demands of our analysis, we will need to make several changes
in preparation for our spatial join.

For each country listed, we need to wrangle the baseline data such that
there is a column corresponding to the quantities listed in the `value`,
`centile_up`, and `centile_down` for each of the following
attributes:`kg_co2e_excl_luc`, `kg_co2e_total`, `l_blue_wf_total`,
`l_green_wf`, and `l_blue_green_wf`. In addition, this process needs to
be repeated for each of the 8 remaining dietary conditions of interest:
`2/3_vegan`, `baseline`, `lacto_ovo_vegetarian`, `low_red_meat`,
`meatless_day`, `no_dairy`, `no_red_meat`, and `vegan`.

To adjust for these requirements, we will first select out the relevant
variables.

``` r
dietary_footprint_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/dietary_footprint_data.csv") %>%
  select(country,diet,attribute,value)
dietary_footprint_data %>%
  head(6)
```

    ##   country      diet                 attribute       value
    ## 1 Armenia 2/3_vegan      kg_co2_luc_feed_palm   0.3180732
    ## 2 Armenia 2/3_vegan       kg_co2_luc_feed_soy  12.2508407
    ## 3 Armenia 2/3_vegan kg_co2_luc_human_palm_soy   0.2436078
    ## 4 Armenia 2/3_vegan        kg_co2_luc_pasture   1.1439146
    ## 5 Armenia 2/3_vegan          kg_co2e_excl_luc 784.8360925
    ## 6 Armenia 2/3_vegan             kg_co2e_total 798.7925288

With this accomplished, we will continue the wrangling process by
pivoting to a wider format. We will achieve this by assigning the
different dietary scenarios and attributes designated under the `diet`
and `attribute` columns, respectively, as variables rather than
observations.

``` r
dietary_footprint_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/dietary_footprint_data.csv") %>%
  select(country,diet,attribute,value) %>%
  pivot_wider(names_from=c(diet,attribute),
              values_from="value")
dietary_footprint_data %>%
  head(6)
```

    ## # A tibble: 6 × 139
    ##   country        2/3_v…¹ 2/3_v…² 2/3_v…³ 2/3_v…⁴ 2/3_v…⁵ 2/3_v…⁶ 2/3_v…⁷ 2/3_v…⁸
    ##   <chr>            <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Armenia         0.318    12.3  2.44e-1  1.14      785.    799.  1.07e6    6.71
    ## 2 Afghanistan     0.435     1.99 0        0.0882    754.    756.  1.18e6    0   
    ## 3 Albania         0.0213   12.8  8.66e-4  4.94      815.    833.  9.45e5  110.  
    ## 4 Algeria         0.254    38.7  2.76e+1  2.39      528.    597.  9.12e5   66.1 
    ## 5 Antigua and B…  0.195    16.9  1.17e+1 15.7       693.    737.  1.13e6 3124.  
    ## 6 Argentina       0.293   125.   1.88e+2  0.0257    957.   1269.  6.75e5   73.3 
    ## # … with 130 more variables: `2/3_vegan_l_blue_wf_excl_pond` <dbl>,
    ## #   `2/3_vegan_l_blue_wf_freshwater_pond` <dbl>,
    ## #   `2/3_vegan_l_blue_wf_total` <dbl>, `2/3_vegan_l_green_wf` <dbl>,
    ## #   baseline_kg_co2_luc_feed_palm <dbl>, baseline_kg_co2_luc_feed_soy <dbl>,
    ## #   baseline_kg_co2_luc_human_palm_soy <dbl>,
    ## #   baseline_kg_co2_luc_pasture <dbl>, baseline_kg_co2e_excl_luc <dbl>,
    ## #   baseline_kg_co2e_total <dbl>, baseline_l_blue_green_wf <dbl>, …

Finally, with these new variables generated we will, again, select out
the combinations of dietary scenarios and attributes relevant to our
analyses.

``` r
dietary_footprint_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/dietary_footprint_data.csv") %>%
  select(country,diet,attribute,value) %>%
  pivot_wider(names_from=c(diet,attribute),
              values_from="value") %>%
  select(country,
  "baseline_kg_co2e_excl_luc",
  "baseline_kg_co2e_total",
  "baseline_l_blue_green_wf",
  "baseline_l_blue_wf_total",
  "baseline_l_green_wf",
  "meatless_day_kg_co2e_excl_luc",
  "meatless_day_kg_co2e_total",
  "meatless_day_l_blue_green_wf",
  "meatless_day_l_blue_wf_total",
  "meatless_day_l_green_wf",
  "no_dairy_kg_co2e_excl_luc",
  "no_dairy_kg_co2e_total",
  "no_dairy_l_blue_green_wf",
  "no_dairy_l_blue_wf_total",
  "no_dairy_l_green_wf",
  "low_red_meat_kg_co2e_excl_luc",
  "low_red_meat_kg_co2e_total",
  "low_red_meat_l_blue_green_wf",
  "low_red_meat_l_blue_wf_total",
  "low_red_meat_l_green_wf",
  "no_red_meat_kg_co2e_excl_luc",
  "no_red_meat_kg_co2e_total",
  "no_red_meat_l_blue_green_wf",
  "no_red_meat_l_blue_wf_total",
  "no_red_meat_l_green_wf",
  "pescetarian_kg_co2e_excl_luc",
  "pescetarian_kg_co2e_total",
  "pescetarian_l_blue_green_wf",
  "pescetarian_l_blue_wf_total",
  "pescetarian_l_green_wf",
  "lacto_ovo_vegetarian_kg_co2e_excl_luc",
  "lacto_ovo_vegetarian_kg_co2e_total",
  "lacto_ovo_vegetarian_l_blue_green_wf",
  "lacto_ovo_vegetarian_l_blue_wf_total",
  "lacto_ovo_vegetarian_l_green_wf",
  "2/3_vegan_kg_co2e_excl_luc",
  "2/3_vegan_kg_co2e_total",
  "2/3_vegan_l_blue_green_wf",
  "2/3_vegan_l_blue_wf_total",
  "2/3_vegan_l_green_wf",
  "vegan_kg_co2e_excl_luc",
  "vegan_kg_co2e_total",
  "vegan_l_blue_green_wf",
  "vegan_l_blue_wf_total",
  "vegan_l_green_wf")
dietary_footprint_data %>%
  head(6)
```

    ## # A tibble: 6 × 46
    ##   country        basel…¹ basel…² basel…³ basel…⁴ basel…⁵ meatl…⁶ meatl…⁷ meatl…⁸
    ##   <chr>            <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Armenia          1604.   1646.  1.44e6 191032.  1.25e6   1558.   1595.  1.39e6
    ## 2 Afghanistan       895.    898.  1.02e6 347777.  6.68e5   1810.   1817.  1.30e6
    ## 3 Albania          1952.   2013.  1.44e6 214147.  1.23e6   1651.   1699.  1.22e6
    ## 4 Algeria           983.   1108.  1.30e6 186077.  1.11e6   1066.   1213.  1.17e6
    ## 5 Antigua and B…   1310.   1400.  1.19e6  77859.  1.11e6   1642.   1740.  1.44e6
    ## 6 Argentina        2952.   3517.  1.02e6  62016.  9.54e5   2125.   2557.  7.88e5
    ## # … with 37 more variables: meatless_day_l_blue_wf_total <dbl>,
    ## #   meatless_day_l_green_wf <dbl>, no_dairy_kg_co2e_excl_luc <dbl>,
    ## #   no_dairy_kg_co2e_total <dbl>, no_dairy_l_blue_green_wf <dbl>,
    ## #   no_dairy_l_blue_wf_total <dbl>, no_dairy_l_green_wf <dbl>,
    ## #   low_red_meat_kg_co2e_excl_luc <dbl>, low_red_meat_kg_co2e_total <dbl>,
    ## #   low_red_meat_l_blue_green_wf <dbl>, low_red_meat_l_blue_wf_total <dbl>,
    ## #   low_red_meat_l_green_wf <dbl>, no_red_meat_kg_co2e_excl_luc <dbl>, …

Finally, we will need to perform the necessary checks to ensure that
country names are consistent across the data sources used for our map
and dietary footprint variables.

We will also eventually need to use the `mutate` function to generate
averages across scenarios.

## Cleaning Our University Enrollment Data

As we did for our dietary footprint data, we will need to read in and
wrangle the university enrollment data found in the `parent-datasets`
folder of our repository.

``` r
university_enrollment_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/university_enrollment_data.csv") %>%
  as_tibble()
university_enrollment_data %>%
  head(6)
```

    ## # A tibble: 6 × 25
    ##   Country.Name Countr…¹ Series Serie…² YR2000 YR2001 YR2002 YR2003 YR2004 YR2005
    ##   <chr>        <chr>    <chr>  <chr>   <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
    ## 1 Afghanistan  AFG      Popul… SP.POP… 20779… 21606… 22600… 23680… 24726… 25654…
    ## 2 Afghanistan  AFG      Enrol… UIS.E.6 ..     ..     ..     ..     ..     ..    
    ## 3 Afghanistan  AFG      Enrol… UIS.E.7 ..     ..     ..     ..     ..     ..    
    ## 4 Afghanistan  AFG      Enrol… UIS.E.8 ..     ..     ..     ..     ..     ..    
    ## 5 Albania      ALB      Popul… SP.POP… 30890… 30601… 30510… 30396… 30269… 30114…
    ## 6 Albania      ALB      Enrol… UIS.E.6 ..     ..     ..     ..     ..     ..    
    ## # … with 15 more variables: YR2006 <chr>, YR2007 <chr>, YR2008 <chr>,
    ## #   YR2009 <chr>, YR2010 <chr>, YR2011 <chr>, YR2012 <chr>, YR2013 <chr>,
    ## #   YR2014 <chr>, YR2015 <chr>, YR2016 <chr>, YR2017 <chr>, YR2018 <chr>,
    ## #   YR2019 <chr>, YR2020 <chr>, and abbreviated variable names ¹​Country.Code,
    ## #   ²​Series.Code

Before we can spatially join the relevant variables from this data
source, we will need to make it so that the data associated with each
included country is captured in a single row, and each subsequent column
captures the following: (1) the total number of students enrolled in
ISCED 6 programs, (2) the total number of students enrolled in ISCED 7
programs, (3) the total number of students enrolled in ISCED 8 programs,
(4) the total number of people living within the country, and (5) the
reference year being used for each of these variables.

We will begin this process by selecting out the `Country.Code` and
`Series.Code` columns, slicing out the 5 bottommost rows, and renaming
the `Country.Name`, `Series`, and reference year variables.

``` r
university_enrollment_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/university_enrollment_data.csv") %>%
  select(-Country.Code,-Series.Code) %>%
  as_tibble(university_enrollment_data) %>%
  slice(1:(n()-5)) %>%
  rename(country=Country.Name,series=Series,yr2000=YR2000,yr2001=YR2001,yr2002=YR2002,yr2003=YR2003,yr2004=YR2004,yr2005=YR2005,yr2006=YR2006,yr2007=YR2007,yr2008=YR2008,yr2009=YR2009,yr2010=YR2010,yr2011=YR2011,yr2012=YR2012,yr2013=YR2013,yr2014=YR2014,yr2015=YR2015,yr2016=YR2016,yr2017=YR2017,yr2018=YR2018,yr2019=YR2019,yr2020=YR2020) 
university_enrollment_data %>%
  tail(6)
```

    ## # A tibble: 6 × 23
    ##   country  series yr2000 yr2001 yr2002 yr2003 yr2004 yr2005 yr2006 yr2007 yr2008
    ##   <chr>    <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
    ## 1 Zambia   Enrol… ..     ..     ..     ..     ..     ..     ..     ..     ..    
    ## 2 Zambia   Enrol… ..     ..     ..     ..     ..     ..     ..     ..     ..    
    ## 3 Zimbabwe Popul… 11881… 11923… 11954… 11982… 12019… 12076… 12155… 12255… 12379…
    ## 4 Zimbabwe Enrol… ..     ..     ..     ..     ..     ..     ..     ..     ..    
    ## 5 Zimbabwe Enrol… ..     ..     ..     ..     ..     ..     ..     ..     ..    
    ## 6 Zimbabwe Enrol… ..     ..     ..     ..     ..     ..     ..     ..     ..    
    ## # … with 12 more variables: yr2009 <chr>, yr2010 <chr>, yr2011 <chr>,
    ## #   yr2012 <chr>, yr2013 <chr>, yr2014 <chr>, yr2015 <chr>, yr2016 <chr>,
    ## #   yr2017 <chr>, yr2018 <chr>, yr2019 <chr>, yr2020 <chr>

With this complete, we now need to convert the reference year columns
from character strings (`chr`) to double strings (`dbl`), which will
coincidently convert the default `--` entries to `NA` values.

``` r
university_enrollment_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/university_enrollment_data.csv") %>%
  select(-Country.Code,-Series.Code) %>%
  as_tibble(university_enrollment_data) %>%
  slice(1:(n()-5)) %>%
  rename(country=Country.Name,series=Series,yr2000=YR2000,yr2001=YR2001,yr2002=YR2002,yr2003=YR2003,yr2004=YR2004,yr2005=YR2005,yr2006=YR2006,yr2007=YR2007,yr2008=YR2008,yr2009=YR2009,yr2010=YR2010,yr2011=YR2011,yr2012=YR2012,yr2013=YR2013,yr2014=YR2014,yr2015=YR2015,yr2016=YR2016,yr2017=YR2017,yr2018=YR2018,yr2019=YR2019,yr2020=YR2020) %>%
  mutate_at(c("yr2000","yr2001","yr2002","yr2003","yr2004","yr2005","yr2006","yr2007","yr2008","yr2009","yr2010","yr2011","yr2012","yr2013","yr2014","yr2015","yr2016","yr2017","yr2018","yr2019","yr2020"),as.double)
university_enrollment_data %>%
  head(6)
```

    ## # A tibble: 6 × 23
    ##   country series  yr2000  yr2001  yr2002  yr2003  yr2004  yr2005  yr2006  yr2007
    ##   <chr>   <chr>    <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Afghan… Popul…  2.08e7  2.16e7  2.26e7  2.37e7  2.47e7  2.57e7  2.64e7  2.71e7
    ## 2 Afghan… Enrol… NA      NA      NA      NA      NA      NA      NA      NA     
    ## 3 Afghan… Enrol… NA      NA      NA      NA      NA      NA      NA      NA     
    ## 4 Afghan… Enrol… NA      NA      NA      NA      NA      NA      NA      NA     
    ## 5 Albania Popul…  3.09e6  3.06e6  3.05e6  3.04e6  3.03e6  3.01e6  2.99e6  2.97e6
    ## 6 Albania Enrol… NA      NA      NA      NA      NA      NA      NA      NA     
    ## # … with 13 more variables: yr2008 <dbl>, yr2009 <dbl>, yr2010 <dbl>,
    ## #   yr2011 <dbl>, yr2012 <dbl>, yr2013 <dbl>, yr2014 <dbl>, yr2015 <dbl>,
    ## #   yr2016 <dbl>, yr2017 <dbl>, yr2018 <dbl>, yr2019 <dbl>, yr2020 <dbl>

We will now turn all of the NA variables in the reference year columns
from `NA` values to zero in order to more easily set up our conditional
rules for pulling the most recent year with recorded data.

``` r
university_enrollment_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/university_enrollment_data.csv") %>%
  select(-Country.Code,-Series.Code) %>%
  as_tibble(university_enrollment_data) %>%
  slice(1:(n()-5)) %>%
  rename(country=Country.Name,series=Series,yr2000=YR2000,yr2001=YR2001,yr2002=YR2002,yr2003=YR2003,yr2004=YR2004,yr2005=YR2005,yr2006=YR2006,yr2007=YR2007,yr2008=YR2008,yr2009=YR2009,yr2010=YR2010,yr2011=YR2011,yr2012=YR2012,yr2013=YR2013,yr2014=YR2014,yr2015=YR2015,yr2016=YR2016,yr2017=YR2017,yr2018=YR2018,yr2019=YR2019,yr2020=YR2020) %>%
  mutate_at(c("yr2000","yr2001","yr2002","yr2003","yr2004","yr2005","yr2006","yr2007","yr2008","yr2009","yr2010","yr2011","yr2012","yr2013","yr2014","yr2015","yr2016","yr2017","yr2018","yr2019","yr2020"),as.double) %>%
  replace(is.na(.),0)
university_enrollment_data %>%
  head(6)
```

    ## # A tibble: 6 × 23
    ##   country  series yr2000 yr2001 yr2002 yr2003 yr2004 yr2005 yr2006 yr2007 yr2008
    ##   <chr>    <chr>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 Afghani… Popul… 2.08e7 2.16e7 2.26e7 2.37e7 2.47e7 2.57e7 2.64e7 2.71e7 2.77e7
    ## 2 Afghani… Enrol… 0      0      0      0      0      0      0      0      0     
    ## 3 Afghani… Enrol… 0      0      0      0      0      0      0      0      0     
    ## 4 Afghani… Enrol… 0      0      0      0      0      0      0      0      0     
    ## 5 Albania  Popul… 3.09e6 3.06e6 3.05e6 3.04e6 3.03e6 3.01e6 2.99e6 2.97e6 2.95e6
    ## 6 Albania  Enrol… 0      0      0      0      0      0      0      0      0     
    ## # … with 12 more variables: yr2009 <dbl>, yr2010 <dbl>, yr2011 <dbl>,
    ## #   yr2012 <dbl>, yr2013 <dbl>, yr2014 <dbl>, yr2015 <dbl>, yr2016 <dbl>,
    ## #   yr2017 <dbl>, yr2018 <dbl>, yr2019 <dbl>, yr2020 <dbl>

Now, we will pivot our dataframe so that the different variables of
interest currently captured in the `series` column are instead
represented as their own variables. Because the new column names will be
a function of how our four observations are named, we will also add the
necessary code to shorten these descriptions.

``` r
university_enrollment_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/university_enrollment_data.csv") %>%
  select(-Country.Code,-Series.Code) %>%
  as_tibble(university_enrollment_data) %>%
  slice(1:(n()-5)) %>%
  rename(country=Country.Name,series=Series,yr2000=YR2000,yr2001=YR2001,yr2002=YR2002,yr2003=YR2003,yr2004=YR2004,yr2005=YR2005,yr2006=YR2006,yr2007=YR2007,yr2008=YR2008,yr2009=YR2009,yr2010=YR2010,yr2011=YR2011,yr2012=YR2012,yr2013=YR2013,yr2014=YR2014,yr2015=YR2015,yr2016=YR2016,yr2017=YR2017,yr2018=YR2018,yr2019=YR2019,yr2020=YR2020) %>%
  mutate_at(c("yr2000","yr2001","yr2002","yr2003","yr2004","yr2005","yr2006","yr2007","yr2008","yr2009","yr2010","yr2011","yr2012","yr2013","yr2014","yr2015","yr2016","yr2017","yr2018","yr2019","yr2020"),as.double) %>%
  replace(is.na(.),0) %>%
  distinct() %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 6 programmes, both sexes (number)"),"isced6_enr")) %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 7 programmes, both sexes (number)"),"isced7_enr")) %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 8 programmes, both sexes (number)"),"isced8_enr")) %>%
  mutate(across("series",str_replace, fixed("Population, total"),"nat_pop")) %>%
  pivot_wider(names_from="series",
              values_from=c("yr2020","yr2019","yr2018","yr2017","yr2016","yr2015","yr2014","yr2013","yr2012","yr2011","yr2010","yr2009","yr2008","yr2007","yr2006","yr2005","yr2004","yr2003","yr2002","yr2001","yr2000"))
university_enrollment_data %>%
  head(6)
```

    ## # A tibble: 6 × 85
    ##   country        yr202…¹ yr202…² yr202…³ yr202…⁴ yr201…⁵ yr201…⁶ yr201…⁷ yr201…⁸
    ##   <chr>            <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Afghanistan          0       0       0       0  3.80e7      0        0     0  
    ## 2 Albania              0       0       0       0  2.87e6  89231    43749  1865  
    ## 3 Algeria              0       0       0       0  4.31e7      0        0     0  
    ## 4 American Samoa       0       0       0       0  5.5 e4      0        0     0  
    ## 5 Andorra              0       0       0       0  7.7 e4    554.      35    23.5
    ## 6 Angola               0       0       0       0  3.18e7      0        0     0  
    ## # … with 76 more variables: yr2018_nat_pop <dbl>, yr2018_isced6_enr <dbl>,
    ## #   yr2018_isced7_enr <dbl>, yr2018_isced8_enr <dbl>, yr2017_nat_pop <dbl>,
    ## #   yr2017_isced6_enr <dbl>, yr2017_isced7_enr <dbl>, yr2017_isced8_enr <dbl>,
    ## #   yr2016_nat_pop <dbl>, yr2016_isced6_enr <dbl>, yr2016_isced7_enr <dbl>,
    ## #   yr2016_isced8_enr <dbl>, yr2015_nat_pop <dbl>, yr2015_isced6_enr <dbl>,
    ## #   yr2015_isced7_enr <dbl>, yr2015_isced8_enr <dbl>, yr2014_nat_pop <dbl>,
    ## #   yr2014_isced6_enr <dbl>, yr2014_isced7_enr <dbl>, …

We are now left with a format that is closer to our desired
specifications, where the data associated with each included country
corresponds to one row, with concatenated column titles representing a
population total and the reference year being used.

For each country, we want to use the most recent year that there is
recorded data for both our national population variable (which is
collected more consistently) and each of our stratified ISCED enrollment
population variables (which are collected less consistently).

The challenge now is to find a way to conditionally mutate five new
columns: (1) one that captures the number of people living with the
country, (2) one that captures the number of people in that country
enrolled in ISCED 6 programs, (3) one that captures the number of people
in that country enrolled in ISCED 7 programs, (4) one that captures the
number of people in that country enrolled in ISCED 8 programs, and,
finally, (5) another that notes the reference year being used across the
four constructed variables.

For this last variable, we will need to first create a column for the
number of ISCED 6, ISCED 7, and ISCED 8 enrollees, as well as for the
national population variable, to evaluate how to coalesce onto the same
reference year.

``` r
university_enrollment_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/university_enrollment_data.csv") %>%
  select(-Country.Code,-Series.Code) %>%
  as_tibble(university_enrollment_data) %>%
  slice(1:(n()-5)) %>%
  rename(country=Country.Name,series=Series,yr2000=YR2000,yr2001=YR2001,yr2002=YR2002,yr2003=YR2003,yr2004=YR2004,yr2005=YR2005,yr2006=YR2006,yr2007=YR2007,yr2008=YR2008,yr2009=YR2009,yr2010=YR2010,yr2011=YR2011,yr2012=YR2012,yr2013=YR2013,yr2014=YR2014,yr2015=YR2015,yr2016=YR2016,yr2017=YR2017,yr2018=YR2018,yr2019=YR2019,yr2020=YR2020) %>%
  mutate_at(c("yr2000","yr2001","yr2002","yr2003","yr2004","yr2005","yr2006","yr2007","yr2008","yr2009","yr2010","yr2011","yr2012","yr2013","yr2014","yr2015","yr2016","yr2017","yr2018","yr2019","yr2020"),as.double) %>%
  replace(is.na(.),0) %>%
  distinct() %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 6 programmes, both sexes (number)"),"isced6_enr")) %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 7 programmes, both sexes (number)"),"isced7_enr")) %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 8 programmes, both sexes (number)"),"isced8_enr")) %>%
  mutate(across("series",str_replace, fixed("Population, total"),"nat_pop")) %>%
  pivot_wider(names_from="series",
              values_from=c("yr2020","yr2019","yr2018","yr2017","yr2016","yr2015","yr2014","yr2013","yr2012","yr2011","yr2010","yr2009","yr2008","yr2007","yr2006","yr2005","yr2004","yr2003","yr2002","yr2001","yr2000")) %>%
  mutate(isced6_ref_year=ifelse(yr2020_isced6_enr>0,2020,ifelse(yr2019_isced6_enr>0,2019,ifelse(yr2018_isced6_enr>0,2018,ifelse(yr2017_isced6_enr>0,2017,ifelse(yr2016_isced6_enr>0,2016,ifelse(yr2015_isced6_enr>0,2015,ifelse(yr2014_isced6_enr>0,2014,ifelse(yr2013_isced6_enr>0,2013,ifelse(yr2012_isced6_enr>0,2012,ifelse(yr2011_isced6_enr>0,2011,ifelse(yr2010_isced6_enr>0,2010,ifelse(yr2009_isced6_enr>0,2009,ifelse(yr2008_isced6_enr>0,2008,ifelse(yr2007_isced6_enr>0,2007,ifelse(yr2006_isced6_enr>0,2006,ifelse(yr2005_isced6_enr>0,2005,ifelse(yr2004_isced6_enr>0,2004,ifelse(yr2003_isced6_enr>0,2003,ifelse(yr2002_isced6_enr>0,2002,ifelse(yr2001_isced6_enr>0,2001,ifelse(yr2000_isced6_enr>0,2000,0)))))))))))))))))))))) %>%
  mutate(isced7_ref_yr=ifelse(yr2020_isced7_enr>0,2020,ifelse(yr2019_isced7_enr>0,2019,ifelse(yr2018_isced7_enr>0,2018,ifelse(yr2017_isced7_enr>0,2017,ifelse(yr2016_isced7_enr>0,2016,ifelse(yr2015_isced7_enr>0,2015,ifelse(yr2014_isced7_enr>0,2014,ifelse(yr2013_isced7_enr>0,2013,ifelse(yr2012_isced7_enr>0,2012,ifelse(yr2011_isced7_enr>0,2011,ifelse(yr2010_isced7_enr>0,2010,ifelse(yr2009_isced7_enr>0,2009,ifelse(yr2008_isced7_enr>0,2008,ifelse(yr2007_isced7_enr>0,2007,ifelse(yr2006_isced7_enr>0,2006,ifelse(yr2005_isced7_enr>0,2005,ifelse(yr2004_isced7_enr>0,2004,ifelse(yr2003_isced7_enr>0,2003,ifelse(yr2002_isced7_enr>0,2002,ifelse(yr2001_isced7_enr>0,2001,ifelse(yr2000_isced7_enr>0,2000,0)))))))))))))))))))))) %>%
  mutate(isced8_ref_yr=ifelse(yr2020_isced8_enr>0,2020,ifelse(yr2019_isced8_enr>0,2019,ifelse(yr2018_isced8_enr>0,2018,ifelse(yr2017_isced8_enr>0,2017,ifelse(yr2016_isced8_enr>0,2016,ifelse(yr2015_isced8_enr>0,2015,ifelse(yr2014_isced8_enr>0,2014,ifelse(yr2013_isced8_enr>0,2013,ifelse(yr2012_isced8_enr>0,2012,ifelse(yr2011_isced8_enr>0,2011,ifelse(yr2010_isced8_enr>0,2010,ifelse(yr2009_isced8_enr>0,2009,ifelse(yr2008_isced8_enr>0,2008,ifelse(yr2007_isced8_enr>0,2007,ifelse(yr2006_isced8_enr>0,2006,ifelse(yr2005_isced8_enr>0,2005,ifelse(yr2004_isced8_enr>0,2004,ifelse(yr2003_isced8_enr>0,2003,ifelse(yr2002_isced8_enr>0,2002,ifelse(yr2001_isced8_enr>0,2001,ifelse(yr2000_isced8_enr>0,2000,0)))))))))))))))))))))) %>%
  mutate(natpop_ref_yr=ifelse(yr2020_nat_pop>0,2020,ifelse(yr2019_nat_pop>0,2019,ifelse(yr2018_nat_pop>0,2018,ifelse(yr2017_nat_pop>0,2017,ifelse(yr2016_nat_pop>0,2016,ifelse(yr2015_nat_pop>0,2015,ifelse(yr2014_nat_pop>0,2014,ifelse(yr2013_nat_pop>0,2013,ifelse(yr2012_nat_pop>0,2012,ifelse(yr2011_nat_pop>0,2011,ifelse(yr2010_nat_pop>0,2010,ifelse(yr2009_nat_pop>0,2009,ifelse(yr2008_nat_pop>0,2008,ifelse(yr2007_nat_pop>0,2007,ifelse(yr2006_nat_pop>0,2006,ifelse(yr2005_nat_pop>0,2005,ifelse(yr2004_nat_pop>0,2004,ifelse(yr2003_nat_pop>0,2003,ifelse(yr2002_nat_pop>0,2002,ifelse(yr2001_nat_pop>0,2001,ifelse(yr2000_nat_pop>0,2000,0))))))))))))))))))))))
university_enrollment_data %>%
  head(6)
```

    ## # A tibble: 6 × 89
    ##   country        yr202…¹ yr202…² yr202…³ yr202…⁴ yr201…⁵ yr201…⁶ yr201…⁷ yr201…⁸
    ##   <chr>            <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Afghanistan          0       0       0       0  3.80e7      0        0     0  
    ## 2 Albania              0       0       0       0  2.87e6  89231    43749  1865  
    ## 3 Algeria              0       0       0       0  4.31e7      0        0     0  
    ## 4 American Samoa       0       0       0       0  5.5 e4      0        0     0  
    ## 5 Andorra              0       0       0       0  7.7 e4    554.      35    23.5
    ## 6 Angola               0       0       0       0  3.18e7      0        0     0  
    ## # … with 80 more variables: yr2018_nat_pop <dbl>, yr2018_isced6_enr <dbl>,
    ## #   yr2018_isced7_enr <dbl>, yr2018_isced8_enr <dbl>, yr2017_nat_pop <dbl>,
    ## #   yr2017_isced6_enr <dbl>, yr2017_isced7_enr <dbl>, yr2017_isced8_enr <dbl>,
    ## #   yr2016_nat_pop <dbl>, yr2016_isced6_enr <dbl>, yr2016_isced7_enr <dbl>,
    ## #   yr2016_isced8_enr <dbl>, yr2015_nat_pop <dbl>, yr2015_isced6_enr <dbl>,
    ## #   yr2015_isced7_enr <dbl>, yr2015_isced8_enr <dbl>, yr2014_nat_pop <dbl>,
    ## #   yr2014_isced6_enr <dbl>, yr2014_isced7_enr <dbl>, …

From a quick glance, we can see that for the countries with available
data across multiple VOIs, that there are inconsistencies in when the
most recent year with recorded data was. Therefore, the assumption that
the collection of these ISCED enrollee estimatesoccur at the same
intervals, and thus would have the same reference year, has been
violated.

While we figure out the best solution for dealing with this, we do know
that for the countries that have `0` values across the newly generated
`isced6_ref_yr`, `isced7_ref_yr`, and `isced8_ref_yr` columns can be
removed, given that they do not have data available for university
enrollment.

Here, we list the countries that meet this condition, whereby the sum of
`isced6_ref_yr`, `isced7_ref_yr`, and `isced8_ref_yr` is equivalent to
`0`.

``` r
university_enrollment_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/university_enrollment_data.csv") %>%
  select(-Country.Code,-Series.Code) %>%
  as_tibble(university_enrollment_data) %>%
  slice(1:(n()-5)) %>%
  rename(country=Country.Name,series=Series,yr2000=YR2000,yr2001=YR2001,yr2002=YR2002,yr2003=YR2003,yr2004=YR2004,yr2005=YR2005,yr2006=YR2006,yr2007=YR2007,yr2008=YR2008,yr2009=YR2009,yr2010=YR2010,yr2011=YR2011,yr2012=YR2012,yr2013=YR2013,yr2014=YR2014,yr2015=YR2015,yr2016=YR2016,yr2017=YR2017,yr2018=YR2018,yr2019=YR2019,yr2020=YR2020) %>%
  mutate_at(c("yr2000","yr2001","yr2002","yr2003","yr2004","yr2005","yr2006","yr2007","yr2008","yr2009","yr2010","yr2011","yr2012","yr2013","yr2014","yr2015","yr2016","yr2017","yr2018","yr2019","yr2020"),as.double) %>%
  replace(is.na(.),0) %>%
  distinct() %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 6 programmes, both sexes (number)"),"isced6_enr")) %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 7 programmes, both sexes (number)"),"isced7_enr")) %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 8 programmes, both sexes (number)"),"isced8_enr")) %>%
  mutate(across("series",str_replace, fixed("Population, total"),"nat_pop")) %>%
  pivot_wider(names_from="series",
              values_from=c("yr2020","yr2019","yr2018","yr2017","yr2016","yr2015","yr2014","yr2013","yr2012","yr2011","yr2010","yr2009","yr2008","yr2007","yr2006","yr2005","yr2004","yr2003","yr2002","yr2001","yr2000")) %>%
  mutate(isced6_ref_yr=ifelse(yr2020_isced6_enr>0,2020,ifelse(yr2019_isced6_enr>0,2019,ifelse(yr2018_isced6_enr>0,2018,ifelse(yr2017_isced6_enr>0,2017,ifelse(yr2016_isced6_enr>0,2016,ifelse(yr2015_isced6_enr>0,2015,ifelse(yr2014_isced6_enr>0,2014,ifelse(yr2013_isced6_enr>0,2013,ifelse(yr2012_isced6_enr>0,2012,ifelse(yr2011_isced6_enr>0,2011,ifelse(yr2010_isced6_enr>0,2010,ifelse(yr2009_isced6_enr>0,2009,ifelse(yr2008_isced6_enr>0,2008,ifelse(yr2007_isced6_enr>0,2007,ifelse(yr2006_isced6_enr>0,2006,ifelse(yr2005_isced6_enr>0,2005,ifelse(yr2004_isced6_enr>0,2004,ifelse(yr2003_isced6_enr>0,2003,ifelse(yr2002_isced6_enr>0,2002,ifelse(yr2001_isced6_enr>0,2001,ifelse(yr2000_isced6_enr>0,2000,0)))))))))))))))))))))) %>%
  mutate(isced7_ref_yr=ifelse(yr2020_isced7_enr>0,2020,ifelse(yr2019_isced7_enr>0,2019,ifelse(yr2018_isced7_enr>0,2018,ifelse(yr2017_isced7_enr>0,2017,ifelse(yr2016_isced7_enr>0,2016,ifelse(yr2015_isced7_enr>0,2015,ifelse(yr2014_isced7_enr>0,2014,ifelse(yr2013_isced7_enr>0,2013,ifelse(yr2012_isced7_enr>0,2012,ifelse(yr2011_isced7_enr>0,2011,ifelse(yr2010_isced7_enr>0,2010,ifelse(yr2009_isced7_enr>0,2009,ifelse(yr2008_isced7_enr>0,2008,ifelse(yr2007_isced7_enr>0,2007,ifelse(yr2006_isced7_enr>0,2006,ifelse(yr2005_isced7_enr>0,2005,ifelse(yr2004_isced7_enr>0,2004,ifelse(yr2003_isced7_enr>0,2003,ifelse(yr2002_isced7_enr>0,2002,ifelse(yr2001_isced7_enr>0,2001,ifelse(yr2000_isced7_enr>0,2000,0)))))))))))))))))))))) %>%
  mutate(isced8_ref_yr=ifelse(yr2020_isced8_enr>0,2020,ifelse(yr2019_isced8_enr>0,2019,ifelse(yr2018_isced8_enr>0,2018,ifelse(yr2017_isced8_enr>0,2017,ifelse(yr2016_isced8_enr>0,2016,ifelse(yr2015_isced8_enr>0,2015,ifelse(yr2014_isced8_enr>0,2014,ifelse(yr2013_isced8_enr>0,2013,ifelse(yr2012_isced8_enr>0,2012,ifelse(yr2011_isced8_enr>0,2011,ifelse(yr2010_isced8_enr>0,2010,ifelse(yr2009_isced8_enr>0,2009,ifelse(yr2008_isced8_enr>0,2008,ifelse(yr2007_isced8_enr>0,2007,ifelse(yr2006_isced8_enr>0,2006,ifelse(yr2005_isced8_enr>0,2005,ifelse(yr2004_isced8_enr>0,2004,ifelse(yr2003_isced8_enr>0,2003,ifelse(yr2002_isced8_enr>0,2002,ifelse(yr2001_isced8_enr>0,2001,ifelse(yr2000_isced8_enr>0,2000,0)))))))))))))))))))))) %>%
  mutate(natpop_ref_yr=ifelse(yr2020_nat_pop>0,2020,ifelse(yr2019_nat_pop>0,2019,ifelse(yr2018_nat_pop>0,2018,ifelse(yr2017_nat_pop>0,2017,ifelse(yr2016_nat_pop>0,2016,ifelse(yr2015_nat_pop>0,2015,ifelse(yr2014_nat_pop>0,2014,ifelse(yr2013_nat_pop>0,2013,ifelse(yr2012_nat_pop>0,2012,ifelse(yr2011_nat_pop>0,2011,ifelse(yr2010_nat_pop>0,2010,ifelse(yr2009_nat_pop>0,2009,ifelse(yr2008_nat_pop>0,2008,ifelse(yr2007_nat_pop>0,2007,ifelse(yr2006_nat_pop>0,2006,ifelse(yr2005_nat_pop>0,2005,ifelse(yr2004_nat_pop>0,2004,ifelse(yr2003_nat_pop>0,2003,ifelse(yr2002_nat_pop>0,2002,ifelse(yr2001_nat_pop>0,2001,ifelse(yr2000_nat_pop>0,2000,0)))))))))))))))))))))) %>%
  filter(isced6_ref_yr+isced7_ref_yr+isced8_ref_yr==0)
university_enrollment_data %>%
  select(country,isced6_ref_yr,isced7_ref_yr,isced8_ref_yr) 
```

    ## # A tibble: 52 × 4
    ##    country                  isced6_ref_yr isced7_ref_yr isced8_ref_yr
    ##    <chr>                            <dbl>         <dbl>         <dbl>
    ##  1 American Samoa                       0             0             0
    ##  2 Angola                               0             0             0
    ##  3 Antigua and Barbuda                  0             0             0
    ##  4 Bahamas, The                         0             0             0
    ##  5 Bolivia                              0             0             0
    ##  6 British Virgin Islands               0             0             0
    ##  7 Cayman Islands                       0             0             0
    ##  8 Central African Republic             0             0             0
    ##  9 Channel Islands                      0             0             0
    ## 10 Djibouti                             0             0             0
    ## # … with 42 more rows

Now, we remove those rows from the dataframe here:

``` r
university_enrollment_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/university_enrollment_data.csv") %>%
  select(-Country.Code,-Series.Code) %>%
  as_tibble(university_enrollment_data) %>%
  slice(1:(n()-5)) %>%
  rename(country=Country.Name,series=Series,yr2000=YR2000,yr2001=YR2001,yr2002=YR2002,yr2003=YR2003,yr2004=YR2004,yr2005=YR2005,yr2006=YR2006,yr2007=YR2007,yr2008=YR2008,yr2009=YR2009,yr2010=YR2010,yr2011=YR2011,yr2012=YR2012,yr2013=YR2013,yr2014=YR2014,yr2015=YR2015,yr2016=YR2016,yr2017=YR2017,yr2018=YR2018,yr2019=YR2019,yr2020=YR2020) %>%
  mutate_at(c("yr2000","yr2001","yr2002","yr2003","yr2004","yr2005","yr2006","yr2007","yr2008","yr2009","yr2010","yr2011","yr2012","yr2013","yr2014","yr2015","yr2016","yr2017","yr2018","yr2019","yr2020"),as.double) %>%
  replace(is.na(.),0) %>%
  distinct() %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 6 programmes, both sexes (number)"),"isced6_enr")) %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 7 programmes, both sexes (number)"),"isced7_enr")) %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 8 programmes, both sexes (number)"),"isced8_enr")) %>%
  mutate(across("series",str_replace, fixed("Population, total"),"nat_pop")) %>%
  pivot_wider(names_from="series",
              values_from=c("yr2020","yr2019","yr2018","yr2017","yr2016","yr2015","yr2014","yr2013","yr2012","yr2011","yr2010","yr2009","yr2008","yr2007","yr2006","yr2005","yr2004","yr2003","yr2002","yr2001","yr2000")) %>%
  mutate(isced6_ref_yr=ifelse(yr2020_isced6_enr>0,2020,ifelse(yr2019_isced6_enr>0,2019,ifelse(yr2018_isced6_enr>0,2018,ifelse(yr2017_isced6_enr>0,2017,ifelse(yr2016_isced6_enr>0,2016,ifelse(yr2015_isced6_enr>0,2015,ifelse(yr2014_isced6_enr>0,2014,ifelse(yr2013_isced6_enr>0,2013,ifelse(yr2012_isced6_enr>0,2012,ifelse(yr2011_isced6_enr>0,2011,ifelse(yr2010_isced6_enr>0,2010,ifelse(yr2009_isced6_enr>0,2009,ifelse(yr2008_isced6_enr>0,2008,ifelse(yr2007_isced6_enr>0,2007,ifelse(yr2006_isced6_enr>0,2006,ifelse(yr2005_isced6_enr>0,2005,ifelse(yr2004_isced6_enr>0,2004,ifelse(yr2003_isced6_enr>0,2003,ifelse(yr2002_isced6_enr>0,2002,ifelse(yr2001_isced6_enr>0,2001,ifelse(yr2000_isced6_enr>0,2000,0)))))))))))))))))))))) %>%
  mutate(isced7_ref_yr=ifelse(yr2020_isced7_enr>0,2020,ifelse(yr2019_isced7_enr>0,2019,ifelse(yr2018_isced7_enr>0,2018,ifelse(yr2017_isced7_enr>0,2017,ifelse(yr2016_isced7_enr>0,2016,ifelse(yr2015_isced7_enr>0,2015,ifelse(yr2014_isced7_enr>0,2014,ifelse(yr2013_isced7_enr>0,2013,ifelse(yr2012_isced7_enr>0,2012,ifelse(yr2011_isced7_enr>0,2011,ifelse(yr2010_isced7_enr>0,2010,ifelse(yr2009_isced7_enr>0,2009,ifelse(yr2008_isced7_enr>0,2008,ifelse(yr2007_isced7_enr>0,2007,ifelse(yr2006_isced7_enr>0,2006,ifelse(yr2005_isced7_enr>0,2005,ifelse(yr2004_isced7_enr>0,2004,ifelse(yr2003_isced7_enr>0,2003,ifelse(yr2002_isced7_enr>0,2002,ifelse(yr2001_isced7_enr>0,2001,ifelse(yr2000_isced7_enr>0,2000,0)))))))))))))))))))))) %>%
  mutate(isced8_ref_yr=ifelse(yr2020_isced8_enr>0,2020,ifelse(yr2019_isced8_enr>0,2019,ifelse(yr2018_isced8_enr>0,2018,ifelse(yr2017_isced8_enr>0,2017,ifelse(yr2016_isced8_enr>0,2016,ifelse(yr2015_isced8_enr>0,2015,ifelse(yr2014_isced8_enr>0,2014,ifelse(yr2013_isced8_enr>0,2013,ifelse(yr2012_isced8_enr>0,2012,ifelse(yr2011_isced8_enr>0,2011,ifelse(yr2010_isced8_enr>0,2010,ifelse(yr2009_isced8_enr>0,2009,ifelse(yr2008_isced8_enr>0,2008,ifelse(yr2007_isced8_enr>0,2007,ifelse(yr2006_isced8_enr>0,2006,ifelse(yr2005_isced8_enr>0,2005,ifelse(yr2004_isced8_enr>0,2004,ifelse(yr2003_isced8_enr>0,2003,ifelse(yr2002_isced8_enr>0,2002,ifelse(yr2001_isced8_enr>0,2001,ifelse(yr2000_isced8_enr>0,2000,0)))))))))))))))))))))) %>%
  mutate(natpop_ref_yr=ifelse(yr2020_nat_pop>0,2020,ifelse(yr2019_nat_pop>0,2019,ifelse(yr2018_nat_pop>0,2018,ifelse(yr2017_nat_pop>0,2017,ifelse(yr2016_nat_pop>0,2016,ifelse(yr2015_nat_pop>0,2015,ifelse(yr2014_nat_pop>0,2014,ifelse(yr2013_nat_pop>0,2013,ifelse(yr2012_nat_pop>0,2012,ifelse(yr2011_nat_pop>0,2011,ifelse(yr2010_nat_pop>0,2010,ifelse(yr2009_nat_pop>0,2009,ifelse(yr2008_nat_pop>0,2008,ifelse(yr2007_nat_pop>0,2007,ifelse(yr2006_nat_pop>0,2006,ifelse(yr2005_nat_pop>0,2005,ifelse(yr2004_nat_pop>0,2004,ifelse(yr2003_nat_pop>0,2003,ifelse(yr2002_nat_pop>0,2002,ifelse(yr2001_nat_pop>0,2001,ifelse(yr2000_nat_pop>0,2000,0)))))))))))))))))))))) %>%
  rowwise() %>%
  filter(!isced6_ref_yr+isced7_ref_yr+isced8_ref_yr==0) 
university_enrollment_data %>%
  head(6)
```

    ## # A tibble: 6 × 89
    ## # Rowwise: 
    ##   country     yr2020_n…¹ yr202…² yr202…³ yr202…⁴ yr201…⁵ yr201…⁶ yr201…⁷ yr201…⁸
    ##   <chr>            <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Afghanistan          0       0       0       0  3.80e7      0        0     0  
    ## 2 Albania              0       0       0       0  2.87e6  89231    43749  1865  
    ## 3 Algeria              0       0       0       0  4.31e7      0        0     0  
    ## 4 Andorra              0       0       0       0  7.7 e4    554.      35    23.5
    ## 5 Arab World           0       0       0       0  0           0        0     0  
    ## 6 Argentina            0       0       0       0  4.49e7      0        0     0  
    ## # … with 80 more variables: yr2018_nat_pop <dbl>, yr2018_isced6_enr <dbl>,
    ## #   yr2018_isced7_enr <dbl>, yr2018_isced8_enr <dbl>, yr2017_nat_pop <dbl>,
    ## #   yr2017_isced6_enr <dbl>, yr2017_isced7_enr <dbl>, yr2017_isced8_enr <dbl>,
    ## #   yr2016_nat_pop <dbl>, yr2016_isced6_enr <dbl>, yr2016_isced7_enr <dbl>,
    ## #   yr2016_isced8_enr <dbl>, yr2015_nat_pop <dbl>, yr2015_isced6_enr <dbl>,
    ## #   yr2015_isced7_enr <dbl>, yr2015_isced8_enr <dbl>, yr2014_nat_pop <dbl>,
    ## #   yr2014_isced6_enr <dbl>, yr2014_isced7_enr <dbl>, …

With these rows removed from the dataset, we can now move on to
constructing the corresponding columns that pull the actual values, both
for the national population variable as well as for the ISCED 6, 7, and
8 enrollee estimates.

``` r
university_enrollment_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/university_enrollment_data.csv") %>%
  select(-Country.Code,-Series.Code) %>%
  as_tibble(university_enrollment_data) %>%
  slice(1:(n()-5)) %>%
  rename(country=Country.Name,series=Series,yr2000=YR2000,yr2001=YR2001,yr2002=YR2002,yr2003=YR2003,yr2004=YR2004,yr2005=YR2005,yr2006=YR2006,yr2007=YR2007,yr2008=YR2008,yr2009=YR2009,yr2010=YR2010,yr2011=YR2011,yr2012=YR2012,yr2013=YR2013,yr2014=YR2014,yr2015=YR2015,yr2016=YR2016,yr2017=YR2017,yr2018=YR2018,yr2019=YR2019,yr2020=YR2020) %>%
  mutate_at(c("yr2000","yr2001","yr2002","yr2003","yr2004","yr2005","yr2006","yr2007","yr2008","yr2009","yr2010","yr2011","yr2012","yr2013","yr2014","yr2015","yr2016","yr2017","yr2018","yr2019","yr2020"),as.double) %>%
  replace(is.na(.),0) %>%
  distinct() %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 6 programmes, both sexes (number)"),"isced6_enr")) %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 7 programmes, both sexes (number)"),"isced7_enr")) %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 8 programmes, both sexes (number)"),"isced8_enr")) %>%
  mutate(across("series",str_replace, fixed("Population, total"),"nat_pop")) %>%
  pivot_wider(names_from="series",
              values_from=c("yr2020","yr2019","yr2018","yr2017","yr2016","yr2015","yr2014","yr2013","yr2012","yr2011","yr2010","yr2009","yr2008","yr2007","yr2006","yr2005","yr2004","yr2003","yr2002","yr2001","yr2000")) %>%
  mutate(isced6_ref_yr=ifelse(yr2020_isced6_enr>0,2020,ifelse(yr2019_isced6_enr>0,2019,ifelse(yr2018_isced6_enr>0,2018,ifelse(yr2017_isced6_enr>0,2017,ifelse(yr2016_isced6_enr>0,2016,ifelse(yr2015_isced6_enr>0,2015,ifelse(yr2014_isced6_enr>0,2014,ifelse(yr2013_isced6_enr>0,2013,ifelse(yr2012_isced6_enr>0,2012,ifelse(yr2011_isced6_enr>0,2011,ifelse(yr2010_isced6_enr>0,2010,ifelse(yr2009_isced6_enr>0,2009,ifelse(yr2008_isced6_enr>0,2008,ifelse(yr2007_isced6_enr>0,2007,ifelse(yr2006_isced6_enr>0,2006,ifelse(yr2005_isced6_enr>0,2005,ifelse(yr2004_isced6_enr>0,2004,ifelse(yr2003_isced6_enr>0,2003,ifelse(yr2002_isced6_enr>0,2002,ifelse(yr2001_isced6_enr>0,2001,ifelse(yr2000_isced6_enr>0,2000,0)))))))))))))))))))))) %>%
  mutate(isced7_ref_yr=ifelse(yr2020_isced7_enr>0,2020,ifelse(yr2019_isced7_enr>0,2019,ifelse(yr2018_isced7_enr>0,2018,ifelse(yr2017_isced7_enr>0,2017,ifelse(yr2016_isced7_enr>0,2016,ifelse(yr2015_isced7_enr>0,2015,ifelse(yr2014_isced7_enr>0,2014,ifelse(yr2013_isced7_enr>0,2013,ifelse(yr2012_isced7_enr>0,2012,ifelse(yr2011_isced7_enr>0,2011,ifelse(yr2010_isced7_enr>0,2010,ifelse(yr2009_isced7_enr>0,2009,ifelse(yr2008_isced7_enr>0,2008,ifelse(yr2007_isced7_enr>0,2007,ifelse(yr2006_isced7_enr>0,2006,ifelse(yr2005_isced7_enr>0,2005,ifelse(yr2004_isced7_enr>0,2004,ifelse(yr2003_isced7_enr>0,2003,ifelse(yr2002_isced7_enr>0,2002,ifelse(yr2001_isced7_enr>0,2001,ifelse(yr2000_isced7_enr>0,2000,0)))))))))))))))))))))) %>%
  mutate(isced8_ref_yr=ifelse(yr2020_isced8_enr>0,2020,ifelse(yr2019_isced8_enr>0,2019,ifelse(yr2018_isced8_enr>0,2018,ifelse(yr2017_isced8_enr>0,2017,ifelse(yr2016_isced8_enr>0,2016,ifelse(yr2015_isced8_enr>0,2015,ifelse(yr2014_isced8_enr>0,2014,ifelse(yr2013_isced8_enr>0,2013,ifelse(yr2012_isced8_enr>0,2012,ifelse(yr2011_isced8_enr>0,2011,ifelse(yr2010_isced8_enr>0,2010,ifelse(yr2009_isced8_enr>0,2009,ifelse(yr2008_isced8_enr>0,2008,ifelse(yr2007_isced8_enr>0,2007,ifelse(yr2006_isced8_enr>0,2006,ifelse(yr2005_isced8_enr>0,2005,ifelse(yr2004_isced8_enr>0,2004,ifelse(yr2003_isced8_enr>0,2003,ifelse(yr2002_isced8_enr>0,2002,ifelse(yr2001_isced8_enr>0,2001,ifelse(yr2000_isced8_enr>0,2000,0)))))))))))))))))))))) %>%
  mutate(natpop_ref_yr=ifelse(yr2020_nat_pop>0,2020,ifelse(yr2019_nat_pop>0,2019,ifelse(yr2018_nat_pop>0,2018,ifelse(yr2017_nat_pop>0,2017,ifelse(yr2016_nat_pop>0,2016,ifelse(yr2015_nat_pop>0,2015,ifelse(yr2014_nat_pop>0,2014,ifelse(yr2013_nat_pop>0,2013,ifelse(yr2012_nat_pop>0,2012,ifelse(yr2011_nat_pop>0,2011,ifelse(yr2010_nat_pop>0,2010,ifelse(yr2009_nat_pop>0,2009,ifelse(yr2008_nat_pop>0,2008,ifelse(yr2007_nat_pop>0,2007,ifelse(yr2006_nat_pop>0,2006,ifelse(yr2005_nat_pop>0,2005,ifelse(yr2004_nat_pop>0,2004,ifelse(yr2003_nat_pop>0,2003,ifelse(yr2002_nat_pop>0,2002,ifelse(yr2001_nat_pop>0,2001,ifelse(yr2000_nat_pop>0,2000,0)))))))))))))))))))))) %>%
  mutate(isced6_enr=ifelse(yr2020_isced6_enr>0,yr2020_isced6_enr,ifelse(yr2019_isced6_enr>0,yr2019_isced6_enr,ifelse(yr2018_isced6_enr>0,yr2018_isced6_enr,ifelse(yr2017_isced6_enr>0,yr2017_isced6_enr,ifelse(yr2016_isced6_enr>0,yr2016_isced6_enr,ifelse(yr2015_isced6_enr>0,yr2015_isced6_enr,ifelse(yr2014_isced6_enr>0,yr2014_isced6_enr,ifelse(yr2013_isced6_enr>0,yr2013_isced6_enr,ifelse(yr2012_isced6_enr>0,yr2012_isced6_enr,ifelse(yr2011_isced6_enr>0,yr2011_isced6_enr,ifelse(yr2010_isced6_enr>0,yr2010_isced6_enr,ifelse(yr2009_isced6_enr>0,yr2009_isced6_enr,ifelse(yr2008_isced6_enr>0,yr2008_isced6_enr,ifelse(yr2007_isced6_enr>0,yr2007_isced6_enr,ifelse(yr2006_isced6_enr>0,yr2006_isced6_enr,ifelse(yr2005_isced6_enr>0,yr2005_isced6_enr,ifelse(yr2004_isced6_enr>0,yr2004_isced6_enr,ifelse(yr2003_isced6_enr>0,yr2003_isced6_enr,ifelse(yr2002_isced6_enr>0,yr2002_isced6_enr,ifelse(yr2001_isced6_enr>0,yr2001_isced6_enr,ifelse(yr2000_isced6_enr>0,yr2000_isced6_enr,0)))))))))))))))))))))) %>%
  mutate(isced7_enr=ifelse(yr2020_isced7_enr>0,yr2020_isced7_enr,ifelse(yr2019_isced7_enr>0,yr2019_isced7_enr,ifelse(yr2018_isced7_enr>0,yr2018_isced7_enr,ifelse(yr2017_isced7_enr>0,yr2017_isced7_enr,ifelse(yr2016_isced7_enr>0,yr2016_isced7_enr,ifelse(yr2015_isced7_enr>0,yr2015_isced7_enr,ifelse(yr2014_isced7_enr>0,yr2014_isced7_enr,ifelse(yr2013_isced7_enr>0,yr2013_isced7_enr,ifelse(yr2012_isced7_enr>0,yr2012_isced7_enr,ifelse(yr2011_isced7_enr>0,yr2011_isced7_enr,ifelse(yr2010_isced7_enr>0,yr2010_isced7_enr,ifelse(yr2009_isced7_enr>0,yr2009_isced7_enr,ifelse(yr2008_isced7_enr>0,yr2008_isced7_enr,ifelse(yr2007_isced7_enr>0,yr2007_isced7_enr,ifelse(yr2006_isced7_enr>0,yr2006_isced7_enr,ifelse(yr2005_isced7_enr>0,yr2005_isced7_enr,ifelse(yr2004_isced7_enr>0,yr2004_isced7_enr,ifelse(yr2003_isced7_enr>0,yr2003_isced7_enr,ifelse(yr2002_isced7_enr>0,yr2002_isced7_enr,ifelse(yr2001_isced7_enr>0,yr2001_isced7_enr,ifelse(yr2000_isced7_enr>0,yr2000_isced7_enr,0)))))))))))))))))))))) %>%
  mutate(isced8_enr=ifelse(yr2020_isced8_enr>0,yr2020_isced8_enr,ifelse(yr2019_isced8_enr>0,yr2019_isced8_enr,ifelse(yr2018_isced8_enr>0,yr2018_isced8_enr,ifelse(yr2017_isced8_enr>0,yr2017_isced8_enr,ifelse(yr2016_isced8_enr>0,yr2016_isced8_enr,ifelse(yr2015_isced8_enr>0,yr2015_isced8_enr,ifelse(yr2014_isced8_enr>0,yr2014_isced8_enr,ifelse(yr2013_isced8_enr>0,yr2013_isced8_enr,ifelse(yr2012_isced8_enr>0,yr2012_isced8_enr,ifelse(yr2011_isced8_enr>0,yr2011_isced8_enr,ifelse(yr2010_isced8_enr>0,yr2010_isced8_enr,ifelse(yr2009_isced8_enr>0,yr2009_isced8_enr,ifelse(yr2008_isced8_enr>0,yr2008_isced8_enr,ifelse(yr2007_isced8_enr>0,yr2007_isced8_enr,ifelse(yr2006_isced8_enr>0,yr2006_isced8_enr,ifelse(yr2005_isced8_enr>0,yr2005_isced8_enr,ifelse(yr2004_isced8_enr>0,yr2004_isced8_enr,ifelse(yr2003_isced8_enr>0,yr2003_isced8_enr,ifelse(yr2002_isced8_enr>0,yr2002_isced8_enr,ifelse(yr2001_isced8_enr>0,yr2001_isced8_enr,ifelse(yr2000_isced8_enr>0,yr2000_isced8_enr,0)))))))))))))))))))))) %>%
   mutate(natpop_est=ifelse(yr2020_nat_pop>0,yr2020_nat_pop,ifelse(yr2019_nat_pop>0,yr2019_nat_pop,ifelse(yr2018_nat_pop>0,yr2018_nat_pop,ifelse(yr2017_nat_pop>0,yr2017_nat_pop,ifelse(yr2016_nat_pop>0,yr2016_nat_pop,ifelse(yr2015_nat_pop>0,yr2015_nat_pop,ifelse(yr2014_nat_pop>0,yr2014_nat_pop,ifelse(yr2013_nat_pop>0,yr2013_nat_pop,ifelse(yr2012_nat_pop>0,yr2012_nat_pop,ifelse(yr2011_nat_pop>0,yr2011_nat_pop,ifelse(yr2010_nat_pop>0,yr2010_nat_pop,ifelse(yr2009_nat_pop>0,yr2009_nat_pop,ifelse(yr2008_nat_pop>0,yr2008_nat_pop,ifelse(yr2007_nat_pop>0,yr2007_nat_pop,ifelse(yr2006_nat_pop>0,yr2006_nat_pop,ifelse(yr2005_nat_pop>0,yr2005_nat_pop,ifelse(yr2004_nat_pop>0,yr2004_nat_pop,ifelse(yr2003_nat_pop>0,yr2003_nat_pop,ifelse(yr2002_nat_pop>0,yr2002_nat_pop,ifelse(yr2001_nat_pop>0,yr2001_nat_pop,ifelse(yr2000_nat_pop>0,yr2000_nat_pop,0)))))))))))))))))))))) %>%
  rowwise() %>%
  filter(!isced6_ref_yr+isced7_ref_yr+isced8_ref_yr==0) %>%
  select(country,isced6_enr,isced6_ref_yr,isced7_enr,isced7_ref_yr,isced8_enr,isced8_ref_yr,natpop_est,natpop_ref_yr) 
university_enrollment_data 
```

    ## # A tibble: 216 × 9
    ## # Rowwise: 
    ##    country     isced6_…¹ isced…² isced…³ isced…⁴ isced…⁵ isced…⁶ natpo…⁷ natpo…⁸
    ##    <chr>           <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Afghanistan   365982     2018   4600     2018  2.8 e1    2018  3.80e7    2019
    ##  2 Albania        89231     2019  43749     2019  1.86e3    2019  2.87e6    2019
    ##  3 Algeria       996087     2018      0        0  0            0  4.31e7    2019
    ##  4 Andorra          554.    2019     35     2019  2.35e1    2019  7.7 e4    2019
    ##  5 Arab World   9261392.    2018 959789.    2018  1.77e5    2018  3.70e8    2013
    ##  6 Argentina    2210454     2017 276648     2017  2.61e4    2017  4.49e7    2019
    ##  7 Armenia        69622     2019  10855     2019  9.85e2    2019  2.96e6    2019
    ##  8 Aruba            848     2016     55     2016  0            0  1.06e5    2019
    ##  9 Australia     999034     2018 304632     2018  5.61e4    2018  2.53e7    2019
    ## 10 Austria       199236.    2018 135346.    2018  2.04e4    2018  8.86e6    2019
    ## # … with 206 more rows, and abbreviated variable names ¹​isced6_enr,
    ## #   ²​isced6_ref_yr, ³​isced7_enr, ⁴​isced7_ref_yr, ⁵​isced8_enr, ⁶​isced8_ref_yr,
    ## #   ⁷​natpop_est, ⁸​natpop_ref_yr

After selecting out the annualized columns for each of the four
population estimates, we are left with 8 columns noting the estimate and
reference year for the most recent year there is available population
data.

In addition to the rows that were removed due to the there being no
provided isced 6, 7, or 8 enrollment estimates across the 20-year window
of investigation, we will also need to remove the rows that represent
aggregated data across regions or groups of countries (i.e., what the
EdStats database refers to as “aggregates.” These include the
designations:

-   “Arab World”
-   “Caribbean small states”
-   “Central Europe and the Baltics”
-   “Early-demographic dividend”
-   “East Asia & Pacific”
-   “East Asia & Pacific (excluding high income)”
-   “East Asia & Pacific (IDA & IBRD countries)”
-   “Euro area”
-   “Europe & Central Asia”
-   “Europe & Central Asia (excluding high income)”
-   “Europe & Central Asia (IDA & IBRD countries)”
-   “European Union”
-   “Fragile and conflict affected situations”
-   “Heavily indebted poor countries (HIPC)”
-   “High income”
-   “IBRD only”
-   “IDA & IBRD total”
-   “IDA blend”
-   “IDA only”
-   “IDA total”
-   “Late-demographic dividend”
-   “Latin America & Caribbean”
-   “Latin America & Caribbean (excluding high income)”
-   “Latin America & the Caribbean (IDA & IBRD countries)”
-   “Least developed countries: UN classification”
-   “Low & middle income”
-   “Low income”
-   “Lower middle income”
-   “Middle East & North Africa”
-   “Middle East & North Africa (excluding high income)”
-   “Middle East & North Africa (IDA & IBRD countries)”
-   “Middle income”
-   “North America”
-   “OECD members”
-   “Other small states”
-   “Pacific island small states”
-   “Post-demographic dividend”
-   “Pre-demographic dividend”
-   “Small states”
-   “South Asia”
-   “South Asia (IDA & IBRD)”
-   “Sub-Saharan Africa”
-   “Sub-Saharan Africa (excluding high income)”
-   “Upper middle income”
-   “World”

``` r
university_enrollment_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/university_enrollment_data.csv") %>%
  select(-Country.Code,-Series.Code) %>%
  as_tibble(university_enrollment_data) %>%
  slice(1:(n()-5)) %>%
  rename(country=Country.Name,series=Series,yr2000=YR2000,yr2001=YR2001,yr2002=YR2002,yr2003=YR2003,yr2004=YR2004,yr2005=YR2005,yr2006=YR2006,yr2007=YR2007,yr2008=YR2008,yr2009=YR2009,yr2010=YR2010,yr2011=YR2011,yr2012=YR2012,yr2013=YR2013,yr2014=YR2014,yr2015=YR2015,yr2016=YR2016,yr2017=YR2017,yr2018=YR2018,yr2019=YR2019,yr2020=YR2020) %>%
  mutate_at(c("yr2000","yr2001","yr2002","yr2003","yr2004","yr2005","yr2006","yr2007","yr2008","yr2009","yr2010","yr2011","yr2012","yr2013","yr2014","yr2015","yr2016","yr2017","yr2018","yr2019","yr2020"),as.double) %>%
  replace(is.na(.),0) %>%
  distinct() %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 6 programmes, both sexes (number)"),"isced6_enr")) %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 7 programmes, both sexes (number)"),"isced7_enr")) %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 8 programmes, both sexes (number)"),"isced8_enr")) %>%
  mutate(across("series",str_replace, fixed("Population, total"),"nat_pop")) %>%
  pivot_wider(names_from="series",
              values_from=c("yr2020","yr2019","yr2018","yr2017","yr2016","yr2015","yr2014","yr2013","yr2012","yr2011","yr2010","yr2009","yr2008","yr2007","yr2006","yr2005","yr2004","yr2003","yr2002","yr2001","yr2000")) %>%
  mutate(isced6_ref_yr=ifelse(yr2020_isced6_enr>0,2020,ifelse(yr2019_isced6_enr>0,2019,ifelse(yr2018_isced6_enr>0,2018,ifelse(yr2017_isced6_enr>0,2017,ifelse(yr2016_isced6_enr>0,2016,ifelse(yr2015_isced6_enr>0,2015,ifelse(yr2014_isced6_enr>0,2014,ifelse(yr2013_isced6_enr>0,2013,ifelse(yr2012_isced6_enr>0,2012,ifelse(yr2011_isced6_enr>0,2011,ifelse(yr2010_isced6_enr>0,2010,ifelse(yr2009_isced6_enr>0,2009,ifelse(yr2008_isced6_enr>0,2008,ifelse(yr2007_isced6_enr>0,2007,ifelse(yr2006_isced6_enr>0,2006,ifelse(yr2005_isced6_enr>0,2005,ifelse(yr2004_isced6_enr>0,2004,ifelse(yr2003_isced6_enr>0,2003,ifelse(yr2002_isced6_enr>0,2002,ifelse(yr2001_isced6_enr>0,2001,ifelse(yr2000_isced6_enr>0,2000,0)))))))))))))))))))))) %>%
  mutate(isced7_ref_yr=ifelse(yr2020_isced7_enr>0,2020,ifelse(yr2019_isced7_enr>0,2019,ifelse(yr2018_isced7_enr>0,2018,ifelse(yr2017_isced7_enr>0,2017,ifelse(yr2016_isced7_enr>0,2016,ifelse(yr2015_isced7_enr>0,2015,ifelse(yr2014_isced7_enr>0,2014,ifelse(yr2013_isced7_enr>0,2013,ifelse(yr2012_isced7_enr>0,2012,ifelse(yr2011_isced7_enr>0,2011,ifelse(yr2010_isced7_enr>0,2010,ifelse(yr2009_isced7_enr>0,2009,ifelse(yr2008_isced7_enr>0,2008,ifelse(yr2007_isced7_enr>0,2007,ifelse(yr2006_isced7_enr>0,2006,ifelse(yr2005_isced7_enr>0,2005,ifelse(yr2004_isced7_enr>0,2004,ifelse(yr2003_isced7_enr>0,2003,ifelse(yr2002_isced7_enr>0,2002,ifelse(yr2001_isced7_enr>0,2001,ifelse(yr2000_isced7_enr>0,2000,0)))))))))))))))))))))) %>%
  mutate(isced8_ref_yr=ifelse(yr2020_isced8_enr>0,2020,ifelse(yr2019_isced8_enr>0,2019,ifelse(yr2018_isced8_enr>0,2018,ifelse(yr2017_isced8_enr>0,2017,ifelse(yr2016_isced8_enr>0,2016,ifelse(yr2015_isced8_enr>0,2015,ifelse(yr2014_isced8_enr>0,2014,ifelse(yr2013_isced8_enr>0,2013,ifelse(yr2012_isced8_enr>0,2012,ifelse(yr2011_isced8_enr>0,2011,ifelse(yr2010_isced8_enr>0,2010,ifelse(yr2009_isced8_enr>0,2009,ifelse(yr2008_isced8_enr>0,2008,ifelse(yr2007_isced8_enr>0,2007,ifelse(yr2006_isced8_enr>0,2006,ifelse(yr2005_isced8_enr>0,2005,ifelse(yr2004_isced8_enr>0,2004,ifelse(yr2003_isced8_enr>0,2003,ifelse(yr2002_isced8_enr>0,2002,ifelse(yr2001_isced8_enr>0,2001,ifelse(yr2000_isced8_enr>0,2000,0)))))))))))))))))))))) %>%
  mutate(natpop_ref_yr=ifelse(yr2020_nat_pop>0,2020,ifelse(yr2019_nat_pop>0,2019,ifelse(yr2018_nat_pop>0,2018,ifelse(yr2017_nat_pop>0,2017,ifelse(yr2016_nat_pop>0,2016,ifelse(yr2015_nat_pop>0,2015,ifelse(yr2014_nat_pop>0,2014,ifelse(yr2013_nat_pop>0,2013,ifelse(yr2012_nat_pop>0,2012,ifelse(yr2011_nat_pop>0,2011,ifelse(yr2010_nat_pop>0,2010,ifelse(yr2009_nat_pop>0,2009,ifelse(yr2008_nat_pop>0,2008,ifelse(yr2007_nat_pop>0,2007,ifelse(yr2006_nat_pop>0,2006,ifelse(yr2005_nat_pop>0,2005,ifelse(yr2004_nat_pop>0,2004,ifelse(yr2003_nat_pop>0,2003,ifelse(yr2002_nat_pop>0,2002,ifelse(yr2001_nat_pop>0,2001,ifelse(yr2000_nat_pop>0,2000,0)))))))))))))))))))))) %>%
  mutate(isced6_enr=ifelse(yr2020_isced6_enr>0,yr2020_isced6_enr,ifelse(yr2019_isced6_enr>0,yr2019_isced6_enr,ifelse(yr2018_isced6_enr>0,yr2018_isced6_enr,ifelse(yr2017_isced6_enr>0,yr2017_isced6_enr,ifelse(yr2016_isced6_enr>0,yr2016_isced6_enr,ifelse(yr2015_isced6_enr>0,yr2015_isced6_enr,ifelse(yr2014_isced6_enr>0,yr2014_isced6_enr,ifelse(yr2013_isced6_enr>0,yr2013_isced6_enr,ifelse(yr2012_isced6_enr>0,yr2012_isced6_enr,ifelse(yr2011_isced6_enr>0,yr2011_isced6_enr,ifelse(yr2010_isced6_enr>0,yr2010_isced6_enr,ifelse(yr2009_isced6_enr>0,yr2009_isced6_enr,ifelse(yr2008_isced6_enr>0,yr2008_isced6_enr,ifelse(yr2007_isced6_enr>0,yr2007_isced6_enr,ifelse(yr2006_isced6_enr>0,yr2006_isced6_enr,ifelse(yr2005_isced6_enr>0,yr2005_isced6_enr,ifelse(yr2004_isced6_enr>0,yr2004_isced6_enr,ifelse(yr2003_isced6_enr>0,yr2003_isced6_enr,ifelse(yr2002_isced6_enr>0,yr2002_isced6_enr,ifelse(yr2001_isced6_enr>0,yr2001_isced6_enr,ifelse(yr2000_isced6_enr>0,yr2000_isced6_enr,0)))))))))))))))))))))) %>%
  mutate(isced7_enr=ifelse(yr2020_isced7_enr>0,yr2020_isced7_enr,ifelse(yr2019_isced7_enr>0,yr2019_isced7_enr,ifelse(yr2018_isced7_enr>0,yr2018_isced7_enr,ifelse(yr2017_isced7_enr>0,yr2017_isced7_enr,ifelse(yr2016_isced7_enr>0,yr2016_isced7_enr,ifelse(yr2015_isced7_enr>0,yr2015_isced7_enr,ifelse(yr2014_isced7_enr>0,yr2014_isced7_enr,ifelse(yr2013_isced7_enr>0,yr2013_isced7_enr,ifelse(yr2012_isced7_enr>0,yr2012_isced7_enr,ifelse(yr2011_isced7_enr>0,yr2011_isced7_enr,ifelse(yr2010_isced7_enr>0,yr2010_isced7_enr,ifelse(yr2009_isced7_enr>0,yr2009_isced7_enr,ifelse(yr2008_isced7_enr>0,yr2008_isced7_enr,ifelse(yr2007_isced7_enr>0,yr2007_isced7_enr,ifelse(yr2006_isced7_enr>0,yr2006_isced7_enr,ifelse(yr2005_isced7_enr>0,yr2005_isced7_enr,ifelse(yr2004_isced7_enr>0,yr2004_isced7_enr,ifelse(yr2003_isced7_enr>0,yr2003_isced7_enr,ifelse(yr2002_isced7_enr>0,yr2002_isced7_enr,ifelse(yr2001_isced7_enr>0,yr2001_isced7_enr,ifelse(yr2000_isced7_enr>0,yr2000_isced7_enr,0)))))))))))))))))))))) %>%
  mutate(isced8_enr=ifelse(yr2020_isced8_enr>0,yr2020_isced8_enr,ifelse(yr2019_isced8_enr>0,yr2019_isced8_enr,ifelse(yr2018_isced8_enr>0,yr2018_isced8_enr,ifelse(yr2017_isced8_enr>0,yr2017_isced8_enr,ifelse(yr2016_isced8_enr>0,yr2016_isced8_enr,ifelse(yr2015_isced8_enr>0,yr2015_isced8_enr,ifelse(yr2014_isced8_enr>0,yr2014_isced8_enr,ifelse(yr2013_isced8_enr>0,yr2013_isced8_enr,ifelse(yr2012_isced8_enr>0,yr2012_isced8_enr,ifelse(yr2011_isced8_enr>0,yr2011_isced8_enr,ifelse(yr2010_isced8_enr>0,yr2010_isced8_enr,ifelse(yr2009_isced8_enr>0,yr2009_isced8_enr,ifelse(yr2008_isced8_enr>0,yr2008_isced8_enr,ifelse(yr2007_isced8_enr>0,yr2007_isced8_enr,ifelse(yr2006_isced8_enr>0,yr2006_isced8_enr,ifelse(yr2005_isced8_enr>0,yr2005_isced8_enr,ifelse(yr2004_isced8_enr>0,yr2004_isced8_enr,ifelse(yr2003_isced8_enr>0,yr2003_isced8_enr,ifelse(yr2002_isced8_enr>0,yr2002_isced8_enr,ifelse(yr2001_isced8_enr>0,yr2001_isced8_enr,ifelse(yr2000_isced8_enr>0,yr2000_isced8_enr,0)))))))))))))))))))))) %>%
   mutate(natpop_est=ifelse(yr2020_nat_pop>0,yr2020_nat_pop,ifelse(yr2019_nat_pop>0,yr2019_nat_pop,ifelse(yr2018_nat_pop>0,yr2018_nat_pop,ifelse(yr2017_nat_pop>0,yr2017_nat_pop,ifelse(yr2016_nat_pop>0,yr2016_nat_pop,ifelse(yr2015_nat_pop>0,yr2015_nat_pop,ifelse(yr2014_nat_pop>0,yr2014_nat_pop,ifelse(yr2013_nat_pop>0,yr2013_nat_pop,ifelse(yr2012_nat_pop>0,yr2012_nat_pop,ifelse(yr2011_nat_pop>0,yr2011_nat_pop,ifelse(yr2010_nat_pop>0,yr2010_nat_pop,ifelse(yr2009_nat_pop>0,yr2009_nat_pop,ifelse(yr2008_nat_pop>0,yr2008_nat_pop,ifelse(yr2007_nat_pop>0,yr2007_nat_pop,ifelse(yr2006_nat_pop>0,yr2006_nat_pop,ifelse(yr2005_nat_pop>0,yr2005_nat_pop,ifelse(yr2004_nat_pop>0,yr2004_nat_pop,ifelse(yr2003_nat_pop>0,yr2003_nat_pop,ifelse(yr2002_nat_pop>0,yr2002_nat_pop,ifelse(yr2001_nat_pop>0,yr2001_nat_pop,ifelse(yr2000_nat_pop>0,yr2000_nat_pop,0)))))))))))))))))))))) %>%
  rowwise() %>%
  filter(!isced6_ref_yr+isced7_ref_yr+isced8_ref_yr==0) %>%
  select(country,isced6_enr,isced6_ref_yr,isced7_enr,isced7_ref_yr,isced8_enr,isced8_ref_yr,natpop_est,natpop_ref_yr) %>%
  filter_at(vars(country),all_vars(!. %in% c("Arab World","Caribbean small states","Central Europe and the Baltics","Early-demographic dividend","East Asia & Pacific","East Asia & Pacific (excluding high income)","East Asia & Pacific (IDA & IBRD countries","Euro area","Europe & Central Asia","Europe & Central Asia (excluding high income)","Europe & Central Asia (IDA & IBRD countries)","European Union","Fragile and conflict affected situations","Heavily indebted poor countries (HIPC)","High income","IBRD only","IDA & IBRD total","IDA blend","IDA only","IDA total","Late-demographic dividend","Latin America & Caribbean","Latin America & Caribbean (excluding high income)","Latin America & the Caribbean (IDA & IBRD countries)","Least developed countries: UN classification","Low & middle income","Low income","Lower middle income","Middle East & North Africa","Middle East & North Africa (excluding high income)","Middle East & North Africa (IDA & IBRD countries)","Middle income","North America","OECD members","Other small states","Pacific island small states","Post-demographic dividend","Pre-demographic dividend","Small states","South Asia","South Asia (IDA & IBRD)","Sub-Saharan Africa","Sub-Saharan Africa (excluding high income)","Upper middle income","World")))
university_enrollment_data 
```

    ## # A tibble: 172 × 9
    ## # Rowwise: 
    ##    country     isced6_…¹ isced…² isced…³ isced…⁴ isced…⁵ isced…⁶ natpo…⁷ natpo…⁸
    ##    <chr>           <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Afghanistan   365982     2018   4600     2018    28      2018  3.80e7    2019
    ##  2 Albania        89231     2019  43749     2019  1865      2019  2.87e6    2019
    ##  3 Algeria       996087     2018      0        0     0         0  4.31e7    2019
    ##  4 Andorra          554.    2019     35     2019    23.5    2019  7.7 e4    2019
    ##  5 Argentina    2210454     2017 276648     2017 26098      2017  4.49e7    2019
    ##  6 Armenia        69622     2019  10855     2019   985      2019  2.96e6    2019
    ##  7 Aruba            848     2016     55     2016     0         0  1.06e5    2019
    ##  8 Australia     999034     2018 304632     2018 56110      2018  2.53e7    2019
    ##  9 Austria       199236.    2018 135346.    2018 20396.     2018  8.86e6    2019
    ## 10 Azerbaijan    160631     2019  20954     2019  2626      2019  1.00e7    2019
    ## # … with 162 more rows, and abbreviated variable names ¹​isced6_enr,
    ## #   ²​isced6_ref_yr, ³​isced7_enr, ⁴​isced7_ref_yr, ⁵​isced8_enr, ⁶​isced8_ref_yr,
    ## #   ⁷​natpop_est, ⁸​natpop_ref_yr

Now, we can generate two new columns: one combining the ISCED 6, 7, and
8 enrollee estimates called `uni_enr_tot`, and another looking at the
proportion of the national population enrolled in university called
`uni_enr_prop`.

``` r
university_enrollment_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/university_enrollment_data.csv") %>%
  select(-Country.Code,-Series.Code) %>%
  as_tibble(university_enrollment_data) %>%
  slice(1:(n()-5)) %>%
  rename(country=Country.Name,series=Series,yr2000=YR2000,yr2001=YR2001,yr2002=YR2002,yr2003=YR2003,yr2004=YR2004,yr2005=YR2005,yr2006=YR2006,yr2007=YR2007,yr2008=YR2008,yr2009=YR2009,yr2010=YR2010,yr2011=YR2011,yr2012=YR2012,yr2013=YR2013,yr2014=YR2014,yr2015=YR2015,yr2016=YR2016,yr2017=YR2017,yr2018=YR2018,yr2019=YR2019,yr2020=YR2020) %>%
  mutate_at(c("yr2000","yr2001","yr2002","yr2003","yr2004","yr2005","yr2006","yr2007","yr2008","yr2009","yr2010","yr2011","yr2012","yr2013","yr2014","yr2015","yr2016","yr2017","yr2018","yr2019","yr2020"),as.double) %>%
  replace(is.na(.),0) %>%
  distinct() %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 6 programmes, both sexes (number)"),"isced6_enr")) %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 7 programmes, both sexes (number)"),"isced7_enr")) %>%
  mutate(across("series",str_replace, fixed("Enrolment in tertiary education, ISCED 8 programmes, both sexes (number)"),"isced8_enr")) %>%
  mutate(across("series",str_replace, fixed("Population, total"),"nat_pop")) %>%
  pivot_wider(names_from="series",
              values_from=c("yr2020","yr2019","yr2018","yr2017","yr2016","yr2015","yr2014","yr2013","yr2012","yr2011","yr2010","yr2009","yr2008","yr2007","yr2006","yr2005","yr2004","yr2003","yr2002","yr2001","yr2000")) %>%
  mutate(isced6_ref_yr=ifelse(yr2020_isced6_enr>0,2020,ifelse(yr2019_isced6_enr>0,2019,ifelse(yr2018_isced6_enr>0,2018,ifelse(yr2017_isced6_enr>0,2017,ifelse(yr2016_isced6_enr>0,2016,ifelse(yr2015_isced6_enr>0,2015,ifelse(yr2014_isced6_enr>0,2014,ifelse(yr2013_isced6_enr>0,2013,ifelse(yr2012_isced6_enr>0,2012,ifelse(yr2011_isced6_enr>0,2011,ifelse(yr2010_isced6_enr>0,2010,ifelse(yr2009_isced6_enr>0,2009,ifelse(yr2008_isced6_enr>0,2008,ifelse(yr2007_isced6_enr>0,2007,ifelse(yr2006_isced6_enr>0,2006,ifelse(yr2005_isced6_enr>0,2005,ifelse(yr2004_isced6_enr>0,2004,ifelse(yr2003_isced6_enr>0,2003,ifelse(yr2002_isced6_enr>0,2002,ifelse(yr2001_isced6_enr>0,2001,ifelse(yr2000_isced6_enr>0,2000,0)))))))))))))))))))))) %>%
  mutate(isced7_ref_yr=ifelse(yr2020_isced7_enr>0,2020,ifelse(yr2019_isced7_enr>0,2019,ifelse(yr2018_isced7_enr>0,2018,ifelse(yr2017_isced7_enr>0,2017,ifelse(yr2016_isced7_enr>0,2016,ifelse(yr2015_isced7_enr>0,2015,ifelse(yr2014_isced7_enr>0,2014,ifelse(yr2013_isced7_enr>0,2013,ifelse(yr2012_isced7_enr>0,2012,ifelse(yr2011_isced7_enr>0,2011,ifelse(yr2010_isced7_enr>0,2010,ifelse(yr2009_isced7_enr>0,2009,ifelse(yr2008_isced7_enr>0,2008,ifelse(yr2007_isced7_enr>0,2007,ifelse(yr2006_isced7_enr>0,2006,ifelse(yr2005_isced7_enr>0,2005,ifelse(yr2004_isced7_enr>0,2004,ifelse(yr2003_isced7_enr>0,2003,ifelse(yr2002_isced7_enr>0,2002,ifelse(yr2001_isced7_enr>0,2001,ifelse(yr2000_isced7_enr>0,2000,0)))))))))))))))))))))) %>%
  mutate(isced8_ref_yr=ifelse(yr2020_isced8_enr>0,2020,ifelse(yr2019_isced8_enr>0,2019,ifelse(yr2018_isced8_enr>0,2018,ifelse(yr2017_isced8_enr>0,2017,ifelse(yr2016_isced8_enr>0,2016,ifelse(yr2015_isced8_enr>0,2015,ifelse(yr2014_isced8_enr>0,2014,ifelse(yr2013_isced8_enr>0,2013,ifelse(yr2012_isced8_enr>0,2012,ifelse(yr2011_isced8_enr>0,2011,ifelse(yr2010_isced8_enr>0,2010,ifelse(yr2009_isced8_enr>0,2009,ifelse(yr2008_isced8_enr>0,2008,ifelse(yr2007_isced8_enr>0,2007,ifelse(yr2006_isced8_enr>0,2006,ifelse(yr2005_isced8_enr>0,2005,ifelse(yr2004_isced8_enr>0,2004,ifelse(yr2003_isced8_enr>0,2003,ifelse(yr2002_isced8_enr>0,2002,ifelse(yr2001_isced8_enr>0,2001,ifelse(yr2000_isced8_enr>0,2000,0)))))))))))))))))))))) %>%
  mutate(natpop_ref_yr=ifelse(yr2020_nat_pop>0,2020,ifelse(yr2019_nat_pop>0,2019,ifelse(yr2018_nat_pop>0,2018,ifelse(yr2017_nat_pop>0,2017,ifelse(yr2016_nat_pop>0,2016,ifelse(yr2015_nat_pop>0,2015,ifelse(yr2014_nat_pop>0,2014,ifelse(yr2013_nat_pop>0,2013,ifelse(yr2012_nat_pop>0,2012,ifelse(yr2011_nat_pop>0,2011,ifelse(yr2010_nat_pop>0,2010,ifelse(yr2009_nat_pop>0,2009,ifelse(yr2008_nat_pop>0,2008,ifelse(yr2007_nat_pop>0,2007,ifelse(yr2006_nat_pop>0,2006,ifelse(yr2005_nat_pop>0,2005,ifelse(yr2004_nat_pop>0,2004,ifelse(yr2003_nat_pop>0,2003,ifelse(yr2002_nat_pop>0,2002,ifelse(yr2001_nat_pop>0,2001,ifelse(yr2000_nat_pop>0,2000,0)))))))))))))))))))))) %>%
  mutate(isced6_enr=ifelse(yr2020_isced6_enr>0,yr2020_isced6_enr,ifelse(yr2019_isced6_enr>0,yr2019_isced6_enr,ifelse(yr2018_isced6_enr>0,yr2018_isced6_enr,ifelse(yr2017_isced6_enr>0,yr2017_isced6_enr,ifelse(yr2016_isced6_enr>0,yr2016_isced6_enr,ifelse(yr2015_isced6_enr>0,yr2015_isced6_enr,ifelse(yr2014_isced6_enr>0,yr2014_isced6_enr,ifelse(yr2013_isced6_enr>0,yr2013_isced6_enr,ifelse(yr2012_isced6_enr>0,yr2012_isced6_enr,ifelse(yr2011_isced6_enr>0,yr2011_isced6_enr,ifelse(yr2010_isced6_enr>0,yr2010_isced6_enr,ifelse(yr2009_isced6_enr>0,yr2009_isced6_enr,ifelse(yr2008_isced6_enr>0,yr2008_isced6_enr,ifelse(yr2007_isced6_enr>0,yr2007_isced6_enr,ifelse(yr2006_isced6_enr>0,yr2006_isced6_enr,ifelse(yr2005_isced6_enr>0,yr2005_isced6_enr,ifelse(yr2004_isced6_enr>0,yr2004_isced6_enr,ifelse(yr2003_isced6_enr>0,yr2003_isced6_enr,ifelse(yr2002_isced6_enr>0,yr2002_isced6_enr,ifelse(yr2001_isced6_enr>0,yr2001_isced6_enr,ifelse(yr2000_isced6_enr>0,yr2000_isced6_enr,0)))))))))))))))))))))) %>%
  mutate(isced7_enr=ifelse(yr2020_isced7_enr>0,yr2020_isced7_enr,ifelse(yr2019_isced7_enr>0,yr2019_isced7_enr,ifelse(yr2018_isced7_enr>0,yr2018_isced7_enr,ifelse(yr2017_isced7_enr>0,yr2017_isced7_enr,ifelse(yr2016_isced7_enr>0,yr2016_isced7_enr,ifelse(yr2015_isced7_enr>0,yr2015_isced7_enr,ifelse(yr2014_isced7_enr>0,yr2014_isced7_enr,ifelse(yr2013_isced7_enr>0,yr2013_isced7_enr,ifelse(yr2012_isced7_enr>0,yr2012_isced7_enr,ifelse(yr2011_isced7_enr>0,yr2011_isced7_enr,ifelse(yr2010_isced7_enr>0,yr2010_isced7_enr,ifelse(yr2009_isced7_enr>0,yr2009_isced7_enr,ifelse(yr2008_isced7_enr>0,yr2008_isced7_enr,ifelse(yr2007_isced7_enr>0,yr2007_isced7_enr,ifelse(yr2006_isced7_enr>0,yr2006_isced7_enr,ifelse(yr2005_isced7_enr>0,yr2005_isced7_enr,ifelse(yr2004_isced7_enr>0,yr2004_isced7_enr,ifelse(yr2003_isced7_enr>0,yr2003_isced7_enr,ifelse(yr2002_isced7_enr>0,yr2002_isced7_enr,ifelse(yr2001_isced7_enr>0,yr2001_isced7_enr,ifelse(yr2000_isced7_enr>0,yr2000_isced7_enr,0)))))))))))))))))))))) %>%
  mutate(isced8_enr=ifelse(yr2020_isced8_enr>0,yr2020_isced8_enr,ifelse(yr2019_isced8_enr>0,yr2019_isced8_enr,ifelse(yr2018_isced8_enr>0,yr2018_isced8_enr,ifelse(yr2017_isced8_enr>0,yr2017_isced8_enr,ifelse(yr2016_isced8_enr>0,yr2016_isced8_enr,ifelse(yr2015_isced8_enr>0,yr2015_isced8_enr,ifelse(yr2014_isced8_enr>0,yr2014_isced8_enr,ifelse(yr2013_isced8_enr>0,yr2013_isced8_enr,ifelse(yr2012_isced8_enr>0,yr2012_isced8_enr,ifelse(yr2011_isced8_enr>0,yr2011_isced8_enr,ifelse(yr2010_isced8_enr>0,yr2010_isced8_enr,ifelse(yr2009_isced8_enr>0,yr2009_isced8_enr,ifelse(yr2008_isced8_enr>0,yr2008_isced8_enr,ifelse(yr2007_isced8_enr>0,yr2007_isced8_enr,ifelse(yr2006_isced8_enr>0,yr2006_isced8_enr,ifelse(yr2005_isced8_enr>0,yr2005_isced8_enr,ifelse(yr2004_isced8_enr>0,yr2004_isced8_enr,ifelse(yr2003_isced8_enr>0,yr2003_isced8_enr,ifelse(yr2002_isced8_enr>0,yr2002_isced8_enr,ifelse(yr2001_isced8_enr>0,yr2001_isced8_enr,ifelse(yr2000_isced8_enr>0,yr2000_isced8_enr,0)))))))))))))))))))))) %>%
   mutate(natpop_est=ifelse(yr2020_nat_pop>0,yr2020_nat_pop,ifelse(yr2019_nat_pop>0,yr2019_nat_pop,ifelse(yr2018_nat_pop>0,yr2018_nat_pop,ifelse(yr2017_nat_pop>0,yr2017_nat_pop,ifelse(yr2016_nat_pop>0,yr2016_nat_pop,ifelse(yr2015_nat_pop>0,yr2015_nat_pop,ifelse(yr2014_nat_pop>0,yr2014_nat_pop,ifelse(yr2013_nat_pop>0,yr2013_nat_pop,ifelse(yr2012_nat_pop>0,yr2012_nat_pop,ifelse(yr2011_nat_pop>0,yr2011_nat_pop,ifelse(yr2010_nat_pop>0,yr2010_nat_pop,ifelse(yr2009_nat_pop>0,yr2009_nat_pop,ifelse(yr2008_nat_pop>0,yr2008_nat_pop,ifelse(yr2007_nat_pop>0,yr2007_nat_pop,ifelse(yr2006_nat_pop>0,yr2006_nat_pop,ifelse(yr2005_nat_pop>0,yr2005_nat_pop,ifelse(yr2004_nat_pop>0,yr2004_nat_pop,ifelse(yr2003_nat_pop>0,yr2003_nat_pop,ifelse(yr2002_nat_pop>0,yr2002_nat_pop,ifelse(yr2001_nat_pop>0,yr2001_nat_pop,ifelse(yr2000_nat_pop>0,yr2000_nat_pop,0)))))))))))))))))))))) %>%
  rowwise() %>%
  filter(!isced6_ref_yr+isced7_ref_yr+isced8_ref_yr==0) %>%
  select(country,isced6_enr,isced6_ref_yr,isced7_enr,isced7_ref_yr,isced8_enr,isced8_ref_yr,natpop_est,natpop_ref_yr) %>%
  filter_at(vars(country),all_vars(!. %in% c("Arab World","Caribbean small states","Central Europe and the Baltics","Early-demographic dividend","East Asia & Pacific","East Asia & Pacific (excluding high income)","East Asia & Pacific (IDA & IBRD countries","Euro area","Europe & Central Asia","Europe & Central Asia (excluding high income)","Europe & Central Asia (IDA & IBRD countries)","European Union","Fragile and conflict affected situations","Heavily indebted poor countries (HIPC)","High income","IBRD only","IDA & IBRD total","IDA blend","IDA only","IDA total","Late-demographic dividend","Latin America & Caribbean","Latin America & Caribbean (excluding high income)","Latin America & the Caribbean (IDA & IBRD countries)","Least developed countries: UN classification","Low & middle income","Low income","Lower middle income","Middle East & North Africa","Middle East & North Africa (excluding high income)","Middle East & North Africa (IDA & IBRD countries)","Middle income","North America","OECD members","Other small states","Pacific island small states","Post-demographic dividend","Pre-demographic dividend","Small states","South Asia","South Asia (IDA & IBRD)","Sub-Saharan Africa","Sub-Saharan Africa (excluding high income)","Upper middle income","World"))) %>%
  mutate(uni_enr_tot=isced6_enr+isced7_enr+isced8_enr) %>%
  mutate(uni_enr_pop=uni_enr_tot/natpop_est)
university_enrollment_data 
```

    ## # A tibble: 172 × 11
    ## # Rowwise: 
    ##    country     isced6_…¹ isced…² isced…³ isced…⁴ isced…⁵ isced…⁶ natpo…⁷ natpo…⁸
    ##    <chr>           <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Afghanistan   365982     2018   4600     2018    28      2018  3.80e7    2019
    ##  2 Albania        89231     2019  43749     2019  1865      2019  2.87e6    2019
    ##  3 Algeria       996087     2018      0        0     0         0  4.31e7    2019
    ##  4 Andorra          554.    2019     35     2019    23.5    2019  7.7 e4    2019
    ##  5 Argentina    2210454     2017 276648     2017 26098      2017  4.49e7    2019
    ##  6 Armenia        69622     2019  10855     2019   985      2019  2.96e6    2019
    ##  7 Aruba            848     2016     55     2016     0         0  1.06e5    2019
    ##  8 Australia     999034     2018 304632     2018 56110      2018  2.53e7    2019
    ##  9 Austria       199236.    2018 135346.    2018 20396.     2018  8.86e6    2019
    ## 10 Azerbaijan    160631     2019  20954     2019  2626      2019  1.00e7    2019
    ## # … with 162 more rows, 2 more variables: uni_enr_tot <dbl>, uni_enr_pop <dbl>,
    ## #   and abbreviated variable names ¹​isced6_enr, ²​isced6_ref_yr, ³​isced7_enr,
    ## #   ⁴​isced7_ref_yr, ⁵​isced8_enr, ⁶​isced8_ref_yr, ⁷​natpop_est, ⁸​natpop_ref_yr

## Spatially Joining Our Dietary Footprint and University Enrollment Data

Now that both of our parent datasets are in formats that align with the
needs of our analysis, we can begin the necessary preparations for our
spatial join. Before performing this operation, we will first need to
identify the inconsistencies in the `region` and `country` designations
between our three data sources.

To accomplish this, we will begin by investigating the differences
between our university enrollment and dietary footprint data, first by
examining the countries included in `dietary_footprint_data` but not
`university_enrollment_data` and then by examining the countries
included in the `university_enrollment_data` but not
`dietary_footprint_data`.

``` r
anti_join(dietary_footprint_data,university_enrollment_data,by="country")
```

    ## # A tibble: 35 × 46
    ##    country       basel…¹ basel…² basel…³ basel…⁴ basel…⁵ meatl…⁶ meatl…⁷ meatl…⁸
    ##    <chr>           <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Antigua and …   1310.   1400.  1.19e6  77859.  1.11e6   1642.   1740.  1.44e6
    ##  2 Bahamas         1727.   1782.  1.03e6 154346.  8.73e5   1980.   2037.  1.15e6
    ##  3 Bolivia (Plu…   2138.   2654.  2.63e6  62642.  2.56e6   2564.   3285.  3.19e6
    ##  4 Solomon Isla…    650.    658.  6.53e5 130067.  5.23e5    905.    920.  8.71e5
    ##  5 Central Afri…   2508.   2512.  1.16e6  61010.  1.10e6   3551.   3559.  1.51e6
    ##  6 China, mainl…   1191.   1258.  8.37e5 135411.  7.02e5   1089.   1151.  7.87e5
    ##  7 Congo            639.    678.  9.92e5  78014.  9.14e5    989.   1061.  1.34e6
    ##  8 Dominica        1181.   1217.  2.37e6 105926.  2.26e6   1372.   1413.  2.14e6
    ##  9 Egypt           1215.   1283.  1.09e6 565678.  5.23e5   1011.   1061.  8.37e5
    ## 10 French Polyn…   1922.   1994.  1.05e6 114577.  9.32e5   1986.   2055.  1.06e6
    ## # … with 25 more rows, 37 more variables: meatless_day_l_blue_wf_total <dbl>,
    ## #   meatless_day_l_green_wf <dbl>, no_dairy_kg_co2e_excl_luc <dbl>,
    ## #   no_dairy_kg_co2e_total <dbl>, no_dairy_l_blue_green_wf <dbl>,
    ## #   no_dairy_l_blue_wf_total <dbl>, no_dairy_l_green_wf <dbl>,
    ## #   low_red_meat_kg_co2e_excl_luc <dbl>, low_red_meat_kg_co2e_total <dbl>,
    ## #   low_red_meat_l_blue_green_wf <dbl>, low_red_meat_l_blue_wf_total <dbl>,
    ## #   low_red_meat_l_green_wf <dbl>, no_red_meat_kg_co2e_excl_luc <dbl>, …

Of the 35 cases where there was an indexed country name appearing within
`dietary_footprint_data` but not `university_enrollment_data`, there
were x instances where the mismatch were due, simply, to differences in
how countries were named. Said differently, this group of cases refers
to countries that were included in both cleaned datasets, but were
indexed under different names. Using how they were listed in
`dietary_footprint_data`, we name each of these instances below (n=18):

-   “China, mainland”
-   “Congo”
-   “Egypt”
-   “China, Hong Kong SAR”
-   “Iran (Islamic Republic of)”
-   “CÃ´te d’Ivoire”
-   “Kyrgyzstan”
-   “Republic of Korea”
-   “China, Macao SAR”
-   “Republic of Moldova”
-   “New Caledonia”
-   “The former Yugoslav Republic of Macedonia”
-   “Czechia”
-   “Slovakia”
-   “United Republic of Tanzania”
-   “United States of America”
-   “Venezuela (Bolivarian Republic of)”
-   “Yemen”

Was in but removed (n=15):

-   “Antigua and Barbuda”
-   “Bahamas”
-   “Solomon Islands”
-   “Central African Republic”
-   “Dominica”
-   “French Polynesia”
-   “Gambia”
-   “Kiribati”
-   “Guyana”
-   “Vanuatu”
-   “Nicaragua”
-   “Paraguay”
-   “Sao Tome and Principe”
-   “Suriname”
-   “Zambia”

Option to supplement (n=2):

-   “Bolivia (Plurinational State of)”
-   “China, Taiwan Province of”

will need to make systematized decisions about whether to (a) supplement
the data made available through the EdStats database with
government-provided enrollment data or (b) remove the

``` r
anti_join(university_enrollment_data,dietary_footprint_data,by="country")
```

    ## # A tibble: 67 × 11
    ## # Rowwise: 
    ##    country       isced…¹ isced…² isced…³ isced…⁴ isced…⁵ isced…⁶ natpo…⁷ natpo…⁸
    ##    <chr>           <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Andorra        5.54e2    2019      35    2019  2.35e1    2019  7.7 e4    2019
    ##  2 Aruba          8.48e2    2016      55    2016  0            0  1.06e5    2019
    ##  3 Bahrain        4.00e4    2019    4035    2019  2.68e2    2019  1.64e6    2019
    ##  4 Bangladesh     2.92e6    2019  563065    2019  1.32e4    2018  1.63e8    2019
    ##  5 Bhutan         1.04e4    2020     398    2020  0            0  7.63e5    2019
    ##  6 Burundi        4.03e4    2018    1262    2018  3.07e2    2018  1.15e7    2019
    ##  7 Chad           3.83e4    2015    2348    2015  1.95e2    2015  1.59e7    2019
    ##  8 China          2.41e7    2019 2409068    2019  4.10e5    2019  1.40e9    2019
    ##  9 Comoros        5.45e3    2014      87    2013  0            0  8.51e5    2019
    ## 10 Congo, Dem. …  3.69e5    2016   93444    2016  2.07e3    2016  8.68e7    2019
    ## # … with 57 more rows, 2 more variables: uni_enr_tot <dbl>, uni_enr_pop <dbl>,
    ## #   and abbreviated variable names ¹​isced6_enr, ²​isced6_ref_yr, ³​isced7_enr,
    ## #   ⁴​isced7_ref_yr, ⁵​isced8_enr, ⁶​isced8_ref_yr, ⁷​natpop_est, ⁸​natpop_ref_yr

Of the 67 cases that fall into this first category, we will need to make
systematized decisions about whether to (a) supplement the data made
available through the EdStats database with government-provided
enrollment data or (b) remove the

From these two lists, we will need to construct a systematized process
for reconciling two types of cases: countries that need to be adjusted
and countries that will be removed from our analysis.

We will first identify the instances where there are differences in the
naming conventions between the two countries.

We will then identify the instances where there is data for the country
in one data source but not the other.

## Writing the Final Data File
