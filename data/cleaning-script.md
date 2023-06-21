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

## Spatially Joining Our Dietary Footprint Data

map gap

## Loading in Our University Enrollment Data

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
source to our aggregated dataset, we will need to make it so that each
row represents a single country, and that there is a column designated
for each of the following variables of interest: (1) the total number of
students enrolled in ISCED 6 programs, (2) the total number of students
enrolled in ISCED 7 programs, (3) the total number of students enrolled
in ISCED 8 programs, (4) the total number of people living within the
country, and (5) the reference year being used for each of these
variables.

We will begin this process by selecting out the `country.code` and
`series.code` columns and renaming the `country.name`, `series`, and
reference year variables.

``` r
university_enrollment_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/university_enrollment_data.csv") %>%
  select(-Country.Code,-Series.Code) %>%
   as_tibble(university_enrollment_data) %>%
  rename(country=Country.Name,series=Series,yr2000=YR2000,yr2001=YR2001,yr2002=YR2002,yr2003=YR2003,yr2004=YR2004,yr2005=YR2005,yr2006=YR2006,yr2007=YR2007,yr2008=YR2008,yr2009=YR2009,yr2010=YR2010,yr2011=YR2011,yr2012=YR2012,yr2013=YR2013,yr2014=YR2014,yr2015=YR2015,yr2016=YR2016,yr2017=YR2017,yr2018=YR2018,yr2019=YR2019,yr2020=YR2020)
university_enrollment_data %>%
  head(6)
```

    ## # A tibble: 6 × 23
    ##   country  series yr2000 yr2001 yr2002 yr2003 yr2004 yr2005 yr2006 yr2007 yr2008
    ##   <chr>    <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
    ## 1 Afghani… Popul… 20779… 21606… 22600… 23680… 24726… 25654… 26433… 27100… 27722…
    ## 2 Afghani… Enrol… ..     ..     ..     ..     ..     ..     ..     ..     ..    
    ## 3 Afghani… Enrol… ..     ..     ..     ..     ..     ..     ..     ..     ..    
    ## 4 Afghani… Enrol… ..     ..     ..     ..     ..     ..     ..     ..     ..    
    ## 5 Albania  Popul… 30890… 30601… 30510… 30396… 30269… 30114… 29925… 29700… 29473…
    ## 6 Albania  Enrol… ..     ..     ..     ..     ..     ..     ..     ..     ..    
    ## # … with 12 more variables: yr2009 <chr>, yr2010 <chr>, yr2011 <chr>,
    ## #   yr2012 <chr>, yr2013 <chr>, yr2014 <chr>, yr2015 <chr>, yr2016 <chr>,
    ## #   yr2017 <chr>, yr2018 <chr>, yr2019 <chr>, yr2020 <chr>

With this complete, we now need to convert the reference year columns
from character strings (`chr`) to doubles (`dbl`), which will
coincidently convert the default `--` entries to `NA` values.

``` r
university_enrollment_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/university_enrollment_data.csv") %>%
  select(-Country.Code,-Series.Code) %>%
   as_tibble(university_enrollment_data) %>%
  rename(country=Country.Name,series=Series,yr2000=YR2000,yr2001=YR2001,yr2002=YR2002,yr2003=YR2003,yr2004=YR2004,yr2005=YR2005,yr2006=YR2006,yr2007=YR2007,yr2008=YR2008,yr2009=YR2009,yr2010=YR2010,yr2011=YR2011,yr2012=YR2012,yr2013=YR2013,yr2014=YR2014,yr2015=YR2015,yr2016=YR2016,yr2017=YR2017,yr2018=YR2018,yr2019=YR2019,yr2020=YR2020) %>%
  mutate_at(c("yr2000","yr2001","yr2002","yr2003","yr2004","yr2005","yr2006","yr2007","yr2008","yr2009","yr2010","yr2011","yr2012","yr2013","yr2014","yr2015","yr2016","yr2017","yr2018","yr2019","yr2020"),as.double)
```

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

``` r
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

Because we want to use the available data from the most recently
provided record, we will need to construct two new columns: one that
pulls the numeric quantity of students enrolled from each class of ISCED
programs and a second documenting the reference year being used. In
order to do so, we will first need to convert all `NA` values to `0`.

``` r
university_enrollment_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/parent-datasets/university_enrollment_data.csv") %>%
  select(-Country.Code,-Series.Code) %>%
   as_tibble(university_enrollment_data) %>%
  rename(country=Country.Name,series=Series,yr2000=YR2000,yr2001=YR2001,yr2002=YR2002,yr2003=YR2003,yr2004=YR2004,yr2005=YR2005,yr2006=YR2006,yr2007=YR2007,yr2008=YR2008,yr2009=YR2009,yr2010=YR2010,yr2011=YR2011,yr2012=YR2012,yr2013=YR2013,yr2014=YR2014,yr2015=YR2015,yr2016=YR2016,yr2017=YR2017,yr2018=YR2018,yr2019=YR2019,yr2020=YR2020) %>%
  mutate_at(c("yr2000","yr2001","yr2002","yr2003","yr2004","yr2005","yr2006","yr2007","yr2008","yr2009","yr2010","yr2011","yr2012","yr2013","yr2014","yr2015","yr2016","yr2017","yr2018","yr2019","yr2020"),as.double) %>%
  mutate(across(where(is.numeric),coalesce,0)) %>%
  mutate(enrollment_total="yr2020")
```

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

``` r
university_enrollment_data %>%
  head(6)
```

    ## # A tibble: 6 × 24
    ##   country  series yr2000 yr2001 yr2002 yr2003 yr2004 yr2005 yr2006 yr2007 yr2008
    ##   <chr>    <chr>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 Afghani… Popul… 2.08e7 2.16e7 2.26e7 2.37e7 2.47e7 2.57e7 2.64e7 2.71e7 2.77e7
    ## 2 Afghani… Enrol… 0      0      0      0      0      0      0      0      0     
    ## 3 Afghani… Enrol… 0      0      0      0      0      0      0      0      0     
    ## 4 Afghani… Enrol… 0      0      0      0      0      0      0      0      0     
    ## 5 Albania  Popul… 3.09e6 3.06e6 3.05e6 3.04e6 3.03e6 3.01e6 2.99e6 2.97e6 2.95e6
    ## 6 Albania  Enrol… 0      0      0      0      0      0      0      0      0     
    ## # … with 13 more variables: yr2009 <dbl>, yr2010 <dbl>, yr2011 <dbl>,
    ## #   yr2012 <dbl>, yr2013 <dbl>, yr2014 <dbl>, yr2015 <dbl>, yr2016 <dbl>,
    ## #   yr2017 <dbl>, yr2018 <dbl>, yr2019 <dbl>, yr2020 <dbl>,
    ## #   enrollment_total <chr>

%\>% mutate(enrollment_total=case_when(yr2020!=0))

## Spatially Joining Our University Enrollment Data

map gap

## Writing the Final Data File
