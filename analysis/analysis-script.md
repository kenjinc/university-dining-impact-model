Analysis and Visualization Script
================

## Required Packages

``` r
library(tidyverse)
library(RColorBrewer)
library(colorspace)
```

## Data Loading

``` r
impact_modeling_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/data/impact-modeling-data.csv")
```

## Included countries

To distinguish the 123 included countries from within the 135 designated
countries within the `world_map_iso` data without available university
enrollment and dietary footprint data, we need to construct a new column
that can aid with this need,

``` r
impact_modeling_data %>%
  mutate(inclusion=ifelse(uni_enr_tot>0,country)) %>%
  ggplot(aes(x=long,y=lat,fill=inclusion,group=group)) + 
  geom_polygon(color="black",size=0.05,alpha=0.33) +
  scale_fill_discrete(h=c(260,260),na.value="white") +
  guides(fill="none") +
  xlab("") + 
  ylab("") +
  labs(caption="") +
  ggtitle("Figure 1. Choropleth map highlighting the 123 countries included in our analyses.") +
  theme(legend.position="none",panel.grid=element_blank(),panel.background=element_rect(fill="aliceblue"),panel.border=element_rect(fill=NA),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Comparing university enrollment totals across countries.

``` r
ggplot(impact_modeling_data,aes(x=long,y=lat,fill=uni_enr_tot,group=group)) + 
  geom_polygon(color="black",size=0.05) + 
  scale_fill_distiller(name="",palette="Purples",trans="reverse",na.value="white",labels=scales::comma) +
  guides(fill=guide_colorbar(reverse=TRUE)) + 
  xlab("") + 
  ylab("") +
  labs(caption="") +
  ggtitle("Figure 2. Choropleth map comparing university enrollment estimates across the 123 included countries.") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="aliceblue"),panel.border=element_rect(fill=NA),axis.text=element_blank(),axis.ticks=element_blank(),legend.key.width=unit(2,"cm"))
```

![](analysis-script_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Comparing university enrollment proportions (university enrollees as a
function of the national population) across countries.

``` r
ggplot(impact_modeling_data,aes(x=long,y=lat,fill=uni_enr_prop,group=group)) + 
  geom_polygon(color="black",size=0.05) + 
  scale_fill_distiller(name="",palette="Purples",trans="reverse",na.value="white",labels=scales::percent) +
  guides(fill=guide_colorbar(reverse=TRUE)) + 
  xlab("") + 
  ylab("") +
  labs(caption="") +
  ggtitle("Figure 4. Choropleth map comparing university enrollment estimates as a proportion of their respective national populations across the 123 included countries.") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="aliceblue"),panel.border=element_rect(fill=NA),axis.text=element_blank(),axis.ticks=element_blank(),legend.key.width=unit(2,"cm"))
```

![](analysis-script_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Comparing per capita baseline carbon footprint (kg co2e) across
countries

``` r
ggplot(impact_modeling_data,aes(x=long,y=lat,fill=baseline_kg_co2e_total,group=group)) + 
  geom_polygon(color="black",size=0.05,alpha=0.9) + 
  scale_fill_continuous_sequential(name="",palette="OrRd",na.value="white",labels=scales::comma) +
  guides() +
  xlab("") + 
  ylab("") +
  labs(caption="") +
  ggtitle("Figure 5. Choropleth map comparing annual per capita dietary greenhouse gas estimates (kg CO2e) at baseline across the 123 included countries.") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="aliceblue"),panel.border=element_rect(fill=NA),axis.text=element_blank(),axis.ticks=element_blank(),legend.key.width=unit(2,"cm"))
```

![](analysis-script_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Comparing per capita water footprint of baseline diet across countries.

``` r
ggplot(impact_modeling_data,aes(x=long,y=lat,fill=baseline_l_blue_green_wf,group=group)) + 
  geom_polygon(color="black",size=0.05,alpha=0.9) + 
  scale_fill_continuous_sequential(name="",palette="BluGrn",na.value="white",labels=scales::comma) +
  guides() +
  xlab("") + 
  ylab("") +
  labs(caption="") +
  ggtitle("Figure 6. Choropleth map comparing annual per capita dietary water footprint (L) at baseline across the 123 included countries.") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_rect(fill="aliceblue"),panel.border=element_rect(fill=NA),axis.text=element_blank(),axis.ticks=element_blank(),legend.key.width=unit(2,"cm"))
```

![](analysis-script_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Reduction Modeling

In order to examine the mitigation potential associated with each of the
explored dietary scenarios, we will need to assemble population-level
estimates by multiplying each country’s annual per capita baseline
estimates against their respective enrollment totals.

To begin this process, we will first create an accessory dataframe
dropping the relevant shapefile and map data.

``` r
reduction_modeling_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/data/impact-modeling-data.csv") %>%
  distinct(country,isced6_enr,isced7_enr,isced8_enr,natpop_est,uni_enr_tot,uni_enr_prop,baseline_kg_co2e_excl_luc,baseline_kg_co2e_total,baseline_l_blue_green_wf,baseline_l_blue_wf_total,baseline_l_green_wf
,meatless_day_kg_co2e_excl_luc,meatless_day_kg_co2e_total,meatless_day_l_blue_green_wf,meatless_day_l_blue_wf_total,meatless_day_l_green_wf,no_dairy_kg_co2e_excl_luc,no_dairy_kg_co2e_total,no_dairy_l_blue_green_wf,no_dairy_l_blue_wf_total,no_dairy_l_green_wf,low_red_meat_kg_co2e_excl_luc,low_red_meat_kg_co2e_total,low_red_meat_l_blue_green_wf,low_red_meat_l_blue_wf_total,low_red_meat_l_green_wf,no_red_meat_kg_co2e_excl_luc,no_red_meat_kg_co2e_total,no_red_meat_l_blue_green_wf,no_red_meat_l_blue_wf_total,no_red_meat_l_green_wf,pescetarian_kg_co2e_excl_luc,pescetarian_kg_co2e_total,pescetarian_l_blue_green_wf,pescetarian_l_blue_wf_total,pescetarian_l_green_wf,lacto_ovo_vegetarian_kg_co2e_excl_luc,lacto_ovo_vegetarian_kg_co2e_total,lacto_ovo_vegetarian_l_blue_green_wf,lacto_ovo_vegetarian_l_blue_wf_total,lacto_ovo_vegetarian_l_green_wf,X2.3_vegan_kg_co2e_excl_luc,X2.3_vegan_kg_co2e_total,X2.3_vegan_l_blue_green_wf,X2.3_vegan_l_blue_wf_total,X2.3_vegan_l_green_wf,vegan_kg_co2e_excl_luc,vegan_kg_co2e_total,vegan_l_blue_green_wf,vegan_l_blue_wf_total,vegan_l_green_wf) %>%
  drop_na()
```

With this complete, we will need to construct 9 new variables: one
isolating the per capita carbon footprint attributable to land-use
change for each of our dietary scenarios.

``` r
reduction_modeling_data <- reduction_modeling_data %>% mutate(baseline_kg_co2e_luc=baseline_kg_co2e_total-baseline_kg_co2e_excl_luc) %>%
  mutate(meatless_day_kg_co2e_luc=meatless_day_kg_co2e_total-meatless_day_kg_co2e_excl_luc) %>%
  mutate(no_dairy_kg_co2e_luc=no_dairy_kg_co2e_total-no_dairy_kg_co2e_excl_luc) %>%
  mutate(low_red_meat_kg_co2e_luc=low_red_meat_kg_co2e_total-low_red_meat_kg_co2e_excl_luc) %>%
  mutate(no_red_meat_kg_co2e_luc=no_red_meat_kg_co2e_total-no_red_meat_kg_co2e_excl_luc) %>%
  mutate(pescetarian_kg_co2e_luc=pescetarian_kg_co2e_total-pescetarian_kg_co2e_excl_luc) %>%
  mutate(lacto_ovo_vegetarian_kg_co2e_luc=lacto_ovo_vegetarian_kg_co2e_total-lacto_ovo_vegetarian_kg_co2e_excl_luc) %>%
  mutate(X2.3_vegan_kg_co2e_luc=X2.3_vegan_kg_co2e_total-X2.3_vegan_kg_co2e_excl_luc) %>%
  mutate(vegan_kg_co2e_luc=vegan_kg_co2e_total-vegan_kg_co2e_excl_luc)
```

With these new variables in place, we can generate population-level
estimates by scaling each of these per capita estimates according to the
university enrollment totals for each country.

``` r
reduction_modeling_data <- reduction_modeling_data %>%
  mutate(pop_baseline_excl_luc=baseline_kg_co2e_excl_luc*uni_enr_tot) %>%
  mutate(pop_baseline_kg_co2e_total=baseline_kg_co2e_total*uni_enr_tot) %>%
  mutate(pop_baseline_kg_co2e_luc=baseline_kg_co2e_luc*uni_enr_tot) %>%
  mutate(pop_baseline_l_blue_green_wf=baseline_l_blue_green_wf*uni_enr_tot) %>%
  mutate(pop_baseline_l_blue_wf_total=baseline_l_blue_wf_total*uni_enr_tot) %>%
  mutate(pop_baseline_l_green_wf=baseline_l_green_wf*uni_enr_tot) %>%
  mutate(pop_meatless_day_kg_co2e_excl_luc=meatless_day_kg_co2e_excl_luc*uni_enr_tot) %>%
  mutate(pop_meatless_day_kg_co2e_total=meatless_day_kg_co2e_total*uni_enr_tot) %>%
  mutate(pop_meatless_day_kg_co2e_luc=meatless_day_kg_co2e_luc*uni_enr_tot) %>%
  mutate(pop_meatless_day_l_blue_green_wf=meatless_day_l_blue_green_wf*uni_enr_tot) %>%
  mutate(pop_meatless_day_l_blue_wf_total=meatless_day_l_blue_wf_total*uni_enr_tot) %>%
  mutate(pop_meatless_day_l_green_wf=meatless_day_l_green_wf*uni_enr_tot) %>%
  mutate(pop_no_dairy_kg_co2e_excl_luc=no_dairy_kg_co2e_excl_luc*uni_enr_tot) %>%
  mutate(pop_no_dairy_kg_co2e_total=no_dairy_kg_co2e_total*uni_enr_tot) %>%
  mutate(pop_no_dairy_kg_co2e_luc=no_dairy_kg_co2e_luc*uni_enr_tot) %>%
  mutate(pop_no_dairy_l_blue_green_wf=no_dairy_l_blue_green_wf*uni_enr_tot) %>%
  mutate(pop_no_dairy_l_blue_wf_total=no_dairy_l_blue_wf_total*uni_enr_tot) %>%
  mutate(pop_no_dairy_l_green_wf=no_dairy_l_green_wf*uni_enr_tot) %>%
  mutate(pop_low_red_meat_kg_co2e_excl_luc=low_red_meat_kg_co2e_excl_luc*uni_enr_tot) %>%
  mutate(pop_low_red_meat_kg_co2e_total=low_red_meat_kg_co2e_total*uni_enr_tot) %>%
  mutate(pop_low_red_meat_kg_co2e_luc=low_red_meat_kg_co2e_luc*uni_enr_tot) %>%
  mutate(pop_low_red_meat_l_blue_green_wf=low_red_meat_l_blue_green_wf*uni_enr_tot) %>%
  mutate(pop_low_red_meat_l_blue_wf_total=low_red_meat_l_blue_wf_total*uni_enr_tot) %>%
  mutate(pop_low_red_meat_l_green_wf=low_red_meat_l_green_wf*uni_enr_tot) %>%
  mutate(pop_no_red_meat_kg_co2e_excl_luc=no_red_meat_kg_co2e_excl_luc*uni_enr_tot) %>%
  mutate(pop_no_red_meat_kg_co2e_total=no_red_meat_kg_co2e_total*uni_enr_tot) %>%
  mutate(pop_no_red_meat_kg_co2e_luc=no_red_meat_kg_co2e_luc*uni_enr_tot) %>%
  mutate(pop_no_red_meat_l_blue_green_wf=no_red_meat_l_blue_green_wf*uni_enr_tot) %>%
  mutate(pop_no_red_meat_l_blue_wf_total=no_red_meat_l_blue_wf_total*uni_enr_tot) %>%
  mutate(pop_no_red_meat_l_green_wf=no_red_meat_l_green_wf*uni_enr_tot) %>%
  mutate(pop_pescetarian_kg_co2e_excl_luc=pescetarian_kg_co2e_excl_luc*uni_enr_tot) %>%
  mutate(pop_pescetarian_kg_co2e_total=pescetarian_kg_co2e_total*uni_enr_tot) %>%
  mutate(pop_pescetarian_kg_co2e_luc=pescetarian_kg_co2e_luc*uni_enr_tot) %>%
  mutate(pop_pescetarian_l_blue_green_wf=pescetarian_l_blue_green_wf*uni_enr_tot) %>%
  mutate(pop_pescetarian_l_blue_wf_total=pescetarian_l_blue_wf_total*uni_enr_tot) %>%
  mutate(pop_pescetarian_l_green_wf=pescetarian_l_green_wf*uni_enr_tot) %>%
  mutate(pop_lacto_ovo_vegetarian_kg_co2e_excl_luc=lacto_ovo_vegetarian_kg_co2e_excl_luc*uni_enr_tot) %>%
  mutate(pop_lacto_ovo_vegetarian_kg_co2e_total=lacto_ovo_vegetarian_kg_co2e_total*uni_enr_tot) %>%
  mutate(pop_lacto_ovo_vegetarian_kg_co2e_luc=lacto_ovo_vegetarian_kg_co2e_luc*uni_enr_tot) %>%
  mutate(pop_lacto_ovo_vegetarian_l_blue_green_wf=lacto_ovo_vegetarian_l_blue_green_wf*uni_enr_tot) %>%
  mutate(pop_lacto_ovo_vegetarian_l_blue_wf_total=lacto_ovo_vegetarian_l_blue_wf_total*uni_enr_tot) %>%
  mutate(pop_lacto_ovo_vegetarian_l_green_wf=lacto_ovo_vegetarian_l_green_wf*uni_enr_tot) %>%
  mutate(pop_X2.3_vegan_kg_co2e_excl_luc=X2.3_vegan_kg_co2e_excl_luc*uni_enr_tot) %>%
  mutate(pop_X2.3_vegan_kg_co2e_total=X2.3_vegan_kg_co2e_total*uni_enr_tot) %>%
  mutate(pop_X2.3_vegan_kg_co2e_luc=X2.3_vegan_kg_co2e_luc*uni_enr_tot) %>%
  mutate(pop_X2.3_vegan_l_blue_green_wf=X2.3_vegan_l_blue_green_wf*uni_enr_tot) %>%
  mutate(pop_X2.3_vegan_l_blue_wf_total=X2.3_vegan_l_blue_wf_total*uni_enr_tot) %>%
  mutate(pop_X2.3_vegan_l_green_wf=X2.3_vegan_l_green_wf*uni_enr_tot) %>%
  mutate(pop_vegan_kg_co2e_excl_luc=vegan_kg_co2e_excl_luc*uni_enr_tot) %>%
  mutate(pop_vegan_kg_co2e_total=vegan_kg_co2e_total*uni_enr_tot) %>%
  mutate(pop_vegan_kg_co2e_luc=vegan_kg_co2e_luc*uni_enr_tot) %>%
  mutate(pop_vegan_l_blue_green_wf=vegan_l_blue_green_wf*uni_enr_tot) %>%
  mutate(pop_vegan_l_blue_wf_total=vegan_l_blue_wf_total*uni_enr_tot) %>%
  mutate(pop_vegan_l_green_wf=vegan_l_green_wf*uni_enr_tot)
```

With these newly constructed population-level estimates, we can now
model how each of these indicators would change in response to the
proposed dietary scenarios. To do so, we will subtract each of the
population-level estimates associated the eight identified dietary
scenarios from the corresponding baseline statistic for each country.

``` r
reduction_modeling_data %>% mutate(red_pop_meatless_day_kg_co2e_excl_luc=pop_baseline_excl_luc-pop_meatless_day_kg_co2e_excl_luc)
```

    ##                    country  isced6_enr isced7_enr   isced8_enr natpop_est
    ## 1              Afghanistan   365982.00    4600.00     28.00000   38042000
    ## 2                  Albania    89231.00   43749.00   1865.00000    2867000
    ## 3                  Algeria   996087.00       0.00      0.00000   43053000
    ## 4                Argentina  2210454.00  276648.00  26098.00000   44901000
    ## 5                  Armenia    69622.00   10855.00    985.00000    2958000
    ## 6                Australia   999034.00  304632.00  56110.00000   25303000
    ## 7                  Austria   199235.73  135346.11  20396.37352    8865000
    ## 8               Azerbaijan   160631.00   20954.00   2626.00000   10032000
    ## 9                 Barbados        0.00       0.00    133.00000     287000
    ## 10                 Belarus   284348.00   14947.00   5652.00000    9467000
    ## 11                 Belgium   372686.00  102785.00  17339.00000   11483000
    ## 12                  Belize     5383.00       0.00      0.00000     390000
    ## 13                   Benin        0.00   12657.00   2060.00000   11801000
    ## 14                 Bermuda        8.00       0.00      0.00000      63973
    ## 15  Bosnia and Herzegovina    67031.00   21004.00    981.00000    3301000
    ## 16                Botswana    34515.00    2266.00    123.00000    2304000
    ## 17                  Brazil  8450755.00  175220.00 115028.00000  211050000
    ## 18                  Brunei     6782.00    1027.00    356.00000     433000
    ## 19                Bulgaria   155091.00   74680.00   6564.00000    6970000
    ## 20            Burkina Faso    43145.00   33243.00   1382.00000   20321000
    ## 21                Cambodia   179258.00   23256.00   1349.00000   16487000
    ## 22                Cameroon   224903.00   62008.00   7258.00000   25876000
    ## 23                  Canada  1005055.00  202679.00  53455.00000   37375000
    ## 24              Cape Verde    10988.00     370.00     62.00000     550000
    ## 25                   Chile   805053.22   96619.92   5789.00000   18952000
    ## 26                   China 24075437.00 2409068.00 410153.00000 1397295000
    ## 27                Colombia  1557594.00  157581.00   6225.00000   50339000
    ## 28        Congo, Dem. Rep.   369160.00   93444.00   2074.00000   86791000
    ## 29              Costa Rica   178008.00   14180.00   2805.00000    5048000
    ## 30           Cote d'Ivoire   101758.00   33982.00   7190.00000   25717000
    ## 31                 Croatia    97605.00   63586.00   3584.00000    4071000
    ## 32                  Cyprus    22615.00   18434.00   1472.00000    1199000
    ## 33          Czech Republic   193614.00  111986.00  22457.00000   10629000
    ## 34                 Denmark   195846.00   70048.00   9437.00000    5818000
    ## 35                 Ecuador   567854.00   21911.00    315.00000   17374000
    ## 36                   Egypt  2646568.00  222873.00  45032.00000  100388000
    ## 37             El Salvador   155759.00   16769.00     63.00000    6454000
    ## 38                 Estonia    28718.58   14590.50   2464.33300    1317000
    ## 39                Ethiopia   555335.00   29697.00   1983.00000  112079000
    ## 40                    Fiji        0.00       0.00    162.00000     890000
    ## 41                 Finland   207313.00   68582.00  18621.00000    5533000
    ## 42                  France  1058491.00  989085.00  66096.00000   67211000
    ## 43                 Georgia   111058.00   33940.00   3512.00000    3721000
    ## 44                 Germany  1872666.00 1054512.00 200400.00000   82806000
    ## 45                   Ghana   289575.00   29110.00   2373.00000   30418000
    ## 46                  Greece   659535.00   78118.00  29221.00000   10696000
    ## 47               Guatemala   247096.00   18537.00    881.00000   17581000
    ## 48                Honduras   238509.00   18076.00    492.00000    9746000
    ## 49                 Hungary   183509.00   79929.00   7676.00000    9732000
    ## 50                 Iceland    12189.00    4421.00    637.00000     356000
    ## 51                   India 30550057.00 4425011.00 173050.00000 1366418000
    ## 52               Indonesia  6659889.00  355463.00  43126.00000  270626000
    ## 53                    Iran  2076361.00  744234.00 141078.00000   82914000
    ## 54                 Ireland   168877.00   33935.00   8517.00000    4893000
    ## 55                  Israel   240653.00   62996.00  11571.00000    9018000
    ## 56                   Italy  1140641.00  713633.00  28338.00000   60340000
    ## 57                 Jamaica    31487.00    2894.00      0.00000    2948000
    ## 58                   Japan  2674263.00  348038.00  80767.00000  126097000
    ## 59                  Jordan   279811.00   26227.00   5048.00000   10102000
    ## 60              Kazakhstan   542458.00   42971.00   5609.00000   18469000
    ## 61                   Kenya   439768.00   51079.00  10215.00000   52574000
    ## 62         Krygyz Republic   123421.00   41164.00   2636.00000    6404000
    ## 63                  Kuwait    81414.00    3382.00     77.00000    4207000
    ## 64                  Latvia    46164.00   18323.00   2196.00000    1916000
    ## 65                 Lebanon   191885.00   25031.00   4230.00000    6856000
    ## 66               Lithuania    88468.00   27076.00   2743.00000    2778000
    ## 67              Luxembourg     3050.00    2534.00    693.00000     615000
    ## 68              Madagascar   123519.00   17479.00    756.00000   26969000
    ## 69                  Malawi        0.00       0.00    154.00000   18629000
    ## 70                Malaysia   680068.00   95047.00  44516.00000   31950000
    ## 71                Maldives     8097.00    1749.00     12.00000     531000
    ## 72                    Mali    63314.00   18001.00    338.00000   19658000
    ## 73                   Malta     8553.00    4452.00    147.00000     485000
    ## 74              Mauritania    15473.00    3419.00    111.00000    4526000
    ## 75               Mauritius    30948.00    4000.00    351.00000    1268000
    ## 76                  Mexico  4035251.00  308188.00  43744.00000  127576000
    ## 77                 Moldova    46779.00   17895.00   2073.00000    3536000
    ## 78              Montenegro    21308.00    1318.00     83.00000     622000
    ## 79                 Morocco   764154.00  131464.00  36653.00000   36472000
    ## 80                 Namibia    38576.00    2292.00    187.00000    2495000
    ## 81                   Nepal   376016.00   60270.00   1891.00000   28609000
    ## 82             Netherlands   668811.00  180016.00  15650.00000   17280000
    ## 83             New Zealand   187577.99   19221.03   9853.82655    4929000
    ## 84                   Niger    42937.00   18912.00   1713.00000   23311000
    ## 85         North Macedonia    56941.00    2767.00    402.00000    2083000
    ## 86                  Norway   199223.00   72404.00   8497.00000    5369000
    ## 87                    Oman    99740.00    5222.00    177.00000    4975000
    ## 88                Pakistan  1679110.00  176844.00  22147.00000  216565000
    ## 89                  Panama   129295.00   17197.00    107.00000    4246000
    ## 90                    Peru  1822684.00   92793.00   8896.00000   32510000
    ## 91             Philippines  3155957.00  236351.00  23827.00000  108117000
    ## 92                  Poland   986723.00  464624.00  41318.00000   37895000
    ## 93                Portugal   205180.00  118194.00  20239.00000   10237000
    ## 94                 Romania   350263.00  168852.00  19756.00000   19358000
    ## 95                  Russia  3032738.00 1213147.00  94582.00000  144369000
    ## 96                  Rwanda    63557.00    4214.00     51.00000   12627000
    ## 97            Saudi Arabia  1305312.00   81212.00  10541.00000   34269000
    ## 98                 Senegal   148277.00   38136.00   8296.00000   16296000
    ## 99                  Serbia   191014.00   47350.00  11407.00000    6955000
    ## 100        Slovak Republic    79000.00   55816.00   6991.00000    5448000
    ## 101               Slovenia    41840.00   21517.00   2824.00000    2067000
    ## 102           South Africa   729513.00   62930.00  23730.00000   58558000
    ## 103            South Korea  2089686.00  242332.00  74750.00000   51782000
    ## 104                  Spain  1212026.00  339934.00  85480.00000   46719000
    ## 105              Sri Lanka   229789.00   44693.00   4684.00000   21757000
    ## 106                 Sweden   241045.00  144769.00  19682.00000   10251000
    ## 107            Switzerland   207284.00   70070.00  25209.00000    8580000
    ## 108                 Taiwan   985144.00  171779.00  28907.00000   23186278
    ## 109               Tanzania   142898.00    9123.00   2014.00000   58005000
    ## 110               Thailand  1859422.00  180683.00  24742.00000   69626000
    ## 111                   Togo    84431.00    3975.00   1652.00000    8082000
    ## 112                Tunisia   163719.00   91806.00  11629.00000   11695000
    ## 113                 Turkey  4112575.00  583939.00  95100.00000   83430000
    ## 114                 Uganda        0.00       0.00   2194.00000   44270000
    ## 115                Ukraine   800886.00  392210.00  24751.00000   44391000
    ## 116         United Kingdom  1620993.00  450591.00 111257.00000   66856000
    ## 117          United States  9057685.06 2651278.84 354198.15770  329534000
    ## 118                Uruguay        0.00       0.00     91.02809    3462000
    ## 119              Venezuela        0.00       0.00   5718.00000   28516000
    ## 120                  Yemen        0.00       0.00     64.00000   29162000
    ## 121               Zimbabwe    99433.00   14899.00    371.00000   14645000
    ## 122       Hong Kong, China   162103.00   52640.01  10697.00100    7508000
    ## 123           Macao, China    25061.00    6465.00   2562.00000     640000
    ##      uni_enr_tot uni_enr_prop baseline_kg_co2e_excl_luc baseline_kg_co2e_total
    ## 1   3.706100e+05 9.742127e-03                  894.9871               898.4078
    ## 2   1.348450e+05 4.703348e-02                 1952.0389              2012.7098
    ## 3   9.960870e+05 2.313630e-02                  982.5261              1107.7870
    ## 4   2.513200e+06 5.597203e-02                 2952.2768              3516.5223
    ## 5   8.146200e+04 2.753955e-02                 1604.2125              1645.6205
    ## 6   1.359776e+06 5.373971e-02                 3015.1629              3142.1003
    ## 7   3.549782e+05 4.004266e-02                 1521.5858              1583.3920
    ## 8   1.842110e+05 1.836234e-02                 1600.3077              1653.5135
    ## 9   1.330000e+02 4.634146e-04                 1407.9734              1581.5435
    ## 10  3.049470e+05 3.221158e-02                 1376.0753              1467.6827
    ## 11  4.928100e+05 4.291649e-02                 1613.1644              1766.9017
    ## 12  5.383000e+03 1.380256e-02                 1008.9116              1085.4359
    ## 13  1.471700e+04 1.247098e-03                  690.3777               821.8233
    ## 14  8.000000e+00 1.250528e-04                 1993.9634              2049.7947
    ## 15  8.901600e+04 2.696637e-02                  983.7554              1036.1921
    ## 16  3.690400e+04 1.601736e-02                 1569.0976              1585.2711
    ## 17  8.741003e+06 4.141674e-02                 2856.1419              4366.1015
    ## 18  8.165000e+03 1.885681e-02                 1783.7341              1856.7795
    ## 19  2.363350e+05 3.390746e-02                  937.9134               989.1980
    ## 20  7.777000e+04 3.827075e-03                 1085.7443              1104.5643
    ## 21  2.038630e+05 1.236508e-02                  851.0628               930.9660
    ## 22  2.941690e+05 1.136841e-02                  887.2857               921.0749
    ## 23  1.261189e+06 3.374419e-02                 1645.1594              1658.6914
    ## 24  1.142000e+04 2.076364e-02                 1035.5358              1095.9136
    ## 25  9.074621e+05 4.788213e-02                 1630.3663              3895.4905
    ## 26  2.689466e+07 1.924766e-02                 1191.0544              1258.2617
    ## 27  1.721400e+06 3.419615e-02                 1806.2567              1972.8242
    ## 28  4.646780e+05 5.353988e-03                  638.9943               677.7450
    ## 29  1.949930e+05 3.862777e-02                 1802.4645              1860.1294
    ## 30  1.429300e+05 5.557802e-03                  531.9976               555.2293
    ## 31  1.647750e+05 4.047531e-02                 1241.9994              1338.5973
    ## 32  4.252100e+04 3.546372e-02                 1199.0651              1411.5938
    ## 33  3.280570e+05 3.086433e-02                 1088.3930              1146.2329
    ## 34  2.753310e+05 4.732399e-02                 1595.3923              1770.7317
    ## 35  5.900800e+05 3.396339e-02                 1335.2684              1506.3045
    ## 36  2.914473e+06 2.903209e-02                 1214.7941              1283.0885
    ## 37  1.725910e+05 2.674171e-02                 1030.4458              1367.8541
    ## 38  4.577342e+04 3.475582e-02                 1058.6422              1124.3830
    ## 39  5.870150e+05 5.237511e-03                  710.9891               717.8578
    ## 40  1.620000e+02 1.820225e-04                 1389.8620              1403.4943
    ## 41  2.945160e+05 5.322899e-02                 1394.1430              1450.8010
    ## 42  2.113672e+06 3.144830e-02                 1801.8850              1958.1279
    ## 43  1.485100e+05 3.991131e-02                 1189.7112              1205.7576
    ## 44  3.127578e+06 3.776994e-02                 1409.1637              1532.2176
    ## 45  3.210580e+05 1.055487e-02                  545.1520               578.9954
    ## 46  7.668740e+05 7.169727e-02                 1691.1896              1802.0541
    ## 47  2.665140e+05 1.515921e-02                  842.3210               892.7137
    ## 48  2.570770e+05 2.637769e-02                  975.2473              1459.1996
    ## 49  2.711140e+05 2.785799e-02                 1005.8398              1047.8904
    ## 50  1.724700e+04 4.844663e-02                 2096.0486              2224.0045
    ## 51  3.514812e+07 2.572282e-02                  768.8551               786.9643
    ## 52  7.058478e+06 2.608204e-02                  658.2904               703.6596
    ## 53  2.961673e+06 3.571982e-02                  840.0465               852.6197
    ## 54  2.113290e+05 4.319007e-02                 2156.4046              2347.5277
    ## 55  3.152200e+05 3.495454e-02                 2082.6980              2344.1241
    ## 56  1.882612e+06 3.120007e-02                 1564.4564              1711.8489
    ## 57  3.438100e+04 1.166248e-02                  957.3330              1029.9690
    ## 58  3.103068e+06 2.460858e-02                 1044.8571              1094.1137
    ## 59  3.110860e+05 3.079450e-02                 1171.3101              1320.4589
    ## 60  5.910380e+05 3.200162e-02                 2306.8041              2364.7877
    ## 61  5.010620e+05 9.530604e-03                 1017.2305              1035.4576
    ## 62  1.672210e+05 2.611196e-02                 1836.1731              1868.8452
    ## 63  8.487300e+04 2.017423e-02                 2351.8394              2620.3307
    ## 64  6.668300e+04 3.480324e-02                  960.5449              1016.2363
    ## 65  2.211460e+05 3.225583e-02                 1196.1641              1402.3321
    ## 66  1.182870e+05 4.257991e-02                 1140.8047              1213.3716
    ## 67  6.277000e+03 1.020650e-02                 2028.8623              2143.2976
    ## 68  1.417540e+05 5.256183e-03                  937.0303               959.3850
    ## 69  1.540000e+02 8.266681e-06                  517.3735               525.3949
    ## 70  8.196310e+05 2.565355e-02                 1174.5486              1402.1087
    ## 71  9.858000e+03 1.856497e-02                 1477.1665              1559.5775
    ## 72  8.165300e+04 4.153678e-03                 1726.0969              1730.9488
    ## 73  1.315200e+04 2.711753e-02                 1479.7297              1629.6737
    ## 74  1.900300e+04 4.198630e-03                 1726.9333              1746.9056
    ## 75  3.529900e+04 2.783833e-02                 1250.1740              1380.1854
    ## 76  4.387183e+06 3.438878e-02                 1200.1769              1218.8209
    ## 77  6.674700e+04 1.887641e-02                  854.3387               885.1354
    ## 78  2.270900e+04 3.650965e-02                 1731.5893              1834.5351
    ## 79  9.322710e+05 2.556128e-02                  885.9539               925.6469
    ## 80  4.105500e+04 1.645491e-02                 1126.5478              1164.1749
    ## 81  4.381770e+05 1.531605e-02                 1097.3768              1142.1832
    ## 82  8.644770e+05 5.002760e-02                 1639.5373              1820.7610
    ## 83  2.166528e+05 4.395473e-02                 2489.6372              2634.2060
    ## 84  6.356200e+04 2.726696e-03                 1332.3927              1335.2770
    ## 85  6.011000e+04 2.885742e-02                 1127.2038              1186.6094
    ## 86  2.801240e+05 5.217433e-02                 1771.5770              1934.9829
    ## 87  1.051390e+05 2.113347e-02                 1726.9860              1836.6585
    ## 88  1.878101e+06 8.672228e-03                 1316.3081              1337.3391
    ## 89  1.465990e+05 3.452638e-02                 1538.6275              1977.4806
    ## 90  1.924373e+06 5.919326e-02                  955.6513              1071.3782
    ## 91  3.416135e+06 3.159665e-02                  864.7852               900.4480
    ## 92  1.492665e+06 3.938950e-02                 1086.0513              1207.7833
    ## 93  3.436130e+05 3.356579e-02                 1710.0194              1923.3867
    ## 94  5.388710e+05 2.783712e-02                 1222.1128              1261.1237
    ## 95  4.340467e+06 3.006509e-02                 1451.5586              1647.2385
    ## 96  6.782200e+04 5.371189e-03                  461.2094               471.4065
    ## 97  1.397065e+06 4.076760e-02                 1264.2446              1424.2394
    ## 98  1.947090e+05 1.194827e-02                  893.2230               919.5938
    ## 99  2.497710e+05 3.591244e-02                  915.8173               927.1896
    ## 100 1.418070e+05 2.602919e-02                  815.1401               852.1398
    ## 101 6.618100e+04 3.201790e-02                 1482.9943              1618.2819
    ## 102 8.161730e+05 1.393786e-02                 1274.3170              1421.9375
    ## 103 2.406768e+06 4.647885e-02                 1524.7804              1676.9165
    ## 104 1.637440e+06 3.504870e-02                 1373.3714              1669.8609
    ## 105 2.791660e+05 1.283109e-02                  580.5800               584.0184
    ## 106 4.054960e+05 3.955673e-02                 1768.1854              1905.6643
    ## 107 3.025630e+05 3.526375e-02                 1668.6515              1814.5086
    ## 108 1.185830e+06 5.114361e-02                 1108.2816              1221.3552
    ## 109 1.540350e+05 2.655547e-03                  918.6856               930.4418
    ## 110 2.064847e+06 2.965626e-02                  822.0311               942.3208
    ## 111 9.005800e+04 1.114303e-02                  437.2379               450.5350
    ## 112 2.671540e+05 2.284344e-02                  884.6677               982.7868
    ## 113 4.791614e+06 5.743275e-02                 1209.6448              1262.7231
    ## 114 2.194000e+03 4.955952e-05                  701.3743               767.7384
    ## 115 1.217847e+06 2.743455e-02                  972.1926               996.5576
    ## 116 2.182841e+06 3.264989e-02                 1757.7410              1968.0993
    ## 117 1.206316e+07 3.660673e-02                 2037.7487              2058.1945
    ## 118 9.102809e+01 2.629350e-05                 2342.8690              2666.5831
    ## 119 5.718000e+03 2.005190e-04                 2397.1922              2762.0133
    ## 120 6.400000e+01 2.194637e-06                  641.3222               688.2674
    ## 121 1.147030e+05 7.832229e-03                  809.8198               816.8037
    ## 122 2.254400e+05 3.002664e-02                 2200.2115              2759.3689
    ## 123 3.408800e+04 5.326250e-02                 1572.1861              1771.9985
    ##     baseline_l_blue_green_wf baseline_l_blue_wf_total baseline_l_green_wf
    ## 1                  1015576.7                347776.76            667799.9
    ## 2                  1442826.5                214146.84           1228679.7
    ## 3                  1297370.6                186077.01           1111293.6
    ## 4                  1015585.2                 62016.32            953568.9
    ## 5                  1441946.2                191032.41           1250913.8
    ## 6                  1517778.9                174219.88           1343559.1
    ## 7                   793855.2                 60194.82            733660.4
    ## 8                  1243636.0                207655.78           1035980.2
    ## 9                   950947.4                 86412.77            864534.6
    ## 10                 1200212.8                 46927.86           1153285.0
    ## 11                  793608.1                 88370.52            705237.6
    ## 12                 1059051.1                 59525.28            999525.8
    ## 13                 1331729.8                 85060.39           1246669.4
    ## 14                 1042449.5                129159.82            913289.7
    ## 15                 1067061.3                 37836.36           1029224.9
    ## 16                 1068891.9                111200.61            957691.3
    ## 17                 1775234.1                 64128.65           1711105.5
    ## 18                 1196462.9                137783.59           1058679.4
    ## 19                 1257690.9                 54994.72           1202696.2
    ## 20                 1477705.9                 35532.58           1442173.4
    ## 21                 1372850.3                147016.82           1225833.5
    ## 22                 1293558.9                 62535.26           1231023.7
    ## 23                 1168002.0                105458.22           1062543.8
    ## 24                 1050939.7                115706.53            935233.2
    ## 25                  879692.5                143694.42            735998.1
    ## 26                  836917.5                135410.95            701506.6
    ## 27                 1032565.0                 54346.37            978218.6
    ## 28                  992345.6                 78014.22            914331.4
    ## 29                 1026022.6                 95094.35            930928.3
    ## 30                 1253739.3                 51858.38           1201880.9
    ## 31                 1104230.4                 49278.54           1054951.8
    ## 32                 1140741.0                148402.21            992338.8
    ## 33                  733147.3                 41644.76            691502.5
    ## 34                  748836.9                 67536.92            681300.0
    ## 35                 1482260.2                133859.52           1348400.7
    ## 36                 1088192.1                565678.41            522513.7
    ## 37                  824119.1                 45471.35            778647.8
    ## 38                  955997.5                 40985.34            915012.1
    ## 39                 1051356.8                 35489.03           1015867.8
    ## 40                 1262677.8                 76104.24           1186573.5
    ## 41                  842626.4                 78296.79            764329.6
    ## 42                  867376.1                 91749.95            775626.1
    ## 43                 1305279.9                102667.86           1202612.0
    ## 44                  721782.0                 72098.37            649683.6
    ## 45                 1196445.7                 93709.11           1102736.6
    ## 46                 1317040.8                181562.67           1135478.2
    ## 47                  829089.5                 40450.99            788638.5
    ## 48                  930887.6                 53946.77            876940.9
    ## 49                 1008969.6                 34539.17            974430.4
    ## 50                  774447.7                116126.39            658321.3
    ## 51                  865068.7                210569.12            654499.6
    ## 52                  951465.2                 85317.58            866147.6
    ## 53                 1543956.7                601044.50            942912.2
    ## 54                  719702.6                 72715.62            646986.9
    ## 55                 1662570.5                205613.90           1456956.6
    ## 56                 1121587.9                113292.51           1008295.3
    ## 57                 1023100.5                 72167.44            950933.1
    ## 58                  721985.3                 92491.33            629494.0
    ## 59                 1108728.5                231294.77            877433.7
    ## 60                 2030785.9                320643.57           1710142.4
    ## 61                  977459.2                 64531.82            912927.3
    ## 62                 1132575.2                219683.15            912892.1
    ## 63                 1370355.6                229493.97           1140861.7
    ## 64                 1062964.7                 49236.20           1013728.5
    ## 65                 1313565.9                251957.54           1061608.4
    ## 66                 1049813.3                 40216.48           1009596.8
    ## 67                  891137.2                 87789.65            803347.6
    ## 68                 1288490.1                187978.04           1100512.0
    ## 69                 1191740.0                 54550.49           1137189.5
    ## 70                 1115810.6                143502.63            972308.0
    ## 71                 1004331.1                233160.04            771171.0
    ## 72                 2194088.4                152280.16           2041808.3
    ## 73                  949576.7                128398.63            821178.1
    ## 74                 1572491.9                115444.67           1457047.2
    ## 75                  969828.2                113119.69            856708.6
    ## 76                 1213274.8                133053.04           1080221.7
    ## 77                 1070594.1                104346.59            966247.5
    ## 78                 1289166.1                 63592.54           1225573.5
    ## 79                 1755977.2                235215.68           1520761.5
    ## 80                 1039663.3                 65198.35            974464.9
    ## 81                 1372342.2                196865.09           1175477.1
    ## 82                  806151.1                 71606.69            734544.5
    ## 83                  995292.8                113815.59            881477.2
    ## 84                 4271656.7                 70072.47           4201584.2
    ## 85                  915940.1                 71837.42            844102.7
    ## 86                  828449.5                 96137.00            732312.5
    ## 87                 1587208.1                511416.14           1075791.9
    ## 88                 1052222.9                383029.76            669193.2
    ## 89                  993146.2                 70727.26            922418.9
    ## 90                  950766.8                156878.68            793888.1
    ## 91                 1022826.8                 81701.44            941125.3
    ## 92                  865393.7                 36330.59            829063.1
    ## 93                 1426916.7                271754.96           1155161.7
    ## 94                 1121089.9                 52600.89           1068489.1
    ## 95                 1480192.5                 80481.81           1399710.6
    ## 96                 1272508.1                 87792.78           1184715.3
    ## 97                 1276052.1                311117.92            964934.2
    ## 98                 1012434.3                 98332.63            914101.7
    ## 99                  939781.2                 38043.28            901738.0
    ## 100                 642723.4                 33707.40            609016.0
    ## 101                 892908.7                 62181.16            830727.5
    ## 102                1086945.7                 89561.23            997384.5
    ## 103                 949560.7                 90354.21            859206.5
    ## 104                1321632.6                206654.93           1114977.7
    ## 105                 957997.3                179239.39            778757.9
    ## 106                 813357.3                 81782.55            731574.7
    ## 107                 759109.6                 69012.03            690097.6
    ## 108                 950496.6                105572.72            844923.9
    ## 109                1183414.8                 72259.25           1111155.5
    ## 110                1220971.8                187150.54           1033821.3
    ## 111                1340443.2                 45159.81           1295283.4
    ## 112                1589767.8                189966.96           1399800.8
    ## 113                1400066.2                168496.32           1231569.9
    ## 114                 994261.1                 66985.41            927275.7
    ## 115                1185372.9                 46723.06           1138649.8
    ## 116                 705194.6                 77574.80            627619.8
    ## 117                1237262.4                144460.43           1092801.9
    ## 118                1595888.5                 54962.90           1540925.7
    ## 119                1303308.8                 63437.42           1239871.3
    ## 120                 851959.1                178264.51            673694.6
    ## 121                1003622.0                 65912.29            937709.7
    ## 122                1571288.0                208495.27           1362792.7
    ## 123                1123938.8                147551.92            976386.9
    ##     meatless_day_kg_co2e_excl_luc meatless_day_kg_co2e_total
    ## 1                       1809.9344                  1817.4949
    ## 2                       1651.1793                  1699.0555
    ## 3                       1066.4401                  1212.7564
    ## 4                       2124.5294                  2556.9938
    ## 5                       1557.5556                  1594.5413
    ## 6                       2536.1825                  2635.1555
    ## 7                       1223.2743                  1269.7309
    ## 8                       1670.2693                  1723.6840
    ## 9                       1389.3571                  1543.4558
    ## 10                      1214.5499                  1289.4822
    ## 11                      1414.6787                  1543.6344
    ## 12                      1070.0759                  1155.6168
    ## 13                       788.2442                   990.0407
    ## 14                      2167.9449                  2224.4087
    ## 15                      1002.2489                  1052.4054
    ## 16                      2401.3612                  2432.0601
    ## 17                      2112.7473                  3219.1744
    ## 18                      1614.1266                  1685.2690
    ## 19                       899.0894                   941.3749
    ## 20                       985.1448                  1003.7817
    ## 21                       768.1549                   897.1488
    ## 22                       840.0675                   885.9991
    ## 23                      1302.2709                  1312.3720
    ## 24                      1178.3112                  1240.3191
    ## 25                      1469.1188                  3386.8340
    ## 26                      1088.7179                  1151.3441
    ## 27                      2390.4664                  2631.9214
    ## 28                       988.9298                  1060.7640
    ## 29                      2082.6198                  2138.9448
    ## 30                       921.0747                   992.7831
    ## 31                      1315.0947                  1406.2327
    ## 32                      1340.2742                  1546.1731
    ## 33                      1114.3565                  1167.0000
    ## 34                      1380.2106                  1530.4715
    ## 35                      1702.3595                  1930.2690
    ## 36                      1011.0399                  1061.2308
    ## 37                      1171.3720                  1528.7944
    ## 38                      1029.5047                  1088.5392
    ## 39                       978.5310                   987.8282
    ## 40                      1526.3633                  1543.6936
    ## 41                      1231.6338                  1278.9050
    ## 42                      1446.6175                  1566.0639
    ## 43                      1706.1500                  1724.6002
    ## 44                      1189.3385                  1286.3318
    ## 45                       582.8511                   640.4659
    ## 46                      1370.2895                  1454.4083
    ## 47                      1112.6831                  1176.1251
    ## 48                      1311.3905                  1912.0559
    ## 49                      1055.6955                  1095.4578
    ## 50                      1734.6470                  1838.8993
    ## 51                      1259.4884                  1286.1557
    ## 52                       830.0272                   903.6356
    ## 53                       807.0731                   821.3549
    ## 54                      1760.1856                  1908.7369
    ## 55                      1502.5171                  1674.3841
    ## 56                      1320.3128                  1434.8575
    ## 57                      1118.8095                  1191.6133
    ## 58                      1049.3581                  1098.5266
    ## 59                      1353.6117                  1533.6254
    ## 60                      1895.5925                  1940.5645
    ## 61                      1408.3602                  1432.3911
    ## 62                      1851.2043                  1885.4118
    ## 63                      1727.2785                  1914.0162
    ## 64                       951.5351                  1001.4693
    ## 65                      1304.1453                  1509.4942
    ## 66                       999.2212                  1057.6890
    ## 67                      1775.4159                  1872.0893
    ## 68                      2020.2911                  2080.5181
    ## 69                       487.4034                   495.2496
    ## 70                      1198.2864                  1495.6694
    ## 71                      1495.4471                  1570.2939
    ## 72                      1910.2156                  1916.3808
    ## 73                      1266.3417                  1385.6836
    ## 74                      1773.3432                  1795.1131
    ## 75                      1131.2885                  1240.2226
    ## 76                      1024.7291                  1041.9094
    ## 77                      1216.5438                  1255.4689
    ## 78                      1445.7958                  1523.7773
    ## 79                       835.2165                   873.8560
    ## 80                      1679.1274                  1737.2167
    ## 81                      1549.4203                  1597.9533
    ## 82                      1424.6885                  1573.3899
    ## 83                      2253.9952                  2385.2048
    ## 84                      1124.3935                  1126.9240
    ## 85                      1312.3487                  1379.2476
    ## 86                      1496.9451                  1632.2020
    ## 87                      1672.1407                  1780.2200
    ## 88                      1898.2293                  1924.3943
    ## 89                      1593.3535                  2032.5959
    ## 90                       972.6245                  1112.0772
    ## 91                      1088.7401                  1148.7668
    ## 92                       957.8462                  1052.3534
    ## 93                      1504.3834                  1686.6353
    ## 94                      1132.6714                  1167.0383
    ## 95                      1274.9927                  1428.5313
    ## 96                       598.8782                   617.6003
    ## 97                      1213.4510                  1371.6429
    ## 98                      1385.8303                  1432.6754
    ## 99                       934.2434                   944.9509
    ## 100                     1065.8119                  1107.0114
    ## 101                     1274.2577                  1390.4679
    ## 102                     1212.1276                  1353.6084
    ## 103                     1273.4961                  1417.7193
    ## 104                     1212.8901                  1460.8426
    ## 105                      884.0138                   891.3321
    ## 106                     1529.6936                  1643.2798
    ## 107                     1544.1054                  1667.0841
    ## 108                     1000.8673                  1107.7260
    ## 109                     1450.4313                  1466.3091
    ## 110                      923.4899                  1141.2863
    ## 111                      519.7310                   537.1583
    ## 112                      803.9901                   889.2031
    ## 113                     1006.5992                  1054.3904
    ## 114                     1162.9325                  1273.2378
    ## 115                      942.7278                   963.9079
    ## 116                     1510.3502                  1683.4490
    ## 117                     1605.2594                  1619.7904
    ## 118                     1802.1036                  2059.4024
    ## 119                     2555.8359                  2926.1690
    ## 120                     1082.6333                  1185.2100
    ## 121                     1699.8813                  1719.5461
    ## 122                     1812.1144                  2243.8914
    ## 123                     1538.4517                  1722.2491
    ##     meatless_day_l_blue_green_wf meatless_day_l_blue_wf_total
    ## 1                      1302835.4                    506810.45
    ## 2                      1223613.2                    183944.88
    ## 3                      1167359.6                    159411.28
    ## 4                       787726.9                     49328.36
    ## 5                      1385885.9                    189848.42
    ## 6                      1288254.6                    150644.60
    ## 7                       631725.2                     47381.54
    ## 8                      1292700.4                    220495.64
    ## 9                       904723.7                     82437.81
    ## 10                     1045219.1                     39675.12
    ## 11                      652342.1                     72800.66
    ## 12                     1135286.3                     63279.62
    ## 13                     1254301.9                     79275.82
    ## 14                     1162276.8                    145662.85
    ## 15                     1071091.1                     37858.87
    ## 16                     1388382.7                    124344.42
    ## 17                     1282942.7                     44843.42
    ## 18                     1046215.0                    118392.26
    ## 19                     1179824.8                     50493.84
    ## 20                     1434156.0                     36978.04
    ## 21                     1242686.6                    169072.01
    ## 22                     1201292.0                     58321.02
    ## 23                      943377.1                     85072.70
    ## 24                     1152663.7                    118190.52
    ## 25                      828050.2                    137422.92
    ## 26                      787346.1                    126725.98
    ## 27                     1211339.8                     59882.32
    ## 28                     1338196.0                    114789.78
    ## 29                     1019280.8                     87344.29
    ## 30                     1318184.7                     55854.44
    ## 31                     1119428.5                     48808.35
    ## 32                     1202581.7                    157690.76
    ## 33                      721812.8                     39742.52
    ## 34                      662439.6                     59671.06
    ## 35                     1921792.7                    161044.10
    ## 36                      836681.9                    437421.82
    ## 37                      921833.5                     49355.48
    ## 38                      943996.3                     39593.80
    ## 39                     1326519.6                     59111.01
    ## 40                     1275810.4                     75623.31
    ## 41                      749333.7                     67759.61
    ## 42                      701221.1                     73726.36
    ## 43                     1560051.5                    126756.32
    ## 44                      606646.4                     60354.40
    ## 45                     1045317.5                     82709.35
    ## 46                     1050542.2                    146020.10
    ## 47                      995910.6                     45736.59
    ## 48                     1221025.0                     57944.82
    ## 49                     1082338.2                     35276.55
    ## 50                      656165.6                     96742.68
    ## 51                     1064284.8                    215944.89
    ## 52                      987372.9                    107453.42
    ## 53                     1373834.1                    527333.33
    ## 54                      595213.1                     60117.10
    ## 55                     1181194.2                    152755.98
    ## 56                      917651.4                     92446.48
    ## 57                     1141443.7                     76432.29
    ## 58                      752078.3                     94237.46
    ## 59                     1071635.0                    214971.33
    ## 60                     1595359.1                    255820.15
    ## 61                     1227546.2                     77150.65
    ## 62                     1154785.8                    230891.10
    ## 63                     1044792.4                    177184.38
    ## 64                     1041784.3                     47209.32
    ## 65                     1270620.1                    261459.08
    ## 66                      926850.1                     34412.85
    ## 67                      788537.0                     76946.38
    ## 68                     1617564.0                    221449.43
    ## 69                     1118087.3                     57242.39
    ## 70                      991961.3                    119414.05
    ## 71                     1013416.3                    233249.81
    ## 72                     2009969.5                    133748.59
    ## 73                      837569.8                    114006.23
    ## 74                     1565226.2                    110758.80
    ## 75                      832857.3                     94623.20
    ## 76                     1088207.8                    119161.04
    ## 77                     1258245.0                    118310.04
    ## 78                     1085672.7                     52421.84
    ## 79                     1548662.5                    198432.51
    ## 80                     1338017.7                     88514.54
    ## 81                     1419011.3                    177135.05
    ## 82                      707329.1                     63018.41
    ## 83                      905800.8                    104047.79
    ## 84                     3855258.6                     66200.28
    ## 85                      929910.3                     71947.56
    ## 86                      712310.1                     80463.92
    ## 87                     1405756.2                    439407.33
    ## 88                     1215625.9                    396841.01
    ## 89                     1047934.6                     74336.24
    ## 90                      940797.2                    152290.37
    ## 91                     1159177.9                    102527.97
    ## 92                      741187.2                     30094.95
    ## 93                     1289964.7                    245354.45
    ## 94                     1029063.0                     47388.84
    ## 95                     1315823.3                     70339.66
    ## 96                     1620313.7                    107141.86
    ## 97                     1168072.0                    277681.31
    ## 98                     1242656.6                    100809.60
    ## 99                      963489.3                     38178.74
    ## 100                     782912.9                     40406.32
    ## 101                     771059.2                     53509.42
    ## 102                     992248.1                     84102.67
    ## 103                     818721.3                     76993.27
    ## 104                    1182903.4                    184320.63
    ## 105                    1054617.0                    171881.03
    ## 106                     714102.9                     69920.30
    ## 107                     649659.3                     60770.27
    ## 108                     892279.2                     97728.90
    ## 109                    1386414.9                     91118.67
    ## 110                    1184525.0                    175469.28
    ## 111                    1532033.0                     48524.79
    ## 112                    1301946.4                    160137.45
    ## 113                    1033198.5                    126007.76
    ## 114                    1421792.2                     98548.87
    ## 115                    1109887.3                     42977.19
    ## 116                     606860.6                     66391.57
    ## 117                     957985.9                    112868.43
    ## 118                    1285833.4                     44613.26
    ## 119                    1363092.2                     64529.20
    ## 120                    1155084.4                    269975.12
    ## 121                    1402710.3                     98471.07
    ## 122                    1304648.3                    176921.10
    ## 123                    1112860.4                    145048.13
    ##     meatless_day_l_green_wf no_dairy_kg_co2e_excl_luc no_dairy_kg_co2e_total
    ## 1                  796024.9                  503.0854               508.6838
    ## 2                 1039668.3                  862.8370               901.7251
    ## 3                 1007948.4                  582.1108               829.6017
    ## 4                  738398.6                 2075.6507              2505.3500
    ## 5                 1196037.4                  962.5140               999.9185
    ## 6                 1137610.0                 2278.5302              2389.0576
    ## 7                  584343.7                  869.5214               920.5979
    ## 8                 1072204.8                  938.9915               999.3511
    ## 9                  822285.9                 1020.4882              1180.8994
    ## 10                1005544.0                  937.9199              1004.7585
    ## 11                 579541.4                 1059.4116              1201.5717
    ## 12                1072006.7                  871.6210               956.9131
    ## 13                1175026.1                  659.2875               852.8737
    ## 14                1016613.9                 2113.2434              2178.4516
    ## 15                1033232.2                  566.2919               630.4358
    ## 16                1264038.3                 1106.0526              1141.6116
    ## 17                1238099.3                 1657.5565              2689.8638
    ## 18                 927822.7                 1285.7974              1349.5787
    ## 19                1129331.0                  588.9629               639.2602
    ## 20                1397178.0                  895.3938               911.9002
    ## 21                1073614.6                  799.2541               907.2887
    ## 22                1142971.0                  753.5072               801.0268
    ## 23                 858304.4                 1108.6067              1119.2036
    ## 24                1034473.2                  795.8586               871.2087
    ## 25                 690627.3                 1378.4157              3418.7635
    ## 26                 660620.2                 1061.7383              1126.2406
    ## 27                1151457.5                 1308.7806              1567.7272
    ## 28                1223406.2                  916.4972               992.6792
    ## 29                 931936.5                  916.3266               970.0858
    ## 30                1262330.3                  713.0646               781.3607
    ## 31                1070620.2                  815.5007               915.3151
    ## 32                1044890.9                 1067.5036              1313.7459
    ## 33                 682070.3                  797.0056               860.4668
    ## 34                 602768.6                 1052.8844              1196.1154
    ## 35                1760748.6                 1374.7631              1665.8717
    ## 36                 399260.1                  798.9345               856.9777
    ## 37                 872478.1                  709.1009               961.2638
    ## 38                 904402.5                  702.8472               765.2143
    ## 39                1267408.6                  469.3847               477.1807
    ## 40                1200187.1                 1224.4690              1240.9326
    ## 41                 681574.1                  755.9799               797.3938
    ## 42                 627494.7                 1101.5398              1223.9380
    ## 43                1433295.2                  584.8822               608.5858
    ## 44                 546292.0                  811.7874               918.7020
    ## 45                 962608.2                  503.0255               562.4014
    ## 46                 904522.1                  971.7612              1059.5449
    ## 47                 950174.0                  597.7516               643.8806
    ## 48                1163080.2                  765.0518              1143.3380
    ## 49                1047061.7                  778.9446               821.1552
    ## 50                 559422.9                 1342.8848              1439.8176
    ## 51                 848339.9                  474.3558               497.3826
    ## 52                 879919.5                  751.0643               825.3666
    ## 53                 846500.8                  598.6586               614.5103
    ## 54                 535096.0                 1339.5281              1508.9895
    ## 55                1028438.2                 1298.7683              1493.3744
    ## 56                 825204.9                  940.2384              1063.9225
    ## 57                1065011.4                  817.8604               893.8391
    ## 58                 657840.9                 1004.2883              1055.7402
    ## 59                 856663.7                  833.4319              1045.7055
    ## 60                1339538.9                 1239.3143              1293.9759
    ## 61                1150395.6                  679.4077               699.2934
    ## 62                 923894.7                 1219.6464              1235.4767
    ## 63                 867608.0                 1590.0252              1817.9498
    ## 64                 994575.0                  603.2016               653.6861
    ## 65                1009161.0                  998.5340              1252.5428
    ## 66                 892437.3                  578.0754               636.4344
    ## 67                 711590.7                 1427.3381              1523.1313
    ## 68                1396114.6                  946.7475              1007.7329
    ## 69                1060844.9                  498.8666               506.7035
    ## 70                 872547.2                 1080.2154              1332.4287
    ## 71                 780166.5                 1069.3809              1147.1310
    ## 72                1876220.9                  879.9901               885.2508
    ## 73                 723563.6                 1022.3976              1153.3466
    ## 74                1454467.4                  983.4643              1007.2548
    ## 75                 738234.1                  902.0226              1007.9501
    ## 76                 969046.7                  888.8689               905.8177
    ## 77                1139934.9                  593.3384               637.4780
    ## 78                1033250.9                  849.2467               941.4699
    ## 79                1350230.0                  593.3326               632.7303
    ## 80                1249503.2                 1084.2338              1159.6428
    ## 81                1241876.2                  721.5271               767.1108
    ## 82                 644310.6                  965.1760              1117.1417
    ## 83                 801753.0                 1806.3456              1932.5786
    ## 84                3789058.3                  888.5740               890.6459
    ## 85                 857962.7                  655.0638               730.7680
    ## 86                 631846.1                 1158.9657              1293.8575
    ## 87                 966348.9                 1273.0581              1409.1784
    ## 88                 818784.9                  709.4656               733.0128
    ## 89                 973598.4                 1447.9004              1896.4591
    ## 90                 788506.8                  767.2827               901.8800
    ## 91                1056649.9                 1031.4933              1100.9752
    ## 92                 711092.2                  575.5178               665.8085
    ## 93                1044610.3                 1275.0510              1465.8433
    ## 94                 981674.2                  664.7088               688.6727
    ## 95                1245483.7                  953.5947              1126.7704
    ## 96                1513171.9                  465.8189               484.1853
    ## 97                 890390.7                  928.6681              1131.8695
    ## 98                1141847.0                  907.3396               954.8659
    ## 99                 925310.6                  636.6933               647.5352
    ## 100                742506.6                  674.3202               713.3683
    ## 101                717549.8                  948.1916              1103.5011
    ## 102                908145.5                 1095.2183              1255.0006
    ## 103                741728.1                 1321.5505              1463.0336
    ## 104                998582.8                  997.8660              1260.6223
    ## 105                882736.0                  551.0219               555.9676
    ## 106                644182.6                 1092.4544              1198.8159
    ## 107                588889.0                  958.3207              1087.2110
    ## 108                794550.3                  972.9255              1082.9674
    ## 109               1295296.3                  534.5401               546.7537
    ## 110               1009055.7                  838.7084              1048.9955
    ## 111               1483508.2                  546.6761               564.2744
    ## 112               1141809.0                  550.3107               645.6817
    ## 113                907190.7                  626.0089               684.8178
    ## 114               1323243.3                  535.6349               599.5304
    ## 115               1066910.1                  613.4405               636.4313
    ## 116                540469.1                 1225.9230              1424.5222
    ## 117                845117.5                 1285.1943              1300.5424
    ## 118               1241220.1                 1448.2104              1727.5437
    ## 119               1298563.0                 1955.9917              2344.2900
    ## 120                885109.3                  679.7923               817.0343
    ## 121               1304239.2                  870.8389               891.1898
    ## 122               1127727.2                 1730.6907              2217.8635
    ## 123                967812.3                 1472.6865              1676.7278
    ##     no_dairy_l_blue_green_wf no_dairy_l_blue_wf_total no_dairy_l_green_wf
    ## 1                  1298970.2                502284.92            796685.3
    ## 2                  1162489.0                156790.55           1005698.5
    ## 3                  1159163.6                170253.88            988909.7
    ## 4                   805330.2                 54337.28            750993.0
    ## 5                  1447261.2                189332.47           1257928.7
    ## 6                  1340466.3                160036.78           1180429.5
    ## 7                   621273.6                 52887.56            568386.1
    ## 8                  1311165.4                226818.07           1084347.4
    ## 9                   907926.1                 91866.25            816059.8
    ## 10                 1019065.4                 42097.30            976968.1
    ## 11                  648932.6                 87534.28            561398.4
    ## 12                 1095622.9                 63854.50           1031768.4
    ## 13                 1303826.2                 82488.74           1221337.5
    ## 14                 1217680.9                153444.83           1064236.0
    ## 15                 1053771.0                 39844.37           1013926.6
    ## 16                 1243694.3                149535.28           1094159.1
    ## 17                 1309891.2                 52889.17           1257002.1
    ## 18                 1101429.9                122199.25            979230.7
    ## 19                 1036915.7                 55559.91            981355.7
    ## 20                 1477588.4                 47028.51           1430559.9
    ## 21                 1336295.6                183216.80           1153078.8
    ## 22                 1202006.8                 61125.38           1140881.4
    ## 23                  923952.7                 94260.05            829692.6
    ## 24                 1118863.2                114778.59           1004084.6
    ## 25                  824847.3                144707.02            680140.2
    ## 26                  792448.1                130688.98            661759.2
    ## 27                 1131718.2                 74076.53           1057641.7
    ## 28                 1391107.9                125474.05           1265633.8
    ## 29                  968882.7                103617.58            865265.2
    ## 30                 1395806.0                 54642.99           1341163.0
    ## 31                 1037032.6                 51334.40            985698.2
    ## 32                 1207297.4                163986.26           1043311.1
    ## 33                  718449.2                 51953.00            666496.2
    ## 34                  636804.6                 63910.75            572893.9
    ## 35                 1749338.8                216494.19           1532844.6
    ## 36                  824040.1                430434.95            393605.2
    ## 37                  900310.8                 53055.53            847255.2
    ## 38                  886233.4                 40642.13            845591.3
    ## 39                 1398513.6                113069.75           1285443.8
    ## 40                 1346795.6                 85475.89           1261319.7
    ## 41                  655556.5                 73927.98            581628.5
    ## 42                  665423.1                 74569.08            590854.0
    ## 43                 1389280.2                101331.89           1287948.3
    ## 44                  591204.8                 65793.71            525411.1
    ## 45                 1034668.3                 87441.74            947226.6
    ## 46                 1014513.9                144270.83            870243.1
    ## 47                  959467.0                 44544.97            914922.0
    ## 48                 1249847.6                 68536.44           1181311.2
    ## 49                  816240.0                 39147.66            777092.4
    ## 50                  685821.6                107974.53            577847.1
    ## 51                 1059169.3                226091.12            833078.1
    ## 52                 1026537.8                119656.96            906880.8
    ## 53                 1364725.3                527197.64            837527.7
    ## 54                  560690.0                 61020.17            499669.9
    ## 55                 1161011.0                152422.33           1008588.7
    ## 56                  879529.2                 93214.88            786314.4
    ## 57                  983426.0                 78404.45            905021.6
    ## 58                  745947.1                100812.13            645135.0
    ## 59                 1047192.6                201689.77            845502.9
    ## 60                 1646739.7                260558.50           1386181.2
    ## 61                 1221040.6                 83762.60           1137278.0
    ## 62                 1152164.4                215592.17            936572.3
    ## 63                 1062779.4                171533.60            891245.8
    ## 64                  959960.3                 49494.29            910466.0
    ## 65                 1238554.7                253351.77            985202.9
    ## 66                  802083.7                 35585.54            766498.2
    ## 67                  742366.7                 77909.99            664456.7
    ## 68                 1684498.6                235457.83           1449040.8
    ## 69                 1140908.1                 63857.84           1077050.3
    ## 70                 1043797.3                135655.85            908141.4
    ## 71                  964410.8                236739.34            727671.5
    ## 72                 1903045.5                150936.88           1752108.6
    ## 73                  864741.0                125929.08            738812.0
    ## 74                 1227980.7                151457.72           1076523.0
    ## 75                  874930.8                103518.38            771412.5
    ## 76                 1034438.4                120675.50            913762.9
    ## 77                 1287497.8                130804.73           1156693.1
    ## 78                 1043365.3                 56842.66            986522.6
    ## 79                 1426862.4                194965.40           1231897.0
    ## 80                 1344542.8                101197.13           1243345.7
    ## 81                 1381715.6                186795.23           1194920.3
    ## 82                  643528.9                 62197.96            581331.0
    ## 83                  957332.0                110543.90            846788.1
    ## 84                 3724749.7                 77402.97           3647346.7
    ## 85                  886350.6                 69046.31            817304.3
    ## 86                  701511.8                 89006.10            612505.7
    ## 87                 1471412.1                461464.98           1009947.1
    ## 88                 1259503.2                511665.17            747838.0
    ## 89                 1126342.2                 82383.07           1043959.1
    ## 90                  873668.0                156172.60            717495.4
    ## 91                 1172811.0                112271.06           1060540.0
    ## 92                  680726.3                 27251.02            653475.3
    ## 93                 1185276.6                233121.69            952154.9
    ## 94                  928978.2                 49841.26            879136.9
    ## 95                 1322540.6                 72400.99           1250139.6
    ## 96                 1608679.8                108267.19           1500412.6
    ## 97                 1250744.8                292319.60            958425.2
    ## 98                 1117047.2                114764.62           1002282.6
    ## 99                  935483.1                 39262.24            896220.9
    ## 100                 726404.6                 41646.33            684758.2
    ## 101                 724090.1                 55771.44            668318.7
    ## 102                1019554.1                102919.38            916634.7
    ## 103                 827687.9                 80208.15            747479.8
    ## 104                1093944.5                176119.11            917825.4
    ## 105                1121425.4                190124.13            931301.3
    ## 106                 642105.1                 76637.47            565467.6
    ## 107                 686068.8                 70510.42            615558.3
    ## 108                 910432.1                103159.76            807272.3
    ## 109                1330661.0                 90334.10           1240326.9
    ## 110                1235686.9                194705.87           1040981.0
    ## 111                1557029.2                 53116.07           1503913.1
    ## 112                1238732.1                145823.76           1092908.3
    ## 113                1030258.1                124262.13            905995.9
    ## 114                1365268.2                109178.17           1256090.0
    ## 115                1087247.8                 43880.62           1043367.1
    ## 116                 585907.0                 72484.08            513422.9
    ## 117                1005111.6                124942.79            880168.8
    ## 118                1235311.7                 49722.97           1185588.7
    ## 119                1317906.8                 68707.33           1249199.4
    ## 120                1227828.5                309478.39            918350.1
    ## 121                1449024.0                111514.76           1337509.3
    ## 122                1372254.4                190962.06           1181292.3
    ## 123                1165967.6                159390.23           1006577.3
    ##     low_red_meat_kg_co2e_excl_luc low_red_meat_kg_co2e_total
    ## 1                       1827.6453                  1835.4105
    ## 2                       1686.5483                  1737.5688
    ## 3                       1065.0916                  1216.5834
    ## 4                       1781.8249                  2180.0622
    ## 5                       1594.1167                  1635.4988
    ## 6                       1979.1783                  2078.5067
    ## 7                       1098.4712                  1140.0235
    ## 8                       1710.9368                  1767.8208
    ## 9                       1323.5183                  1489.3929
    ## 10                      1097.7739                  1160.9924
    ## 11                      1298.8449                  1420.4598
    ## 12                      1042.7975                  1124.7821
    ## 13                       808.9551                  1019.2413
    ## 14                      1523.2754                  1558.3509
    ## 15                      1006.3278                  1059.9678
    ## 16                      2289.0830                  2318.2121
    ## 17                      2061.2678                  3170.0221
    ## 18                      1541.7340                  1604.9544
    ## 19                       894.6898                   942.7441
    ## 20                      1106.6937                  1125.0357
    ## 21                       832.5564                   941.6037
    ## 22                       901.8354                   949.0916
    ## 23                      1248.8494                  1259.1386
    ## 24                      1185.7879                  1255.9315
    ## 25                      1446.5316                  3416.4486
    ## 26                      1100.2120                  1162.4409
    ## 27                      2165.9080                  2384.8224
    ## 28                      1006.1530                  1081.4225
    ## 29                      1799.6870                  1855.0838
    ## 30                       900.1099                   970.5744
    ## 31                      1120.7942                  1198.1833
    ## 32                      1177.1159                  1369.3856
    ## 33                      1031.0319                  1080.2416
    ## 34                      1335.2688                  1487.7684
    ## 35                      1726.1858                  1960.4008
    ## 36                      1022.5338                  1078.1413
    ## 37                      1089.6115                  1448.0060
    ## 38                      1001.0350                  1058.4752
    ## 39                       993.5379                  1002.5587
    ## 40                      1545.4715                  1562.8459
    ## 41                      1218.7585                  1265.1874
    ## 42                      1331.9184                  1444.9273
    ## 43                      1408.9699                  1427.5549
    ## 44                      1097.3624                  1185.6212
    ## 45                       592.3935                   653.9468
    ## 46                      1302.1907                  1383.0184
    ## 47                       946.4418                  1003.5064
    ## 48                      1193.8171                  1774.7760
    ## 49                       993.1147                  1032.8396
    ## 50                      1713.0315                  1820.2947
    ## 51                      1251.2354                  1277.9950
    ## 52                       836.5601                   908.7215
    ## 53                       799.1329                   814.6475
    ## 54                      1659.7828                  1805.0868
    ## 55                      1567.8493                  1764.6501
    ## 56                      1244.1264                  1353.1069
    ## 57                       952.3363                  1025.9742
    ## 58                      1122.0802                  1174.9644
    ## 59                      1212.5652                  1391.5532
    ## 60                      1909.3728                  1957.3910
    ## 61                      1449.3775                  1473.3784
    ## 62                      1918.0371                  1952.1283
    ## 63                      1809.1847                  2018.1685
    ## 64                       916.8593                   966.4401
    ## 65                      1244.0616                  1465.0746
    ## 66                      1008.8559                  1068.7016
    ## 67                      1487.1586                  1567.1272
    ## 68                      1983.3991                  2046.9412
    ## 69                       542.3265                   550.3947
    ## 70                      1138.0214                  1386.7140
    ## 71                      1456.9377                  1538.5118
    ## 72                      1910.6011                  1916.6521
    ## 73                      1228.2044                  1346.0950
    ## 74                      1764.7212                  1786.0073
    ## 75                      1140.9424                  1251.4445
    ## 76                      1019.8445                  1035.8126
    ## 77                      1138.3196                  1180.3192
    ## 78                      1364.1776                  1432.1316
    ## 79                       784.5945                   821.8945
    ## 80                      1682.7473                  1747.9575
    ## 81                      1570.6689                  1619.6865
    ## 82                      1401.3250                  1552.2348
    ## 83                      1817.7529                  1944.3040
    ## 84                      1238.7689                  1241.3926
    ## 85                      1196.9673                  1262.8960
    ## 86                      1530.3252                  1671.3912
    ## 87                      1645.3916                  1757.7756
    ## 88                      1848.6370                  1874.4791
    ## 89                      1594.9026                  2046.5765
    ## 90                       969.6511                  1095.6720
    ## 91                      1085.9171                  1152.1199
    ## 92                       913.7008                  1010.4091
    ## 93                      1392.8204                  1561.9667
    ## 94                      1141.0680                  1177.4919
    ## 95                      1291.3368                  1463.2441
    ## 96                       622.7067                   641.2035
    ## 97                      1199.4910                  1367.4576
    ## 98                      1329.8998                  1377.1689
    ## 99                       901.9445                   912.9014
    ## 100                      902.1379                   942.1378
    ## 101                     1228.0873                  1343.1595
    ## 102                     1202.4600                  1351.9241
    ## 103                     1321.1637                  1458.6613
    ## 104                     1082.0472                  1298.6188
    ## 105                      839.8221                   846.7332
    ## 106                     1419.2590                  1524.7633
    ## 107                     1316.0805                  1426.6808
    ## 108                     1023.3748                  1128.2311
    ## 109                     1452.5329                  1468.5127
    ## 110                      911.1963                  1108.0382
    ## 111                      585.3743                   603.5677
    ## 112                      777.0382                   862.6199
    ## 113                      998.7823                  1046.9608
    ## 114                     1167.4659                  1279.1868
    ## 115                      857.6587                   879.4654
    ## 116                     1492.6623                  1671.9637
    ## 117                     1443.7257                  1457.3872
    ## 118                     1776.6680                  2033.8914
    ## 119                     2481.0707                  2856.9997
    ## 120                     1037.1209                  1139.3542
    ## 121                     1622.0918                  1641.9020
    ## 122                     1292.6811                  1547.6497
    ## 123                     1197.9341                  1324.7308
    ##     low_red_meat_l_blue_green_wf low_red_meat_l_blue_wf_total
    ## 1                      1377793.3                    565371.28
    ## 2                      1249775.2                    187203.58
    ## 3                      1187684.4                    159276.67
    ## 4                       792125.7                     52626.21
    ## 5                      1472671.7                    193113.29
    ## 6                      1191890.9                    148785.14
    ## 7                       613600.2                     47459.82
    ## 8                      1329608.4                    222011.00
    ## 9                       948834.5                     90502.92
    ## 10                     1009158.7                     39061.02
    ## 11                      650426.0                     73857.64
    ## 12                     1122978.0                     65499.91
    ## 13                     1312708.7                     83140.93
    ## 14                      991330.0                    142782.37
    ## 15                     1091545.2                     38704.52
    ## 16                     1417920.2                    141429.76
    ## 17                     1350700.0                     50144.88
    ## 18                     1121579.1                    122602.34
    ## 19                     1197180.9                     54830.59
    ## 20                     1479923.3                     50994.60
    ## 21                     1338030.0                    182498.98
    ## 22                     1209218.0                     60684.80
    ## 23                      951895.5                     89773.17
    ## 24                     1142421.9                    118803.42
    ## 25                      846508.7                    143482.65
    ## 26                      794890.3                    131976.09
    ## 27                     1205581.9                     64660.87
    ## 28                     1394143.0                    122709.36
    ## 29                      993744.2                     90926.52
    ## 30                     1410628.3                     55489.84
    ## 31                     1056237.5                     46196.68
    ## 32                     1134653.8                    154271.42
    ## 33                      712453.0                     42568.11
    ## 34                      671948.9                     61911.05
    ## 35                     1930228.4                    168565.17
    ## 36                      853749.8                    443380.23
    ## 37                      916523.0                     52355.41
    ## 38                      947827.8                     39974.93
    ## 39                     1452791.0                    119380.57
    ## 40                     1353643.0                     82528.54
    ## 41                      746433.3                     71518.55
    ## 42                      679879.6                     73655.53
    ## 43                     1547984.7                    120169.73
    ## 44                      597137.6                     60985.70
    ## 45                     1036284.9                     86853.39
    ## 46                     1031522.6                    144025.23
    ## 47                      980829.2                     45649.19
    ## 48                     1184661.3                     62992.11
    ## 49                     1037556.8                     36020.47
    ## 50                      671630.4                    103676.41
    ## 51                     1073166.4                    220952.27
    ## 52                     1039625.6                    115978.15
    ## 53                     1389798.7                    527613.26
    ## 54                      588205.3                     59648.71
    ## 55                     1251578.4                    154785.57
    ## 56                      901073.3                     92229.77
    ## 57                     1062814.5                     76189.69
    ## 58                      782020.8                     99992.75
    ## 59                     1067953.2                    210228.11
    ## 60                     1613979.8                    255326.18
    ## 61                     1264317.3                     79437.82
    ## 62                     1194624.6                    233317.81
    ## 63                     1065758.3                    175724.59
    ## 64                     1051449.5                     49112.40
    ## 65                     1293933.2                    252899.82
    ## 66                      959594.7                     36815.09
    ## 67                      713549.8                     76834.85
    ## 68                     1750800.5                    226951.76
    ## 69                     1148654.0                     63713.04
    ## 70                     1034856.3                    131828.37
    ## 71                     1000381.9                    231426.18
    ## 72                     2033749.4                    145472.17
    ## 73                      870299.3                    121405.91
    ## 74                     1600982.5                    149028.90
    ## 75                      869101.8                    101601.09
    ## 76                     1085999.4                    122286.45
    ## 77                     1264408.6                    121588.43
    ## 78                     1070339.2                     51070.75
    ## 79                     1529447.0                    198693.24
    ## 80                     1384560.6                    102484.81
    ## 81                     1443105.8                    180078.60
    ## 82                      719612.0                     64184.32
    ## 83                      862375.9                    101691.35
    ## 84                     3817925.1                     82237.52
    ## 85                      933406.1                     69629.61
    ## 86                      733298.6                     84882.56
    ## 87                     1448108.9                    447553.23
    ## 88                     1279903.9                    433103.19
    ## 89                     1081827.5                     77787.78
    ## 90                      946274.6                    152326.06
    ## 91                     1166208.0                    108828.64
    ## 92                      758041.4                     30395.67
    ## 93                     1235756.5                    239268.13
    ## 94                     1046744.5                     49112.65
    ## 95                     1393162.9                     75314.33
    ## 96                     1633693.3                    108344.21
    ## 97                     1209257.8                    290530.02
    ## 98                     1210535.0                    113286.56
    ## 99                      964481.3                     38656.77
    ## 100                     756488.0                     40068.25
    ## 101                     763109.7                     53642.56
    ## 102                    1037089.7                    102417.63
    ## 103                     828380.0                     79168.79
    ## 104                    1121812.6                    179119.00
    ## 105                    1125622.6                    183360.30
    ## 106                     698723.7                     73554.89
    ## 107                     654950.7                     58141.22
    ## 108                     903204.6                    101960.17
    ## 109                    1435321.7                     96003.35
    ## 110                    1237147.3                    190457.07
    ## 111                    1545804.0                     53074.30
    ## 112                    1320315.9                    158604.87
    ## 113                    1053688.7                    127023.41
    ## 114                    1429264.0                    106248.06
    ## 115                    1096676.9                     43228.52
    ## 116                     615245.2                     68619.80
    ## 117                     986698.8                    117935.00
    ## 118                    1312976.1                     46865.97
    ## 119                    1381792.9                     67529.45
    ## 120                    1233828.3                    301642.77
    ## 121                    1501689.2                    112088.78
    ## 122                    1106795.0                    183729.98
    ## 123                    1005662.5                    152162.42
    ##     low_red_meat_l_green_wf no_red_meat_kg_co2e_excl_luc
    ## 1                  812422.0                    1946.3292
    ## 2                 1062571.6                    1269.8213
    ## 3                 1028407.7                     942.6309
    ## 4                  739499.5                     695.4313
    ## 5                 1279558.4                    1099.7443
    ## 6                 1043105.7                    1061.0393
    ## 7                  566140.4                     876.0417
    ## 8                 1107597.4                    1207.7849
    ## 9                  858331.5                     823.0032
    ## 10                 970097.6                     950.2128
    ## 11                 576568.4                    1026.7205
    ## 12                1057478.1                     598.5257
    ## 13                1229567.7                     605.3216
    ## 14                 848547.7                     787.8012
    ## 15                1052840.7                     847.1234
    ## 16                1276490.5                    1829.5425
    ## 17                1300555.1                    1050.9396
    ## 18                 998976.8                    1022.5945
    ## 19                1142350.4                     778.6306
    ## 20                1428928.8                     533.5485
    ## 21                1155531.1                     592.0333
    ## 22                1148533.2                     483.0364
    ## 23                 862122.4                     748.3160
    ## 24                1023618.5                     897.7747
    ## 25                 703026.0                     621.4916
    ## 26                 662914.2                     695.9749
    ## 27                1140921.0                    1585.4156
    ## 28                1271433.7                     635.9986
    ## 29                 902817.7                    1646.2294
    ## 30                1355138.5                     779.4250
    ## 31                1010040.8                     977.4544
    ## 32                 980382.4                    1048.8194
    ## 33                 669884.9                     891.2408
    ## 34                 610037.8                     898.6373
    ## 35                1761663.2                    1049.0489
    ## 36                 410369.6                     818.0664
    ## 37                 864167.6                     777.1113
    ## 38                 907852.9                     764.7970
    ## 39                1333410.4                     822.9644
    ## 40                1271114.4                     904.2512
    ## 41                 674914.8                     932.2088
    ## 42                 606224.1                     900.5283
    ## 43                1427815.0                    1412.3280
    ## 44                 536151.9                     862.2757
    ## 45                 949431.5                     507.1425
    ## 46                 887497.4                    1108.0749
    ## 47                 935180.0                     711.3094
    ## 48                1121669.2                     872.9487
    ## 49                1001536.3                     747.2344
    ## 50                 567953.9                    1122.7745
    ## 51                 852214.1                    1178.9036
    ## 52                 923647.4                     716.4748
    ## 53                 862185.5                     674.6693
    ## 54                 528556.6                    1013.6144
    ## 55                1096792.9                     865.9710
    ## 56                 808843.6                    1030.4293
    ## 57                 986624.8                     710.6319
    ## 58                 682028.0                     674.3287
    ## 59                 857725.0                     929.2527
    ## 60                1358653.7                    1449.3017
    ## 61                1184879.5                    1215.4474
    ## 62                 961306.8                    1326.3752
    ## 63                 890033.7                     763.6020
    ## 64                1002337.1                     762.9017
    ## 65                1041033.3                     901.8526
    ## 66                 922779.6                     875.3030
    ## 67                 636715.0                     969.4669
    ## 68                1523848.7                    1667.5245
    ## 69                1084941.0                     314.9597
    ## 70                 903027.9                     861.5539
    ## 71                 768955.7                    1062.2544
    ## 72                1888277.2                    1770.7017
    ## 73                 748893.4                     747.5695
    ## 74                1451953.6                    1319.3698
    ## 75                 767500.7                     806.0931
    ## 76                 963712.9                     569.3879
    ## 77                1142820.2                    1176.3977
    ## 78                1019268.5                    1090.1341
    ## 79                1330753.7                     603.4464
    ## 80                1282075.7                    1161.3669
    ## 81                1263027.2                    1318.8617
    ## 82                 655427.6                     952.2532
    ## 83                 760684.5                    1180.8209
    ## 84                3735687.6                     602.0637
    ## 85                 863776.5                    1122.9710
    ## 86                 648416.0                     939.0878
    ## 87                1000555.6                    1112.1357
    ## 88                 846800.7                    1709.6569
    ## 89                1004039.7                     794.4280
    ## 90                 793948.6                     670.7225
    ## 91                1057379.4                     757.6203
    ## 92                 727645.7                     791.7774
    ## 93                 996488.4                     903.5885
    ## 94                 997631.8                     905.6065
    ## 95                1317848.5                     799.8959
    ## 96                1525349.1                     472.3532
    ## 97                 918727.8                     783.1613
    ## 98                1097248.5                     977.0078
    ## 99                 925824.5                     695.1663
    ## 100                716419.8                     780.5227
    ## 101                709467.1                     892.4985
    ## 102                934672.1                     683.5976
    ## 103                749211.2                     787.9844
    ## 104                942693.6                     782.7750
    ## 105                942262.3                     782.4946
    ## 106                625168.8                     937.8434
    ## 107                596809.4                     972.6405
    ## 108                801244.5                     759.7306
    ## 109               1339318.4                    1311.9538
    ## 110               1046690.2                     729.8824
    ## 111               1492729.7                     447.5326
    ## 112               1161711.0                     643.3207
    ## 113                926665.3                     893.8273
    ## 114               1323015.9                     990.8655
    ## 115               1053448.4                     753.8409
    ## 116                546625.4                     874.9768
    ## 117                868763.8                     855.4446
    ## 118               1266110.1                     874.4085
    ## 119               1314263.4                    1167.4430
    ## 120                932185.5                     801.1471
    ## 121               1389600.5                    1283.3295
    ## 122                923065.0                     863.8431
    ## 123                853500.0                     886.3409
    ##     no_red_meat_kg_co2e_total no_red_meat_l_blue_green_wf
    ## 1                   1954.9493                   1288914.8
    ## 2                   1289.9829                    982500.7
    ## 3                   1099.5725                   1070016.2
    ## 4                   1061.9136                    710699.4
    ## 5                   1116.2393                   1063690.3
    ## 6                   1156.3334                    930742.7
    ## 7                    899.3781                    528167.9
    ## 8                   1263.4540                   1098626.9
    ## 9                    890.9179                    803364.3
    ## 10                   977.3970                    925440.5
    ## 11                  1099.7574                    533510.2
    ## 12                   638.9745                   1067025.3
    ## 13                   789.8658                   1224540.5
    ## 14                   794.1154                    742117.3
    ## 15                   891.5858                    991527.0
    ## 16                  1862.8102                   1231284.0
    ## 17                  1556.5373                   1046020.5
    ## 18                  1084.2920                   1013801.0
    ## 19                   797.4342                   1072917.1
    ## 20                   550.9164                   1384571.8
    ## 21                   692.6366                   1290364.0
    ## 22                   529.1519                   1171466.0
    ## 23                   754.5985                    743963.8
    ## 24                   938.4450                   1069011.2
    ## 25                   892.1042                    701616.0
    ## 26                   736.6589                    667014.5
    ## 27                  1807.9018                   1103375.6
    ## 28                   704.8084                   1341253.6
    ## 29                  1674.1850                    949108.8
    ## 30                   846.4707                   1377263.2
    ## 31                  1011.5102                    881116.1
    ## 32                  1178.7047                    919288.8
    ## 33                   913.0071                    631874.7
    ## 34                  1018.3861                    531136.5
    ## 35                  1275.0356                   1654542.7
    ## 36                   847.8518                    762719.2
    ## 37                   910.7661                    892297.9
    ## 38                   790.6945                    801882.0
    ## 39                   831.9701                   1315074.0
    ## 40                   922.6396                   1129156.9
    ## 41                   955.4069                    603353.2
    ## 42                   968.1412                    543762.0
    ## 43                  1423.3426                   1400887.9
    ## 44                   909.1975                    494092.8
    ## 45                   564.2180                   1007676.0
    ## 46                  1153.9301                    870278.3
    ## 47                   763.1098                    932165.2
    ## 48                  1149.9598                   1200830.6
    ## 49                   760.9341                    938311.1
    ## 50                  1201.0940                    514974.1
    ## 51                  1205.2635                   1061273.7
    ## 52                   787.7751                   1010430.8
    ## 53                   691.2420                   1331035.6
    ## 54                  1090.4755                    461492.6
    ## 55                   894.6779                    847223.6
    ## 56                  1088.8085                    777216.2
    ## 57                   737.3271                   1008744.7
    ## 58                   710.9304                    622581.6
    ## 59                  1089.4234                    995767.6
    ## 60                  1478.2473                   1474961.2
    ## 61                  1239.4105                   1098438.4
    ## 62                  1359.2025                    971467.4
    ## 63                   871.6330                    962005.2
    ## 64                   786.3417                    903939.0
    ## 65                  1024.7672                   1055054.0
    ## 66                   901.6568                    850331.9
    ## 67                  1014.1877                    552298.8
    ## 68                  1732.9488                   1598765.7
    ## 69                   322.5008                   1107711.0
    ## 70                  1125.3685                    988547.2
    ## 71                  1100.1381                    909545.7
    ## 72                  1777.9124                   1850553.2
    ## 73                   776.0609                    684073.0
    ## 74                  1344.1145                   1427789.3
    ## 75                   899.9052                    791042.9
    ## 76                   584.0717                    967224.2
    ## 77                  1212.1286                   1248456.9
    ## 78                  1110.8062                    951905.3
    ## 79                   642.8373                   1443947.0
    ## 80                  1227.3789                   1257251.4
    ## 81                  1367.5715                   1303132.4
    ## 82                  1024.2114                    568416.4
    ## 83                  1306.3843                    695941.0
    ## 84                   604.3225                   3715475.8
    ## 85                  1174.6126                    867023.6
    ## 86                  1014.7599                    580981.7
    ## 87                  1221.8534                   1209766.6
    ## 88                  1735.3883                   1118455.4
    ## 89                   972.5165                    987304.6
    ## 90                   814.1551                    812076.2
    ## 91                   817.9884                   1113703.4
    ## 92                   813.6762                    680537.1
    ## 93                   999.8317                   1009760.3
    ## 94                   925.2268                    901322.1
    ## 95                   816.5735                   1155270.2
    ## 96                   491.5702                   1564548.9
    ## 97                   937.4798                   1126216.8
    ## 98                  1025.2486                   1136274.0
    ## 99                   702.4745                    825087.3
    ## 100                  794.7647                    678640.8
    ## 101                 1001.4511                    658117.9
    ## 102                  838.8549                    865419.0
    ## 103                  911.3001                    648510.7
    ## 104                  898.6263                    963306.2
    ## 105                  789.4181                   1097779.8
    ## 106                  991.5112                    559030.9
    ## 107                 1018.5976                    553803.2
    ## 108                  847.0067                    805951.4
    ## 109                 1327.7499                   1305090.2
    ## 110                  910.2033                   1191628.2
    ## 111                  464.2639                   1509937.2
    ## 112                  729.4833                   1133810.1
    ## 113                  944.8707                    958181.9
    ## 114                 1094.2559                   1364493.0
    ## 115                  765.7683                   1055970.0
    ## 116                  980.6153                    481667.4
    ## 117                  857.9898                    757963.0
    ## 118                 1081.3377                   1053066.3
    ## 119                 1370.8606                   1024760.8
    ## 120                  917.7527                   1092950.0
    ## 121                 1302.9337                   1412135.2
    ## 122                  960.1876                    919782.4
    ## 123                  948.7706                    877650.0
    ##     no_red_meat_l_blue_wf_total no_red_meat_l_green_wf
    ## 1                     568457.29               720457.5
    ## 2                     169421.09               813079.6
    ## 3                     157833.34               912182.9
    ## 4                      49111.59               661587.9
    ## 5                     185940.33               877750.0
    ## 6                     137804.87               792937.8
    ## 7                      45331.76               482836.2
    ## 8                     223442.52               875184.4
    ## 9                      87585.11               715779.2
    ## 10                     36550.79               888889.7
    ## 11                     71382.68               462127.6
    ## 12                     60994.47              1006030.8
    ## 13                     83215.16              1141325.4
    ## 14                    133620.20               608497.1
    ## 15                     34998.33               956528.6
    ## 16                    139484.99              1091799.0
    ## 17                     46410.50               999610.0
    ## 18                    121460.06               892341.0
    ## 19                     47579.25              1025337.8
    ## 20                     48450.57              1336121.3
    ## 21                    191758.10              1098605.9
    ## 22                     60470.80              1110995.2
    ## 23                     85516.06               658447.8
    ## 24                    112228.34               956782.9
    ## 25                    141163.39               560452.6
    ## 26                    135599.87               531414.6
    ## 27                     66167.56              1037208.0
    ## 28                    124984.64              1216269.0
    ## 29                     89247.00               859861.8
    ## 30                     55093.41              1322169.8
    ## 31                     39734.81               841381.3
    ## 32                    145591.45               773697.3
    ## 33                     38519.45               593355.3
    ## 34                     55822.71               475313.7
    ## 35                    170859.90              1483682.8
    ## 36                    422707.16               340012.0
    ## 37                     50950.38               841347.5
    ## 38                     33016.73               768865.3
    ## 39                    118956.09              1196118.0
    ## 40                     80302.32              1048854.6
    ## 41                     67323.29               536029.9
    ## 42                     65816.69               477945.3
    ## 43                    119237.74              1281650.1
    ## 44                     55677.73               438415.0
    ## 45                     87038.38               920637.6
    ## 46                    132697.83               737580.5
    ## 47                     43656.76               888508.4
    ## 48                     61127.89              1139702.7
    ## 49                     31889.66               906421.4
    ## 50                     94672.02               420302.1
    ## 51                    220513.69               840760.0
    ## 52                    117535.27               892895.6
    ## 53                    535940.11               795095.5
    ## 54                     52625.27               408867.4
    ## 55                    150783.13               696440.5
    ## 56                     85613.99               691602.2
    ## 57                     76035.94               932708.8
    ## 58                     90895.22               531686.4
    ## 59                    209326.23               786441.4
    ## 60                    252406.85              1222554.3
    ## 61                     80271.53              1018166.9
    ## 62                    228971.54               742495.8
    ## 63                    174628.26               787376.9
    ## 64                     43030.00               860909.0
    ## 65                    264476.67               790577.3
    ## 66                     30464.93               819867.0
    ## 67                     69705.09               482593.8
    ## 68                    231023.85              1367741.8
    ## 69                     64437.57              1043273.5
    ## 70                    133208.26               855338.9
    ## 71                    229506.37               680039.3
    ## 72                    147696.43              1702856.7
    ## 73                    105773.45               578299.5
    ## 74                    150354.48              1277434.9
    ## 75                     99557.60               691485.3
    ## 76                    118352.46               848871.7
    ## 77                    120932.13              1127524.7
    ## 78                     43352.57               908552.8
    ## 79                    199553.85              1244393.2
    ## 80                    100897.72              1156353.7
    ## 81                    177881.83              1125250.6
    ## 82                     57239.17               511177.3
    ## 83                     93566.72               602374.2
    ## 84                     79896.45              3635579.4
    ## 85                     66749.53               800274.0
    ## 86                     79591.78               501390.0
    ## 87                    446122.57               763644.0
    ## 88                    435245.84               683209.6
    ## 89                     74561.88               912742.7
    ## 90                    153504.98               658571.2
    ## 91                    114377.58               999325.8
    ## 92                     23953.31               656583.8
    ## 93                    206179.84               803580.4
    ## 94                     41761.11               859561.0
    ## 95                     65555.90              1089714.3
    ## 96                    107203.07              1457345.8
    ## 97                    290505.72               835711.1
    ## 98                    113371.35              1022902.7
    ## 99                     33208.23               791879.1
    ## 100                    35966.63               642674.2
    ## 101                    48169.19               609948.7
    ## 102                   101128.26               764290.8
    ## 103                    71825.39               576685.4
    ## 104                   163672.33               799633.9
    ## 105                   183043.86               914736.0
    ## 106                    68524.62               490506.3
    ## 107                    53628.56               500174.6
    ## 108                   103674.99               702276.4
    ## 109                    97419.36              1207670.8
    ## 110                   191964.52               999663.7
    ## 111                    52216.78              1457720.5
    ## 112                   146828.16               986982.0
    ## 113                   126021.06               832160.8
    ## 114                   110687.17              1253805.8
    ## 115                    39992.90              1015977.1
    ## 116                    64072.36               417595.1
    ## 117                   109780.38               648182.6
    ## 118                    41094.35              1011972.0
    ## 119                    65697.20               959063.6
    ## 120                   302915.80               790034.2
    ## 121                   114766.42              1297368.8
    ## 122                   183031.37               736751.1
    ## 123                   153886.36               723763.6
    ##     pescetarian_kg_co2e_excl_luc pescetarian_kg_co2e_total
    ## 1                       879.0195                  881.5167
    ## 2                      1256.7550                 1271.3019
    ## 3                       618.3569                  681.1100
    ## 4                       616.0545                  887.1841
    ## 5                      1069.6377                 1077.4663
    ## 6                      1110.9270                 1120.8631
    ## 7                       820.7151                  830.9263
    ## 8                      1127.4019                 1151.9549
    ## 9                       815.4360                  873.4248
    ## 10                      842.2334                  859.2536
    ## 11                      957.2524                  996.5277
    ## 12                      524.5064                  571.9262
    ## 13                      488.3080                  517.7540
    ## 14                      909.6575                  910.2483
    ## 15                      801.2140                  822.2034
    ## 16                     1027.7440                 1041.2965
    ## 17                      767.0913                 1053.4013
    ## 18                     1090.3795                 1119.5004
    ## 19                      722.9528                  727.0294
    ## 20                      494.5771                  511.9318
    ## 21                      576.7401                  653.2140
    ## 22                      393.3495                  423.2711
    ## 23                      710.5346                  712.2968
    ## 24                      732.4313                  738.1462
    ## 25                      638.6138                  827.4585
    ## 26                      655.0268                  683.8947
    ## 27                      945.6500                 1049.1267
    ## 28                      628.8802                  646.4838
    ## 29                     1163.2406                 1184.9666
    ## 30                      606.5892                  614.8966
    ## 31                      903.7514                  922.0514
    ## 32                     1027.2198                 1045.5628
    ## 33                      792.7928                  801.7087
    ## 34                      863.8506                  942.9841
    ## 35                      832.5686                  938.3366
    ## 36                      600.5620                  613.7625
    ## 37                      703.5787                  816.6904
    ## 38                      744.0907                  753.5322
    ## 39                      683.5901                  692.0379
    ## 40                      794.6788                  799.7065
    ## 41                      918.0952                  932.7027
    ## 42                      925.6277                  960.4986
    ## 43                     1182.9382                 1184.7977
    ## 44                      864.9402                  893.3720
    ## 45                      409.9149                  417.9726
    ## 46                     1047.6953                 1071.6840
    ## 47                      540.3304                  581.6096
    ## 48                      592.2942                  737.7532
    ## 49                      703.3874                  709.5625
    ## 50                     1092.8546                 1132.4417
    ## 51                      750.5027                  769.5537
    ## 52                      633.6178                  660.1071
    ## 53                      563.2489                  565.3576
    ## 54                     1051.8133                 1086.9216
    ## 55                      811.9998                  824.5953
    ## 56                      948.7461                  981.5409
    ## 57                      727.5414                  733.4432
    ## 58                      656.3005                  671.5994
    ## 59                      674.4313                  689.2373
    ## 60                     1073.7410                 1086.8388
    ## 61                      839.8328                  861.4014
    ## 62                     1046.8139                 1069.0389
    ## 63                      738.2614                  749.0495
    ## 64                      790.1116                  799.5900
    ## 65                      694.4979                  707.0768
    ## 66                      851.6012                  865.8009
    ## 67                      980.2673                 1004.5501
    ## 68                      684.1121                  696.2646
    ## 69                      294.6322                  301.2500
    ## 70                      848.2938                  950.3356
    ## 71                     1040.5531                 1050.5606
    ## 72                     1033.9425                 1038.5429
    ## 73                      704.0826                  713.2375
    ## 74                      883.7733                  895.8151
    ## 75                      724.3743                  814.0771
    ## 76                      498.0066                  505.5939
    ## 77                      949.7040                  956.6659
    ## 78                     1073.8977                 1085.0861
    ## 79                      526.4980                  545.9938
    ## 80                      882.2669                  886.3528
    ## 81                      656.3287                  696.3222
    ## 82                      921.2658                  962.9064
    ## 83                     1128.0856                 1173.2342
    ## 84                      604.5096                  606.4808
    ## 85                      900.1864                  914.8154
    ## 86                      954.8995                  998.1504
    ## 87                     1063.3033                 1088.5872
    ## 88                     1139.6142                 1159.3254
    ## 89                      694.2757                  816.4731
    ## 90                      583.4000                  695.3128
    ## 91                      714.2198                  717.2980
    ## 92                      754.8682                  771.4419
    ## 93                      954.7032                 1003.0260
    ## 94                      894.1377                  909.5734
    ## 95                      830.8940                  836.3966
    ## 96                      406.3880                  425.2634
    ## 97                      716.1180                  746.8807
    ## 98                      645.1853                  660.9800
    ## 99                      641.1139                  643.3822
    ## 100                     735.4063                  741.9206
    ## 101                     836.9682                  912.9293
    ## 102                     575.3822                  589.6344
    ## 103                     748.0092                  836.7185
    ## 104                     831.5805                  883.4259
    ## 105                     602.0645                  604.0418
    ## 106                     969.7061                 1001.4356
    ## 107                     973.8819                  998.7527
    ## 108                     621.1363                  686.5346
    ## 109                     828.3273                  840.4639
    ## 110                     614.6566                  687.9953
    ## 111                     418.4600                  426.8414
    ## 112                     553.3624                  616.3483
    ## 113                     684.6213                  725.2745
    ## 114                     670.6828                  729.7318
    ## 115                     712.1547                  715.2191
    ## 116                     919.5768                  959.8366
    ## 117                     874.8201                  876.6985
    ## 118                     841.0121                 1038.0033
    ## 119                     940.9539                 1066.6240
    ## 120                     518.8840                  532.0839
    ## 121                     545.3204                  547.1161
    ## 122                     902.6425                  938.2352
    ## 123                     837.9386                  851.5971
    ##     pescetarian_l_blue_green_wf pescetarian_l_blue_wf_total
    ## 1                     1168045.6                   512937.25
    ## 2                      942245.0                   157578.50
    ## 3                      864259.9                   158495.29
    ## 4                      644455.1                    51421.86
    ## 5                     1041135.5                   197009.61
    ## 6                      847182.8                   173832.32
    ## 7                      502022.9                    66161.43
    ## 8                     1025564.6                   207104.16
    ## 9                      671319.0                    82426.71
    ## 10                     798316.1                    57170.55
    ## 11                     525461.6                   101574.71
    ## 12                     941143.2                    62878.69
    ## 13                    1152138.0                    89964.30
    ## 14                     701958.2                   152929.69
    ## 15                     898070.4                    36770.00
    ## 16                     989750.2                   146446.35
    ## 17                     878223.4                    61763.09
    ## 18                    1017207.8                   137739.00
    ## 19                     959379.5                    63478.34
    ## 20                    1388754.5                    51887.73
    ## 21                    1303586.5                   195177.13
    ## 22                    1140031.4                    61560.24
    ## 23                     703556.5                   103493.44
    ## 24                    1098752.2                   109531.67
    ## 25                     597028.9                   131247.15
    ## 26                     651082.7                   143491.95
    ## 27                     877819.9                    79581.13
    ## 28                    1280055.1                   152013.58
    ## 29                     826081.9                    94427.59
    ## 30                    1294142.6                    48181.43
    ## 31                     808935.8                    44747.32
    ## 32                     810529.3                   177892.53
    ## 33                     557713.3                    67274.26
    ## 34                     495348.2                    55380.83
    ## 35                    1193455.4                   189721.65
    ## 36                     695632.9                   392679.09
    ## 37                     829955.4                    51506.10
    ## 38                     743489.8                    33212.37
    ## 39                    1328028.5                   134515.26
    ## 40                    1025038.0                    84069.33
    ## 41                     586753.6                    69281.10
    ## 42                     518651.9                    80341.58
    ## 43                    1257393.0                   112800.39
    ## 44                     479197.4                    72236.50
    ## 45                     968553.4                    92017.74
    ## 46                     783653.6                   128595.65
    ## 47                     851072.0                    42738.22
    ## 48                     877950.0                    62828.30
    ## 49                     836835.4                    61575.96
    ## 50                     470531.4                    96854.40
    ## 51                    1023336.0                   217728.68
    ## 52                     932101.0                   130415.21
    ## 53                    1173386.3                   528087.09
    ## 54                     435664.2                    62433.80
    ## 55                     677940.7                   173560.58
    ## 56                     714404.2                    93246.69
    ## 57                     783968.3                    63029.41
    ## 58                     577744.9                    92521.44
    ## 59                     716461.9                   184159.34
    ## 60                    1294647.5                   258742.07
    ## 61                    1090073.8                    82962.13
    ## 62                     907657.9                   216096.22
    ## 63                     783897.9                   203430.66
    ## 64                     798567.6                    52429.84
    ## 65                     788596.0                   205688.04
    ## 66                     766837.3                    30676.93
    ## 67                     531059.5                    79424.12
    ## 68                    1413320.3                   216580.06
    ## 69                    1076282.0                    66376.50
    ## 70                     828367.8                   153576.55
    ## 71                     857811.5                   232027.72
    ## 72                    1662439.5                   157790.42
    ## 73                     635344.9                   100747.54
    ## 74                    1164053.4                   159790.01
    ## 75                     658166.0                   106528.90
    ## 76                     841049.1                   118606.46
    ## 77                     991594.3                   143632.36
    ## 78                     909405.4                    45092.23
    ## 79                    1116163.2                   195631.28
    ## 80                    1107653.9                   106705.56
    ## 81                    1225088.8                   186347.55
    ## 82                     520272.3                    56342.35
    ## 83                     674938.6                   106277.45
    ## 84                    3675637.4                    86069.96
    ## 85                     735522.7                    70803.94
    ## 86                     566460.3                    86875.18
    ## 87                    1022848.2                   456208.64
    ## 88                    1026329.0                   427343.37
    ## 89                     791092.8                    71912.14
    ## 90                     696797.8                   164634.01
    ## 91                     984986.0                   137665.04
    ## 92                     602325.4                    30269.12
    ## 93                     913960.1                   188790.25
    ## 94                     861252.5                    52355.95
    ## 95                    1049947.3                    74761.09
    ## 96                    1528872.5                   106388.29
    ## 97                     896072.6                   346744.86
    ## 98                     950853.1                   121932.57
    ## 99                     779969.3                    37805.03
    ## 100                    601731.5                    51123.47
    ## 101                    588070.3                    58216.84
    ## 102                    656640.8                   110086.78
    ## 103                    563649.1                    69624.94
    ## 104                    870289.8                   164563.25
    ## 105                   1016203.3                   190824.66
    ## 106                    557074.3                    82601.93
    ## 107                    564253.5                    81362.07
    ## 108                    735428.0                   107328.34
    ## 109                   1258869.9                    98892.17
    ## 110                   1096061.0                   219561.50
    ## 111                   1486454.2                    53496.68
    ## 112                   1010434.7                   127481.30
    ## 113                    799587.5                   116488.73
    ## 114                   1341427.2                   115705.37
    ## 115                    891743.6                    45497.49
    ## 116                    458159.6                    87971.78
    ## 117                    755752.5                   154470.61
    ## 118                   1040265.7                    42931.72
    ## 119                    770001.7                    76982.37
    ## 120                    905894.7                   259362.64
    ## 121                   1136555.5                   131550.16
    ## 122                    833435.6                   205907.75
    ## 123                    810197.7                   176396.00
    ##     pescetarian_l_green_wf lacto_ovo_vegetarian_kg_co2e_excl_luc
    ## 1                 655108.3                             2170.7392
    ## 2                 784666.5                             1243.6415
    ## 3                 705764.6                             1074.5227
    ## 4                 593033.3                              849.9150
    ## 5                 844125.9                             1283.6285
    ## 6                 673350.5                             1636.9438
    ## 7                 435861.5                             1002.3996
    ## 8                 818460.4                             1426.2640
    ## 9                 588892.3                             1389.9985
    ## 10                741145.6                             1181.6031
    ## 11                423886.9                             1154.9213
    ## 12                878264.6                              714.3302
    ## 13               1062173.7                              738.5802
    ## 14                549028.5                             1115.4800
    ## 15                861300.4                              977.7755
    ## 16                843303.9                             2162.5288
    ## 17                816460.3                             1405.9557
    ## 18                879468.8                             1724.4690
    ## 19                895901.2                             1020.7520
    ## 20               1336866.8                              547.0215
    ## 21               1108409.4                              476.7142
    ## 22               1078471.2                              469.4602
    ## 23                600063.0                              871.1238
    ## 24                989220.5                             1133.4510
    ## 25                465781.7                              797.9380
    ## 26                507590.8                              679.8453
    ## 27                798238.8                             2170.0965
    ## 28               1128041.5                              868.1348
    ## 29                731654.3                             2063.4812
    ## 30               1245961.1                             1125.9612
    ## 31                764188.4                             1193.8626
    ## 32                632636.8                             1525.5069
    ## 33                490439.0                             1001.0521
    ## 34                439967.4                              811.0539
    ## 35               1003733.7                             1406.3703
    ## 36                302953.8                              916.9480
    ## 37                778449.3                              926.2472
    ## 38                710277.4                              715.8248
    ## 39               1193513.2                              829.6762
    ## 40                940968.7                             1249.5670
    ## 41                517472.5                              838.6725
    ## 42                438310.4                             1078.9197
    ## 43               1144592.6                             2079.8873
    ## 44                406960.9                              993.0674
    ## 45                876535.7                              528.9829
    ## 46                655058.0                             1296.5265
    ## 47                808333.8                              827.0368
    ## 48                815121.7                             1125.0781
    ## 49                775259.4                              911.9663
    ## 50                373677.0                              836.6946
    ## 51                805607.3                             1254.1171
    ## 52                801685.8                              854.3920
    ## 53                645299.2                              854.7144
    ## 54                373230.4                             1218.1153
    ## 55                504380.1                             1110.5299
    ## 56                621157.5                             1322.4697
    ## 57                720938.9                             1274.0190
    ## 58                485223.5                              614.6688
    ## 59                532302.6                             1521.2368
    ## 60               1035905.4                             1812.9109
    ## 61               1007111.6                             1297.2477
    ## 62                691561.6                             1462.8449
    ## 63                580467.2                             1095.9464
    ## 64                746137.7                              916.9618
    ## 65                582907.9                             1133.5995
    ## 66                736160.3                              783.6695
    ## 67                451635.3                             1078.7217
    ## 68               1196740.2                             2327.2315
    ## 69               1009905.5                              300.3256
    ## 70                674791.3                             1341.0744
    ## 71                625783.7                             1695.1414
    ## 72               1504649.1                             2037.2928
    ## 73                534597.3                              738.8203
    ## 74               1004263.3                             1534.4267
    ## 75                551637.1                             1058.8549
    ## 76                722442.7                              644.6475
    ## 77                847962.0                             1719.7351
    ## 78                864313.1                             1162.2335
    ## 79                920532.0                              894.5583
    ## 80               1000948.3                             1719.2454
    ## 81               1038741.2                             1404.8457
    ## 82                463930.0                             1012.3461
    ## 83                568661.1                             1735.9590
    ## 84               3589567.5                              598.7431
    ## 85                664718.8                             1315.2171
    ## 86                479585.1                             1031.0767
    ## 87                566639.6                             1600.4569
    ## 88                598985.7                             1842.8833
    ## 89                719180.6                             1100.1061
    ## 90                532163.8                              970.7796
    ## 91                847321.0                             1076.5962
    ## 92                572056.3                             1093.6650
    ## 93                725169.9                             1004.9556
    ## 94                808896.6                             1082.2919
    ## 95                975186.2                             1020.0044
    ## 96               1422484.2                              455.9069
    ## 97                549327.8                             1226.0365
    ## 98                828920.5                             1821.7762
    ## 99                742164.2                              751.5227
    ## 100               550608.0                              935.8018
    ## 101               529853.4                             1040.4885
    ## 102               546554.0                             1207.6244
    ## 103               494024.2                              663.8831
    ## 104               705726.6                             1008.0348
    ## 105               825378.7                             1088.9903
    ## 106               474472.4                             1040.8048
    ## 107               482891.4                             1180.2372
    ## 108               628099.6                              598.6284
    ## 109              1159977.7                             1480.1999
    ## 110               876499.5                              827.6746
    ## 111              1432957.5                              355.6535
    ## 112               882953.4                              784.1512
    ## 113               683098.7                             1053.5001
    ## 114              1225721.8                             1135.7317
    ## 115               846246.1                              917.2359
    ## 116               370187.8                             1179.8917
    ## 117               601281.9                             1190.4554
    ## 118               997334.0                              980.9358
    ## 119               693019.4                             2226.8943
    ## 120               646532.0                             1016.5057
    ## 121              1005005.4                             1703.8706
    ## 122               627527.9                             1077.1296
    ## 123               633801.7                             1014.6737
    ##     lacto_ovo_vegetarian_kg_co2e_total lacto_ovo_vegetarian_l_blue_green_wf
    ## 1                            2178.3697                            1308713.0
    ## 2                            1258.1884                             941367.4
    ## 3                            1189.7864                            1045411.0
    ## 4                            1197.2915                             690582.9
    ## 5                            1294.2358                            1054930.0
    ## 6                            1651.2620                             905101.6
    ## 7                            1020.2073                             515796.0
    ## 8                            1458.8634                            1071252.6
    ## 9                            1464.7026                             789273.5
    ## 10                           1218.8092                             931815.3
    ## 11                           1224.7011                             521503.8
    ## 12                            768.1750                            1025486.8
    ## 13                            875.8706                            1204233.8
    ## 14                           1117.0403                             785260.3
    ## 15                           1007.0310                             948366.6
    ## 16                           2181.9421                            1254554.9
    ## 17                           2010.3756                             986332.5
    ## 18                           1818.3015                            1067345.0
    ## 19                           1028.4250                            1187942.9
    ## 20                            567.4284                            1398068.0
    ## 21                            698.4486                            1197489.8
    ## 22                            507.4440                            1153736.3
    ## 23                            874.0152                             750157.8
    ## 24                           1146.6446                            1214114.5
    ## 25                           1226.2538                             688411.7
    ## 26                            726.5607                             634068.2
    ## 27                           2351.2385                            1100529.6
    ## 28                            907.1805                            1300548.2
    ## 29                           2092.7630                             906202.3
    ## 30                           1209.7687                            1594178.2
    ## 31                           1224.3633                             961191.8
    ## 32                           1574.2915                             943829.2
    ## 33                           1018.0807                             618021.2
    ## 34                            890.1874                             482913.6
    ## 35                           1576.0662                            1776606.8
    ## 36                            933.7156                             728754.3
    ## 37                           1104.3405                             875139.3
    ## 38                            725.9986                             746597.7
    ## 39                            838.4768                            1342840.6
    ## 40                           1258.9829                            1093322.7
    ## 41                            853.2799                             541397.8
    ## 42                           1130.5362                             553383.7
    ## 43                           2083.6910                            1579253.0
    ## 44                           1031.7351                             490876.7
    ## 45                            563.1849                            1121416.1
    ## 46                           1341.5537                             893542.5
    ## 47                            878.6603                             904385.3
    ## 48                           1512.4473                             977131.2
    ## 49                            922.9460                            1064490.3
    ## 50                            876.2817                             415881.2
    ## 51                           1278.5172                            1051098.8
    ## 52                            934.4150                            1042826.0
    ## 53                            861.5991                            1278046.6
    ## 54                           1273.8946                             460791.2
    ## 55                           1132.7940                             758888.8
    ## 56                           1382.0682                             812391.4
    ## 57                           1297.5964                            1185208.2
    ## 58                            641.5429                             613609.6
    ## 59                           1554.3199                             933481.2
    ## 60                           1839.6060                            1483634.4
    ## 61                           1321.7810                            1108843.0
    ## 62                           1497.7502                             992875.5
    ## 63                           1120.5815                             890285.7
    ## 64                            933.4859                             936326.4
    ## 65                           1158.8953                            1009238.4
    ## 66                            797.8692                             772462.4
    ## 67                           1115.2995                             534854.5
    ## 68                           2366.9356                            1561779.6
    ## 69                            307.1806                            1042598.0
    ## 70                           1820.9847                             926564.2
    ## 71                           1729.6242                            1129125.9
    ## 72                           2043.8611                            1843453.0
    ## 73                            750.8331                             648621.8
    ## 74                           1552.1581                            1537705.1
    ## 75                           1156.6538                             740329.0
    ## 76                            655.3180                             989908.2
    ## 77                           1740.4444                            1270468.0
    ## 78                           1175.5164                             930238.1
    ## 79                            924.5743                            1517129.0
    ## 80                           1732.0301                            1222363.6
    ## 81                           1449.9404                            1294264.5
    ## 82                           1062.8235                             545489.6
    ## 83                           1839.0308                             726972.7
    ## 84                            600.7142                            3672710.7
    ## 85                           1343.6530                             852304.2
    ## 86                           1106.4573                             568181.0
    ## 87                           1655.9214                            1169816.6
    ## 88                           1867.2598                            1107655.0
    ## 89                           1309.0467                             862416.8
    ## 90                           1186.1517                             904054.0
    ## 91                           1095.6195                            1131634.0
    ## 92                           1126.2676                             717114.9
    ## 93                           1092.1483                            1081059.3
    ## 94                           1104.3170                             922974.3
    ## 95                           1029.1380                            1149910.3
    ## 96                            475.9814                            1540036.4
    ## 97                           1300.1048                             998536.6
    ## 98                           1863.9419                            1488148.0
    ## 99                            754.5994                             804786.3
    ## 100                           947.9370                             700671.7
    ## 101                          1135.1620                             653290.3
    ## 102                          1234.6190                             845045.2
    ## 103                           825.0102                             690404.8
    ## 104                          1128.4207                            1080042.1
    ## 105                          1097.0157                            1095503.6
    ## 106                          1084.2818                             551921.6
    ## 107                          1220.9399                             532741.2
    ## 108                           692.8795                             750916.0
    ## 109                          1495.4728                            1306037.0
    ## 110                          1083.4408                            1175596.4
    ## 111                           365.2699                            1605344.1
    ## 112                           852.0187                            1159185.4
    ## 113                          1098.9682                             910257.2
    ## 114                          1237.5435                            1376961.7
    ## 115                           921.9877                            1053908.6
    ## 116                          1265.2261                             485099.1
    ## 117                          1193.9316                             759347.4
    ## 118                          1174.5690                            1076173.7
    ## 119                          2459.6168                            1092655.6
    ## 120                          1037.6227                            1025694.7
    ## 121                          1711.0703                            1319626.9
    ## 122                          1149.6183                             842883.3
    ## 123                          1061.5373                             816644.4
    ##     lacto_ovo_vegetarian_l_blue_wf_total lacto_ovo_vegetarian_l_green_wf
    ## 1                              594591.93                        714121.1
    ## 2                              156057.87                        785309.5
    ## 3                              160218.93                        885192.1
    ## 4                               50753.04                        639829.9
    ## 5                              183460.74                        871469.2
    ## 6                              123289.50                        781812.1
    ## 7                               34721.15                        481074.8
    ## 8                              211403.44                        859849.1
    ## 9                               75646.74                        713626.8
    ## 10                              28085.79                        903729.5
    ## 11                              50747.56                        470756.3
    ## 12                              55451.91                        970034.9
    ## 13                              62066.42                       1142167.4
    ## 14                             108888.80                        676371.5
    ## 15                              32784.97                        915581.6
    ## 16                             141123.44                       1113431.5
    ## 17                              34374.48                        951958.0
    ## 18                             107367.67                        959977.3
    ## 19                              44272.54                       1143670.4
    ## 20                              51066.22                       1347001.8
    ## 21                              65511.65                       1131978.1
    ## 22                              44138.32                       1109598.0
    ## 23                              67001.41                        683156.4
    ## 24                             114513.11                       1099601.4
    ## 25                             125967.97                        562443.8
    ## 26                              97939.32                        536128.9
    ## 27                              47972.11                       1052557.5
    ## 28                              65401.48                       1235146.7
    ## 29                              63306.49                        842895.8
    ## 30                              53560.69                       1540617.5
    ## 31                              36834.68                        924357.1
    ## 32                             131490.11                        812339.1
    ## 33                              30102.55                        587918.7
    ## 34                              43037.18                        439876.4
    ## 35                             111142.54                       1665464.3
    ## 36                             398042.27                        330712.0
    ## 37                              47328.04                        827811.3
    ## 38                              25868.78                        720728.9
    ## 39                             135562.28                       1207278.4
    ## 40                              72232.67                       1021090.1
    ## 41                              37227.67                        504170.1
    ## 42                              55400.80                        497982.9
    ## 43                             138632.86                       1440620.1
    ## 44                              47331.37                        443545.3
    ## 45                              58725.93                       1062690.2
    ## 46                             131552.68                        761989.8
    ## 47                              40827.87                        863557.5
    ## 48                              45184.80                        931946.4
    ## 49                              26301.19                       1038189.1
    ## 50                              50826.95                        365054.2
    ## 51                             200549.22                        850549.5
    ## 52                              61989.57                        980836.5
    ## 53                             525653.72                        752392.9
    ## 54                              46411.83                        414379.4
    ## 55                             140578.43                        618310.4
    ## 56                              78685.36                        733706.0
    ## 57                              63102.79                       1122105.4
    ## 58                              63844.72                        549764.9
    ## 59                             218563.57                        714917.6
    ## 60                             258783.99                       1224850.4
    ## 61                              62419.46                       1046423.6
    ## 62                             240884.65                        751990.8
    ## 63                             183046.46                        707239.3
    ## 64                              36049.66                        900276.7
    ## 65                             236691.67                        772546.8
    ## 66                              21939.71                        750522.7
    ## 67                              47540.90                        487313.6
    ## 68                             188111.62                       1373668.0
    ## 69                              39993.96                       1002604.0
    ## 70                              84425.99                        842138.2
    ## 71                             247995.94                        881130.0
    ## 72                             129617.18                       1713835.8
    ## 73                              92974.12                        555647.7
    ## 74                             147874.28                       1389830.9
    ## 75                              94478.23                        645850.8
    ## 76                             117158.30                        872749.9
    ## 77                             110348.32                       1160119.6
    ## 78                              37958.36                        892279.8
    ## 79                             212403.10                       1304725.9
    ## 80                             101391.28                       1120972.3
    ## 81                             168433.40                       1125831.1
    ## 82                              49780.76                        495708.8
    ## 83                              86393.80                        640578.9
    ## 84                              80956.31                       3591754.3
    ## 85                              64393.02                        787911.2
    ## 86                              50564.08                        517616.9
    ## 87                             427456.64                        742359.9
    ## 88                             419921.26                        687733.7
    ## 89                              61842.54                        800574.3
    ## 90                             151645.64                        752408.3
    ## 91                              63391.40                       1068242.6
    ## 92                              22956.87                        694158.0
    ## 93                             203659.77                        877399.6
    ## 94                              37045.98                        885928.3
    ## 95                              54088.62                       1095821.7
    ## 96                              99927.75                       1440108.6
    ## 97                             308392.72                        690143.9
    ## 98                              95000.74                       1393147.3
    ## 99                              26808.74                        777977.6
    ## 100                             32467.60                        668204.1
    ## 101                             44190.43                        609099.9
    ## 102                            105883.68                        739161.5
    ## 103                             59713.76                        630691.0
    ## 104                            164384.12                        915658.0
    ## 105                            151439.83                        944063.8
    ## 106                             42319.39                        509602.2
    ## 107                             39535.93                        493205.2
    ## 108                             73755.37                        677160.6
    ## 109                             79645.10                       1226391.9
    ## 110                            155960.04                       1019636.4
    ## 111                             45630.45                       1559713.6
    ## 112                            149455.85                       1009729.5
    ## 113                            119913.88                        790343.3
    ## 114                             52353.72                       1324608.0
    ## 115                             36454.72                       1017453.9
    ## 116                             50985.55                        434113.6
    ## 117                             90961.97                        668385.4
    ## 118                             39365.05                       1036808.7
    ## 119                             59156.75                       1033498.9
    ## 120                            288756.53                        736938.2
    ## 121                            101684.90                       1217942.0
    ## 122                            138487.75                        704395.6
    ## 123                             99864.17                        716780.3
    ##     X2.3_vegan_kg_co2e_excl_luc X2.3_vegan_kg_co2e_total
    ## 1                      753.6340                 756.1502
    ## 2                      815.2231                 833.0340
    ## 3                      528.3851                 597.2911
    ## 4                      956.5001                1269.1834
    ## 5                      784.8361                 798.7925
    ## 6                     1088.0534                1130.5242
    ## 7                      600.6277                 618.7759
    ## 8                      754.8884                 778.4354
    ## 9                      629.8362                 718.6603
    ## 10                     610.3670                 642.6805
    ## 11                     674.4342                 735.7358
    ## 12                     536.1291                 615.2437
    ## 13                     426.0466                 503.3199
    ## 14                     942.0433                 963.9601
    ## 15                     526.7469                 545.7872
    ## 16                     985.2493                1004.8302
    ## 17                     913.8956                1393.1516
    ## 18                     720.2677                 747.0441
    ## 19                     493.0789                 509.5149
    ## 20                     528.2636                 548.7761
    ## 21                     503.6717                 675.5729
    ## 22                     456.6058                 497.0014
    ## 23                     644.1755                 648.4227
    ## 24                     579.7007                 603.1676
    ## 25                     706.8513                1457.6168
    ## 26                     650.0061                 704.0758
    ## 27                     963.5494                1097.8111
    ## 28                     479.3245                 515.5779
    ## 29                     844.2739                 881.9171
    ## 30                     445.3798                 471.5969
    ## 31                     626.9342                 670.3618
    ## 32                     622.0749                 699.6703
    ## 33                     556.8435                 576.8764
    ## 34                     717.4966                 774.2417
    ## 35                     762.8805                 894.7563
    ## 36                     535.1102                 565.3126
    ## 37                     564.1454                 695.8901
    ## 38                     544.9163                 567.3387
    ## 39                     486.9204                 494.9145
    ## 40                     717.1868                 725.5568
    ## 41                     616.9001                 638.4484
    ## 42                     690.3361                 746.6632
    ## 43                     730.7370                 737.7026
    ## 44                     588.7904                 650.9758
    ## 45                     335.1253                 359.0290
    ## 46                     660.5396                 691.6308
    ## 47                     534.6976                 576.7485
    ## 48                     595.1285                 815.8686
    ## 49                     547.8172                 563.1718
    ## 50                     804.2555                 842.8120
    ## 51                     622.9602                 642.6923
    ## 52                     466.7607                 507.7390
    ## 53                     469.0675                 474.2390
    ## 54                     782.2230                 836.9692
    ## 55                     709.2844                 780.8181
    ## 56                     620.0144                 676.4417
    ## 57                     528.3888                 555.5646
    ## 58                     573.5336                 612.5952
    ## 59                     631.4539                 705.4689
    ## 60                     852.0832                 876.7269
    ## 61                     638.6294                 659.2307
    ## 62                     834.4453                 846.2827
    ## 63                     809.0615                 882.7378
    ## 64                     532.3428                 550.9102
    ## 65                     636.1508                 719.3915
    ## 66                     538.5533                 561.1196
    ## 67                     832.3594                 867.9251
    ## 68                     851.5028                 878.1091
    ## 69                     335.2659                 342.2350
    ## 70                     563.8117                 684.0139
    ## 71                     683.4431                 711.1173
    ## 72                     824.0300                 829.1997
    ## 73                     651.0579                 696.9094
    ## 74                     765.3120                 777.6478
    ## 75                     554.7230                 648.3799
    ## 76                     513.7586                 524.6570
    ## 77                     579.9724                 594.4820
    ## 78                     718.4480                 748.2220
    ## 79                     457.0593                 480.7889
    ## 80                     742.7176                 764.6250
    ## 81                     737.5816                 779.3724
    ## 82                     664.5957                 732.2887
    ## 83                     971.5940                1021.7971
    ## 84                     575.1636                 576.9544
    ## 85                     663.1705                 688.5446
    ## 86                     694.9042                 746.4944
    ## 87                     756.0292                 803.2492
    ## 88                     796.9822                 815.2287
    ## 89                     746.9522                 986.7691
    ## 90                     494.5253                 612.0954
    ## 91                     561.9803                 587.1087
    ## 92                     497.2327                 533.6344
    ## 93                     738.4228                 862.0371
    ## 94                     581.0495                 593.7237
    ## 95                     648.9522                 714.2188
    ## 96                     389.1275                 408.6190
    ## 97                     582.3078                 649.6323
    ## 98                     619.7138                 644.0983
    ## 99                     508.0749                 512.3443
    ## 100                    554.3671                 571.0936
    ## 101                    607.5833                 721.0844
    ## 102                    572.9845                 633.7221
    ## 103                    667.4432                 811.4595
    ## 104                    614.6134                 722.1774
    ## 105                    479.1718                 481.8735
    ## 106                    712.4178                 757.8591
    ## 107                    725.4427                 772.5225
    ## 108                    545.3563                 650.4934
    ## 109                    648.6802                 660.5536
    ## 110                    512.6693                 647.2217
    ## 111                    360.6199                 372.1746
    ## 112                    446.5536                 522.2515
    ## 113                    539.4975                 587.0451
    ## 114                    518.2854                 573.9156
    ## 115                    538.6471                 549.9909
    ## 116                    697.1852                 762.7344
    ## 117                    734.1675                 740.5162
    ## 118                    845.4583                1116.5928
    ## 119                   1030.4884                1218.0226
    ## 120                    538.7314                 584.2962
    ## 121                    725.8351                 733.6031
    ## 122                    845.6994                1021.9823
    ## 123                    752.0723                 822.3302
    ##     X2.3_vegan_l_blue_green_wf X2.3_vegan_l_blue_wf_total X2.3_vegan_l_green_wf
    ## 1                    1181193.4                  491555.05              689638.3
    ## 2                     945348.8                  136343.78              809005.1
    ## 3                     911940.3                  166664.52              745275.8
    ## 4                     674639.9                   51269.35              623370.5
    ## 5                    1065401.1                  172584.32              892816.8
    ## 6                     898958.3                  132428.29              766530.0
    ## 7                     488658.2                   41011.87              447646.3
    ## 8                    1046054.6                  204970.73              841083.9
    ## 9                     748065.9                   78211.20              669854.7
    ## 10                    795966.5                   30977.95              764988.6
    ## 11                    496864.5                   58381.68              438482.9
    ## 12                    990141.5                   53733.85              936407.6
    ## 13                   1191790.3                   66330.40             1125459.9
    ## 14                    773658.4                  123985.49              649672.9
    ## 15                    874910.2                   35670.42              839239.7
    ## 16                   1047810.9                  141612.02              906198.8
    ## 17                    967816.3                   37866.42              929949.9
    ## 18                   1045076.4                  103207.08              941869.3
    ## 19                    860867.5                   49010.69              811856.8
    ## 20                   1421844.4                   44572.01             1377272.3
    ## 21                   1219603.1                  104595.26             1115007.8
    ## 22                   1183068.3                   49319.89             1133748.4
    ## 23                    715171.1                   78818.14              636353.0
    ## 24                   1229609.8                  109532.11             1120077.7
    ## 25                    622455.8                  116569.96              505885.9
    ## 26                    629721.3                  102598.57              527122.7
    ## 27                    906733.5                   50533.17              856200.4
    ## 28                   1278149.6                   81412.41             1196737.2
    ## 29                    846761.1                   67097.54              779663.6
    ## 30                   1369712.8                   42660.87             1327051.9
    ## 31                    810242.0                   43269.88              766972.1
    ## 32                    843759.6                  160339.63              683419.9
    ## 33                    520473.9                   36396.20              484077.7
    ## 34                    495024.0                   50780.53              444243.4
    ## 35                   1231169.8                  138737.22             1092432.6
    ## 36                    671412.1                  365937.81              305474.3
    ## 37                    849904.0                   47087.47              802816.6
    ## 38                    713494.9                   31395.79              682099.1
    ## 39                   1315754.1                  112461.87             1203292.2
    ## 40                   1120874.1                   76712.52             1044161.5
    ## 41                    534883.8                   51228.25              483655.5
    ## 42                    522953.7                   60768.64              462185.1
    ## 43                   1189948.7                  110613.12             1079335.6
    ## 44                    465616.2                   52731.96              412884.2
    ## 45                   1095737.5                   66866.45             1028871.0
    ## 46                    755782.1                  114935.82              640846.3
    ## 47                    868362.5                   40757.01              827605.5
    ## 48                    975175.0                   46048.26              929126.8
    ## 49                    672971.4                   31806.74              641164.6
    ## 50                    503086.7                   75701.04              427385.6
    ## 51                   1016930.9                  194178.71              822752.2
    ## 52                    916663.2                   72178.99              844484.2
    ## 53                   1228041.0                  526083.13              701957.8
    ## 54                    431905.7                   54437.10              377468.6
    ## 55                    780593.0                  163072.16              617520.9
    ## 56                    719272.8                   75922.57              643350.3
    ## 57                    874003.7                   66654.45              807349.3
    ## 58                    593615.0                   72715.18              520899.8
    ## 59                    800779.6                  189775.05              611004.6
    ## 60                   1272790.7                  251264.33             1021526.4
    ## 61                   1127362.4                   66022.55             1061339.8
    ## 62                    898327.8                  191771.32              706556.5
    ## 63                    823070.9                  173986.64              649084.2
    ## 64                    778111.8                   44060.10              734051.7
    ## 65                    890258.3                  206534.94              683723.4
    ## 66                    673988.5                   27813.06              646175.5
    ## 67                    529997.4                   54231.90              475765.5
    ## 68                   1436699.9                  189415.09             1247284.8
    ## 69                   1051504.1                   46605.88             1004898.2
    ## 70                    852908.6                   97512.64              755396.0
    ## 71                    955001.9                  224459.15              730542.8
    ## 72                   1709777.5                  127975.78             1581801.7
    ## 73                    632684.7                   95559.45              537125.3
    ## 74                   1103807.9                  125471.01              978336.9
    ## 75                    708580.7                   92746.51              615834.2
    ## 76                    881725.2                  106916.25              774808.9
    ## 77                    939245.7                   94559.44              844686.2
    ## 78                    856853.1                   44955.66              811897.5
    ## 79                   1180529.9                  200726.92              979803.0
    ## 80                   1133154.4                   92925.01             1040229.4
    ## 81                   1275471.7                  174759.68             1100712.1
    ## 82                    535078.0                   55220.11              479857.9
    ## 83                    715637.7                   95919.59              619718.2
    ## 84                   3672807.6                   76106.47             3596701.1
    ## 85                    709773.9                   62453.34              647320.6
    ## 86                    543527.4                   63685.22              479842.2
    ## 87                   1138116.5                  441029.57              697086.9
    ## 88                   1022620.2                  408589.14              614031.0
    ## 89                    907388.9                   66351.39              841037.6
    ## 90                    740770.1                  163817.31              576952.8
    ## 91                   1014824.8                   67884.26              946940.6
    ## 92                    565493.0                   21618.66              543874.3
    ## 93                    970170.6                  181013.51              789157.1
    ## 94                    784172.7                   40399.52              743773.2
    ## 95                   1014750.9                   53019.15              961731.7
    ## 96                   1550434.4                  102422.91             1448011.5
    ## 97                    937526.6                  303980.04              633546.6
    ## 98                   1222632.4                   95365.90             1127266.5
    ## 99                    787063.0                   32272.97              754790.0
    ## 100                   596615.8                   34086.07              562529.7
    ## 101                   578054.8                   48720.31              529334.5
    ## 102                   758712.2                  102654.96              656057.2
    ## 103                   657017.4                   60110.49              596906.9
    ## 104                   892204.1                  144386.33              747817.8
    ## 105                  1022987.6                  161619.47              861368.2
    ## 106                   506521.7                   56641.01              449880.7
    ## 107                   529123.8                   48446.92              480676.8
    ## 108                   768478.5                   78664.00              689814.5
    ## 109                  1256848.1                   77799.22             1179048.9
    ## 110                  1079763.6                  164320.43              915443.2
    ## 111                  1595884.5                   47108.03             1548776.5
    ## 112                  1053544.2                  127511.55              926032.7
    ## 113                   815055.7                  107504.47              707551.3
    ## 114                  1342586.2                   65884.27             1276701.9
    ## 115                   877347.0                   35813.49              841533.5
    ## 116                   450155.9                   59316.58              390839.3
    ## 117                   743296.8                  103695.91              639600.9
    ## 118                  1112855.9                   43673.51             1069182.4
    ## 119                   918184.4                   56517.19              861667.2
    ## 120                   982315.1                  269409.00              712906.1
    ## 121                  1152363.3                   94625.64             1057737.7
    ## 122                   957850.0                  156876.59              800973.4
    ## 123                   863332.1                  115963.63              747368.4
    ##     vegan_kg_co2e_excl_luc vegan_kg_co2e_total vegan_l_blue_green_wf
    ## 1                 255.5508            255.5508             1120862.2
    ## 2                 363.2835            363.2844              782696.2
    ## 3                 260.0278            287.6409              774068.3
    ## 4                 266.2676            511.9696              610001.0
    ## 5                 375.6491            375.8927              877579.1
    ## 6                 289.0375            296.2025              672380.8
    ## 7                 270.8953            272.5019              407463.9
    ## 8                 276.8642            283.7427              904277.7
    ## 9                 250.1291            299.6997              660116.1
    ## 10                305.5300            313.3902              661889.9
    ## 11                282.6662            305.2095              408222.6
    ## 12                239.5103            312.7704              908419.1
    ## 13                240.8092            250.4453             1156362.2
    ## 14                241.3871            241.4550              547931.1
    ## 15                286.9564            288.6970              766592.6
    ## 16                257.2902            270.3717              866372.6
    ## 17                255.5613            379.3978              785535.6
    ## 18                282.5335            289.0178             1046268.0
    ## 19                300.2148            300.8417              702065.3
    ## 20                263.3127            284.9105             1412681.2
    ## 21                347.1433            548.2266             1204294.9
    ## 22                233.9910            270.9563             1169993.4
    ## 23                279.2083            279.9279              584966.5
    ## 24                276.6570            276.7857             1273203.7
    ## 25                269.7601            312.9341              508022.1
    ## 26                396.5775            445.0430              538135.7
    ## 27                231.7268            307.3657              745196.2
    ## 28                214.4556            230.1863             1244989.1
    ## 29                223.5060            249.5548              751078.0
    ## 30                224.6063            229.0860             1418476.3
    ## 31                272.6996            287.2189              642462.3
    ## 32                278.3977            278.7483              642785.8
    ## 33                268.6568            269.4165              411155.2
    ## 34                338.7074            342.7674              396355.6
    ## 35                268.4746            347.4823              873759.6
    ## 36                289.3047            306.7275              579783.2
    ## 37                240.1050            244.0668              810048.1
    ## 38                276.4886            276.5332              581794.3
    ## 39                228.7106            236.0118             1311731.5
    ## 40                289.5321            292.7625             1028198.6
    ## 41                276.7865            282.7513              410330.9
    ## 42                281.5547            300.6698              421500.2
    ## 43                274.1752            274.1780             1006497.4
    ## 44                272.1573            312.0783              385453.6
    ## 45                206.7734            211.8705             1127289.0
    ## 46                299.5178            300.8375              595318.8
    ## 47                221.9010            252.2714              796961.4
    ## 48                221.4715            234.4742              831925.6
    ## 49                281.9005            282.6528              466800.6
    ## 50                264.2304            264.5502              406523.5
    ## 51                304.2484            320.3240              992155.1
    ## 52                287.1578            312.3556              885929.5
    ## 53                304.0347            304.0347             1147162.1
    ## 54                248.0659            248.1785              339050.2
    ## 55                280.0006            288.9007              545100.4
    ## 56                270.0367            292.8264              611311.9
    ## 57                246.1125            246.3722              743930.8
    ## 58                299.3973            331.5475              502844.2
    ## 59                284.3470            293.1184              653839.1
    ## 60                323.4384            336.3949             1102196.1
    ## 61                244.5046            263.4330             1067378.5
    ## 62                293.7026            294.4129              756606.3
    ## 63                297.3591            300.9962              699334.5
    ## 64                319.8702            319.9699              637487.4
    ## 65                287.9414            295.1236              678295.7
    ## 66                290.2734            291.1999              534692.1
    ## 67                302.7732            302.7772              379587.3
    ## 68                292.6869            300.7727             1341619.1
    ## 69                243.6073            250.0553             1011921.7
    ## 70                258.4734            305.2957              777932.6
    ## 71                294.0823            294.8065              935437.2
    ## 72                291.5269            296.2325             1545805.1
    ## 73                299.4558            299.6181              514496.5
    ## 74                241.3868            248.6689              870805.3
    ## 75                260.4042            345.4944              638731.7
    ## 76                226.6041            233.8190              770292.3
    ## 77                303.6193            304.4032              780764.6
    ## 78                331.1439            331.4226              729490.5
    ## 79                272.9828            288.5389              993835.9
    ## 80                277.8558            277.8970             1021084.8
    ## 81                319.6144            357.7475             1193306.4
    ## 82                250.1874            269.1909              435465.8
    ## 83                287.2236            294.5788              605653.9
    ## 84                256.7444            258.1188             3566369.7
    ## 85                338.8205            340.2269              593238.6
    ## 86                255.0621            259.8293              447125.3
    ## 87                291.9998            304.4056              984635.0
    ## 88                241.7465            255.8846              917119.7
    ## 89                282.6482            403.5605              821656.3
    ## 90                255.3107            368.2662              637694.7
    ## 91                297.5885            301.8507              940353.0
    ## 92                278.2443            280.4344              475639.8
    ## 93                313.8235            400.1975              792864.7
    ## 94                301.0402            301.8396              652886.9
    ## 95                314.6870            323.7838              850388.5
    ## 96                272.3379            292.3267             1508805.0
    ## 97                267.7781            282.6587              808126.0
    ## 98                272.9848            285.7491             1233077.9
    ## 99                279.7639            280.1783              685624.6
    ## 100               287.8070            289.8749              496613.8
    ## 101               254.7664            365.1182              471738.6
    ## 102               253.0377            263.8632              629677.3
    ## 103               313.6157            458.9372              565472.4
    ## 104               298.3922            325.1315              738282.7
    ## 105               293.8322            294.2846             1010580.1
    ## 106               263.0390            268.5656              389216.0
    ## 107               285.7890            288.0630              459112.8
    ## 108               284.0809            387.3066              694797.8
    ## 109               250.2854            260.1061             1185366.6
    ## 110               299.2744            395.3690             1026638.8
    ## 111               267.3913            275.3588             1633919.5
    ## 112               266.1811            335.6761              917446.4
    ## 113               309.8551            357.0872              695739.2
    ## 114               193.6952            221.2799             1299247.3
    ## 115               334.4825            339.5390              756412.0
    ## 116               263.0645            267.5252              361656.7
    ## 117               264.0546            265.3908              619399.0
    ## 118               298.6757            571.4226             1008895.6
    ## 119               240.4028            325.0700              673194.1
    ## 120               261.2701            271.5407              885148.0
    ## 121               239.1445            239.9253             1020266.2
    ## 122               301.2432            319.8383              745970.4
    ## 123               315.2345            317.3113              713883.2
    ##     vegan_l_blue_wf_total vegan_l_green_wf baseline_kg_co2e_luc
    ## 1               491242.47         629619.7             3.420731
    ## 2               110219.31         672476.9            60.670969
    ## 3               170358.44         603709.9           125.260881
    ## 4                52358.56         557642.4           564.245491
    ## 5               163419.97         714159.1            41.407968
    ## 6               121040.54         551340.2           126.937423
    ## 7                36772.00         370691.9            61.806216
    ## 8               196450.59         707827.1            53.205846
    ## 9                75531.97         584584.2           173.570008
    ## 10               25663.59         636226.3            91.607431
    ## 11               49334.42         358888.2           153.737282
    ## 12               48308.65         860110.5            76.524256
    ## 13               58423.58        1097938.6           131.445527
    ## 14              110082.32         437848.8            55.831329
    ## 15               34153.37         732439.3            52.436745
    ## 16              151644.08         714728.6            16.173454
    ## 17               33505.51         752030.1          1509.959656
    ## 18               94695.78         951572.2            73.045419
    ## 19               47750.67         654314.6            51.284550
    ## 20               49543.01        1363138.2            18.820015
    ## 21               63726.85        1140568.1            79.903259
    ## 22               43637.43        1126356.0            33.789173
    ## 23               74184.92         510781.6            13.531980
    ## 24              104896.45        1168307.3            60.377709
    ## 25              105188.90         402833.2          2265.124215
    ## 26               88135.98         449999.8            67.207325
    ## 27               44866.09         700330.1           166.567493
    ## 28               60608.04        1184381.0            38.750685
    ## 29               54971.02         696107.0            57.664860
    ## 30               35872.94        1382603.4            23.231689
    ## 31               39502.84         602959.5            96.597932
    ## 32              159480.68         483305.1           212.528625
    ## 33               33919.72         377235.5            57.839939
    ## 34               44949.11         351406.5           175.339372
    ## 35              123425.31         750334.3           171.036065
    ## 36              326914.17         252869.0            68.294320
    ## 37               45784.52         764263.6           337.408341
    ## 38               26153.03         555641.2            65.740792
    ## 39              145508.25        1166223.2             6.868731
    ## 40               76974.57         951224.0            13.632284
    ## 41               40418.24         369912.6            56.657951
    ## 42               52762.64         368737.6           156.242912
    ## 43              103531.23         902966.2            16.046415
    ## 44               47835.48         337618.1           123.053830
    ## 45               56946.38        1070342.6            33.843348
    ## 46               98188.06         497130.7           110.864464
    ## 47               37858.16         759103.2            50.392775
    ## 48               39036.65         792888.9           483.952341
    ## 49               29323.89         437476.7            42.050643
    ## 50               61353.91         345169.6           127.955943
    ## 51              182012.65         810142.4            18.109226
    ## 52               50753.11         835176.4            45.369124
    ## 53              525318.07         621844.0            12.573119
    ## 54               50454.99         288595.2           191.123058
    ## 55              167215.46         377884.9           261.426122
    ## 56               66513.86         544798.0           147.392527
    ## 57               60654.73         683276.0            72.635974
    ## 58               59421.31         443422.9            49.256654
    ## 59              177476.26         476362.8           149.148775
    ## 60              249233.41         852962.7            57.983578
    ## 61               59230.90        1008147.6            18.227052
    ## 62              173044.22         583562.1            32.672058
    ## 63              172876.28         526458.3           268.491310
    ## 64               41555.52         595931.9            55.691375
    ## 65              177008.92         501286.8           206.168029
    ## 66               23473.74         511218.3            72.566901
    ## 67               40424.20         339163.1           114.435292
    ## 68              170619.77        1170999.3            22.354715
    ## 69               39850.25         972071.4             8.021434
    ## 70               83646.26         694286.3           227.560102
    ## 71              221292.67         714144.6            82.411054
    ## 72              124745.08        1421060.0             4.851885
    ## 73               84583.38         429913.1           149.944024
    ## 74              135920.06         734885.3            19.972291
    ## 75               91796.08         546935.7           130.011459
    ## 76              100626.96         669665.3            18.643931
    ## 77               82020.66         698744.0            30.796657
    ## 78               40017.28         689473.2           102.945755
    ## 79              203038.35         790797.5            39.692986
    ## 80               96203.30         924881.5            37.627021
    ## 81              172846.86        1020459.5            44.806336
    ## 82               50217.83         385248.0           181.223732
    ## 83               90384.32         515269.5           144.568881
    ## 84               82289.24        3484080.5             2.884293
    ## 85               57076.69         536161.9            59.405555
    ## 86               52804.21         394321.1           163.405915
    ## 87              440844.80         543790.2           109.672474
    ## 88              416386.56         500733.2            21.030980
    ## 89               61317.82         760338.5           438.853131
    ## 90              169527.05         468167.6           115.726884
    ## 91               47301.03         893052.0            35.662800
    ## 92               16785.66         458854.2           121.732000
    ## 93              145368.48         647496.3           213.367235
    ## 94               36042.96         616843.9            39.010914
    ## 95               43004.65         807383.9           195.679956
    ## 96               99462.27        1409342.7            10.197094
    ## 97              319688.68         488437.3           159.994816
    ## 98               92159.98        1140917.9            26.370838
    ## 99               28372.58         657252.0            11.372344
    ## 100              30264.39         466349.4            36.999738
    ## 101              45549.18         426189.4           135.287541
    ## 102             113746.20         515931.1           147.620457
    ## 103              50229.14         515243.2           152.136034
    ## 104             122757.80         615524.9           296.489568
    ## 105             154785.25         855794.9             3.438411
    ## 106              47701.29         341514.7           137.478856
    ## 107              40515.71         418597.1           145.857122
    ## 108              67133.76         627664.1           113.073679
    ## 109              70183.37        1115183.2            11.756116
    ## 110             157120.24         869518.6           120.289726
    ## 111              46158.45        1587761.1            13.297112
    ## 112             110308.47         807137.9            98.119056
    ## 113              97745.00         597994.2            53.078236
    ## 114              45702.38        1253544.9            66.364035
    ## 115              31688.10         724723.9            24.365014
    ## 116              54495.24         307161.4           210.358292
    ## 117              97284.10         522114.9            20.445806
    ## 118              42766.29         966129.3           323.714124
    ## 119              52063.48         621130.6           364.821085
    ## 120             270691.05         614456.9            46.945244
    ## 121              92970.74         927295.5             6.983999
    ## 122             143651.56         602318.9           559.157363
    ## 123              97656.05         616227.2           199.812413
    ##     meatless_day_kg_co2e_luc no_dairy_kg_co2e_luc low_red_meat_kg_co2e_luc
    ## 1                   7.560487             5.598448                 7.765229
    ## 2                  47.876216            38.888086                51.020473
    ## 3                 146.316332           247.490872               151.491784
    ## 4                 432.464463           429.699317               398.237329
    ## 5                  36.985702            37.404500                41.382093
    ## 6                  98.973059           110.527403                99.328407
    ## 7                  46.456597            51.076467                41.552318
    ## 8                  53.414741            60.359648                56.883962
    ## 9                 154.098675           160.411162               165.874600
    ## 10                 74.932303            66.838535                63.218525
    ## 11                128.955735           142.160065               121.614925
    ## 12                 85.540847            85.292118                81.984614
    ## 13                201.796465           193.586142               210.286212
    ## 14                 56.463796            65.208197                35.075513
    ## 15                 50.156423            64.143896                53.639916
    ## 16                 30.698904            35.558946                29.129071
    ## 17               1106.427119          1032.307241              1108.754364
    ## 18                 71.142402            63.781279                63.220348
    ## 19                 42.285504            50.297219                48.054253
    ## 20                 18.636964            16.506358                18.341968
    ## 21                128.993850           108.034618               109.047246
    ## 22                 45.931546            47.519574                47.256168
    ## 23                 10.101044            10.596935                10.289245
    ## 24                 62.007823            75.350044                70.143521
    ## 25               1917.715237          2040.347793              1969.917066
    ## 26                 62.626235            64.502255                62.228940
    ## 27                241.454975           258.946586               218.914368
    ## 28                 71.834241            76.181952                75.269537
    ## 29                 56.324939            53.759212                55.396827
    ## 30                 71.708402            68.296091                70.464485
    ## 31                 91.138029            99.814342                77.389073
    ## 32                205.898881           246.242273               192.269692
    ## 33                 52.643500            63.461212                49.209722
    ## 34                150.260948           143.231010               152.499615
    ## 35                227.909563           291.108617               234.214990
    ## 36                 50.190887            58.043223                55.607543
    ## 37                357.422389           252.162947               358.394489
    ## 38                 59.034483            62.367136                57.440179
    ## 39                  9.297215             7.795973                 9.020836
    ## 40                 17.330339            16.463583                17.374388
    ## 41                 47.271268            41.413866                46.428919
    ## 42                119.446355           122.398243               113.008909
    ## 43                 18.450163            23.703536                18.584963
    ## 44                 96.993369           106.914578                88.258791
    ## 45                 57.614815            59.375887                61.553373
    ## 46                 84.118772            87.783718                80.827729
    ## 47                 63.442089            46.129035                57.064612
    ## 48                600.665371           378.286197               580.958904
    ## 49                 39.762226            42.210593                39.724976
    ## 50                104.252293            96.932817               107.263282
    ## 51                 26.667272            23.026796                26.759571
    ## 52                 73.608442            74.302252                72.161374
    ## 53                 14.281812            15.851646                15.514660
    ## 54                148.551309           169.461340               145.304052
    ## 55                171.867027           194.606084               196.800856
    ## 56                114.544714           123.684142               108.980474
    ## 57                 72.803781            75.978664                73.637887
    ## 58                 49.168479            51.451977                52.884195
    ## 59                180.013717           212.273519               178.987998
    ## 60                 44.971997            54.661548                48.018148
    ## 61                 24.030890            19.885707                24.000957
    ## 62                 34.207525            15.830381                34.091232
    ## 63                186.737671           227.924654               208.983869
    ## 64                 49.934188            50.484508                49.580808
    ## 65                205.348923           254.008743               221.012993
    ## 66                 58.467855            58.359000                59.845724
    ## 67                 96.673408            95.793196                79.968621
    ## 68                 60.226989            60.985424                63.542070
    ## 69                  7.846162             7.836850                 8.068165
    ## 70                297.383073           252.213306               248.692572
    ## 71                 74.846775            77.750180                81.574105
    ## 72                  6.165237             5.260693                 6.051043
    ## 73                119.341932           130.949002               117.890620
    ## 74                 21.769990            23.790519                21.286144
    ## 75                108.934098           105.927571               110.502029
    ## 76                 17.180312            16.948809                15.968114
    ## 77                 38.925137            44.139627                41.999605
    ## 78                 77.981482            92.223115                67.954039
    ## 79                 38.639544            39.397682                37.300001
    ## 80                 58.089260            75.408995                65.210216
    ## 81                 48.533060            45.583710                49.017597
    ## 82                148.701376           151.965650               150.909770
    ## 83                131.209534           126.233079               126.551032
    ## 84                  2.530462             2.071904                 2.623677
    ## 85                 66.898934            75.704205                65.928684
    ## 86                135.256913           134.891772               141.065909
    ## 87                108.079235           136.120287               112.383995
    ## 88                 26.165047            23.547221                25.842173
    ## 89                439.242409           448.558669               451.673934
    ## 90                139.452663           134.597350               126.020927
    ## 91                 60.026682            69.481903                66.202831
    ## 92                 94.507207            90.290667                96.708354
    ## 93                182.251852           190.792336               169.146318
    ## 94                 34.366929            23.963899                36.423891
    ## 95                153.538604           173.175668               171.907299
    ## 96                 18.722121            18.366425                18.496734
    ## 97                158.191878           203.201379               167.966522
    ## 98                 46.845013            47.526368                47.269137
    ## 99                 10.707508            10.841944                10.956913
    ## 100                41.199468            39.048063                39.999956
    ## 101               116.210198           155.309483               115.072166
    ## 102               141.480844           159.782313               149.464148
    ## 103               144.223209           141.483082               137.497518
    ## 104               247.952444           262.756315               216.571576
    ## 105                 7.318274             4.945720                 6.911018
    ## 106               113.586189           106.361569               105.504251
    ## 107               122.978770           128.890258               110.600378
    ## 108               106.858663           110.041908               104.856273
    ## 109                15.877806            12.213573                15.979830
    ## 110               217.796340           210.287058               196.841898
    ## 111                17.427307            17.598323                18.193438
    ## 112                85.212914            95.371063                85.581719
    ## 113                47.791229            58.808941                48.178422
    ## 114               110.305325            63.895479               111.720921
    ## 115                21.180114            22.990710                21.806668
    ## 116               173.098798           198.599141               179.301413
    ## 117                14.530942            15.348093                13.661543
    ## 118               257.298786           279.333354               257.223417
    ## 119               370.333048           388.298227               375.929040
    ## 120               102.576677           137.241987               102.233319
    ## 121                19.664837            20.350851                19.810234
    ## 122               431.777017           487.172745               254.968549
    ## 123               183.797410           204.041234               126.796646
    ##     no_red_meat_kg_co2e_luc pescetarian_kg_co2e_luc
    ## 1                  8.620091                2.497214
    ## 2                 20.161641               14.546869
    ## 3                156.941676               62.753137
    ## 4                366.482290              271.129602
    ## 5                 16.495030                7.828525
    ## 6                 95.294104                9.936181
    ## 7                 23.336354               10.211175
    ## 8                 55.669030               24.552906
    ## 9                 67.914609               57.988862
    ## 10                27.184188               17.020197
    ## 11                73.036927               39.275289
    ## 12                40.448792               47.419812
    ## 13               184.544199               29.445954
    ## 14                 6.314257                0.590707
    ## 15                44.462434               20.989336
    ## 16                33.267750               13.552587
    ## 17               505.597715              286.309976
    ## 18                61.697459               29.120845
    ## 19                18.803663                4.076519
    ## 20                17.367897               17.354677
    ## 21               100.603299               76.473827
    ## 22                46.115499               29.921599
    ## 23                 6.282434                1.762148
    ## 24                40.670217                5.714920
    ## 25               270.612591              188.844712
    ## 26                40.684032               28.867947
    ## 27               222.486188              103.476740
    ## 28                68.809763               17.603593
    ## 29                27.955594               21.725913
    ## 30                67.045706                8.307404
    ## 31                34.055748               18.300029
    ## 32               129.885296               18.343003
    ## 33                21.766239                8.915984
    ## 34               119.748803               79.133537
    ## 35               225.986620              105.768012
    ## 36                29.785427               13.200433
    ## 37               133.654845              113.111640
    ## 38                25.897495                9.441517
    ## 39                 9.005663                8.447775
    ## 40                18.388482                5.027638
    ## 41                23.198057               14.607479
    ## 42                67.612886               34.870817
    ## 43                11.014559                1.859490
    ## 44                46.921768               28.431803
    ## 45                57.075515                8.057693
    ## 46                45.855169               23.988725
    ## 47                51.800353               41.279126
    ## 48               277.011053              145.458966
    ## 49                13.699706                6.175142
    ## 50                78.319427               39.587033
    ## 51                26.359900               19.051015
    ## 52                71.300225               26.489288
    ## 53                16.572690                2.108623
    ## 54                76.861026               35.108368
    ## 55                28.706879               12.595484
    ## 56                58.379187               32.794809
    ## 57                26.695182                5.901804
    ## 58                36.601752               15.298822
    ## 59               160.170639               14.806028
    ## 60                28.945566               13.097752
    ## 61                23.963102               21.568608
    ## 62                32.827308               22.225060
    ## 63               108.031039               10.788156
    ## 64                23.439993                9.478394
    ## 65               122.914607               12.578945
    ## 66                26.353889               14.199707
    ## 67                44.720794               24.282829
    ## 68                65.424390               12.152518
    ## 69                 7.541153                6.617822
    ## 70               263.814602              102.041777
    ## 71                37.883768               10.007413
    ## 72                 7.210711                4.600375
    ## 73                28.491410                9.154874
    ## 74                24.744688               12.041759
    ## 75                93.812138               89.702877
    ## 76                14.683731                7.587357
    ## 77                35.730830                6.961876
    ## 78                20.672035               11.188456
    ## 79                39.390943               19.495813
    ## 80                66.011940                4.085958
    ## 81                48.709799               39.993415
    ## 82                71.958184               41.640640
    ## 83               125.563455               45.148631
    ## 84                 2.258735                1.971169
    ## 85                51.641532               14.629014
    ## 86                75.672148               43.250888
    ## 87               109.717711               25.283943
    ## 88                25.731396               19.711213
    ## 89               178.088488              122.197418
    ## 90               143.432592              111.912850
    ## 91                60.368068                3.078261
    ## 92                21.898757               16.573670
    ## 93                96.243200               48.322805
    ## 94                19.620300               15.435733
    ## 95                16.677625                5.502551
    ## 96                19.217077               18.875433
    ## 97               154.318444               30.762770
    ## 98                48.240818               15.794697
    ## 99                 7.308229                2.268231
    ## 100               14.241992                6.514382
    ## 101              108.952601               75.961074
    ## 102              155.257346               14.252185
    ## 103              123.315775               88.709323
    ## 104              115.851277               51.845400
    ## 105                6.923548                1.977307
    ## 106               53.667763               31.729486
    ## 107               45.957175               24.870791
    ## 108               87.276059               65.398280
    ## 109               15.796057               12.136629
    ## 110              180.320815               73.338727
    ## 111               16.731266                8.381353
    ## 112               86.162569               62.985951
    ## 113               51.043429               40.653242
    ## 114              103.390415               59.049000
    ## 115               11.927334                3.064372
    ## 116              105.638448               40.259809
    ## 117                2.545252                1.878394
    ## 118              206.929164              196.991294
    ## 119              203.417511              125.670072
    ## 120              116.605619               13.199962
    ## 121               19.604268                1.795722
    ## 122               96.344439               35.592656
    ## 123               62.429652               13.658501
    ##     lacto_ovo_vegetarian_kg_co2e_luc X2.3_vegan_kg_co2e_luc vegan_kg_co2e_luc
    ## 1                           7.630503               2.516273      0.000000e+00
    ## 2                          14.546869              17.810946      8.659834e-04
    ## 3                         115.263621              68.905992      2.761310e+01
    ## 4                         347.376437             312.683250      2.457020e+02
    ## 5                          10.607354              13.956436      2.436078e-01
    ## 6                          14.318192              42.470745      7.165015e+00
    ## 7                          17.807668              18.148240      1.606651e+00
    ## 8                          32.599416              23.546929      6.878413e+00
    ## 9                          74.704059              88.824102      4.957060e+01
    ## 10                         37.206165              32.313470      7.860209e+00
    ## 11                         69.779890              61.301698      2.254336e+01
    ## 12                         53.844767              79.114578      7.326010e+01
    ## 13                        137.290422              77.273255      9.636147e+00
    ## 14                          1.560304              21.916762      6.795422e-02
    ## 15                         29.255460              19.040355      1.740574e+00
    ## 16                         19.413288              19.580940      1.308149e+01
    ## 17                        604.419854             479.256030      1.238365e+02
    ## 18                         93.832508              26.776410      6.484256e+00
    ## 19                          7.673009              16.436011      6.268895e-01
    ## 20                         20.406940              20.512516      2.159779e+01
    ## 21                        221.734387             171.901209      2.010833e+02
    ## 22                         37.983816              40.395585      3.696529e+01
    ## 23                          2.891326               4.247260      7.195577e-01
    ## 24                         13.193633              23.466942      1.286523e-01
    ## 25                        428.315812             750.765493      4.317400e+01
    ## 26                         46.715448              54.069664      4.846548e+01
    ## 27                        181.141991             134.261653      7.563891e+01
    ## 28                         39.045696              36.253476      1.573071e+01
    ## 29                         29.281836              37.643241      2.604880e+01
    ## 30                         83.807525              26.217086      4.479688e+00
    ## 31                         30.500705              43.427627      1.451932e+01
    ## 32                         48.784624              77.595311      3.506716e-01
    ## 33                         17.028667              20.032898      7.596946e-01
    ## 34                         79.133537              56.745115      4.059914e+00
    ## 35                        169.695933             131.875740      7.900769e+01
    ## 36                         16.767592              30.202317      1.742276e+01
    ## 37                        178.093361             131.744720      3.961800e+00
    ## 38                         10.173851              22.422356      4.457367e-02
    ## 39                          8.800603               7.994104      7.301164e+00
    ## 40                          9.415939               8.370026      3.230336e+00
    ## 41                         14.607479              21.548240      5.964744e+00
    ## 42                         51.616447              56.327167      1.911508e+01
    ## 43                          3.803730               6.965601      2.784390e-03
    ## 44                         38.667636              62.185444      3.992100e+01
    ## 45                         34.201982              23.903727      5.097113e+00
    ## 46                         45.027105              31.091190      1.319760e+00
    ## 47                         51.623529              42.050857      3.037036e+01
    ## 48                        387.369185             220.740037      1.300269e+01
    ## 49                         10.979706              15.354595      7.522358e-01
    ## 50                         39.587033              38.556495      3.198239e-01
    ## 51                         24.400092              19.732128      1.607562e+01
    ## 52                         80.023006              40.978319      2.519780e+01
    ## 53                          6.884720               5.171553      0.000000e+00
    ## 54                         55.779272              54.746225      1.126794e-01
    ## 55                         22.264052              71.533725      8.900159e+00
    ## 56                         59.598467              56.427284      2.278971e+01
    ## 57                         23.577362              27.175798      2.596043e-01
    ## 58                         26.874179              39.061535      3.215021e+01
    ## 59                         33.083062              74.014997      8.771417e+00
    ## 60                         26.695092              24.643729      1.295652e+01
    ## 61                         24.533260              20.601331      1.892842e+01
    ## 62                         34.905286              11.837335      7.103861e-01
    ## 63                         24.635141              73.676299      3.637068e+00
    ## 64                         16.524028              18.567339      9.973453e-02
    ## 65                         25.295809              83.240757      7.182248e+00
    ## 66                         14.199707              22.566289      9.264937e-01
    ## 67                         36.577837              35.565756      3.965720e-03
    ## 68                         39.704128              26.606357      8.085803e+00
    ## 69                          6.854937               6.969136      6.448021e+00
    ## 70                        479.910282             120.202184      4.682234e+01
    ## 71                         34.482791              27.674176      7.242117e-01
    ## 72                          6.568307               5.169741      4.705583e+00
    ## 73                         12.012820              45.851520      1.622222e-01
    ## 74                         17.731378              12.335781      7.282126e+00
    ## 75                         97.798903              93.656838      8.509028e+01
    ## 76                         10.670535              10.898385      7.214940e+00
    ## 77                         20.709289              14.509613      7.838631e-01
    ## 78                         13.282898              29.773996      2.787044e-01
    ## 79                         30.015977              23.729647      1.555607e+01
    ## 80                         12.784709              21.907453      4.117010e-02
    ## 81                         45.094722              41.790791      3.813313e+01
    ## 82                         50.477482              67.693013      1.900351e+01
    ## 83                        103.071875              50.203166      7.355177e+00
    ## 84                          1.971169               1.790800      1.374361e+00
    ## 85                         28.435945              25.374115      1.406456e+00
    ## 86                         75.380648              51.590231      4.767201e+00
    ## 87                         55.464457              47.220042      1.240588e+01
    ## 88                         24.376567              18.246440      1.413810e+01
    ## 89                        208.940590             239.816881      1.209123e+02
    ## 90                        215.372070             117.570134      1.129555e+02
    ## 91                         19.023313              25.128341      4.262223e+00
    ## 92                         32.602538              36.401641      2.190136e+00
    ## 93                         87.192649             123.614307      8.637393e+01
    ## 94                         22.025152              12.674220      7.993849e-01
    ## 95                          9.133547              65.266595      9.096835e+00
    ## 96                         20.074442              19.491467      1.998883e+01
    ## 97                         74.068287              67.324524      1.488055e+01
    ## 98                         42.165776              24.384488      1.276429e+01
    ## 99                          3.076657               4.269370      4.143972e-01
    ## 100                        12.135212              16.726449      2.067918e+00
    ## 101                        94.673498             113.501073      1.103518e+02
    ## 102                        26.994618              60.737563      1.082540e+01
    ## 103                       161.127179             144.016313      1.453215e+02
    ## 104                       120.385887             107.564082      2.673936e+01
    ## 105                         8.025314               2.701761      4.524238e-01
    ## 106                        43.476937              45.441390      5.526553e+00
    ## 107                        40.702728              47.079766      2.273928e+00
    ## 108                        94.251105             105.137082      1.032257e+02
    ## 109                        15.272906              11.873349      9.820711e+00
    ## 110                       255.766182             134.552446      9.609465e+01
    ## 111                         9.616377              11.554694      7.967477e+00
    ## 112                        67.867464              75.697967      6.949504e+01
    ## 113                        45.468067              47.547556      4.723212e+01
    ## 114                       101.811747              55.630129      2.758473e+01
    ## 115                         4.751876              11.343747      5.056544e+00
    ## 116                        85.334430              65.549220      4.460733e+00
    ## 117                         3.476137               6.348623      1.336230e+00
    ## 118                       193.633205             271.134472      2.727469e+02
    ## 119                       232.722415             187.534196      8.466722e+01
    ## 120                        21.117047              45.564829      1.027060e+01
    ## 121                         7.199657               7.767982      7.807892e-01
    ## 122                        72.488705             176.282920      1.859518e+01
    ## 123                        46.863572              70.257830      2.076887e+00
    ##     pop_baseline_excl_luc pop_baseline_kg_co2e_total pop_baseline_kg_co2e_luc
    ## 1            3.316912e+08               3.329589e+08             1.267757e+06
    ## 2            2.632227e+08               2.714039e+08             8.181177e+06
    ## 3            9.786815e+08               1.103452e+09             1.247707e+08
    ## 4            7.419662e+09               8.837724e+09             1.418062e+09
    ## 5            1.306824e+08               1.340555e+08             3.373176e+06
    ## 6            4.099946e+09               4.272553e+09             1.726065e+08
    ## 7            5.401298e+08               5.620697e+08             2.193986e+07
    ## 8            2.947943e+08               3.045954e+08             9.801102e+06
    ## 9            1.872605e+05               2.103453e+05             2.308481e+04
    ## 10           4.196300e+08               4.475654e+08             2.793541e+07
    ## 11           7.949835e+08               8.707468e+08             7.576327e+07
    ## 12           5.430971e+06               5.842901e+06             4.119301e+05
    ## 13           1.016029e+07               1.209477e+07             1.934484e+06
    ## 14           1.595171e+04               1.639836e+04             4.466506e+02
    ## 15           8.756997e+07               9.223768e+07             4.667709e+06
    ## 16           5.790598e+07               5.850284e+07             5.968652e+05
    ## 17           2.496554e+10               3.816411e+10             1.319856e+10
    ## 18           1.456419e+07               1.516060e+07             5.964158e+05
    ## 19           2.216618e+08               2.337821e+08             1.212033e+07
    ## 20           8.443834e+07               8.590197e+07             1.463633e+06
    ## 21           1.735002e+08               1.897895e+08             1.628932e+07
    ## 22           2.610120e+08               2.709517e+08             9.939727e+06
    ## 23           2.074857e+09               2.091923e+09             1.706638e+07
    ## 24           1.182582e+07               1.251533e+07             6.895134e+05
    ## 25           1.479496e+09               3.535010e+09             2.055514e+09
    ## 26           3.203300e+10               3.384052e+10             1.807518e+09
    ## 27           3.109290e+09               3.396020e+09             2.867293e+08
    ## 28           2.969266e+08               3.149332e+08             1.800659e+07
    ## 29           3.514680e+08               3.627122e+08             1.124424e+07
    ## 30           7.603842e+07               7.935892e+07             3.320505e+06
    ## 31           2.046504e+08               2.205674e+08             1.591692e+07
    ## 32           5.098545e+07               6.002238e+07             9.036930e+06
    ## 33           3.570549e+08               3.760297e+08             1.897480e+07
    ## 34           4.392610e+08               4.875373e+08             4.827636e+07
    ## 35           7.879152e+08               8.888402e+08             1.009250e+08
    ## 36           3.540485e+09               3.739527e+09             1.990420e+08
    ## 37           1.778457e+08               2.360793e+08             5.823364e+07
    ## 38           4.845767e+07               5.146685e+07             3.009181e+06
    ## 39           4.173613e+08               4.213933e+08             4.032048e+06
    ## 40           2.251576e+05               2.273661e+05             2.208430e+03
    ## 41           4.105974e+08               4.272841e+08             1.668667e+07
    ## 42           3.808594e+09               4.138840e+09             3.302463e+08
    ## 43           1.766840e+08               1.790671e+08             2.383053e+06
    ## 44           4.407269e+09               4.792130e+09             3.848605e+08
    ## 45           1.750254e+08               1.858911e+08             1.086568e+07
    ## 46           1.296929e+09               1.381948e+09             8.501908e+07
    ## 47           2.244903e+08               2.379207e+08             1.343038e+07
    ## 48           2.507137e+08               3.751267e+08             1.244130e+08
    ## 49           2.726972e+08               2.840978e+08             1.140052e+07
    ## 50           3.615055e+07               3.835741e+07             2.206856e+06
    ## 51           2.702381e+10               2.766031e+10             6.365052e+08
    ## 52           4.646529e+09               4.966766e+09             3.202370e+08
    ## 53           2.487943e+09               2.525181e+09             3.723747e+07
    ## 54           4.557108e+08               4.961007e+08             4.038984e+07
    ## 55           6.565081e+08               7.389148e+08             8.240674e+07
    ## 56           2.945264e+09               3.222747e+09             2.774829e+08
    ## 57           3.291407e+07               3.541136e+07             2.497297e+06
    ## 58           3.242263e+09               3.395109e+09             1.528467e+08
    ## 59           3.643782e+08               4.107763e+08             4.639810e+07
    ## 60           1.363409e+09               1.397679e+09             3.427050e+07
    ## 61           5.096956e+08               5.188284e+08             9.132883e+06
    ## 62           3.070467e+08               3.125102e+08             5.463454e+06
    ## 63           1.996077e+08               2.223953e+08             2.278766e+07
    ## 64           6.405201e+07               6.776568e+07             3.713668e+06
    ## 65           2.645269e+08               3.101201e+08             4.559324e+07
    ## 66           1.349424e+08               1.435261e+08             8.583721e+06
    ## 67           1.273517e+07               1.345348e+07             7.183103e+05
    ## 68           1.328278e+08               1.359967e+08             3.168870e+06
    ## 69           7.967552e+04               8.091082e+04             1.235301e+03
    ## 70           9.626964e+08               1.149212e+09             1.865153e+08
    ## 71           1.456191e+07               1.537432e+07             8.124082e+05
    ## 72           1.409410e+08               1.413372e+08             3.961710e+05
    ## 73           1.946141e+07               2.143347e+07             1.972064e+06
    ## 74           3.281691e+07               3.319645e+07             3.795334e+05
    ## 75           4.412989e+07               4.871917e+07             4.589275e+06
    ## 76           5.265396e+09               5.347190e+09             8.179434e+07
    ## 77           5.702455e+07               5.908013e+07             2.055584e+06
    ## 78           3.932266e+07               4.166046e+07             2.337795e+06
    ## 79           8.259492e+08               8.629538e+08             3.700462e+07
    ## 80           4.625042e+07               4.779520e+07             1.544777e+06
    ## 81           4.808453e+08               5.004784e+08             1.963311e+07
    ## 82           1.417342e+09               1.574006e+09             1.566637e+08
    ## 83           5.393870e+08               5.707082e+08             3.132126e+07
    ## 84           8.468955e+07               8.487288e+07             1.833314e+05
    ## 85           6.775622e+07               7.132709e+07             3.570868e+06
    ## 86           4.962612e+08               5.420352e+08             4.577392e+07
    ## 87           1.815736e+08               1.931044e+08             1.153085e+07
    ## 88           2.472160e+09               2.511658e+09             3.949831e+07
    ## 89           2.255613e+08               2.898967e+08             6.433543e+07
    ## 90           1.839030e+09               2.061731e+09             2.227017e+08
    ## 91           2.954223e+09               3.076052e+09             1.218289e+08
    ## 92           1.621111e+09               1.802816e+09             1.817051e+08
    ## 93           5.875849e+08               6.609007e+08             7.331576e+07
    ## 94           6.585612e+08               6.795830e+08             2.102185e+07
    ## 95           6.300442e+09               7.149785e+09             8.493424e+08
    ## 96           3.128014e+07               3.197173e+07             6.915873e+05
    ## 97           1.766232e+09               1.989755e+09             2.235232e+08
    ## 98           1.739186e+08               1.790532e+08             5.134640e+06
    ## 99           2.287446e+08               2.315851e+08             2.840482e+06
    ## 100          1.155926e+08               1.208394e+08             5.246822e+06
    ## 101          9.814605e+07               1.070995e+08             8.953465e+06
    ## 102          1.040063e+09               1.160547e+09             1.204838e+08
    ## 103          3.669793e+09               4.035949e+09             3.661561e+08
    ## 104          2.248813e+09               2.734297e+09             4.854839e+08
    ## 105          1.620782e+08               1.630381e+08             9.598876e+05
    ## 106          7.169921e+08               7.727392e+08             5.574713e+07
    ## 107          5.048722e+08               5.490032e+08             4.413097e+07
    ## 108          1.314234e+09               1.448320e+09             1.340862e+08
    ## 109          1.415097e+08               1.433206e+08             1.810853e+06
    ## 110          1.697368e+09               1.945748e+09             2.483799e+08
    ## 111          3.937677e+07               4.057428e+07             1.197511e+06
    ## 112          2.363425e+08               2.625554e+08             2.621290e+07
    ## 113          5.796151e+09               6.050481e+09             2.543304e+08
    ## 114          1.538815e+06               1.684418e+06             1.456027e+05
    ## 115          1.183982e+09               1.213655e+09             2.967286e+07
    ## 116          3.836869e+09               4.296048e+09             4.591787e+08
    ## 117          2.458169e+10               2.482833e+10             2.466411e+08
    ## 118          2.132669e+05               2.427340e+05             2.946708e+04
    ## 119          1.370715e+07               1.579319e+07             2.086047e+06
    ## 120          4.104462e+04               4.404911e+04             3.004496e+03
    ## 121          9.288875e+07               9.368984e+07             8.010857e+05
    ## 122          4.960157e+08               6.220722e+08             1.260564e+08
    ## 123          5.359268e+07               6.040389e+07             6.811206e+06
    ##     pop_baseline_l_blue_green_wf pop_baseline_l_blue_wf_total
    ## 1                   3.763829e+11                 1.288895e+11
    ## 2                   1.945579e+11                 2.887663e+10
    ## 3                   1.292294e+12                 1.853489e+11
    ## 4                   2.552369e+12                 1.558594e+11
    ## 5                   1.174638e+11                 1.556188e+10
    ## 6                   2.063839e+12                 2.369000e+11
    ## 7                   2.818013e+11                 2.136785e+10
    ## 8                   2.290914e+11                 3.825248e+10
    ## 9                   1.264760e+08                 1.149290e+07
    ## 10                  3.660013e+11                 1.431051e+10
    ## 11                  3.910980e+11                 4.354987e+10
    ## 12                  5.700872e+09                 3.204246e+08
    ## 13                  1.959907e+10                 1.251834e+09
    ## 14                  8.339596e+06                 1.033279e+06
    ## 15                  9.498553e+10                 3.368041e+09
    ## 16                  3.944639e+10                 4.103747e+09
    ## 17                  1.551733e+13                 5.605487e+11
    ## 18                  9.769120e+09                 1.125003e+09
    ## 19                  2.972364e+11                 1.299718e+10
    ## 20                  1.149212e+11                 2.763369e+09
    ## 21                  2.798734e+11                 2.997129e+10
    ## 22                  3.805249e+11                 1.839594e+10
    ## 23                  1.473071e+12                 1.330027e+11
    ## 24                  1.200173e+10                 1.321369e+09
    ## 25                  7.982876e+11                 1.303972e+11
    ## 26                  2.250861e+13                 3.641831e+12
    ## 27                  1.777457e+12                 9.355184e+10
    ## 28                  4.611212e+11                 3.625149e+10
    ## 29                  2.000672e+11                 1.854273e+10
    ## 30                  1.791970e+11                 7.412118e+09
    ## 31                  1.819496e+11                 8.119871e+09
    ## 32                  4.850545e+10                 6.310210e+09
    ## 33                  2.405141e+11                 1.366186e+10
    ## 34                  2.061780e+11                 1.859501e+10
    ## 35                  8.746521e+11                 7.898782e+10
    ## 36                  3.171507e+12                 1.648654e+12
    ## 37                  1.422355e+11                 7.847945e+09
    ## 38                  4.375927e+10                 1.876039e+09
    ## 39                  6.171622e+11                 2.083259e+10
    ## 40                  2.045538e+08                 1.232889e+07
    ## 41                  2.481670e+11                 2.305966e+10
    ## 42                  1.833349e+12                 1.939293e+11
    ## 43                  1.938471e+11                 1.524720e+10
    ## 44                  2.257429e+12                 2.254933e+11
    ## 45                  3.841285e+11                 3.008606e+10
    ## 46                  1.010004e+12                 1.392357e+11
    ## 47                  2.209640e+11                 1.078075e+10
    ## 48                  2.393098e+11                 1.386848e+10
    ## 49                  2.735458e+11                 9.364053e+09
    ## 50                  1.335690e+10                 2.002832e+09
    ## 51                  3.040554e+13                 7.401108e+12
    ## 52                  6.715896e+12                 6.022123e+11
    ## 53                  4.572695e+12                 1.780097e+12
    ## 54                  1.520940e+11                 1.536692e+10
    ## 55                  5.240755e+11                 6.481361e+10
    ## 56                  2.111515e+12                 2.132858e+11
    ## 57                  3.517522e+10                 2.481189e+09
    ## 58                  2.240370e+12                 2.870069e+11
    ## 59                  3.449099e+11                 7.195257e+10
    ## 60                  1.200272e+12                 1.895125e+11
    ## 61                  4.897676e+11                 3.233444e+10
    ## 62                  1.893904e+11                 3.673564e+10
    ## 63                  1.163062e+11                 1.947784e+10
    ## 64                  7.088168e+10                 3.283217e+09
    ## 65                  2.904898e+11                 5.571940e+10
    ## 66                  1.241793e+11                 4.757087e+09
    ## 67                  5.593668e+09                 5.510556e+08
    ## 68                  1.826486e+11                 2.664664e+10
    ## 69                  1.835280e+08                 8.400775e+06
    ## 70                  9.145530e+11                 1.176192e+11
    ## 71                  9.900696e+09                 2.298492e+09
    ## 72                  1.791539e+11                 1.243413e+10
    ## 73                  1.248883e+10                 1.688699e+09
    ## 74                  2.988206e+10                 2.193795e+09
    ## 75                  3.423397e+10                 3.993012e+09
    ## 76                  5.322858e+12                 5.837280e+11
    ## 77                  7.145894e+10                 6.964822e+09
    ## 78                  2.927567e+10                 1.444123e+09
    ## 79                  1.637047e+12                 2.192848e+11
    ## 80                  4.268337e+10                 2.676718e+09
    ## 81                  6.013288e+11                 8.626176e+10
    ## 82                  6.968991e+11                 6.190234e+10
    ## 83                  2.156330e+11                 2.465847e+10
    ## 84                  2.715150e+11                 4.453946e+09
    ## 85                  5.505716e+10                 4.318147e+09
    ## 86                  2.320686e+11                 2.693028e+10
    ## 87                  1.668775e+11                 5.376978e+10
    ## 88                  1.976181e+12                 7.193686e+11
    ## 89                  1.455942e+11                 1.036855e+10
    ## 90                  1.829630e+12                 3.018931e+11
    ## 91                  3.494114e+12                 2.791031e+11
    ## 92                  1.291743e+12                 5.422940e+10
    ## 93                  4.903071e+11                 9.337854e+10
    ## 94                  6.041229e+11                 2.834510e+10
    ## 95                  6.424727e+12                 3.493286e+11
    ## 96                  8.630404e+10                 5.954282e+09
    ## 97                  1.782728e+12                 4.346520e+11
    ## 98                  1.971301e+11                 1.914625e+10
    ## 99                  2.347301e+11                 9.502107e+09
    ## 100                 9.114267e+10                 4.779946e+09
    ## 101                 5.909359e+10                 4.115211e+09
    ## 102                 8.871357e+11                 7.309746e+10
    ## 103                 2.285372e+12                 2.174616e+11
    ## 104                 2.164094e+12                 3.383850e+11
    ## 105                 2.674403e+11                 5.003754e+10
    ## 106                 3.298131e+11                 3.316249e+10
    ## 107                 2.296785e+11                 2.088049e+10
    ## 108                 1.127127e+12                 1.251913e+11
    ## 109                 1.822873e+11                 1.113045e+10
    ## 110                 2.521120e+12                 3.864372e+11
    ## 111                 1.207176e+11                 4.067002e+09
    ## 112                 4.247128e+11                 5.075043e+10
    ## 113                 6.708577e+12                 8.073693e+11
    ## 114                 2.181409e+09                 1.469660e+08
    ## 115                 1.443603e+12                 5.690154e+10
    ## 116                 1.539328e+12                 1.693334e+11
    ## 117                 1.492530e+13                 1.742650e+12
    ## 118                 1.452707e+08                 5.003168e+06
    ## 119                 7.452319e+09                 3.627352e+08
    ## 120                 5.452538e+07                 1.140893e+07
    ## 121                 1.151185e+11                 7.560337e+09
    ## 122                 3.542312e+11                 4.700318e+10
    ## 123                 3.831283e+10                 5.029750e+09
    ##     pop_baseline_l_green_wf pop_meatless_day_kg_co2e_excl_luc
    ## 1              2.474933e+11                      6.707798e+08
    ## 2              1.656813e+11                      2.226533e+08
    ## 3              1.106945e+12                      1.062267e+09
    ## 4              2.396509e+12                      5.339367e+09
    ## 5              1.019019e+11                      1.268816e+08
    ## 6              1.826939e+12                      3.448640e+09
    ## 7              2.604335e+11                      4.342357e+08
    ## 8              1.908389e+11                      3.076820e+08
    ## 9              1.149831e+08                      1.847845e+05
    ## 10             3.516908e+11                      3.703733e+08
    ## 11             3.475481e+11                      6.971678e+08
    ## 12             5.380447e+09                      5.760219e+06
    ## 13             1.834723e+10                      1.160059e+07
    ## 14             7.306318e+06                      1.734356e+04
    ## 15             9.161749e+10                      8.921619e+07
    ## 16             3.534264e+10                      8.861983e+07
    ## 17             1.495678e+13                      1.846753e+10
    ## 18             8.644117e+09                      1.317934e+07
    ## 19             2.842392e+11                      2.124863e+08
    ## 20             1.121578e+11                      7.661471e+07
    ## 21             2.499021e+11                      1.565984e+08
    ## 22             3.621290e+11                      2.471218e+08
    ## 23             1.340069e+12                      1.642410e+09
    ## 24             1.068036e+10                      1.345631e+07
    ## 25             6.678904e+11                      1.333170e+09
    ## 26             1.886678e+13                      2.928069e+10
    ## 27             1.683906e+12                      4.114949e+09
    ## 28             4.248697e+11                      4.595339e+08
    ## 29             1.815245e+11                      4.060963e+08
    ## 30             1.717848e+11                      1.316492e+08
    ## 31             1.738297e+11                      2.166947e+08
    ## 32             4.219524e+10                      5.698980e+07
    ## 33             2.268522e+11                      3.655724e+08
    ## 34             1.875830e+11                      3.800148e+08
    ## 35             7.956643e+11                      1.004528e+09
    ## 36             1.522852e+12                      2.946648e+09
    ## 37             1.343876e+11                      2.021683e+08
    ## 38             4.188323e+10                      4.712395e+07
    ## 39             5.963296e+11                      5.744124e+08
    ## 40             1.922249e+08                      2.472709e+05
    ## 41             2.251073e+11                      3.627358e+08
    ## 42             1.639419e+12                      3.057675e+09
    ## 43             1.785999e+11                      2.533803e+08
    ## 44             2.031936e+12                      3.719749e+09
    ## 45             3.540424e+11                      1.871290e+08
    ## 46             8.707687e+11                      1.050839e+09
    ## 47             2.101832e+11                      2.965456e+08
    ## 48             2.254413e+11                      3.371283e+08
    ## 49             2.641817e+11                      2.862138e+08
    ## 50             1.135407e+10                      2.991746e+07
    ## 51             2.300443e+13                      4.426865e+10
    ## 52             6.113684e+12                      5.858728e+09
    ## 53             2.792598e+12                      2.390287e+09
    ## 54             1.367271e+11                      3.719783e+08
    ## 55             4.592619e+11                      4.736234e+08
    ## 56             1.898229e+12                      2.485637e+09
    ## 57             3.269403e+10                      3.846579e+07
    ## 58             1.953363e+12                      3.256230e+09
    ## 59             2.729574e+11                      4.210896e+08
    ## 60             1.010759e+12                      1.120367e+09
    ## 61             4.574332e+11                      7.056758e+08
    ## 62             1.526547e+11                      3.095602e+08
    ## 63             9.682835e+10                      1.465993e+08
    ## 64             6.759846e+10                      6.345122e+07
    ## 65             2.347704e+11                      2.884065e+08
    ## 66             1.194222e+11                      1.181949e+08
    ## 67             5.042613e+09                      1.114429e+07
    ## 68             1.560020e+11                      2.863843e+08
    ## 69             1.751272e+08                      7.506013e+04
    ## 70             7.969338e+11                      9.821527e+08
    ## 71             7.602204e+09                      1.474212e+07
    ## 72             1.667198e+11                      1.559748e+08
    ## 73             1.080013e+10                      1.665493e+07
    ## 74             2.768827e+10                      3.369884e+07
    ## 75             3.024096e+10                      3.993335e+07
    ## 76             4.739130e+12                      4.495674e+09
    ## 77             6.449412e+10                      8.120065e+07
    ## 78             2.783155e+10                      3.283258e+07
    ## 79             1.417762e+12                      7.786481e+08
    ## 80             4.000666e+10                      6.893658e+07
    ## 81             5.150670e+11                      6.789203e+08
    ## 82             6.349968e+11                      1.231610e+09
    ## 83             1.909745e+11                      4.883345e+08
    ## 84             2.670611e+11                      7.146870e+07
    ## 85             5.073901e+10                      7.888528e+07
    ## 86             2.051383e+11                      4.193302e+08
    ## 87             1.131077e+11                      1.758072e+08
    ## 88             1.256812e+12                      3.565066e+09
    ## 89             1.352257e+11                      2.335840e+08
    ## 90             1.527737e+12                      1.871692e+09
    ## 91             3.215011e+12                      3.719283e+09
    ## 92             1.237513e+12                      1.429744e+09
    ## 93             3.969286e+11                      5.169257e+08
    ## 94             5.757778e+11                      6.103638e+08
    ## 95             6.075398e+12                      5.534064e+09
    ## 96             8.034976e+10                      4.061712e+07
    ## 97             1.348076e+12                      1.695270e+09
    ## 98             1.779838e+11                      2.698336e+08
    ## 99             2.252280e+11                      2.333469e+08
    ## 100            8.636273e+10                      1.511396e+08
    ## 101            5.497838e+10                      8.433165e+07
    ## 102            8.140383e+11                      9.893058e+08
    ## 103            2.067911e+12                      3.065010e+09
    ## 104            1.825709e+12                      1.986035e+09
    ## 105            2.174027e+11                      2.467866e+08
    ## 106            2.966506e+11                      6.202847e+08
    ## 107            2.087980e+11                      4.671891e+08
    ## 108            1.001936e+12                      1.186858e+09
    ## 109            1.711568e+11                      2.234172e+08
    ## 110            2.134683e+12                      1.906865e+09
    ## 111            1.166506e+11                      4.680593e+07
    ## 112            3.739624e+11                      2.147892e+08
    ## 113            5.901207e+12                      4.823235e+09
    ## 114            2.034443e+09                      2.551474e+06
    ## 115            1.386701e+12                      1.148098e+09
    ## 116            1.369994e+12                      3.296854e+09
    ## 117            1.318265e+13                      1.936450e+10
    ## 118            1.402675e+08                      1.640421e+05
    ## 119            7.089584e+09                      1.461427e+07
    ## 120            4.311645e+07                      6.928853e+04
    ## 121            1.075581e+11                      1.949815e+08
    ## 122            3.072280e+11                      4.085231e+08
    ## 123            3.328308e+10                      5.244274e+07
    ##     pop_meatless_day_kg_co2e_total pop_meatless_day_kg_co2e_luc
    ## 1                     6.735818e+08                 2.801992e+06
    ## 2                     2.291091e+08                 6.455868e+06
    ## 3                     1.208011e+09                 1.457438e+08
    ## 4                     6.426237e+09                 1.086870e+09
    ## 5                     1.298945e+08                 3.012929e+06
    ## 6                     3.583221e+09                 1.345812e+08
    ## 7                     4.507268e+08                 1.649108e+07
    ## 8                     3.175216e+08                 9.839583e+06
    ## 9                     2.052796e+05                 2.049512e+04
    ## 10                    3.932237e+08                 2.285038e+07
    ## 11                    7.607185e+08                 6.355068e+07
    ## 12                    6.220685e+06                 4.604664e+05
    ## 13                    1.457043e+07                 2.969839e+06
    ## 14                    1.779527e+04                 4.517104e+02
    ## 15                    9.368092e+07                 4.464724e+06
    ## 16                    8.975275e+07                 1.132912e+06
    ## 17                    2.813881e+10                 9.671283e+09
    ## 18                    1.376022e+07                 5.808777e+05
    ## 19                    2.224798e+08                 9.993544e+06
    ## 20                    7.806411e+07                 1.449397e+06
    ## 21                    1.828954e+08                 2.629707e+07
    ## 22                    2.606335e+08                 1.351164e+07
    ## 23                    1.655149e+09                 1.273933e+07
    ## 24                    1.416444e+07                 7.081293e+05
    ## 25                    3.073424e+09                 1.740254e+09
    ## 26                    3.096501e+10                 1.684311e+09
    ## 27                    4.530589e+09                 4.156406e+08
    ## 28                    4.929137e+08                 3.337979e+07
    ## 29                    4.170793e+08                 1.098297e+07
    ## 30                    1.418985e+08                 1.024928e+07
    ## 31                    2.317120e+08                 1.501727e+07
    ## 32                    6.574483e+07                 8.755026e+06
    ## 33                    3.828425e+08                 1.727007e+07
    ## 34                    4.213862e+08                 4.137150e+07
    ## 35                    1.139013e+09                 1.344849e+08
    ## 36                    3.092928e+09                 1.462800e+08
    ## 37                    2.638562e+08                 6.168789e+07
    ## 38                    4.982616e+07                 2.702210e+06
    ## 39                    5.798700e+08                 5.457605e+06
    ## 40                    2.500784e+05                 2.807515e+03
    ## 41                    3.766580e+08                 1.392214e+07
    ## 42                    3.310145e+09                 2.524704e+08
    ## 43                    2.561204e+08                 2.740034e+06
    ## 44                    4.023103e+09                 3.033543e+08
    ## 45                    2.056267e+08                 1.849770e+07
    ## 46                    1.115348e+09                 6.450850e+07
    ## 47                    3.134538e+08                 1.690820e+07
    ## 48                    4.915456e+08                 1.544173e+08
    ## 49                    2.969939e+08                 1.078010e+07
    ## 50                    3.171550e+07                 1.798039e+06
    ## 51                    4.520595e+10                 9.373044e+08
    ## 52                    6.378292e+09                 5.195636e+08
    ## 53                    2.432585e+09                 4.229806e+07
    ## 54                    4.033715e+08                 3.139320e+07
    ## 55                    5.277994e+08                 5.417592e+07
    ## 56                    2.701280e+09                 2.156433e+08
    ## 57                    4.096886e+07                 2.503067e+06
    ## 58                    3.408803e+09                 1.525731e+08
    ## 59                    4.770894e+08                 5.599975e+07
    ## 60                    1.146947e+09                 2.658016e+07
    ## 61                    7.177167e+08                 1.204097e+07
    ## 62                    3.152805e+08                 5.720217e+06
    ## 63                    1.624483e+08                 1.584899e+07
    ## 64                    6.678098e+07                 3.329761e+06
    ## 65                    3.338186e+08                 4.541209e+07
    ## 66                    1.251109e+08                 6.915987e+06
    ## 67                    1.175110e+07                 6.068190e+05
    ## 68                    2.949218e+08                 8.537417e+06
    ## 69                    7.626844e+04                 1.208309e+03
    ## 70                    1.225897e+09                 2.437444e+08
    ## 71                    1.547996e+07                 7.378395e+05
    ## 72                    1.564782e+08                 5.034101e+05
    ## 73                    1.822451e+07                 1.569585e+06
    ## 74                    3.411254e+07                 4.136951e+05
    ## 75                    4.377862e+07                 3.845265e+06
    ## 76                    4.571047e+09                 7.537317e+07
    ## 77                    8.379878e+07                 2.598136e+06
    ## 78                    3.460346e+07                 1.770881e+06
    ## 79                    8.146707e+08                 3.602253e+07
    ## 80                    7.132143e+07                 2.384855e+06
    ## 81                    7.001864e+08                 2.126607e+07
    ## 82                    1.360159e+09                 1.285489e+08
    ## 83                    5.167614e+08                 2.842692e+07
    ## 84                    7.162954e+07                 1.608412e+05
    ## 85                    8.290657e+07                 4.021295e+06
    ## 86                    4.572189e+08                 3.788871e+07
    ## 87                    1.871705e+08                 1.136334e+07
    ## 88                    3.614207e+09                 4.914060e+07
    ## 89                    2.979765e+08                 6.439250e+07
    ## 90                    2.140051e+09                 2.683589e+08
    ## 91                    3.924342e+09                 2.050592e+08
    ## 92                    1.570811e+09                 1.410676e+08
    ## 93                    5.795498e+08                 6.262411e+07
    ## 94                    6.288831e+08                 1.851934e+07
    ## 95                    6.200493e+09                 6.664292e+08
    ## 96                    4.188689e+07                 1.269772e+06
    ## 97                    1.916274e+09                 2.210043e+08
    ## 98                    2.789548e+08                 9.121146e+06
    ## 99                    2.360213e+08                 2.674425e+06
    ## 100                   1.569820e+08                 5.842373e+06
    ## 101                   9.202256e+07                 7.690907e+06
    ## 102                   1.104779e+09                 1.154728e+08
    ## 103                   3.412121e+09                 3.471118e+08
    ## 104                   2.392042e+09                 4.060072e+08
    ## 105                   2.488296e+08                 2.043013e+06
    ## 106                   6.663434e+08                 4.605875e+07
    ## 107                   5.043980e+08                 3.720883e+07
    ## 108                   1.313575e+09                 1.267162e+08
    ## 109                   2.258629e+08                 2.445738e+06
    ## 110                   2.356582e+09                 4.497161e+08
    ## 111                   4.837540e+07                 1.569468e+06
    ## 112                   2.375542e+08                 2.276497e+07
    ## 113                   5.052232e+09                 2.289971e+08
    ## 114                   2.793484e+06                 2.420099e+05
    ## 115                   1.173892e+09                 2.579414e+07
    ## 116                   3.674702e+09                 3.778472e+08
    ## 117                   1.953979e+10                 1.752891e+08
    ## 118                   1.874635e+05                 2.342142e+04
    ## 119                   1.673183e+07                 2.117564e+06
    ## 120                   7.585344e+04                 6.564907e+03
    ## 121                   1.972371e+08                 2.255616e+06
    ## 122                   5.058629e+08                 9.733982e+07
    ## 123                   5.870803e+07                 6.265286e+06
    ##     pop_meatless_day_l_blue_green_wf pop_meatless_day_l_blue_wf_total
    ## 1                       4.828438e+11                     1.878290e+11
    ## 2                       1.649981e+11                     2.480405e+10
    ## 3                       1.162792e+12                     1.587875e+11
    ## 4                       1.979715e+12                     1.239720e+11
    ## 5                       1.128970e+11                     1.546543e+10
    ## 6                       1.751738e+12                     2.048429e+11
    ## 7                       2.242487e+11                     1.681941e+10
    ## 8                       2.381296e+11                     4.061772e+10
    ## 9                       1.203283e+08                     1.096423e+07
    ## 10                      3.187364e+11                     1.209881e+10
    ## 11                      3.214807e+11                     3.587689e+10
    ## 12                      6.111246e+09                     3.406342e+08
    ## 13                      1.845956e+10                     1.166702e+09
    ## 14                      9.298214e+06                     1.165303e+06
    ## 15                      9.534425e+10                     3.370045e+09
    ## 16                      5.123687e+10                     4.588806e+09
    ## 17                      1.121421e+13                     3.919764e+11
    ## 18                      8.542345e+09                     9.666728e+08
    ## 19                      2.788339e+11                     1.193346e+10
    ## 20                      1.115343e+11                     2.875783e+09
    ## 21                      2.533378e+11                     3.446753e+10
    ## 22                      3.533829e+11                     1.715624e+10
    ## 23                      1.189777e+12                     1.072927e+11
    ## 24                      1.316342e+10                     1.349736e+09
    ## 25                      7.514242e+11                     1.247061e+11
    ## 26                      2.117541e+13                     3.408252e+12
    ## 27                      2.085200e+12                     1.030814e+11
    ## 28                      6.218302e+11                     5.334028e+10
    ## 29                      1.987526e+11                     1.703152e+10
    ## 30                      1.884081e+11                     7.983276e+09
    ## 31                      1.844538e+11                     8.042396e+09
    ## 32                      5.113497e+10                     6.705169e+09
    ## 33                      2.367957e+11                     1.303781e+10
    ## 34                      1.823902e+11                     1.642929e+10
    ## 35                      1.134011e+12                     9.502890e+10
    ## 36                      2.438487e+12                     1.274854e+12
    ## 37                      1.591002e+11                     8.518311e+09
    ## 38                      4.320994e+10                     1.812344e+09
    ## 39                      7.786869e+11                     3.469905e+10
    ## 40                      2.066813e+08                     1.225098e+07
    ## 41                      2.206908e+11                     1.995629e+10
    ## 42                      1.482151e+12                     1.558334e+11
    ## 43                      2.316832e+11                     1.882458e+10
    ## 44                      1.897334e+12                     1.887631e+11
    ## 45                      3.356076e+11                     2.655450e+10
    ## 46                      8.056335e+11                     1.119790e+11
    ## 47                      2.654241e+11                     1.218944e+10
    ## 48                      3.138974e+11                     1.489628e+10
    ## 49                      2.934371e+11                     9.563968e+09
    ## 50                      1.131689e+10                     1.668521e+09
    ## 51                      3.740761e+13                     7.590056e+12
    ## 52                      6.969350e+12                     7.584576e+11
    ## 53                      4.068847e+12                     1.561789e+12
    ## 54                      1.257858e+11                     1.270449e+10
    ## 55                      3.723360e+11                     4.815174e+10
    ## 56                      1.727582e+12                     1.740409e+11
    ## 57                      3.924398e+10                     2.627819e+09
    ## 58                      2.333750e+12                     2.924252e+11
    ## 59                      3.333707e+11                     6.687457e+10
    ## 60                      9.429178e+11                     1.511994e+11
    ## 61                      6.150768e+11                     3.865726e+10
    ## 62                      1.931044e+11                     3.860984e+10
    ## 63                      8.867466e+10                     1.503817e+10
    ## 64                      6.946930e+10                     3.148059e+09
    ## 65                      2.809925e+11                     5.782063e+10
    ## 66                      1.096343e+11                     4.070592e+09
    ## 67                      4.949647e+09                     4.829924e+08
    ## 68                      2.292962e+11                     3.139134e+10
    ## 69                      1.721854e+08                     8.815329e+06
    ## 70                      8.130422e+11                     9.787546e+10
    ## 71                      9.990257e+09                     2.299377e+09
    ## 72                      1.641200e+11                     1.092097e+10
    ## 73                      1.101572e+10                     1.499410e+09
    ## 74                      2.974399e+10                     2.104750e+09
    ## 75                      2.939903e+10                     3.340104e+09
    ## 76                      4.774167e+12                     5.227813e+11
    ## 77                      8.398408e+10                     7.896840e+09
    ## 78                      2.465454e+10                     1.190448e+09
    ## 79                      1.443773e+12                     1.849929e+11
    ## 80                      5.493232e+10                     3.633965e+09
    ## 81                      6.217781e+11                     7.761651e+10
    ## 82                      6.114697e+11                     5.447797e+10
    ## 83                      1.962443e+11                     2.254225e+10
    ## 84                      2.450479e+11                     4.207822e+09
    ## 85                      5.589691e+10                     4.324768e+09
    ## 86                      1.995351e+11                     2.253987e+10
    ## 87                      1.477998e+11                     4.619885e+10
    ## 88                      2.283068e+12                     7.453075e+11
    ## 89                      1.536262e+11                     1.089762e+10
    ## 90                      1.810445e+12                     2.930635e+11
    ## 91                      3.959908e+12                     3.502494e+11
    ## 92                      1.106344e+12                     4.492169e+10
    ## 93                      4.432486e+11                     8.430698e+10
    ## 94                      5.545322e+11                     2.553647e+10
    ## 95                      5.711288e+12                     3.053070e+11
    ## 96                      1.098929e+11                     7.266575e+09
    ## 97                      1.631872e+12                     3.879388e+11
    ## 98                      2.419564e+11                     1.962854e+10
    ## 99                      2.406517e+11                     9.535943e+09
    ## 100                     1.110225e+11                     5.729899e+09
    ## 101                     5.102947e+10                     3.541307e+09
    ## 102                     8.098461e+11                     6.864233e+10
    ## 103                     1.970472e+12                     1.853049e+11
    ## 104                     1.936933e+12                     3.018140e+11
    ## 105                     2.944132e+11                     4.798334e+10
    ## 106                     2.895659e+11                     2.835240e+10
    ## 107                     1.965629e+11                     1.838684e+10
    ## 108                     1.058091e+12                     1.158899e+11
    ## 109                     2.135564e+11                     1.403546e+10
    ## 110                     2.445863e+12                     3.623172e+11
    ## 111                     1.379718e+11                     4.370046e+09
    ## 112                     3.478202e+11                     4.278136e+10
    ## 113                     4.950688e+12                     6.037805e+11
    ## 114                     3.119412e+09                     2.162162e+08
    ## 115                     1.351673e+12                     5.233965e+10
    ## 116                     1.324680e+12                     1.449223e+11
    ## 117                     1.155634e+13                     1.361550e+12
    ## 118                     1.170470e+08                     4.061059e+06
    ## 119                     7.794161e+09                     3.689780e+08
    ## 120                     7.392540e+07                     1.727841e+07
    ## 121                     1.608951e+11                     1.129493e+10
    ## 122                     2.941199e+11                     3.988509e+10
    ## 123                     3.793518e+10                     4.944401e+09
    ##     pop_meatless_day_l_green_wf pop_no_dairy_kg_co2e_excl_luc
    ## 1                  2.950148e+11                  1.864485e+08
    ## 2                  1.401941e+11                  1.163493e+08
    ## 3                  1.004004e+12                  5.798330e+08
    ## 4                  1.855743e+12                  5.216525e+09
    ## 5                  9.743160e+10                  7.840832e+07
    ## 6                  1.546895e+12                  3.098291e+09
    ## 7                  2.074293e+11                  3.086612e+08
    ## 8                  1.975119e+11                  1.729726e+08
    ## 9                  1.093640e+08                  1.357249e+05
    ## 10                 3.066376e+11                  2.860159e+08
    ## 11                 2.856038e+11                  5.220886e+08
    ## 12                 5.770612e+09                  4.691936e+06
    ## 13                 1.729286e+10                  9.702734e+06
    ## 14                 8.132911e+06                  1.690595e+04
    ## 15                 9.197420e+10                  5.040904e+07
    ## 16                 4.664807e+10                  4.081777e+07
    ## 17                 1.082223e+13                  1.448871e+10
    ## 18                 7.575673e+09                  1.049854e+07
    ## 19                 2.669004e+11                  1.391926e+08
    ## 20                 1.086585e+11                  6.963478e+07
    ## 21                 2.188703e+11                  1.629383e+08
    ## 22                 3.362266e+11                  2.216585e+08
    ## 23                 1.082484e+12                  1.398163e+09
    ## 24                 1.181368e+10                  9.088705e+06
    ## 25                 6.267181e+11                  1.250860e+09
    ## 26                 1.776715e+13                  2.855509e+10
    ## 27                 1.982119e+12                  2.252935e+09
    ## 28                 5.684900e+11                  4.258761e+08
    ## 29                 1.817211e+11                  1.786773e+08
    ## 30                 1.804249e+11                  1.019183e+08
    ## 31                 1.764114e+11                  1.343741e+08
    ## 32                 4.442981e+10                  4.539132e+07
    ## 33                 2.237579e+11                  2.614633e+08
    ## 34                 1.659609e+11                  2.898917e+08
    ## 35                 1.038983e+12                  8.112202e+08
    ## 36                 1.163633e+12                  2.328473e+09
    ## 37                 1.505819e+11                  1.223844e+08
    ## 38                 4.139759e+10                  3.217172e+07
    ## 39                 7.439878e+11                  2.755359e+08
    ## 40                 1.944303e+08                  1.983640e+05
    ## 41                 2.007345e+11                  2.226482e+08
    ## 42                 1.326318e+12                  2.328294e+09
    ## 43                 2.128587e+11                  8.686086e+07
    ## 44                 1.708571e+12                  2.538928e+09
    ## 45                 3.090531e+11                  1.615004e+08
    ## 46                 6.936545e+11                  7.452184e+08
    ## 47                 2.532347e+11                  1.593092e+08
    ## 48                 2.990012e+11                  1.966772e+08
    ## 49                 2.838731e+11                  2.111828e+08
    ## 50                 9.648367e+09                  2.316073e+07
    ## 51                 2.981755e+13                  1.667271e+10
    ## 52                 6.210893e+12                  5.301371e+09
    ## 53                 2.507059e+12                  1.773031e+09
    ## 54                 1.130813e+11                  2.830811e+08
    ## 55                 3.241843e+11                  4.093977e+08
    ## 56                 1.553541e+12                  1.770104e+09
    ## 57                 3.661616e+10                  2.811886e+07
    ## 58                 2.041325e+12                  3.116375e+09
    ## 59                 2.664961e+11                  2.592690e+08
    ## 60                 7.917184e+11                  7.324819e+08
    ## 61                 5.764195e+11                  3.404254e+08
    ## 62                 1.544946e+11                  2.039505e+08
    ## 63                 7.363649e+10                  1.349502e+08
    ## 64                 6.632124e+10                  4.022329e+07
    ## 65                 2.231719e+11                  2.208218e+08
    ## 66                 1.055637e+11                  6.837881e+07
    ## 67                 4.466655e+09                  8.959401e+06
    ## 68                 1.979048e+11                  1.342052e+08
    ## 69                 1.633701e+08                  7.682546e+04
    ## 70                 7.151667e+11                  8.853781e+08
    ## 71                 7.690881e+09                  1.054196e+07
    ## 72                 1.531991e+11                  7.185384e+07
    ## 73                 9.516308e+09                  1.344657e+07
    ## 74                 2.763924e+10                  1.868877e+07
    ## 75                 2.605893e+10                  3.184049e+07
    ## 76                 4.251385e+12                  3.899630e+09
    ## 77                 7.608724e+10                  3.960356e+07
    ## 78                 2.346409e+10                  1.928554e+07
    ## 79                 1.258780e+12                  5.531468e+08
    ## 80                 5.129835e+10                  4.451322e+07
    ## 81                 5.441616e+11                  3.161566e+08
    ## 82                 5.569917e+11                  8.343725e+08
    ## 83                 1.737021e+11                  3.913499e+08
    ## 84                 2.408401e+11                  5.647954e+07
    ## 85                 5.157214e+10                  3.937589e+07
    ## 86                 1.769953e+11                  3.246541e+08
    ## 87                 1.016010e+11                  1.338481e+08
    ## 88                 1.537761e+12                  1.332448e+09
    ## 89                 1.427285e+11                  2.122608e+08
    ## 90                 1.517381e+12                  1.476538e+09
    ## 91                 3.609659e+12                  3.523720e+09
    ## 92                 1.061422e+12                  8.590553e+08
    ## 93                 3.589417e+11                  4.381241e+08
    ## 94                 5.289957e+11                  3.581923e+08
    ## 95                 5.405981e+12                  4.139047e+09
    ## 96                 1.026263e+11                  3.159277e+07
    ## 97                 1.243934e+12                  1.297410e+09
    ## 98                 2.223279e+11                  1.766672e+08
    ## 99                 2.311158e+11                  1.590275e+08
    ## 100                1.052926e+11                  9.562332e+07
    ## 101                4.748816e+10                  6.275227e+07
    ## 102                7.412038e+11                  8.938876e+08
    ## 103                1.785167e+12                  3.180666e+09
    ## 104                1.635119e+12                  1.633946e+09
    ## 105                2.464299e+11                  1.538266e+08
    ## 106                2.612135e+11                  4.429859e+08
    ## 107                1.781760e+11                  2.899524e+08
    ## 108                9.422016e+11                  1.153724e+09
    ## 109                1.995210e+11                  8.233789e+07
    ## 110                2.083546e+12                  1.731805e+09
    ## 111                1.336018e+11                  4.923256e+07
    ## 112                3.050388e+11                  1.470177e+08
    ## 113                4.346908e+12                  2.999593e+09
    ## 114                2.903196e+09                  1.175183e+06
    ## 115                1.299333e+12                  7.470767e+08
    ## 116                1.179758e+12                  2.675995e+09
    ## 117                1.019479e+13                  1.550351e+10
    ## 118                1.129859e+08                  1.318278e+05
    ## 119                7.425183e+09                  1.118436e+07
    ## 120                5.664699e+07                  4.350671e+04
    ## 121                1.496001e+11                  9.988784e+07
    ## 122                2.542348e+11                  3.901669e+08
    ## 123                3.299078e+10                  5.020094e+07
    ##     pop_no_dairy_kg_co2e_total pop_no_dairy_kg_co2e_luc
    ## 1                 1.885233e+08             2.074841e+06
    ## 2                 1.215931e+08             5.243864e+06
    ## 3                 8.263554e+08             2.465224e+08
    ## 4                 6.296446e+09             1.079920e+09
    ## 5                 8.145536e+07             3.047045e+06
    ## 6                 3.248583e+09             1.502925e+08
    ## 7                 3.267922e+08             1.813103e+07
    ## 8                 1.840915e+08             1.111891e+07
    ## 9                 1.570596e+05             2.133468e+04
    ## 10                3.063981e+08             2.038221e+07
    ## 11                5.921465e+08             7.005790e+07
    ## 12                5.151063e+06             4.591275e+05
    ## 13                1.255174e+07             2.849007e+06
    ## 14                1.742761e+04             5.216656e+02
    ## 15                5.611887e+07             5.709833e+06
    ## 16                4.213003e+07             1.312267e+06
    ## 17                2.351211e+10             9.023401e+09
    ## 18                1.101931e+07             5.207741e+05
    ## 19                1.510796e+08             1.188699e+07
    ## 20                7.091848e+07             1.283699e+06
    ## 21                1.849626e+08             2.202426e+07
    ## 22                2.356373e+08             1.397879e+07
    ## 23                1.411527e+09             1.336474e+07
    ## 24                9.949203e+06             8.604975e+05
    ## 25                3.102398e+09             1.851538e+09
    ## 26                3.028985e+10             1.734766e+09
    ## 27                2.698686e+09             4.457507e+08
    ## 28                4.612762e+08             3.540008e+07
    ## 29                1.891599e+08             1.048267e+07
    ## 30                1.116799e+08             9.761560e+06
    ## 31                1.508210e+08             1.644691e+07
    ## 32                5.586179e+07             1.047047e+07
    ## 33                2.822822e+08             2.081889e+07
    ## 34                3.293276e+08             3.943594e+07
    ## 35                9.829976e+08             1.717774e+08
    ## 36                2.497638e+09             1.691654e+08
    ## 37                1.659055e+08             4.352106e+07
    ## 38                3.502647e+07             2.854757e+06
    ## 39                2.801122e+08             4.576353e+06
    ## 40                2.010311e+05             2.667100e+03
    ## 41                2.348452e+08             1.219705e+07
    ## 42                2.587004e+09             2.587097e+08
    ## 43                9.038107e+07             3.520212e+06
    ## 44                2.873312e+09             3.343837e+08
    ## 45                1.805635e+08             1.906310e+07
    ## 46                8.125374e+08             6.731905e+07
    ## 47                1.716032e+08             1.229403e+07
    ## 48                2.939259e+08             9.724868e+07
    ## 49                2.226267e+08             1.144388e+07
    ## 50                2.483253e+07             1.671800e+06
    ## 51                1.748206e+10             8.093485e+08
    ## 52                5.825832e+09             5.244608e+08
    ## 53                1.819978e+09             4.694739e+07
    ## 54                3.188932e+08             3.581210e+07
    ## 55                4.707415e+08             6.134373e+07
    ## 56                2.002953e+09             2.328492e+08
    ## 57                3.073108e+07             2.612222e+06
    ## 58                3.276034e+09             1.596590e+08
    ## 59                3.253043e+08             6.603532e+07
    ## 60                7.647889e+08             3.230705e+07
    ## 61                3.503893e+08             9.963972e+06
    ## 62                2.065977e+08             2.647172e+06
    ## 63                1.542949e+08             1.934465e+07
    ## 64                4.358975e+07             3.366458e+06
    ## 65                2.769948e+08             5.617302e+07
    ## 66                7.528192e+07             6.903111e+06
    ## 67                9.560695e+06             6.012939e+05
    ## 68                1.428502e+08             8.644928e+06
    ## 69                7.803234e+04             1.206875e+03
    ## 70                1.092100e+09             2.067218e+08
    ## 71                1.130842e+07             7.664613e+05
    ## 72                7.228339e+07             4.295513e+05
    ## 73                1.516881e+07             1.722241e+06
    ## 74                1.914086e+07             4.520912e+05
    ## 75                3.557963e+07             3.739137e+06
    ## 76                3.973988e+09             7.435753e+07
    ## 77                4.254974e+07             2.946188e+06
    ## 78                2.137984e+07             2.094295e+06
    ## 79                5.898761e+08             3.672932e+07
    ## 80                4.760913e+07             3.095916e+06
    ## 81                3.361303e+08             1.997373e+07
    ## 82                9.657433e+08             1.313708e+08
    ## 83                4.186987e+08             2.734876e+07
    ## 84                5.661123e+07             1.316943e+05
    ## 85                4.392647e+07             4.550580e+06
    ## 86                3.624405e+08             3.778642e+07
    ## 87                1.481596e+08             1.431155e+07
    ## 88                1.376672e+09             4.422406e+07
    ## 89                2.780190e+08             6.575825e+07
    ## 90                1.735554e+09             2.590155e+08
    ## 91                3.761080e+09             2.373596e+08
    ## 92                9.938290e+08             1.347737e+08
    ## 93                5.036828e+08             6.555873e+07
    ## 94                3.711057e+08             1.291345e+07
    ## 95                4.890710e+09             7.516633e+08
    ## 96                3.283842e+07             1.245648e+06
    ## 97                1.581295e+09             2.838855e+08
    ## 98                1.859210e+08             9.253812e+06
    ## 99                1.617355e+08             2.708003e+06
    ## 100               1.011606e+08             5.537289e+06
    ## 101               7.303080e+07             1.027854e+07
    ## 102               1.024298e+09             1.304100e+08
    ## 103               3.521183e+09             3.405170e+08
    ## 104               2.064193e+09             4.302477e+08
    ## 105               1.552073e+08             1.380677e+06
    ## 106               4.861151e+08             4.312919e+07
    ## 107               3.289498e+08             3.899742e+07
    ## 108               1.284215e+09             1.304910e+08
    ## 109               8.421920e+07             1.881318e+06
    ## 110               2.166015e+09             4.342106e+08
    ## 111               5.081742e+07             1.584870e+06
    ## 112               1.724965e+08             2.547876e+07
    ## 113               3.281383e+09             2.817897e+08
    ## 114               1.315370e+06             1.401867e+05
    ## 115               7.750759e+08             2.799917e+07
    ## 116               3.109505e+09             4.335103e+08
    ## 117               1.568865e+10             1.851465e+08
    ## 118               1.572550e+05             2.542718e+04
    ## 119               1.340465e+07             2.220289e+06
    ## 120               5.229019e+04             8.783487e+03
    ## 121               1.022221e+08             2.334304e+06
    ## 122               4.999952e+08             1.098282e+08
    ## 123               5.715630e+07             6.955358e+06
    ##     pop_no_dairy_l_blue_green_wf pop_no_dairy_l_blue_wf_total
    ## 1                   4.814114e+11                 1.861518e+11
    ## 2                   1.567558e+11                 2.114242e+10
    ## 3                   1.154628e+12                 1.695877e+11
    ## 4                   2.023956e+12                 1.365604e+11
    ## 5                   1.178968e+11                 1.542340e+10
    ## 6                   1.822734e+12                 2.176142e+11
    ## 7                   2.205386e+11                 1.877393e+10
    ## 8                   2.415311e+11                 4.178238e+10
    ## 9                   1.207542e+08                 1.221821e+07
    ## 10                  3.107609e+11                 1.283744e+10
    ## 11                  3.198005e+11                 4.313777e+10
    ## 12                  5.897738e+09                 3.437287e+08
    ## 13                  1.918841e+10                 1.213987e+09
    ## 14                  9.741447e+06                 1.227559e+06
    ## 15                  9.380248e+10                 3.546786e+09
    ## 16                  4.589730e+10                 5.518450e+09
    ## 17                  1.144976e+13                 4.623044e+11
    ## 18                  8.993175e+09                 9.977568e+08
    ## 19                  2.450595e+11                 1.313075e+10
    ## 20                  1.149120e+11                 3.657407e+09
    ## 21                  2.724212e+11                 3.735113e+10
    ## 22                  3.535931e+11                 1.798119e+10
    ## 23                  1.165279e+12                 1.188797e+11
    ## 24                  1.277742e+10                 1.310772e+09
    ## 25                  7.485177e+11                 1.313161e+11
    ## 26                  2.131262e+13                 3.514835e+12
    ## 27                  1.948140e+12                 1.275153e+11
    ## 28                  6.464172e+11                 5.830503e+10
    ## 29                  1.889254e+11                 2.020470e+10
    ## 30                  1.995025e+11                 7.810122e+09
    ## 31                  1.708770e+11                 8.458626e+09
    ## 32                  5.133549e+10                 6.972860e+09
    ## 33                  2.356923e+11                 1.704355e+10
    ## 34                  1.753321e+11                 1.759661e+10
    ## 35                  1.032250e+12                 1.277489e+11
    ## 36                  2.401643e+12                 1.254491e+12
    ## 37                  1.553855e+11                 9.156907e+09
    ## 38                  4.056593e+10                 1.860329e+09
    ## 39                  8.209484e+11                 6.637364e+10
    ## 40                  2.181809e+08                 1.384709e+07
    ## 41                  1.930719e+11                 2.177297e+10
    ## 42                  1.406486e+12                 1.576146e+11
    ## 43                  2.063220e+11                 1.504880e+10
    ## 44                  1.849039e+12                 2.057750e+11
    ## 45                  3.321885e+11                 2.807387e+10
    ## 46                  7.780044e+11                 1.106375e+11
    ## 47                  2.557114e+11                 1.187186e+10
    ## 48                  3.213071e+11                 1.761914e+10
    ## 49                  2.212941e+11                 1.061348e+10
    ## 50                  1.182837e+10                 1.862237e+09
    ## 51                  3.722781e+13                 7.946677e+12
    ## 52                  7.245794e+12                 8.445960e+11
    ## 53                  4.041870e+12                 1.561387e+12
    ## 54                  1.184901e+11                 1.289533e+10
    ## 55                  3.659739e+11                 4.804657e+10
    ## 56                  1.655812e+12                 1.754874e+11
    ## 57                  3.381117e+10                 2.695623e+09
    ## 58                  2.314725e+12                 3.128269e+11
    ## 59                  3.257670e+11                 6.274286e+10
    ## 60                  9.732857e+11                 1.540000e+11
    ## 61                  6.118170e+11                 4.197026e+10
    ## 62                  1.926661e+11                 3.605154e+10
    ## 63                  9.020128e+10                 1.455857e+10
    ## 64                  6.401303e+10                 3.300428e+09
    ## 65                  2.739014e+11                 5.602773e+10
    ## 66                  9.487607e+10                 4.209306e+09
    ## 67                  4.659836e+09                 4.890410e+08
    ## 68                  2.387844e+11                 3.337709e+10
    ## 69                  1.756999e+08                 9.834107e+06
    ## 70                  8.555286e+11                 1.111877e+11
    ## 71                  9.507162e+09                 2.333776e+09
    ## 72                  1.553894e+11                 1.232445e+10
    ## 73                  1.137307e+10                 1.656219e+09
    ## 74                  2.333532e+10                 2.878151e+09
    ## 75                  3.088418e+10                 3.654095e+09
    ## 76                  4.538270e+12                 5.294255e+11
    ## 77                  8.593662e+10                 8.730824e+09
    ## 78                  2.369378e+10                 1.290840e+09
    ## 79                  1.330222e+12                 1.817606e+11
    ## 80                  5.520021e+10                 4.154648e+09
    ## 81                  6.054360e+11                 8.184937e+10
    ## 82                  5.563160e+11                 5.376871e+10
    ## 83                  2.074087e+11                 2.394965e+10
    ## 84                  2.367525e+11                 4.919887e+09
    ## 85                  5.327854e+10                 4.150374e+09
    ## 86                  1.965103e+11                 2.493274e+10
    ## 87                  1.547028e+11                 4.851797e+10
    ## 88                  2.365474e+12                 9.609589e+11
    ## 89                  1.651206e+11                 1.207728e+10
    ## 90                  1.681263e+12                 3.005343e+11
    ## 91                  4.006481e+12                 3.835331e+11
    ## 92                  1.016096e+12                 4.067664e+10
    ## 93                  4.072765e+11                 8.010364e+10
    ## 94                  5.005994e+11                 2.685801e+10
    ## 95                  5.740444e+12                 3.142541e+11
    ## 96                  1.091039e+11                 7.342898e+09
    ## 97                  1.747372e+12                 4.083895e+11
    ## 98                  2.174991e+11                 2.234570e+10
    ## 99                  2.336566e+11                 9.806568e+09
    ## 100                 1.030093e+11                 5.905741e+09
    ## 101                 4.792101e+10                 3.691010e+09
    ## 102                 8.321325e+11                 8.400002e+10
    ## 103                 1.992053e+12                 1.930424e+11
    ## 104                 1.791268e+12                 2.883845e+11
    ## 105                 3.130638e+11                 5.307619e+10
    ## 106                 2.603710e+11                 3.107619e+10
    ## 107                 2.075790e+11                 2.133385e+10
    ## 108                 1.079618e+12                 1.223299e+11
    ## 109                 2.049684e+11                 1.391461e+10
    ## 110                 2.551504e+12                 4.020378e+11
    ## 111                 1.402229e+11                 4.783527e+09
    ## 112                 3.309322e+11                 3.895740e+10
    ## 113                 4.936599e+12                 5.954162e+11
    ## 114                 2.995398e+09                 2.395369e+08
    ## 115                 1.324101e+12                 5.343988e+10
    ## 116                 1.278942e+12                 1.582212e+11
    ## 117                 1.212482e+13                 1.507205e+12
    ## 118                 1.124481e+08                 4.526187e+06
    ## 119                 7.535791e+09                 3.928685e+08
    ## 120                 7.858103e+07                 1.980662e+07
    ## 121                 1.662074e+11                 1.279108e+10
    ## 122                 3.093611e+11                 4.305049e+10
    ## 123                 3.974550e+10                 5.433294e+09
    ##     pop_no_dairy_l_green_wf pop_low_red_meat_kg_co2e_excl_luc
    ## 1              2.952595e+11                      6.773436e+08
    ## 2              1.356134e+11                      2.274226e+08
    ## 3              9.850401e+11                      1.060924e+09
    ## 4              1.887395e+12                      4.478082e+09
    ## 5              1.024734e+11                      1.298599e+08
    ## 6              1.605120e+12                      2.691239e+09
    ## 7              2.017647e+11                      3.899334e+08
    ## 8              1.997487e+11                      3.151734e+08
    ## 9              1.085360e+08                      1.760279e+05
    ## 10             2.979235e+11                      3.347629e+08
    ## 11             2.766627e+11                      6.400837e+08
    ## 12             5.554009e+09                      5.613379e+06
    ## 13             1.797442e+10                      1.190539e+07
    ## 14             8.513888e+06                      1.218620e+04
    ## 15             9.025569e+10                      8.957928e+07
    ## 16             4.037885e+10                      8.447632e+07
    ## 17             1.098746e+13                      1.801755e+10
    ## 18             7.995418e+09                      1.258826e+07
    ## 19             2.319287e+11                      2.114465e+08
    ## 20             1.112546e+11                      8.606757e+07
    ## 21             2.350701e+11                      1.697275e+08
    ## 22             3.356119e+11                      2.652920e+08
    ## 23             1.046399e+12                      1.575035e+09
    ## 24             1.146665e+10                      1.354170e+07
    ## 25             6.172015e+11                      1.312673e+09
    ## 26             1.779779e+13                      2.958982e+10
    ## 27             1.820624e+12                      3.728394e+09
    ## 28             5.881122e+11                      4.675372e+08
    ## 29             1.687207e+11                      3.509264e+08
    ## 30             1.916924e+11                      1.286527e+08
    ## 31             1.624184e+11                      1.846789e+08
    ## 32             4.436263e+10                      5.005215e+07
    ## 33             2.186487e+11                      3.382372e+08
    ## 34             1.577354e+11                      3.676409e+08
    ## 35             9.045009e+11                      1.018588e+09
    ## 36             1.147152e+12                      2.980147e+09
    ## 37             1.462286e+11                      1.880571e+08
    ## 38             3.870560e+10                      4.582079e+07
    ## 39             7.545748e+11                      5.832216e+08
    ## 40             2.043338e+08                      2.503664e+05
    ## 41             1.712989e+11                      3.589439e+08
    ## 42             1.248872e+12                      2.815239e+09
    ## 43             1.912732e+11                      2.092461e+08
    ## 44             1.643264e+12                      3.432087e+09
    ## 45             3.041147e+11                      1.901927e+08
    ## 46             6.673668e+11                      9.986162e+08
    ## 47             2.438395e+11                      2.522400e+08
    ## 48             3.036879e+11                      3.069029e+08
    ## 49             2.106806e+11                      2.692473e+08
    ## 50             9.966129e+09                      2.954465e+07
    ## 51             2.928113e+13                      4.397857e+10
    ## 52             6.401198e+12                      5.904841e+09
    ## 53             2.480483e+12                      2.366770e+09
    ## 54             1.055947e+11                      3.507602e+08
    ## 55             3.179273e+11                      4.942174e+08
    ## 56             1.480325e+12                      2.342207e+09
    ## 57             3.111555e+10                      3.274228e+07
    ## 58             2.001898e+12                      3.481891e+09
    ## 59             2.630241e+11                      3.772120e+08
    ## 60             8.192858e+11                      1.128512e+09
    ## 61             5.698468e+11                      7.262280e+08
    ## 62             1.566146e+11                      3.207361e+08
    ## 63             7.564270e+10                      1.535509e+08
    ## 64             6.071260e+10                      6.113893e+07
    ## 65             2.178737e+11                      2.751192e+08
    ## 66             9.066677e+10                      1.193345e+08
    ## 67             4.170795e+09                      9.334894e+06
    ## 68             2.054073e+11                      2.811548e+08
    ## 69             1.658657e+08                      8.351829e+04
    ## 70             7.443409e+11                      9.327576e+08
    ## 71             7.173386e+09                      1.436249e+07
    ## 72             1.430649e+11                      1.560063e+08
    ## 73             9.716855e+09                      1.615334e+07
    ## 74             2.045717e+10                      3.353500e+07
    ## 75             2.723009e+10                      4.027413e+07
    ## 76             4.008845e+12                      4.474244e+09
    ## 77             7.720580e+10                      7.597942e+07
    ## 78             2.240294e+10                      3.097911e+07
    ## 79             1.148462e+12                      7.314547e+08
    ## 80             5.104556e+10                      6.908519e+07
    ## 81             5.235866e+11                      6.882310e+08
    ## 82             5.025472e+11                      1.211413e+09
    ## 83             1.834590e+11                      3.938213e+08
    ## 84             2.318327e+11                      7.873863e+07
    ## 85             4.912816e+10                      7.194971e+07
    ## 86             1.715775e+11                      4.286808e+08
    ## 87             1.061848e+11                      1.729948e+08
    ## 88             1.404515e+12                      3.471927e+09
    ## 89             1.530434e+11                      2.338111e+08
    ## 90             1.380729e+12                      1.865970e+09
    ## 91             3.622948e+12                      3.709639e+09
    ## 92             9.754197e+11                      1.363849e+09
    ## 93             3.271728e+11                      4.785912e+08
    ## 94             4.737414e+11                      6.148884e+08
    ## 95             5.426190e+12                      5.605005e+09
    ## 96             1.017610e+11                      4.223322e+07
    ## 97             1.338982e+12                      1.675767e+09
    ## 98             1.951534e+11                      2.589435e+08
    ## 99             2.238500e+11                      2.252796e+08
    ## 100            9.710351e+10                      1.279295e+08
    ## 101            4.423000e+10                      8.127605e+07
    ## 102            7.481325e+11                      9.814154e+08
    ## 103            1.799010e+12                      3.179735e+09
    ## 104            1.502884e+12                      1.771787e+09
    ## 105            2.599876e+11                      2.344498e+08
    ## 106            2.292949e+11                      5.755039e+08
    ## 107            1.862452e+11                      3.981973e+08
    ## 108            9.572877e+11                      1.213549e+09
    ## 109            1.910538e+11                      2.237409e+08
    ## 110            2.149467e+12                      1.881481e+09
    ## 111            1.354394e+11                      5.271764e+07
    ## 112            2.919748e+11                      2.075889e+08
    ## 113            4.341183e+12                      4.785779e+09
    ## 114            2.755862e+09                      2.561420e+06
    ## 115            1.270662e+12                      1.044497e+09
    ## 116            1.120721e+12                      3.258244e+09
    ## 117            1.061762e+13                      1.741590e+10
    ## 118            1.079219e+08                      1.617267e+05
    ## 119            7.142922e+09                      1.418676e+07
    ## 120            5.877441e+07                      6.637574e+04
    ## 121            1.534163e+11                      1.860588e+08
    ## 122            2.663106e+11                      2.914221e+08
    ## 123            3.431221e+10                      4.083518e+07
    ##     pop_low_red_meat_kg_co2e_total pop_low_red_meat_kg_co2e_luc
    ## 1                     6.802215e+08                 2.877871e+06
    ## 2                     2.343025e+08                 6.879856e+06
    ## 3                     1.211823e+09                 1.508990e+08
    ## 4                     5.478932e+09                 1.000850e+09
    ## 5                     1.332310e+08                 3.371068e+06
    ## 6                     2.826303e+09                 1.350644e+08
    ## 7                     4.046835e+08                 1.475017e+07
    ## 8                     3.256520e+08                 1.047865e+07
    ## 9                     1.980893e+05                 2.206132e+04
    ## 10                    3.540412e+08                 1.927830e+07
    ## 11                    7.000168e+08                 5.993305e+07
    ## 12                    6.054702e+06                 4.413232e+05
    ## 13                    1.500017e+07                 3.094782e+06
    ## 14                    1.246681e+04                 2.806041e+02
    ## 15                    9.435409e+07                 4.774811e+06
    ## 16                    8.555130e+07                 1.074979e+06
    ## 17                    2.770917e+10                 9.691625e+09
    ## 18                    1.310445e+07                 5.161941e+05
    ## 19                    2.228034e+08                 1.135690e+07
    ## 20                    8.749403e+07                 1.426455e+06
    ## 21                    1.919582e+08                 2.223070e+07
    ## 22                    2.791933e+08                 1.390130e+07
    ## 23                    1.588012e+09                 1.297668e+07
    ## 24                    1.434274e+07                 8.010390e+05
    ## 25                    3.100298e+09                 1.787625e+09
    ## 26                    3.126345e+10                 1.673626e+09
    ## 27                    4.105233e+09                 3.768392e+08
    ## 28                    5.025133e+08                 3.497610e+07
    ## 29                    3.617284e+08                 1.080199e+07
    ## 30                    1.387242e+08                 1.007149e+07
    ## 31                    1.974307e+08                 1.275178e+07
    ## 32                    5.822765e+07                 8.175500e+06
    ## 33                    3.543808e+08                 1.614359e+07
    ## 34                    4.096288e+08                 4.198787e+07
    ## 35                    1.156793e+09                 1.382056e+08
    ## 36                    3.142214e+09                 1.620667e+08
    ## 37                    2.499128e+08                 6.185566e+07
    ## 38                    4.845003e+07                 2.629233e+06
    ## 39                    5.885170e+08                 5.295366e+06
    ## 40                    2.531810e+05                 2.814651e+03
    ## 41                    3.726179e+08                 1.367406e+07
    ## 42                    3.054102e+09                 2.388638e+08
    ## 43                    2.120062e+08                 2.760053e+06
    ## 44                    3.708123e+09                 2.760363e+08
    ## 45                    2.099549e+08                 1.976220e+07
    ## 46                    1.060601e+09                 6.198468e+07
    ## 47                    2.674485e+08                 1.520852e+07
    ## 48                    4.562541e+08                 1.493512e+08
    ## 49                    2.800173e+08                 1.077000e+07
    ## 50                    3.139462e+07                 1.849970e+06
    ## 51                    4.491912e+10                 9.405486e+08
    ## 52                    6.414191e+09                 5.093495e+08
    ## 53                    2.412720e+09                 4.594935e+07
    ## 54                    3.814672e+08                 3.070696e+07
    ## 55                    5.562530e+08                 6.203557e+07
    ## 56                    2.547375e+09                 2.051679e+08
    ## 57                    3.527402e+07                 2.531744e+06
    ## 58                    3.645995e+09                 1.641033e+08
    ## 59                    4.328927e+08                 5.568066e+07
    ## 60                    1.156892e+09                 2.838055e+07
    ## 61                    7.382539e+08                 1.202597e+07
    ## 62                    3.264368e+08                 5.700770e+06
    ## 63                    1.712880e+08                 1.773709e+07
    ## 64                    6.444513e+07                 3.306197e+06
    ## 65                    3.239954e+08                 4.887614e+07
    ## 66                    1.264135e+08                 7.078971e+06
    ## 67                    9.836857e+06                 5.019630e+05
    ## 68                    2.901621e+08                 9.007343e+06
    ## 69                    8.476079e+04                 1.242497e+03
    ## 70                    1.136594e+09                 2.038361e+08
    ## 71                    1.516665e+07                 8.041575e+05
    ## 72                    1.565004e+08                 4.940858e+05
    ## 73                    1.770384e+07                 1.550497e+06
    ## 74                    3.393950e+07                 4.045006e+05
    ## 75                    4.417474e+07                 3.900611e+06
    ## 76                    4.544299e+09                 7.005504e+07
    ## 77                    7.878277e+07                 2.803348e+06
    ## 78                    3.252228e+07                 1.543168e+06
    ## 79                    7.662284e+08                 3.477371e+07
    ## 80                    7.176239e+07                 2.677205e+06
    ## 81                    7.097094e+08                 2.147838e+07
    ## 82                    1.341871e+09                 1.304580e+08
    ## 83                    4.212390e+08                 2.741764e+07
    ## 84                    7.890540e+07                 1.667662e+05
    ## 85                    7.591268e+07                 3.962973e+06
    ## 86                    4.681968e+08                 3.951595e+07
    ## 87                    1.848108e+08                 1.181594e+07
    ## 88                    3.520461e+09                 4.853421e+07
    ## 89                    3.000261e+08                 6.621495e+07
    ## 90                    2.108482e+09                 2.425113e+08
    ## 91                    3.935797e+09                 2.261578e+08
    ## 92                    1.508202e+09                 1.443532e+08
    ## 93                    5.367121e+08                 5.812087e+07
    ## 94                    6.345162e+08                 1.962778e+07
    ## 95                    6.351163e+09                 7.461580e+08
    ## 96                    4.348770e+07                 1.254486e+06
    ## 97                    1.910427e+09                 2.346601e+08
    ## 98                    2.681472e+08                 9.203726e+06
    ## 99                    2.280163e+08                 2.736719e+06
    ## 100                   1.336017e+08                 5.672274e+06
    ## 101                   8.889164e+07                 7.615591e+06
    ## 102                   1.103404e+09                 1.219886e+08
    ## 103                   3.510659e+09                 3.309246e+08
    ## 104                   2.126410e+09                 3.546230e+08
    ## 105                   2.363791e+08                 1.929321e+06
    ## 106                   6.182854e+08                 4.278155e+07
    ## 107                   4.316608e+08                 3.346358e+07
    ## 108                   1.337890e+09                 1.243417e+08
    ## 109                   2.262024e+08                 2.461453e+06
    ## 110                   2.287929e+09                 4.064484e+08
    ## 111                   5.435610e+07                 1.638465e+06
    ## 112                   2.304524e+08                 2.286350e+07
    ## 113                   5.016632e+09                 2.308524e+08
    ## 114                   2.806536e+06                 2.451157e+05
    ## 115                   1.071054e+09                 2.655719e+07
    ## 116                   3.649631e+09                 3.913865e+08
    ## 117                   1.758070e+10                 1.648014e+08
    ## 118                   1.851412e+05                 2.341456e+04
    ## 119                   1.633632e+07                 2.149562e+06
    ## 120                   7.291867e+04                 6.542932e+03
    ## 121                   1.883311e+08                 2.272293e+06
    ## 122                   3.489022e+08                 5.748011e+07
    ## 123                   4.515742e+07                 4.322244e+06
    ##     pop_low_red_meat_l_blue_green_wf pop_low_red_meat_l_blue_wf_total
    ## 1                       5.106240e+11                     2.095323e+11
    ## 2                       1.685259e+11                     2.524347e+10
    ## 3                       1.183037e+12                     1.586534e+11
    ## 4                       1.990770e+12                     1.322602e+11
    ## 5                       1.199668e+11                     1.573140e+10
    ## 6                       1.620705e+12                     2.023145e+11
    ## 7                       2.178147e+11                     1.684720e+10
    ## 8                       2.449285e+11                     4.089687e+10
    ## 9                       1.261950e+08                     1.203689e+07
    ## 10                      3.077399e+11                     1.191154e+10
    ## 11                      3.205365e+11                     3.639779e+10
    ## 12                      6.044991e+09                     3.525860e+08
    ## 13                      1.931913e+10                     1.223585e+09
    ## 14                      7.930640e+06                     1.142259e+06
    ## 15                      9.716499e+10                     3.445322e+09
    ## 16                      5.232693e+10                     5.219324e+09
    ## 17                      1.180647e+13                     4.383165e+11
    ## 18                      9.157694e+09                     1.001048e+09
    ## 19                      2.829358e+11                     1.295839e+10
    ## 20                      1.150936e+11                     3.965850e+09
    ## 21                      2.727748e+11                     3.720479e+10
    ## 22                      3.557144e+11                     1.785159e+10
    ## 23                      1.200520e+12                     1.132209e+11
    ## 24                      1.304646e+10                     1.356735e+09
    ## 25                      7.681746e+11                     1.302051e+11
    ## 26                      2.137830e+13                     3.549452e+12
    ## 27                      2.075289e+12                     1.113072e+11
    ## 28                      6.478276e+11                     5.702034e+10
    ## 29                      1.937732e+11                     1.773003e+10
    ## 30                      2.016211e+11                     7.931163e+09
    ## 31                      1.740415e+11                     7.612058e+09
    ## 32                      4.824661e+10                     6.559775e+09
    ## 33                      2.337252e+11                     1.396477e+10
    ## 34                      1.850084e+11                     1.704603e+10
    ## 35                      1.138989e+12                     9.946693e+10
    ## 36                      2.488231e+12                     1.292220e+12
    ## 37                      1.581836e+11                     9.036072e+09
    ## 38                      4.338532e+10                     1.829789e+09
    ## 39                      8.528101e+11                     7.007819e+10
    ## 40                      2.192902e+08                     1.336962e+07
    ## 41                      2.198365e+11                     2.106336e+10
    ## 42                      1.437043e+12                     1.556836e+11
    ## 43                      2.298912e+11                     1.784641e+10
    ## 44                      1.867595e+12                     1.907375e+11
    ## 45                      3.327076e+11                     2.788498e+10
    ## 46                      7.910479e+11                     1.104492e+11
    ## 47                      2.614047e+11                     1.216615e+10
    ## 48                      3.045492e+11                     1.619382e+10
    ## 49                      2.812962e+11                     9.765654e+09
    ## 50                      1.158361e+10                     1.788107e+09
    ## 51                      3.771978e+13                     7.766057e+12
    ## 52                      7.338174e+12                     8.186292e+11
    ## 53                      4.116129e+12                     1.562618e+12
    ## 54                      1.243048e+11                     1.260550e+10
    ## 55                      3.945226e+11                     4.879151e+10
    ## 56                      1.696371e+12                     1.736329e+11
    ## 57                      3.654063e+10                     2.619478e+09
    ## 58                      2.426664e+12                     3.102843e+11
    ## 59                      3.322253e+11                     6.539902e+10
    ## 60                      9.539234e+11                     1.509075e+11
    ## 61                      6.335014e+11                     3.980327e+10
    ## 62                      1.997663e+11                     3.901564e+10
    ## 63                      9.045410e+10                     1.491427e+10
    ## 64                      7.011381e+10                     3.274962e+09
    ## 65                      2.861481e+11                     5.592778e+10
    ## 66                      1.135076e+11                     4.354747e+09
    ## 67                      4.478952e+09                     4.822924e+08
    ## 68                      2.481830e+11                     3.217132e+10
    ## 69                      1.768927e+08                     9.811808e+06
    ## 70                      8.482003e+11                     1.080506e+11
    ## 71                      9.861765e+09                     2.281399e+09
    ## 72                      1.660617e+11                     1.187824e+10
    ## 73                      1.144618e+10                     1.596731e+09
    ## 74                      3.042347e+10                     2.831996e+09
    ## 75                      3.067842e+10                     3.586417e+09
    ## 76                      4.764478e+12                     5.364930e+11
    ## 77                      8.439548e+10                     8.115663e+09
    ## 78                      2.430633e+10                     1.159766e+09
    ## 79                      1.425859e+12                     1.852359e+11
    ## 80                      5.684313e+10                     4.207514e+09
    ## 81                      6.323358e+11                     7.890630e+10
    ## 82                      6.220880e+11                     5.548587e+10
    ## 83                      1.868362e+11                     2.203172e+10
    ## 84                      2.426750e+11                     5.227181e+09
    ## 85                      5.610704e+10                     4.185436e+09
    ## 86                      2.054145e+11                     2.377764e+10
    ## 87                      1.522527e+11                     4.705530e+10
    ## 88                      2.403789e+12                     8.134115e+11
    ## 89                      1.585948e+11                     1.140361e+10
    ## 90                      1.820985e+12                     2.931322e+11
    ## 91                      3.983924e+12                     3.717733e+11
    ## 92                      1.131502e+12                     4.537055e+10
    ## 93                      4.246220e+11                     8.221564e+10
    ## 94                      5.640602e+11                     2.646538e+10
    ## 95                      6.046978e+12                     3.268994e+11
    ## 96                      1.108003e+11                     7.348121e+09
    ## 97                      1.689412e+12                     4.058893e+11
    ## 98                      2.357021e+11                     2.205791e+10
    ## 99                      2.408995e+11                     9.655340e+09
    ## 100                     1.072753e+11                     5.681959e+09
    ## 101                     5.050336e+10                     3.550118e+09
    ## 102                     8.464446e+11                     8.359050e+10
    ## 103                     1.993718e+12                     1.905409e+11
    ## 104                     1.836901e+12                     2.932966e+11
    ## 105                     3.142356e+11                     5.118796e+10
    ## 106                     2.833297e+11                     2.982622e+10
    ## 107                     1.981638e+11                     1.759138e+10
    ## 108                     1.071047e+12                     1.209074e+11
    ## 109                     2.210898e+11                     1.478788e+10
    ## 110                     2.554520e+12                     3.932647e+11
    ## 111                     1.392120e+11                     4.779765e+09
    ## 112                     3.527277e+11                     4.237193e+10
    ## 113                     5.048870e+12                     6.086471e+11
    ## 114                     3.135805e+09                     2.331083e+08
    ## 115                     1.335585e+12                     5.264572e+10
    ## 116                     1.342982e+12                     1.497861e+11
    ## 117                     1.190271e+13                     1.422669e+12
    ## 118                     1.195177e+08                     4.266120e+06
    ## 119                     7.901092e+09                     3.861334e+08
    ## 120                     7.896501e+07                     1.930514e+07
    ## 121                     1.722483e+11                     1.285692e+10
    ## 122                     2.495159e+11                     4.142009e+10
    ## 123                     3.428102e+10                     5.186913e+09
    ##     pop_low_red_meat_l_green_wf pop_no_red_meat_kg_co2e_excl_luc
    ## 1                  3.010917e+11                     7.213291e+08
    ## 2                  1.432825e+11                     1.712291e+08
    ## 3                  1.024384e+12                     9.389424e+08
    ## 4                  1.858510e+12                     1.747758e+09
    ## 5                  1.042354e+11                     8.958737e+07
    ## 6                  1.418390e+12                     1.442776e+09
    ## 7                  2.009675e+11                     3.109757e+08
    ## 8                  2.040316e+11                     2.224873e+08
    ## 9                  1.141581e+08                     1.094594e+05
    ## 10                 2.958284e+11                     2.897646e+08
    ## 11                 2.841387e+11                     5.059781e+08
    ## 12                 5.692405e+09                     3.221864e+06
    ## 13                 1.809555e+10                     8.908518e+06
    ## 14                 6.788381e+06                     6.302409e+03
    ## 15                 9.371967e+10                     7.540754e+07
    ## 16                 4.710760e+10                     6.751744e+07
    ## 17                 1.136816e+13                     9.186266e+09
    ## 18                 8.156646e+09                     8.349484e+06
    ## 19                 2.699774e+11                     1.840177e+08
    ## 20                 1.111278e+11                     4.149407e+07
    ## 21                 2.355700e+11                     1.206937e+08
    ## 22                 3.378629e+11                     1.420943e+08
    ## 23                 1.087299e+12                     9.437679e+08
    ## 24                 1.168972e+10                     1.025259e+07
    ## 25                 6.379695e+11                     5.639801e+08
    ## 26                 1.782885e+13                     1.871801e+10
    ## 27                 1.963981e+12                     2.729134e+09
    ## 28                 5.908073e+11                     2.955346e+08
    ## 29                 1.760431e+11                     3.210032e+08
    ## 30                 1.936899e+11                     1.114032e+08
    ## 31                 1.664295e+11                     1.610601e+08
    ## 32                 4.168684e+10                     4.459685e+07
    ## 33                 2.197604e+11                     2.923778e+08
    ## 34                 1.679623e+11                     2.474227e+08
    ## 35                 1.039522e+12                     6.190228e+08
    ## 36                 1.196011e+12                     2.384232e+09
    ## 37                 1.491476e+11                     1.341224e+08
    ## 38                 4.155553e+10                     3.500737e+07
    ## 39                 7.827319e+11                     4.830925e+08
    ## 40                 2.059205e+08                     1.464887e+05
    ## 41                 1.987732e+11                     2.745504e+08
    ## 42                 1.281359e+12                     1.903422e+09
    ## 43                 2.120448e+11                     2.097448e+08
    ## 44                 1.676857e+12                     2.696835e+09
    ## 45                 3.048226e+11                     1.628222e+08
    ## 46                 6.805987e+11                     8.497539e+08
    ## 47                 2.492386e+11                     1.895739e+08
    ## 48                 2.883553e+11                     2.244150e+08
    ## 49                 2.715305e+11                     2.025857e+08
    ## 50                 9.795502e+09                     1.936449e+07
    ## 51                 2.995372e+13                     4.143624e+10
    ## 52                 6.519545e+12                     5.057222e+09
    ## 53                 2.553511e+12                     1.998150e+09
    ## 54                 1.116993e+11                     2.142061e+08
    ## 55                 3.457310e+11                     2.729714e+08
    ## 56                 1.522739e+12                     1.939899e+09
    ## 57                 3.392115e+10                     2.443223e+07
    ## 58                 2.116379e+12                     2.092488e+09
    ## 59                 2.668263e+11                     2.890775e+08
    ## 60                 8.030159e+11                     8.565924e+08
    ## 61                 5.936981e+11                     6.090145e+08
    ## 62                 1.607507e+11                     2.217978e+08
    ## 63                 7.553983e+10                     6.480919e+07
    ## 64                 6.683885e+10                     5.087258e+07
    ## 65                 2.302204e+11                     1.994411e+08
    ## 66                 1.091528e+11                     1.035370e+08
    ## 67                 3.996660e+09                     6.085344e+06
    ## 68                 2.160116e+11                     2.363783e+08
    ## 69                 1.670809e+08                     4.850379e+04
    ## 70                 7.401497e+11                     7.061563e+08
    ## 71                 7.580365e+09                     1.047170e+07
    ## 72                 1.541835e+11                     1.445831e+08
    ## 73                 9.849446e+09                     9.832034e+06
    ## 74                 2.759147e+10                     2.507198e+07
    ## 75                 2.709201e+10                     2.845428e+07
    ## 76                 4.227985e+12                     2.498009e+09
    ## 77                 7.627982e+10                     7.852102e+07
    ## 78                 2.314657e+10                     2.475586e+07
    ## 79                 1.240623e+12                     5.625756e+08
    ## 80                 5.263562e+10                     4.767992e+07
    ## 81                 5.534295e+11                     5.778949e+08
    ## 82                 5.666021e+11                     8.232010e+08
    ## 83                 1.648045e+11                     2.558282e+08
    ## 84                 2.374478e+11                     3.826837e+07
    ## 85                 5.192161e+10                     6.750179e+07
    ## 86                 1.816369e+11                     2.630610e+08
    ## 87                 1.051974e+11                     1.169288e+08
    ## 88                 1.590377e+12                     3.210908e+09
    ## 89                 1.471912e+11                     1.164624e+08
    ## 90                 1.527853e+12                     1.290720e+09
    ## 91                 3.612151e+12                     2.588133e+09
    ## 92                 1.086131e+12                     1.181858e+09
    ## 93                 3.424064e+11                     3.104847e+08
    ## 94                 5.375949e+11                     4.880051e+08
    ## 95                 5.720078e+12                     3.471922e+09
    ## 96                 1.034522e+11                     3.203594e+07
    ## 97                 1.283522e+12                     1.094127e+09
    ## 98                 2.136441e+11                     1.902322e+08
    ## 99                 2.312441e+11                     1.736324e+08
    ## 100                1.015933e+11                     1.106836e+08
    ## 101                4.695325e+10                     5.906645e+07
    ## 102                7.628541e+11                     5.579339e+08
    ## 103                1.803177e+12                     1.896496e+09
    ## 104                1.543604e+12                     1.281747e+09
    ## 105                2.630476e+11                     2.184459e+08
    ## 106                2.535034e+11                     3.802917e+08
    ## 107                1.805725e+11                     2.942850e+08
    ## 108                9.501397e+11                     9.009113e+08
    ## 109                2.063019e+11                     2.020868e+08
    ## 110                2.161255e+12                     1.507096e+09
    ## 111                1.344323e+11                     4.030389e+07
    ## 112                3.103557e+11                     1.718657e+08
    ## 113                4.440223e+12                     4.282875e+09
    ## 114                2.902697e+09                     2.173959e+06
    ## 115                1.282939e+12                     9.180629e+08
    ## 116                1.193196e+12                     1.909935e+09
    ## 117                1.048004e+13                     1.031937e+10
    ## 118                1.152516e+08                     7.959574e+04
    ## 119                7.514958e+09                     6.675439e+06
    ## 120                5.965987e+07                     5.127341e+04
    ## 121                1.593913e+11                     1.472017e+08
    ## 122                2.080958e+11                     1.947448e+08
    ## 123                2.909411e+10                     3.021359e+07
    ##     pop_no_red_meat_kg_co2e_total pop_no_red_meat_kg_co2e_luc
    ## 1                    7.245238e+08                3.194692e+06
    ## 2                    1.739477e+08                2.718696e+06
    ## 3                    1.095270e+09                1.563276e+08
    ## 4                    2.668801e+09                9.210433e+08
    ## 5                    9.093109e+07                1.343718e+06
    ## 6                    1.572354e+09                1.295786e+08
    ## 7                    3.192596e+08                8.283897e+06
    ## 8                    2.327421e+08                1.025485e+07
    ## 9                    1.184921e+05                9.032643e+03
    ## 10                   2.980543e+08                8.289736e+06
    ## 11                   5.419714e+08                3.599333e+07
    ## 12                   3.439600e+06                2.177358e+05
    ## 13                   1.162446e+07                2.715937e+06
    ## 14                   6.352923e+03                5.051406e+01
    ## 15                   7.936541e+07                3.957868e+06
    ## 16                   6.874515e+07                1.227713e+06
    ## 17                   1.360570e+10                4.419431e+09
    ## 18                   8.853244e+06                5.037598e+05
    ## 19                   1.884616e+08                4.443964e+06
    ## 20                   4.284477e+07                1.350701e+06
    ## 21                   1.412030e+08                2.050929e+07
    ## 22                   1.556601e+08                1.356575e+07
    ## 23                   9.516913e+08                7.923336e+06
    ## 24                   1.071704e+07                4.644539e+05
    ## 25                   8.095508e+08                2.455707e+08
    ## 26                   1.981219e+10                1.094183e+09
    ## 27                   3.112122e+09                3.829877e+08
    ## 28                   3.275090e+08                3.197438e+07
    ## 29                   3.264543e+08                5.451145e+06
    ## 30                   1.209861e+08                9.582843e+06
    ## 31                   1.666716e+08                5.611536e+06
    ## 32                   5.011970e+07                5.522853e+06
    ## 33                   2.995184e+08                7.140567e+06
    ## 34                   2.803933e+08                3.297056e+07
    ## 35                   7.523730e+08                1.333502e+08
    ## 36                   2.471041e+09                8.680882e+07
    ## 37                   1.571900e+08                2.306762e+07
    ## 38                   3.619279e+07                1.185417e+06
    ## 39                   4.883789e+08                5.286459e+06
    ## 40                   1.494676e+05                2.978934e+03
    ## 41                   2.813826e+08                6.832199e+06
    ## 42                   2.046333e+09                1.429115e+08
    ## 43                   2.113806e+08                1.635772e+06
    ## 44                   2.843586e+09                1.467515e+08
    ## 45                   1.811467e+08                1.832455e+07
    ## 46                   8.849190e+08                3.516514e+07
    ## 47                   2.033794e+08                1.380552e+07
    ## 48                   2.956282e+08                7.121317e+07
    ## 49                   2.062999e+08                3.714182e+06
    ## 50                   2.071527e+07                1.350775e+06
    ## 51                   4.236274e+10                9.265009e+08
    ## 52                   5.560493e+09                5.032711e+08
    ## 53                   2.047233e+09                4.908289e+07
    ## 54                   2.304491e+08                1.624296e+07
    ## 55                   2.820204e+08                9.048982e+06
    ## 56                   2.049804e+09                1.099054e+08
    ## 57                   2.535004e+07                9.178071e+05
    ## 58                   2.206066e+09                1.135777e+08
    ## 59                   3.389044e+08                4.982684e+07
    ## 60                   8.737003e+08                1.710793e+07
    ## 61                   6.210215e+08                1.200700e+07
    ## 62                   2.272872e+08                5.489415e+06
    ## 63                   7.397811e+07                9.168918e+06
    ## 64                   5.243562e+07                1.563049e+06
    ## 65                   2.266232e+08                2.718207e+07
    ## 66                   1.066543e+08                3.117322e+06
    ## 67                   6.366056e+06                2.807124e+05
    ## 68                   2.456524e+08                9.274169e+06
    ## 69                   4.966513e+04                1.161338e+03
    ## 70                   9.223869e+08                2.162306e+08
    ## 71                   1.084516e+07                3.734582e+05
    ## 72                   1.451719e+08                5.887762e+05
    ## 73                   1.020675e+07                3.747190e+05
    ## 74                   2.554221e+07                4.702233e+05
    ## 75                   3.176575e+07                3.311475e+06
    ## 76                   2.562429e+09                6.442021e+07
    ## 77                   8.090595e+07                2.384926e+06
    ## 78                   2.522530e+07                4.694412e+05
    ## 79                   5.992986e+08                3.672303e+07
    ## 80                   5.039004e+07                2.710120e+06
    ## 81                   5.992384e+08                2.134351e+07
    ## 82                   8.854072e+08                6.220619e+07
    ## 83                   2.830319e+08                2.720368e+07
    ## 84                   3.841194e+07                1.435697e+05
    ## 85                   7.060596e+07                3.104173e+06
    ## 86                   2.842586e+08                2.119758e+07
    ## 87                   1.284644e+08                1.153561e+07
    ## 88                   3.259235e+09                4.832616e+07
    ## 89                   1.425699e+08                2.610759e+07
    ## 90                   1.566738e+09                2.760178e+08
    ## 91                   2.794359e+09                2.062255e+08
    ## 92                   1.214546e+09                3.268751e+07
    ## 93                   3.435552e+08                3.307041e+07
    ## 94                   4.985779e+08                1.057281e+07
    ## 95                   3.544310e+09                7.238868e+07
    ## 96                   3.333928e+07                1.303341e+06
    ## 97                   1.309720e+09                2.155929e+08
    ## 98                   1.996251e+08                9.392921e+06
    ## 99                   1.754578e+08                1.825384e+06
    ## 100                  1.127032e+08                2.019614e+06
    ## 101                  6.627704e+07                7.210592e+06
    ## 102                  6.846507e+08                1.267169e+08
    ## 103                  2.193288e+09                2.967925e+08
    ## 104                  1.471447e+09                1.896995e+08
    ## 105                  2.203787e+08                1.932819e+06
    ## 106                  4.020538e+08                2.176206e+07
    ## 107                  3.081900e+08                1.390494e+07
    ## 108                  1.004406e+09                1.034946e+08
    ## 109                  2.045199e+08                2.433146e+06
    ## 110                  1.879430e+09                3.723349e+08
    ## 111                  4.181068e+07                1.506784e+06
    ## 112                  1.948844e+08                2.301868e+07
    ## 113                  4.527456e+09                2.445804e+08
    ## 114                  2.400797e+06                2.268386e+05
    ## 115                  9.325886e+08                1.452567e+07
    ## 116                  2.140527e+09                2.305919e+08
    ## 117                  1.035007e+10                3.070379e+07
    ## 118                  9.843211e+04                1.883637e+04
    ## 119                  7.838581e+06                1.163141e+06
    ## 120                  5.873617e+04                7.462760e+03
    ## 121                  1.494504e+08                2.248668e+06
    ## 122                  2.164647e+08                2.171989e+07
    ## 123                  3.234169e+07                2.128102e+06
    ##     pop_no_red_meat_l_blue_green_wf pop_no_red_meat_l_blue_wf_total
    ## 1                      4.776847e+11                    2.106760e+11
    ## 2                      1.324853e+11                    2.284559e+10
    ## 3                      1.065829e+12                    1.572157e+11
    ## 4                      1.786130e+12                    1.234272e+11
    ## 5                      8.665034e+10                    1.514707e+10
    ## 6                      1.265602e+12                    1.873837e+11
    ## 7                      1.874881e+11                    1.609179e+10
    ## 8                      2.023792e+11                    4.116057e+10
    ## 9                      1.068474e+08                    1.164882e+07
    ## 10                     2.822103e+11                    1.114605e+10
    ## 11                     2.629192e+11                    3.517810e+10
    ## 12                     5.743797e+09                    3.283332e+08
    ## 13                     1.802156e+10                    1.224677e+09
    ## 14                     5.936938e+06                    1.068962e+06
    ## 15                     8.826176e+10                    3.115412e+09
    ## 16                     4.543930e+10                    5.147554e+09
    ## 17                     9.143269e+12                    4.056743e+11
    ## 18                     8.277685e+09                    9.917214e+08
    ## 19                     2.535679e+11                    1.124464e+10
    ## 20                     1.076782e+11                    3.768001e+09
    ## 21                     2.630575e+11                    3.909238e+10
    ## 22                     3.446090e+11                    1.778864e+10
    ## 23                     9.382790e+11                    1.078519e+11
    ## 24                     1.220811e+10                    1.281648e+09
    ## 25                     6.366900e+11                    1.281004e+11
    ## 26                     1.793913e+13                    3.646912e+12
    ## 27                     1.899351e+12                    1.139008e+11
    ## 28                     6.232511e+11                    5.807761e+10
    ## 29                     1.850696e+11                    1.740254e+10
    ## 30                     1.968522e+11                    7.874501e+09
    ## 31                     1.451859e+11                    6.547304e+09
    ## 32                     3.908908e+10                    6.190694e+09
    ## 33                     2.072909e+11                    1.263657e+10
    ## 34                     1.462383e+11                    1.536972e+10
    ## 35                     9.763125e+11                    1.008210e+11
    ## 36                     2.222924e+12                    1.231969e+12
    ## 37                     1.540026e+11                    8.793576e+09
    ## 38                     3.670488e+10                    1.511288e+09
    ## 39                     7.719682e+11                    6.982901e+10
    ## 40                     1.829234e+08                    1.300898e+07
    ## 41                     1.776972e+11                    1.982779e+10
    ## 42                     1.149334e+12                    1.391149e+11
    ## 43                     2.080459e+11                    1.770800e+10
    ## 44                     1.545314e+12                    1.741364e+11
    ## 45                     3.235224e+11                    2.794437e+10
    ## 46                     6.673938e+11                    1.017625e+11
    ## 47                     2.484351e+11                    1.163514e+10
    ## 48                     3.087059e+11                    1.571457e+10
    ## 49                     2.543893e+11                    8.645732e+09
    ## 50                     8.881758e+09                    1.632808e+09
    ## 51                     3.730177e+13                    7.750641e+12
    ## 52                     7.132104e+12                    8.296201e+11
    ## 53                     3.942092e+12                    1.587279e+12
    ## 54                     9.752678e+10                    1.112125e+10
    ## 55                     2.670618e+11                    4.752986e+10
    ## 56                     1.463197e+12                    1.611779e+11
    ## 57                     3.468165e+10                    2.614192e+09
    ## 58                     1.931913e+12                    2.820541e+11
    ## 59                     3.097694e+11                    6.511846e+10
    ## 60                     8.717581e+11                    1.491820e+11
    ## 61                     5.503857e+11                    4.022101e+10
    ## 62                     1.624497e+11                    3.828885e+10
    ## 63                     8.164827e+10                    1.482122e+10
    ## 64                     6.027736e+10                    2.869369e+09
    ## 65                     2.333210e+11                    5.848796e+10
    ## 66                     1.005832e+11                    3.603605e+09
    ## 67                     3.466780e+09                    4.375388e+08
    ## 68                     2.266314e+11                    3.274855e+10
    ## 69                     1.705875e+08                    9.923386e+06
    ## 70                     8.102439e+11                    1.091816e+11
    ## 71                     8.966301e+09                    2.262474e+09
    ## 72                     1.511032e+11                    1.205986e+10
    ## 73                     8.996928e+09                    1.391132e+09
    ## 74                     2.713228e+10                    2.857186e+09
    ## 75                     2.792302e+10                    3.514284e+09
    ## 76                     4.243390e+12                    5.192339e+11
    ## 77                     8.333075e+10                    8.071857e+09
    ## 78                     2.161682e+10                    9.844936e+08
    ## 79                     1.346150e+12                    1.860383e+11
    ## 80                     5.161646e+10                    4.142356e+09
    ## 81                     5.710027e+11                    7.794373e+10
    ## 82                     4.913829e+11                    4.948195e+10
    ## 83                     1.507776e+11                    2.027150e+10
    ## 84                     2.361631e+11                    5.078378e+09
    ## 85                     5.211679e+10                    4.012314e+09
    ## 86                     1.627469e+11                    2.229557e+10
    ## 87                     1.271937e+11                    4.690488e+10
    ## 88                     2.100572e+12                    8.174356e+11
    ## 89                     1.447379e+11                    1.093070e+10
    ## 90                     1.562737e+12                    2.954008e+11
    ## 91                     3.804561e+12                    3.907293e+11
    ## 92                     1.015814e+12                    3.575427e+10
    ## 93                     3.469668e+11                    7.084607e+10
    ## 94                     4.856964e+11                    2.250385e+10
    ## 95                     5.014412e+12                    2.845432e+11
    ## 96                     1.061108e+11                    7.270727e+09
    ## 97                     1.573398e+12                    4.058554e+11
    ## 98                     2.212428e+11                    2.207442e+10
    ## 99                     2.060829e+11                    8.294454e+09
    ## 100                    9.623602e+10                    5.100320e+09
    ## 101                    4.355490e+10                    3.187885e+09
    ## 102                    7.063317e+11                    8.253815e+10
    ## 103                    1.560815e+12                    1.728670e+11
    ## 104                    1.577356e+12                    2.680036e+11
    ## 105                    3.064628e+11                    5.109962e+10
    ## 106                    2.266848e+11                    2.778646e+10
    ## 107                    1.675603e+11                    1.622602e+10
    ## 108                    9.557214e+11                    1.229409e+11
    ## 109                    2.010296e+11                    1.500599e+10
    ## 110                    2.460530e+12                    3.963774e+11
    ## 111                    1.359819e+11                    4.702538e+09
    ## 112                    3.029019e+11                    3.922573e+10
    ## 113                    4.591238e+12                    6.038443e+11
    ## 114                    2.993698e+09                    2.428476e+08
    ## 115                    1.286010e+12                    4.870524e+10
    ## 116                    1.051403e+12                    1.398598e+11
    ## 117                    9.143431e+12                    1.324299e+12
    ## 118                    9.585862e+07                    3.740740e+06
    ## 119                    5.859582e+09                    3.756566e+08
    ## 120                    6.994880e+07                    1.938661e+07
    ## 121                    1.619761e+11                    1.316405e+10
    ## 122                    2.073558e+11                    4.126260e+10
    ## 123                    2.991733e+10                    5.245678e+09
    ##     pop_no_red_meat_l_green_wf pop_pescetarian_kg_co2e_excl_luc
    ## 1                 2.670088e+11                     3.257734e+08
    ## 2                 1.096397e+11                     1.694671e+08
    ## 3                 9.086135e+11                     6.159372e+08
    ## 4                 1.662703e+12                     1.548268e+09
    ## 5                 7.150327e+10                     8.713483e+07
    ## 6                 1.078218e+12                     1.510612e+09
    ## 7                 1.713963e+11                     2.913360e+08
    ## 8                 1.612186e+11                     2.076798e+08
    ## 9                 9.519863e+07                     1.084530e+05
    ## 10                2.710643e+11                     2.568366e+08
    ## 11                2.277411e+11                     4.717436e+08
    ## 12                5.415464e+09                     2.823418e+06
    ## 13                1.679689e+10                     7.186429e+06
    ## 14                4.867977e+06                     7.277260e+03
    ## 15                8.514635e+10                     7.132087e+07
    ## 16                4.029175e+10                     3.792786e+07
    ## 17                8.737594e+12                     6.705147e+09
    ## 18                7.285964e+09                     8.902949e+06
    ## 19                2.423232e+11                     1.708591e+08
    ## 20                1.039102e+11                     3.846326e+07
    ## 21                2.239651e+11                     1.175760e+08
    ## 22                3.268203e+11                     1.157112e+08
    ## 23                8.304271e+11                     8.961184e+08
    ## 24                1.092646e+10                     8.364365e+06
    ## 25                5.085896e+11                     5.795179e+08
    ## 26                1.429221e+13                     1.761672e+10
    ## 27                1.785450e+12                     1.627842e+09
    ## 28                5.651734e+11                     2.922268e+08
    ## 29                1.676670e+11                     2.268238e+08
    ## 30                1.889777e+11                     8.669979e+07
    ## 31                1.386386e+11                     1.489156e+08
    ## 32                3.289838e+10                     4.367841e+07
    ## 33                1.946543e+11                     2.600812e+08
    ## 34                1.308686e+11                     2.378449e+08
    ## 35                8.754915e+11                     4.912821e+08
    ## 36                9.909558e+11                     1.750322e+09
    ## 37                1.452090e+11                     1.214314e+08
    ## 38                3.519359e+10                     3.405957e+07
    ## 39                7.021392e+11                     4.012777e+08
    ## 40                1.699144e+08                     1.287380e+05
    ## 41                1.578694e+11                     2.703937e+08
    ## 42                1.010220e+12                     1.956473e+09
    ## 43                1.903379e+11                     1.756782e+08
    ## 44                1.371177e+12                     2.705168e+09
    ## 45                2.955781e+11                     1.316064e+08
    ## 46                5.656313e+11                     8.034503e+08
    ## 47                2.367999e+11                     1.440056e+08
    ## 48                2.929913e+11                     1.522652e+08
    ## 49                2.457435e+11                     1.906982e+08
    ## 50                7.248950e+09                     1.884846e+07
    ## 51                2.955113e+13                     2.637876e+10
    ## 52                6.302484e+12                     4.472377e+09
    ## 53                2.354813e+12                     1.668159e+09
    ## 54                8.640553e+10                     2.222786e+08
    ## 55                2.195320e+11                     2.559586e+08
    ## 56                1.302019e+12                     1.786121e+09
    ## 57                3.206746e+10                     2.501360e+07
    ## 58                1.649859e+12                     2.036545e+09
    ## 59                2.446509e+11                     2.098061e+08
    ## 60                7.225761e+11                     6.346218e+08
    ## 61                5.101647e+11                     4.208083e+08
    ## 62                1.241609e+11                     1.750493e+08
    ## 63                6.682704e+10                     6.265846e+07
    ## 64                5.740799e+10                     5.268701e+07
    ## 65                1.748330e+11                     1.535854e+08
    ## 66                9.697961e+10                     1.007334e+08
    ## 67                3.029241e+09                     6.153138e+06
    ## 68                1.938829e+11                     9.697562e+07
    ## 69                1.606641e+08                     4.537336e+04
    ## 70                7.010623e+11                     6.952879e+08
    ## 71                6.703828e+09                     1.025777e+07
    ## 72                1.390434e+11                     8.442451e+07
    ## 73                7.605795e+09                     9.260095e+06
    ## 74                2.427509e+10                     1.679434e+07
    ## 75                2.440874e+10                     2.556969e+07
    ## 76                3.724156e+12                     2.184846e+09
    ## 77                7.525889e+10                     6.338989e+07
    ## 78                2.063232e+10                     2.438714e+07
    ## 79                1.160112e+12                     4.908388e+08
    ## 80                4.747410e+10                     3.622147e+07
    ## 81                4.930589e+11                     2.875882e+08
    ## 82                4.419010e+11                     7.964131e+08
    ## 83                1.305061e+11                     2.444030e+08
    ## 84                2.310847e+11                     3.842384e+07
    ## 85                4.810447e+10                     5.411020e+07
    ## 86                1.404514e+11                     2.674903e+08
    ## 87                8.028877e+10                     1.117946e+08
    ## 88                1.283137e+12                     2.140310e+09
    ## 89                1.338072e+11                     1.017801e+08
    ## 90                1.267337e+12                     1.122679e+09
    ## 91                3.413832e+12                     2.439871e+09
    ## 92                9.800597e+11                     1.126765e+09
    ## 93                2.761207e+11                     3.280484e+08
    ## 94                4.631925e+11                     4.818249e+08
    ## 95                4.729869e+12                     3.606468e+09
    ## 96                9.884011e+10                     2.756205e+07
    ## 97                1.167543e+12                     1.000463e+09
    ## 98                1.991684e+11                     1.256234e+08
    ## 99                1.977884e+11                     1.601317e+08
    ## 100               9.113569e+10                     1.042858e+08
    ## 101               4.036702e+10                     5.539139e+07
    ## 102               6.237935e+11                     4.696114e+08
    ## 103               1.387948e+12                     1.800285e+09
    ## 104               1.309352e+12                     1.361663e+09
    ## 105               2.553632e+11                     1.680759e+08
    ## 106               1.988983e+11                     3.932119e+08
    ## 107               1.513343e+11                     2.946606e+08
    ## 108               8.327804e+11                     7.365621e+08
    ## 109               1.860236e+11                     1.275914e+08
    ## 110               2.064153e+12                     1.269172e+09
    ## 111               1.312794e+11                     3.768567e+07
    ## 112               2.636762e+11                     1.478330e+08
    ## 113               3.987393e+12                     3.280441e+09
    ## 114               2.750850e+09                     1.471478e+06
    ## 115               1.237305e+12                     8.672955e+08
    ## 116               9.115436e+11                     2.007290e+09
    ## 117               7.819132e+12                     1.055310e+10
    ## 118               9.211788e+07                     7.655572e+04
    ## 119               5.483926e+09                     5.380374e+06
    ## 120               5.056219e+07                     3.320857e+04
    ## 121               1.488121e+11                     6.254989e+07
    ## 122               1.660932e+11                     2.034917e+08
    ## 123               2.467165e+10                     2.856365e+07
    ##     pop_pescetarian_kg_co2e_total pop_pescetarian_kg_co2e_luc
    ## 1                    3.266989e+08                9.254924e+05
    ## 2                    1.714287e+08                1.961573e+06
    ## 3                    6.784448e+08                6.250758e+07
    ## 4                    2.229671e+09                6.814029e+08
    ## 5                    8.777256e+07                6.377273e+05
    ## 6                    1.524123e+09                1.351098e+07
    ## 7                    2.949607e+08                3.624745e+06
    ## 8                    2.122028e+08                4.522915e+06
    ## 9                    1.161655e+05                7.712519e+03
    ## 10                   2.620268e+08                5.190258e+06
    ## 11                   4.910988e+08                1.935526e+07
    ## 12                   3.078679e+06                2.552608e+05
    ## 13                   7.619785e+06                4.333561e+05
    ## 14                   7.281986e+03                4.725656e+00
    ## 15                   7.318925e+07                1.868387e+06
    ## 16                   3.842801e+07                5.001447e+05
    ## 17                   9.207784e+09                2.502636e+09
    ## 18                   9.140721e+06                2.377717e+05
    ## 19                   1.718225e+08                9.634240e+05
    ## 20                   3.981293e+07                1.349673e+06
    ## 21                   1.331662e+08                1.559018e+07
    ## 22                   1.245132e+08                8.802007e+06
    ## 23                   8.983408e+08                2.222401e+06
    ## 24                   8.429630e+06                6.526438e+04
    ## 25                   7.508873e+08                1.713694e+08
    ## 26                   1.839311e+10                7.763936e+08
    ## 27                   1.805967e+09                1.781249e+08
    ## 28                   3.004068e+08                8.180002e+06
    ## 29                   2.310602e+08                4.236401e+06
    ## 30                   8.788717e+07                1.187377e+06
    ## 31                   1.519310e+08                3.015387e+06
    ## 32                   4.445838e+07                7.799628e+05
    ## 33                   2.630062e+08                2.924951e+06
    ## 34                   2.596328e+08                2.178792e+07
    ## 35                   5.536937e+08                6.241159e+07
    ## 36                   1.788794e+09                3.847231e+07
    ## 37                   1.409534e+08                1.952205e+07
    ## 38                   3.449174e+07                4.321705e+05
    ## 39                   4.062366e+08                4.958970e+06
    ## 40                   1.295524e+05                8.144773e+02
    ## 41                   2.746959e+08                4.302136e+06
    ## 42                   2.030179e+09                7.370547e+07
    ## 43                   1.759543e+08                2.761528e+05
    ## 44                   2.794091e+09                8.892268e+07
    ## 45                   1.341934e+08                2.586987e+06
    ## 46                   8.218466e+08                1.839633e+07
    ## 47                   1.550071e+08                1.100147e+07
    ## 48                   1.896594e+08                3.739415e+07
    ## 49                   1.923723e+08                1.674167e+06
    ## 50                   1.953122e+07                6.827576e+05
    ## 51                   2.704837e+10                6.696073e+08
    ## 52                   4.659351e+09                1.869741e+08
    ## 53                   1.674404e+09                6.245052e+06
    ## 54                   2.296981e+08                7.419416e+06
    ## 55                   2.599289e+08                3.970349e+06
    ## 56                   1.847861e+09                6.173990e+07
    ## 57                   2.521651e+07                2.029099e+05
    ## 58                   2.084019e+09                4.747328e+07
    ## 59                   2.144121e+08                4.605948e+06
    ## 60                   6.423630e+08                7.741269e+06
    ## 61                   4.316155e+08                1.080721e+07
    ## 62                   1.787658e+08                3.716497e+06
    ## 63                   6.357408e+07                9.156232e+05
    ## 64                   5.331906e+07                6.320477e+05
    ## 65                   1.563672e+08                2.781783e+06
    ## 66                   1.024130e+08                1.679641e+06
    ## 67                   6.305561e+06                1.524233e+05
    ## 68                   9.869829e+07                1.722668e+06
    ## 69                   4.639250e+04                1.019145e+03
    ## 70                   7.789245e+08                8.363660e+07
    ## 71                   1.035643e+07                9.865307e+04
    ## 72                   8.480014e+07                3.756344e+05
    ## 73                   9.380500e+06                1.204049e+05
    ## 74                   1.702317e+07                2.288296e+05
    ## 75                   2.873611e+07                3.166422e+06
    ## 76                   2.218133e+09                3.328712e+07
    ## 77                   6.385458e+07                4.646844e+05
    ## 78                   2.464122e+07                2.540787e+05
    ## 79                   5.090142e+08                1.817538e+07
    ## 80                   3.638922e+07                1.677490e+05
    ## 81                   3.051124e+08                1.752419e+07
    ## 82                   8.324104e+08                3.599738e+07
    ## 83                   2.541845e+08                9.781579e+06
    ## 84                   3.854913e+07                1.252915e+05
    ## 85                   5.498955e+07                8.793500e+05
    ## 86                   2.796059e+08                1.211561e+07
    ## 87                   1.144530e+08                2.658328e+06
    ## 88                   2.177330e+09                3.701965e+07
    ## 89                   1.196941e+08                1.791402e+07
    ## 90                   1.338041e+09                2.153621e+08
    ## 91                   2.450387e+09                1.051575e+07
    ## 92                   1.151504e+09                2.473894e+07
    ## 93                   3.446528e+08                1.660434e+07
    ## 94                   4.901427e+08                8.317869e+06
    ## 95                   3.630352e+09                2.388364e+07
    ## 96                   2.884222e+07                1.280170e+06
    ## 97                   1.043441e+09                4.297759e+07
    ## 98                   1.286988e+08                3.075370e+06
    ## 99                   1.606982e+08                5.665383e+05
    ## 100                  1.052095e+08                9.237850e+05
    ## 101                  6.041857e+07                5.027180e+06
    ## 102                  4.812437e+08                1.163225e+07
    ## 103                  2.013787e+09                2.135028e+08
    ## 104                  1.446557e+09                8.489373e+07
    ## 105                  1.686279e+08                5.519969e+05
    ## 106                  4.060781e+08                1.286618e+07
    ## 107                  3.021856e+08                7.524981e+06
    ## 108                  8.141133e+08                7.755124e+07
    ## 109                  1.294609e+08                1.869466e+06
    ## 110                  1.420605e+09                1.514332e+08
    ## 111                  3.844048e+07                7.548079e+05
    ## 112                  1.646599e+08                1.682695e+07
    ## 113                  3.475236e+09                1.947946e+08
    ## 114                  1.601032e+06                1.295535e+05
    ## 115                  8.710274e+08                3.731937e+06
    ## 116                  2.095171e+09                8.788076e+07
    ## 117                  1.057576e+10                2.265937e+07
    ## 118                  9.448746e+04                1.793174e+04
    ## 119                  6.098956e+06                7.185815e+05
    ## 120                  3.405337e+04                8.447976e+02
    ## 121                  6.275586e+07                2.059747e+05
    ## 122                  2.115158e+08                8.024009e+06
    ## 123                  2.902924e+07                4.655910e+05
    ##     pop_pescetarian_l_blue_green_wf pop_pescetarian_l_blue_wf_total
    ## 1                      4.328894e+11                    1.900997e+11
    ## 2                      1.270570e+11                    2.124867e+10
    ## 3                      8.608780e+11                    1.578751e+11
    ## 4                      1.619645e+12                    1.292334e+11
    ## 5                      8.481298e+10                    1.604880e+10
    ## 6                      1.151979e+12                    2.363730e+11
    ## 7                      1.782072e+11                    2.348587e+10
    ## 8                      1.889203e+11                    3.815086e+10
    ## 9                      8.928543e+07                    1.096275e+07
    ## 10                     2.434441e+11                    1.743399e+10
    ## 11                     2.589527e+11                    5.005703e+10
    ## 12                     5.066174e+09                    3.384760e+08
    ## 13                     1.695602e+10                    1.324005e+09
    ## 14                     5.615665e+06                    1.223437e+06
    ## 15                     7.994264e+10                    3.273119e+09
    ## 16                     3.652574e+10                    5.404456e+09
    ## 17                     7.676553e+12                    5.398713e+11
    ## 18                     8.305502e+09                    1.124639e+09
    ## 19                     2.267350e+11                    1.500215e+10
    ## 20                     1.080034e+11                    4.035309e+09
    ## 21                     2.657531e+11                    3.978940e+10
    ## 22                     3.353619e+11                    1.810912e+10
    ## 23                     8.873177e+11                    1.305248e+11
    ## 24                     1.254775e+10                    1.250852e+09
    ## 25                     5.417811e+11                    1.191018e+11
    ## 26                     1.751065e+13                    3.859167e+12
    ## 27                     1.511079e+12                    1.369910e+11
    ## 28                     5.948135e+11                    7.063737e+10
    ## 29                     1.610802e+11                    1.841272e+10
    ## 30                     1.849718e+11                    6.886572e+09
    ## 31                     1.332924e+11                    7.373239e+09
    ## 32                     3.446452e+10                    7.564168e+09
    ## 33                     1.829617e+11                    2.206979e+10
    ## 34                     1.363847e+11                    1.524806e+10
    ## 35                     7.042342e+11                    1.119510e+11
    ## 36                     2.027403e+12                    1.144453e+12
    ## 37                     1.432428e+11                    8.889489e+09
    ## 38                     3.403207e+10                    1.520243e+09
    ## 39                     7.795726e+11                    7.896247e+10
    ## 40                     1.660562e+08                    1.361923e+07
    ## 41                     1.728083e+11                    2.040439e+10
    ## 42                     1.096260e+12                    1.698157e+11
    ## 43                     1.867354e+11                    1.675199e+10
    ## 44                     1.498727e+12                    2.259253e+11
    ## 45                     3.109618e+11                    2.954303e+10
    ## 46                     6.009636e+11                    9.861666e+10
    ## 47                     2.268226e+11                    1.139033e+10
    ## 48                     2.257008e+11                    1.615171e+10
    ## 49                     2.268778e+11                    1.669411e+10
    ## 50                     8.115254e+09                    1.670448e+09
    ## 51                     3.596834e+13                    7.652753e+12
    ## 52                     6.579214e+12                    9.205329e+11
    ## 53                     3.475186e+12                    1.564021e+12
    ## 54                     9.206848e+10                    1.319407e+10
    ## 55                     2.137005e+11                    5.470977e+10
    ## 56                     1.344946e+12                    1.755473e+11
    ## 57                     2.695361e+10                    2.167014e+09
    ## 58                     1.792782e+12                    2.871003e+11
    ## 59                     2.228813e+11                    5.728939e+10
    ## 60                     7.651859e+11                    1.529264e+11
    ## 61                     5.461945e+11                    4.156917e+10
    ## 62                     1.517795e+11                    3.613583e+10
    ## 63                     6.653177e+10                    1.726577e+10
    ## 64                     5.325088e+10                    3.496179e+09
    ## 65                     1.743948e+11                    4.548709e+10
    ## 66                     9.070688e+10                    3.628682e+09
    ## 67                     3.333460e+09                    4.985452e+08
    ## 68                     2.003438e+11                    3.070109e+10
    ## 69                     1.657474e+08                    1.022198e+07
    ## 70                     6.789560e+11                    1.258761e+11
    ## 71                     8.456305e+09                    2.287329e+09
    ## 72                     1.357432e+11                    1.288406e+10
    ## 73                     8.356056e+09                    1.325032e+09
    ## 74                     2.212051e+10                    3.036490e+09
    ## 75                     2.323260e+10                    3.760364e+09
    ## 76                     3.689837e+12                    5.203482e+11
    ## 77                     6.618595e+10                    9.587029e+09
    ## 78                     2.065169e+10                    1.023999e+09
    ## 79                     1.040567e+12                    1.823814e+11
    ## 80                     4.547473e+10                    4.380797e+09
    ## 81                     5.368057e+11                    8.165321e+10
    ## 82                     4.497635e+11                    4.870666e+10
    ## 83                     1.462274e+11                    2.302531e+10
    ## 84                     2.336309e+11                    5.470779e+09
    ## 85                     4.421227e+10                    4.256025e+09
    ## 86                     1.586791e+11                    2.433582e+10
    ## 87                     1.075412e+11                    4.796532e+10
    ## 88                     1.927550e+12                    8.025940e+11
    ## 89                     1.159734e+11                    1.054225e+10
    ## 90                     1.340899e+12                    3.168172e+11
    ## 91                     3.364845e+12                    4.702823e+11
    ## 92                     8.990700e+11                    4.518166e+10
    ## 93                     3.140486e+11                    6.487079e+10
    ## 94                     4.641040e+11                    2.821310e+10
    ## 95                     4.557261e+12                    3.244980e+11
    ## 96                     1.036912e+11                    7.215467e+09
    ## 97                     1.251872e+12                    4.844251e+11
    ## 98                     1.851396e+11                    2.374137e+10
    ## 99                     1.948137e+11                    9.442601e+09
    ## 100                    8.532974e+10                    7.249666e+09
    ## 101                    3.891908e+10                    3.852849e+09
    ## 102                    5.359325e+11                    8.984986e+10
    ## 103                    1.356573e+12                    1.675711e+11
    ## 104                    1.425047e+12                    2.694625e+11
    ## 105                    2.836894e+11                    5.327176e+10
    ## 106                    2.258914e+11                    3.349475e+10
    ## 107                    1.707222e+11                    2.461715e+10
    ## 108                    8.720925e+11                    1.272732e+11
    ## 109                    1.939100e+11                    1.523285e+10
    ## 110                    2.263198e+12                    4.533609e+11
    ## 111                    1.338671e+11                    4.817804e+09
    ## 112                    2.699417e+11                    3.405714e+10
    ## 113                    3.831314e+12                    5.581691e+11
    ## 114                    2.943091e+09                    2.538576e+08
    ## 115                    1.086007e+12                    5.540899e+10
    ## 116                    1.000089e+12                    1.920284e+11
    ## 117                    9.116765e+12                    1.863404e+12
    ## 118                    9.469340e+07                    3.907992e+06
    ## 119                    4.402870e+09                    4.401852e+08
    ## 120                    5.797726e+07                    1.659921e+07
    ## 121                    1.303663e+11                    1.508920e+10
    ## 122                    1.878897e+11                    4.641985e+10
    ## 123                    2.761802e+10                    6.012987e+09
    ##     pop_pescetarian_l_green_wf pop_lacto_ovo_vegetarian_kg_co2e_excl_luc
    ## 1                 2.427897e+11                              8.044977e+08
    ## 2                 1.058083e+11                              1.676988e+08
    ## 3                 7.030029e+11                              1.070318e+09
    ## 4                 1.490411e+12                              2.136006e+09
    ## 5                 6.876419e+10                              1.045669e+08
    ## 6                 9.156058e+11                              2.225877e+09
    ## 7                 1.547213e+11                              3.558300e+08
    ## 8                 1.507694e+11                              2.627335e+08
    ## 9                 7.832268e+07                              1.848698e+05
    ## 10                2.260101e+11                              3.603263e+08
    ## 11                2.088957e+11                              5.691567e+08
    ## 12                4.727698e+09                              3.845240e+06
    ## 13                1.563201e+10                              1.086968e+07
    ## 14                4.392228e+06                              8.923840e+03
    ## 15                7.666952e+10                              8.703767e+07
    ## 16                3.112129e+10                              7.980596e+07
    ## 17                7.136682e+12                              1.228946e+10
    ## 18                7.180863e+09                              1.408029e+07
    ## 19                2.117328e+11                              2.412394e+08
    ## 20                1.039681e+11                              4.254186e+07
    ## 21                2.259637e+11                              9.718440e+07
    ## 22                3.172528e+11                              1.381006e+08
    ## 23                7.567929e+11                              1.098652e+09
    ## 24                1.129690e+10                              1.294401e+07
    ## 25                4.226793e+11                              7.240985e+08
    ## 26                1.365148e+13                              1.828421e+10
    ## 27                1.374088e+12                              3.735604e+09
    ## 28                5.241761e+11                              4.034032e+08
    ## 29                1.426675e+11                              4.023644e+08
    ## 30                1.780852e+11                              1.609336e+08
    ## 31                1.259191e+11                              1.967187e+08
    ## 32                2.690035e+10                              6.486608e+07
    ## 33                1.608919e+11                              3.284021e+08
    ## 34                1.211367e+11                              2.233083e+08
    ## 35                5.922832e+11                              8.298710e+08
    ## 36                8.829506e+11                              2.672420e+09
    ## 37                1.343533e+11                              1.598619e+08
    ## 38                3.251182e+10                              3.276575e+07
    ## 39                7.006102e+11                              4.870324e+08
    ## 40                1.524369e+08                              2.024298e+05
    ## 41                1.524039e+11                              2.470025e+08
    ## 42                9.264443e+11                              2.280482e+09
    ## 43                1.699834e+11                              3.088841e+08
    ## 44                1.272802e+12                              3.105896e+09
    ## 45                2.814188e+11                              1.698342e+08
    ## 46                5.023469e+11                              9.942725e+08
    ## 47                2.154323e+11                              2.204169e+08
    ## 48                2.095490e+11                              2.892317e+08
    ## 49                2.101837e+11                              2.472468e+08
    ## 50                6.444807e+09                              1.443047e+07
    ## 51                2.831558e+13                              4.407985e+10
    ## 52                5.658682e+12                              6.030707e+09
    ## 53                1.911165e+12                              2.531385e+09
    ## 54                7.887441e+10                              2.574231e+08
    ## 55                1.589907e+11                              3.500612e+08
    ## 56                1.169399e+12                              2.489697e+09
    ## 57                2.478660e+10                              4.380205e+07
    ## 58                1.505681e+12                              1.907359e+09
    ## 59                1.655919e+11                              4.732355e+08
    ## 60                6.122595e+11                              1.071499e+09
    ## 61                5.046254e+11                              6.500015e+08
    ## 62                1.156436e+11                              2.446184e+08
    ## 63                4.926600e+10                              9.301626e+07
    ## 64                4.975470e+10                              6.114577e+07
    ## 65                1.289078e+11                              2.506910e+08
    ## 66                8.707820e+10                              9.269792e+07
    ## 67                2.834915e+09                              6.771136e+06
    ## 68                1.696427e+11                              3.298944e+08
    ## 69                1.555255e+08                              4.625015e+04
    ## 70                5.530799e+11                              1.099186e+09
    ## 71                6.168976e+09                              1.671070e+07
    ## 72                1.228591e+11                              1.663511e+08
    ## 73                7.031024e+09                              9.716964e+06
    ## 74                1.908402e+10                              2.915871e+07
    ## 75                1.947224e+10                              3.737652e+07
    ## 76                3.169488e+12                              2.828186e+09
    ## 77                5.659892e+10                              1.147872e+08
    ## 78                1.962769e+10                              2.639316e+07
    ## 79                8.581853e+11                              8.339708e+08
    ## 80                4.109393e+10                              7.058362e+07
    ## 81                4.551525e+11                              6.155711e+08
    ## 82                4.010568e+11                              8.751499e+08
    ## 83                1.232020e+11                              3.761004e+08
    ## 84                2.281601e+11                              3.805731e+07
    ## 85                3.995625e+10                              7.905770e+07
    ## 86                1.343433e+11                              2.888293e+08
    ## 87                5.957592e+10                              1.682704e+08
    ## 88                1.124956e+12                              3.461121e+09
    ## 89                1.054312e+11                              1.612745e+08
    ## 90                1.024082e+12                              1.868142e+09
    ## 91                2.894563e+12                              3.677798e+09
    ## 92                8.538884e+11                              1.632476e+09
    ## 93                2.491778e+11                              3.453158e+08
    ## 94                4.358909e+11                              5.832157e+08
    ## 95                4.232763e+12                              4.427296e+09
    ## 96                9.647572e+10                              3.092052e+07
    ## 97                7.674466e+11                              1.712853e+09
    ## 98                1.613983e+11                              3.547162e+08
    ## 99                1.853711e+11                              1.877086e+08
    ## 100               7.808007e+10                              1.327032e+08
    ## 101               3.506623e+10                              6.886057e+07
    ## 102               4.460827e+11                              9.856304e+08
    ## 103               1.189002e+12                              1.597812e+09
    ## 104               1.155585e+12                              1.650597e+09
    ## 105               2.304177e+11                              3.040091e+08
    ## 106               1.923967e+11                              4.220422e+08
    ## 107               1.461051e+11                              3.570961e+08
    ## 108               7.448194e+11                              7.098715e+08
    ## 109               1.786772e+11                              2.280026e+08
    ## 110               1.809837e+12                              1.709021e+09
    ## 111               1.290493e+11                              3.202944e+07
    ## 112               2.358845e+11                              2.094891e+08
    ## 113               3.273145e+12                              5.047966e+09
    ## 114               2.689234e+09                              2.491795e+06
    ## 115               1.030598e+12                              1.117053e+09
    ## 116               8.080611e+11                              2.575516e+09
    ## 117               7.253361e+12                              1.436066e+10
    ## 118               9.078541e+07                              8.929271e+04
    ## 119               3.962685e+09                              1.273338e+07
    ## 120               4.137805e+07                              6.505636e+04
    ## 121               1.152771e+11                              1.954391e+08
    ## 122               1.414699e+11                              2.428281e+08
    ## 123               2.160503e+10                              3.458820e+07
    ##     pop_lacto_ovo_vegetarian_kg_co2e_total pop_lacto_ovo_vegetarian_kg_co2e_luc
    ## 1                             8.073256e+08                         2.827941e+06
    ## 2                             1.696604e+08                         1.961573e+06
    ## 3                             1.185131e+09                         1.148126e+08
    ## 4                             3.009033e+09                         8.730265e+08
    ## 5                             1.054310e+08                         8.640963e+05
    ## 6                             2.245346e+09                         1.946953e+07
    ## 7                             3.621514e+08                         6.321334e+06
    ## 8                             2.687387e+08                         6.005171e+06
    ## 9                             1.948054e+05                         9.935640e+03
    ## 10                            3.716722e+08                         1.134591e+07
    ## 11                            6.035450e+08                         3.438823e+07
    ## 12                            4.135086e+06                         2.898464e+05
    ## 13                            1.289019e+07                         2.020503e+06
    ## 14                            8.936322e+03                         1.248243e+01
    ## 15                            8.964187e+07                         2.604204e+06
    ## 16                            8.052239e+07                         7.164280e+05
    ## 17                            1.757270e+10                         5.283236e+09
    ## 18                            1.484643e+07                         7.661424e+05
    ## 19                            2.430528e+08                         1.813401e+06
    ## 20                            4.412891e+07                         1.587048e+06
    ## 21                            1.423878e+08                         4.520344e+07
    ## 22                            1.492743e+08                         1.117366e+07
    ## 23                            1.102298e+09                         3.646508e+06
    ## 24                            1.309468e+07                         1.506713e+05
    ## 25                            1.112779e+09                         3.886804e+08
    ## 26                            1.954060e+10                         1.256396e+09
    ## 27                            4.047422e+09                         3.118178e+08
    ## 28                            4.215468e+08                         1.814368e+07
    ## 29                            4.080741e+08                         5.709753e+06
    ## 30                            1.729122e+08                         1.197861e+07
    ## 31                            2.017445e+08                         5.025754e+06
    ## 32                            6.694045e+07                         2.074371e+06
    ## 33                            3.339885e+08                         5.586373e+06
    ## 34                            2.450962e+08                         2.178792e+07
    ## 35                            9.300051e+08                         1.001342e+08
    ## 36                            2.721289e+09                         4.886869e+07
    ## 37                            1.905992e+08                         3.073731e+07
    ## 38                            3.323144e+07                         4.656919e+05
    ## 39                            4.921985e+08                         5.166086e+06
    ## 40                            2.039552e+05                         1.525382e+03
    ## 41                            2.513046e+08                         4.302136e+06
    ## 42                            2.389583e+09                         1.091002e+08
    ## 43                            3.094490e+08                         5.648920e+05
    ## 44                            3.226832e+09                         1.209360e+08
    ## 45                            1.808150e+08                         1.098082e+07
    ## 46                            1.028803e+09                         3.453012e+07
    ## 47                            2.341753e+08                         1.375839e+07
    ## 48                            3.888154e+08                         9.958371e+07
    ## 49                            2.502236e+08                         2.976752e+06
    ## 50                            1.511323e+07                         6.827576e+05
    ## 51                            4.493747e+10                         8.576173e+08
    ## 52                            6.595548e+09                         5.648406e+08
    ## 53                            2.551775e+09                         2.039029e+07
    ## 54                            2.692109e+08                         1.178778e+07
    ## 55                            3.570793e+08                         7.018074e+06
    ## 56                            2.601898e+09                         1.122008e+08
    ## 57                            4.461266e+07                         8.106133e+05
    ## 58                            1.990751e+09                         8.339240e+07
    ## 59                            4.835272e+08                         1.029168e+07
    ## 60                            1.087277e+09                         1.577781e+07
    ## 61                            6.622942e+08                         1.229268e+07
    ## 62                            2.504553e+08                         5.836897e+06
    ## 63                            9.510712e+07                         2.090858e+06
    ## 64                            6.224764e+07                         1.101872e+06
    ## 65                            2.562851e+08                         5.594067e+06
    ## 66                            9.437756e+07                         1.679641e+06
    ## 67                            7.000735e+06                         2.295991e+05
    ## 68                            3.355226e+08                         5.628219e+06
    ## 69                            4.730581e+04                         1.055660e+03
    ## 70                            1.492535e+09                         3.933493e+08
    ## 71                            1.705064e+07                         3.399314e+05
    ## 72                            1.668874e+08                         5.363220e+05
    ## 73                            9.874957e+06                         1.579926e+05
    ## 74                            2.949566e+07                         3.369494e+05
    ## 75                            4.082872e+07                         3.452203e+06
    ## 76                            2.875000e+09                         4.681359e+07
    ## 77                            1.161694e+08                         1.382283e+06
    ## 78                            2.669480e+07                         3.016413e+05
    ## 79                            8.619538e+08                         2.798302e+07
    ## 80                            7.110849e+07                         5.248762e+05
    ## 81                            6.353305e+08                         1.975947e+07
    ## 82                            9.187865e+08                         4.363662e+07
    ## 83                            3.984313e+08                         2.233081e+07
    ## 84                            3.818260e+07                         1.252915e+05
    ## 85                            8.076698e+07                         1.709285e+06
    ## 86                            3.099452e+08                         2.111593e+07
    ## 87                            1.741019e+08                         5.831478e+06
    ## 88                            3.506903e+09                         4.578166e+07
    ## 89                            1.919049e+08                         3.063048e+07
    ## 90                            2.282598e+09                         4.144562e+08
    ## 91                            3.742784e+09                         6.498621e+07
    ## 92                            1.681140e+09                         4.866467e+07
    ## 93                            3.752763e+08                         2.996053e+07
    ## 94                            5.950844e+08                         1.186872e+07
    ## 95                            4.466939e+09                         3.964386e+07
    ## 96                            3.228201e+07                         1.361489e+06
    ## 97                            1.816331e+09                         1.034782e+08
    ## 98                            3.629263e+08                         8.210056e+06
    ## 99                            1.884770e+08                         7.684596e+05
    ## 100                           1.344241e+08                         1.720858e+06
    ## 101                           7.512616e+07                         6.265587e+06
    ## 102                           1.007663e+09                         2.203228e+07
    ## 103                           1.985608e+09                         3.877957e+08
    ## 104                           1.847721e+09                         1.971247e+08
    ## 105                           3.062495e+08                         2.240395e+06
    ## 106                           4.396719e+08                         1.762972e+07
    ## 107                           3.694112e+08                         1.231514e+07
    ## 108                           8.216373e+08                         1.117658e+08
    ## 109                           2.303551e+08                         2.352562e+06
    ## 110                           2.237139e+09                         5.281180e+08
    ## 111                           3.289548e+07                         8.660317e+05
    ## 112                           2.276202e+08                         1.813106e+07
    ## 113                           5.265831e+09                         2.178654e+08
    ## 114                           2.715170e+06                         2.233750e+05
    ## 115                           1.122840e+09                         5.787058e+06
    ## 116                           2.761787e+09                         1.862715e+08
    ## 117                           1.440259e+10                         4.193320e+07
    ## 118                           1.069188e+05                         1.762606e+04
    ## 119                           1.406409e+07                         1.330707e+06
    ## 120                           6.640786e+04                         1.351491e+03
    ## 121                           1.962649e+08                         8.258222e+05
    ## 122                           2.591700e+08                         1.634185e+07
    ## 123                           3.618568e+07                         1.597485e+06
    ##     pop_lacto_ovo_vegetarian_l_blue_green_wf
    ## 1                               4.850221e+11
    ## 2                               1.269387e+11
    ## 3                               1.041320e+12
    ## 4                               1.735573e+12
    ## 5                               8.593670e+10
    ## 6                               1.230735e+12
    ## 7                               1.830963e+11
    ## 8                               1.973365e+11
    ## 9                               1.049734e+08
    ## 10                              2.841543e+11
    ## 11                              2.570023e+11
    ## 12                              5.520195e+09
    ## 13                              1.772271e+10
    ## 14                              6.282082e+06
    ## 15                              8.441980e+10
    ## 16                              4.629810e+10
    ## 17                              8.621536e+12
    ## 18                              8.714872e+09
    ## 19                              2.807525e+11
    ## 20                              1.087278e+11
    ## 21                              2.441239e+11
    ## 22                              3.393935e+11
    ## 23                              9.460908e+11
    ## 24                              1.386519e+10
    ## 25                              6.247076e+11
    ## 26                              1.705305e+13
    ## 27                              1.894452e+12
    ## 28                              6.043361e+11
    ## 29                              1.767031e+11
    ## 30                              2.278559e+11
    ## 31                              1.583804e+11
    ## 32                              4.013256e+10
    ## 33                              2.027462e+11
    ## 34                              1.329611e+11
    ## 35                              1.048340e+12
    ## 36                              2.123935e+12
    ## 37                              1.510412e+11
    ## 38                              3.417433e+10
    ## 39                              7.882676e+11
    ## 40                              1.771183e+08
    ## 41                              1.594503e+11
    ## 42                              1.169672e+12
    ## 43                              2.345349e+11
    ## 44                              1.535255e+12
    ## 45                              3.600396e+11
    ## 46                              6.852345e+11
    ## 47                              2.410314e+11
    ## 48                              2.511980e+11
    ## 49                              2.885982e+11
    ## 50                              7.172703e+09
    ## 51                              3.694414e+13
    ## 52                              7.360765e+12
    ## 53                              3.785156e+12
    ## 54                              9.737854e+10
    ## 55                              2.392169e+11
    ## 56                              1.529418e+12
    ## 57                              4.074864e+10
    ## 58                              1.904072e+12
    ## 59                              2.903929e+11
    ## 60                              8.768843e+11
    ## 61                              5.555991e+11
    ## 62                              1.660296e+11
    ## 63                              7.556122e+10
    ## 64                              6.243705e+10
    ## 65                              2.231890e+11
    ## 66                              9.137226e+10
    ## 67                              3.357282e+09
    ## 68                              2.213885e+11
    ## 69                              1.605601e+08
    ## 70                              7.594407e+11
    ## 71                              1.113092e+10
    ## 72                              1.505235e+11
    ## 73                              8.530674e+09
    ## 74                              2.922101e+10
    ## 75                              2.613287e+10
    ## 76                              4.342908e+12
    ## 77                              8.479992e+10
    ## 78                              2.112478e+10
    ## 79                              1.414375e+12
    ## 80                              5.018414e+10
    ## 81                              5.671169e+11
    ## 82                              4.715632e+11
    ## 83                              1.575007e+11
    ## 84                              2.334448e+11
    ## 85                              5.123201e+10
    ## 86                              1.591611e+11
    ## 87                              1.229933e+11
    ## 88                              2.080288e+12
    ## 89                              1.264294e+11
    ## 90                              1.739737e+12
    ## 91                              3.865815e+12
    ## 92                              1.070412e+12
    ## 93                              3.714660e+11
    ## 94                              4.973641e+11
    ## 95                              4.991148e+12
    ## 96                              1.044483e+11
    ## 97                              1.395021e+12
    ## 98                              2.897558e+11
    ## 99                              2.010123e+11
    ## 100                             9.936016e+10
    ## 101                             4.323540e+10
    ## 102                             6.897031e+11
    ## 103                             1.661644e+12
    ## 104                             1.768504e+12
    ## 105                             3.058274e+11
    ## 106                             2.238020e+11
    ## 107                             1.611878e+11
    ## 108                             8.904587e+11
    ## 109                             2.011754e+11
    ## 110                             2.427427e+12
    ## 111                             1.445741e+11
    ## 112                             3.096810e+11
    ## 113                             4.361601e+12
    ## 114                             3.021054e+09
    ## 115                             1.283499e+12
    ## 116                             1.058894e+12
    ## 117                             9.160131e+12
    ## 118                             9.796204e+07
    ## 119                             6.247805e+09
    ## 120                             6.564446e+07
    ## 121                             1.513652e+11
    ## 122                             1.900196e+11
    ## 123                             2.783778e+10
    ##     pop_lacto_ovo_vegetarian_l_blue_wf_total
    ## 1                               2.203617e+11
    ## 2                               2.104362e+10
    ## 3                               1.595920e+11
    ## 4                               1.275525e+11
    ## 5                               1.494508e+10
    ## 6                               1.676461e+11
    ## 7                               1.232525e+10
    ## 8                               3.894284e+10
    ## 9                               1.006102e+07
    ## 10                              8.564677e+09
    ## 11                              2.500891e+10
    ## 12                              2.984976e+08
    ## 13                              9.134315e+08
    ## 14                              8.711104e+05
    ## 15                              2.918387e+09
    ## 16                              5.208019e+09
    ## 17                              3.004674e+11
    ## 18                              8.766570e+08
    ## 19                              1.046315e+10
    ## 20                              3.971420e+09
    ## 21                              1.335540e+10
    ## 22                              1.298413e+10
    ## 23                              8.450144e+10
    ## 24                              1.307740e+09
    ## 25                              1.143112e+11
    ## 26                              2.634045e+12
    ## 27                              8.257918e+10
    ## 28                              3.039063e+10
    ## 29                              1.234432e+10
    ## 30                              7.655430e+09
    ## 31                              6.069434e+09
    ## 32                              5.591091e+09
    ## 33                              9.875352e+09
    ## 34                              1.184947e+10
    ## 35                              6.558299e+10
    ## 36                              1.160083e+12
    ## 37                              8.168394e+09
    ## 38                              1.184103e+09
    ## 39                              7.957709e+10
    ## 40                              1.170169e+07
    ## 41                              1.096415e+10
    ## 42                              1.170991e+11
    ## 43                              2.058837e+10
    ## 44                              1.480325e+11
    ## 45                              1.885443e+10
    ## 46                              1.008843e+11
    ## 47                              1.088120e+10
    ## 48                              1.161597e+10
    ## 49                              7.130619e+09
    ## 50                              8.766124e+08
    ## 51                              7.048928e+12
    ## 52                              4.375520e+11
    ## 53                              1.556814e+12
    ## 54                              9.808165e+09
    ## 55                              4.431313e+10
    ## 56                              1.481340e+11
    ## 57                              2.169537e+09
    ## 58                              1.981145e+11
    ## 59                              6.799207e+10
    ## 60                              1.529512e+11
    ## 61                              3.127602e+10
    ## 62                              4.028097e+10
    ## 63                              1.553570e+10
    ## 64                              2.403900e+09
    ## 65                              5.234342e+10
    ## 66                              2.595183e+09
    ## 67                              2.984142e+08
    ## 68                              2.666557e+10
    ## 69                              6.159070e+06
    ## 70                              6.919816e+10
    ## 71                              2.444744e+09
    ## 72                              1.058363e+10
    ## 73                              1.222796e+09
    ## 74                              2.810055e+09
    ## 75                              3.334987e+09
    ## 76                              5.139949e+11
    ## 77                              7.365419e+09
    ## 78                              8.619964e+08
    ## 79                              1.980173e+11
    ## 80                              4.162619e+09
    ## 81                              7.380364e+10
    ## 82                              4.303432e+10
    ## 83                              1.871746e+10
    ## 84                              5.145745e+09
    ## 85                              3.870664e+09
    ## 86                              1.416421e+10
    ## 87                              4.494236e+10
    ## 88                              7.886545e+11
    ## 89                              9.066055e+09
    ## 90                              2.918228e+11
    ## 91                              2.165536e+11
    ## 92                              3.426692e+10
    ## 93                              6.998014e+10
    ## 94                              1.996300e+10
    ## 95                              2.347699e+11
    ## 96                              6.777300e+09
    ## 97                              4.308447e+11
    ## 98                              1.849750e+10
    ## 99                              6.696045e+09
    ## 100                             4.604133e+09
    ## 101                             2.924567e+09
    ## 102                             8.641940e+10
    ## 103                             1.437172e+11
    ## 104                             2.691691e+11
    ## 105                             4.227685e+10
    ## 106                             1.716034e+10
    ## 107                             1.196211e+10
    ## 108                             8.746134e+10
    ## 109                             1.226813e+10
    ## 110                             3.220336e+11
    ## 111                             4.109387e+09
    ## 112                             3.992773e+10
    ## 113                             5.745810e+11
    ## 114                             1.148641e+08
    ## 115                             4.439627e+10
    ## 116                             1.112934e+11
    ## 117                             1.097289e+12
    ## 118                             3.583325e+06
    ## 119                             3.382583e+08
    ## 120                             1.848042e+07
    ## 121                             1.166356e+10
    ## 122                             3.122068e+10
    ## 123                             3.404170e+09
    ##     pop_lacto_ovo_vegetarian_l_green_wf pop_X2.3_vegan_kg_co2e_excl_luc
    ## 1                          2.646604e+11                    2.793043e+08
    ## 2                          1.058951e+11                    1.099288e+08
    ## 3                          8.817283e+11                    5.263175e+08
    ## 4                          1.608020e+12                    2.403876e+09
    ## 5                          7.099163e+10                    6.393432e+07
    ## 6                          1.063089e+12                    1.479509e+09
    ## 7                          1.707711e+11                    2.132097e+08
    ## 8                          1.583937e+11                    1.390588e+08
    ## 9                          9.491237e+07                    8.376821e+04
    ## 10                         2.755896e+11                    1.861296e+08
    ## 11                         2.319934e+11                    3.323679e+08
    ## 12                         5.221698e+09                    2.885983e+06
    ## 13                         1.680928e+10                    6.270128e+06
    ## 14                         5.410972e+06                    7.536347e+03
    ## 15                         8.150142e+10                    4.688890e+07
    ## 16                         4.109008e+10                    3.635964e+07
    ## 17                         8.321068e+12                    7.988364e+09
    ## 18                         7.838215e+09                    5.880986e+06
    ## 19                         2.702893e+11                    1.165318e+08
    ## 20                         1.047563e+11                    4.108306e+07
    ## 21                         2.307685e+11                    1.026800e+08
    ## 22                         3.264093e+11                    1.343193e+08
    ## 23                         8.615893e+11                    8.124270e+08
    ## 24                         1.255745e+10                    6.620182e+06
    ## 25                         5.103964e+11                    6.414408e+08
    ## 26                         1.441900e+13                    1.748169e+10
    ## 27                         1.811872e+12                    1.658654e+09
    ## 28                         5.739455e+11                    2.227315e+08
    ## 29                         1.643588e+11                    1.646275e+08
    ## 30                         2.202005e+11                    6.365814e+07
    ## 31                         1.523109e+11                    1.033031e+08
    ## 32                         3.454147e+10                    2.645125e+07
    ## 33                         1.928708e+11                    1.826764e+08
    ## 34                         1.211116e+11                    1.975491e+08
    ## 35                         9.827572e+11                    4.501605e+08
    ## 36                         9.638512e+11                    1.559564e+09
    ## 37                         1.428728e+11                    9.736642e+07
    ## 38                         3.299022e+10                    2.494268e+07
    ## 39                         7.086905e+11                    2.858296e+08
    ## 40                         1.654166e+08                    1.161843e+05
    ## 41                         1.484862e+11                    1.816870e+08
    ## 42                         1.052572e+12                    1.459144e+09
    ## 43                         2.139465e+11                    1.085217e+08
    ## 44                         1.387223e+12                    1.841488e+09
    ## 45                         3.411852e+11                    1.075946e+08
    ## 46                         5.843502e+11                    5.065507e+08
    ## 47                         2.301502e+11                    1.425044e+08
    ## 48                         2.395820e+11                    1.529939e+08
    ## 49                         2.814676e+11                    1.485209e+08
    ## 50                         6.296090e+09                    1.387100e+07
    ## 51                         2.989522e+13                    2.189588e+10
    ## 52                         6.923213e+12                    3.294620e+09
    ## 53                         2.228342e+12                    1.389224e+09
    ## 54                         8.757038e+10                    1.653064e+08
    ## 55                         1.949038e+11                    2.235806e+08
    ## 56                         1.381284e+12                    1.167247e+09
    ## 57                         3.857911e+10                    1.816653e+07
    ## 58                         1.705958e+12                    1.779714e+09
    ## 59                         2.224009e+11                    1.964365e+08
    ## 60                         7.239332e+11                    5.036136e+08
    ## 61                         5.243231e+11                    3.199929e+08
    ## 62                         1.257487e+11                    1.395368e+08
    ## 63                         6.002552e+10                    6.866748e+07
    ## 64                         6.003315e+10                    3.549822e+07
    ## 65                         1.708456e+11                    1.406822e+08
    ## 66                         8.877708e+10                    6.370386e+07
    ## 67                         3.058868e+09                    5.224720e+06
    ## 68                         1.947229e+11                    1.207039e+08
    ## 69                         1.544010e+08                    5.163094e+04
    ## 70                         6.902426e+11                    4.621175e+08
    ## 71                         8.686179e+09                    6.737382e+06
    ## 72                         1.399398e+11                    6.728452e+07
    ## 73                         7.307878e+09                    8.562713e+06
    ## 74                         2.641096e+10                    1.454322e+07
    ## 75                         2.279789e+10                    1.958117e+07
    ## 76                         3.828914e+12                    2.253953e+09
    ## 77                         7.743451e+10                    3.871142e+07
    ## 78                         2.026278e+10                    1.631524e+07
    ## 79                         1.216358e+12                    4.261031e+08
    ## 80                         4.602152e+10                    3.049227e+07
    ## 81                         4.933133e+11                    3.231913e+08
    ## 82                         4.285289e+11                    5.745277e+08
    ## 83                         1.387832e+11                    2.104986e+08
    ## 84                         2.282991e+11                    3.655855e+07
    ## 85                         4.736134e+10                    3.986318e+07
    ## 86                         1.449969e+11                    1.946593e+08
    ## 87                         7.805098e+10                    7.948815e+07
    ## 88                         1.291633e+12                    1.496813e+09
    ## 89                         1.173634e+11                    1.095024e+08
    ## 90                         1.447914e+12                    9.516510e+08
    ## 91                         3.649261e+12                    1.919801e+09
    ## 92                         1.036145e+12                    7.422019e+08
    ## 93                         3.014859e+11                    2.537317e+08
    ## 94                         4.774011e+11                    3.131107e+08
    ## 95                         4.756378e+12                    2.816756e+09
    ## 96                         9.767105e+10                    2.639140e+07
    ## 97                         9.641759e+11                    8.135218e+08
    ## 98                         2.712583e+11                    1.206639e+08
    ## 99                         1.943162e+11                    1.269024e+08
    ## 100                        9.475602e+10                    7.861314e+07
    ## 101                        4.031084e+10                    4.021047e+07
    ## 102                        6.032837e+11                    4.676545e+08
    ## 103                        1.517927e+12                    1.606381e+09
    ## 104                        1.499335e+12                    1.006392e+09
    ## 105                        2.635505e+11                    1.337685e+08
    ## 106                        2.066417e+11                    2.888826e+08
    ## 107                        1.492257e+11                    2.194921e+08
    ## 108                        8.029974e+11                    6.466999e+08
    ## 109                        1.889073e+11                    9.991946e+07
    ## 110                        2.105393e+12                    1.058584e+09
    ## 111                        1.404647e+11                    3.247671e+07
    ## 112                        2.697533e+11                    1.192986e+08
    ## 113                        3.787020e+12                    2.585064e+09
    ## 114                        2.906190e+09                    1.137118e+06
    ## 115                        1.239103e+12                    6.559898e+08
    ## 116                        9.476009e+11                    1.521844e+09
    ## 117                        8.062842e+12                    8.856382e+09
    ## 118                        9.437871e+07                    7.696046e+04
    ## 119                        5.909547e+09                    5.892333e+06
    ## 120                        4.716405e+07                    3.447881e+04
    ## 121                        1.397016e+11                    8.325546e+07
    ## 122                        1.587989e+11                    1.906545e+08
    ## 123                        2.443361e+10                    2.563664e+07
    ##     pop_X2.3_vegan_kg_co2e_total pop_X2.3_vegan_kg_co2e_luc
    ## 1                   2.802368e+08               9.325558e+05
    ## 2                   1.123305e+08               2.401717e+06
    ## 3                   5.949539e+08               6.863636e+07
    ## 4                   3.189712e+09               7.858355e+08
    ## 5                   6.507124e+07               1.136919e+06
    ## 6                   1.537260e+09               5.775070e+07
    ## 7                   2.196520e+08               6.442230e+06
    ## 8                   1.433964e+08               4.337603e+06
    ## 9                   9.558181e+04               1.181361e+04
    ## 10                  1.959835e+08               9.853896e+06
    ## 11                  3.625780e+08               3.021009e+07
    ## 12                  3.311857e+06               4.258738e+05
    ## 13                  7.407359e+06               1.137231e+06
    ## 14                  7.711681e+03               1.753341e+02
    ## 15                  4.858380e+07               1.694896e+06
    ## 16                  3.708225e+07               7.226150e+05
    ## 17                  1.217754e+10               4.189178e+09
    ## 18                  6.099615e+06               2.186294e+05
    ## 19                  1.204162e+08               3.884405e+06
    ## 20                  4.267832e+07               1.595258e+06
    ## 21                  1.377243e+08               3.504430e+07
    ## 22                  1.462024e+08               1.188313e+07
    ## 23                  8.177836e+08               5.356597e+06
    ## 24                  6.888174e+06               2.679925e+05
    ## 25                  1.322732e+09               6.812913e+08
    ## 26                  1.893588e+10               1.454185e+09
    ## 27                  1.889772e+09               2.311180e+08
    ## 28                  2.395777e+08               1.684619e+07
    ## 29                  1.719677e+08               7.340169e+06
    ## 30                  6.740535e+07               3.747208e+06
    ## 31                  1.104589e+08               7.155787e+06
    ## 32                  2.975068e+07               3.299430e+06
    ## 33                  1.892483e+08               6.571932e+06
    ## 34                  2.131727e+08               1.562369e+07
    ## 35                  5.279778e+08               7.781724e+07
    ## 36                  1.647588e+09               8.802384e+07
    ## 37                  1.201044e+08               2.273795e+07
    ## 38                  2.596903e+07               1.026348e+06
    ## 39                  2.905223e+08               4.692659e+06
    ## 40                  1.175402e+05               1.355944e+03
    ## 41                  1.880333e+08               6.346302e+06
    ## 42                  1.578201e+09               1.190572e+08
    ## 43                  1.095562e+08               1.034461e+06
    ## 44                  2.035978e+09               1.944898e+08
    ## 45                  1.152691e+08               7.674483e+06
    ## 46                  5.303937e+08               2.384303e+07
    ## 47                  1.537115e+08               1.120714e+07
    ## 48                  2.097410e+08               5.674719e+07
    ## 49                  1.526837e+08               4.162846e+06
    ## 50                  1.453598e+07               6.649839e+05
    ## 51                  2.258942e+10               6.935472e+08
    ## 52                  3.583865e+09               2.892446e+08
    ## 53                  1.404541e+09               1.531645e+07
    ## 54                  1.768759e+08               1.156946e+07
    ## 55                  2.461295e+08               2.254886e+07
    ## 56                  1.273477e+09               1.062307e+08
    ## 57                  1.910087e+07               9.343311e+05
    ## 58                  1.900924e+09               1.212106e+08
    ## 59                  2.194615e+08               2.302503e+07
    ## 60                  5.181789e+08               1.456538e+07
    ## 61                  3.303154e+08               1.032254e+07
    ## 62                  1.415162e+08               1.979451e+06
    ## 63                  7.492061e+07               6.253128e+06
    ## 64                  3.673634e+07               1.238126e+06
    ## 65                  1.590906e+08               1.840836e+07
    ## 66                  6.637316e+07               2.669299e+06
    ## 67                  5.447966e+06               2.232463e+05
    ## 68                  1.244755e+08               3.771558e+06
    ## 69                  5.270419e+04               1.073247e+03
    ## 70                  5.606390e+08               9.852144e+07
    ## 71                  7.010194e+06               2.728120e+05
    ## 72                  6.770664e+07               4.221249e+05
    ## 73                  9.165752e+06               6.030392e+05
    ## 74                  1.477764e+07               2.344169e+05
    ## 75                  2.288716e+07               3.305993e+06
    ## 76                  2.301766e+09               4.781321e+07
    ## 77                  3.967989e+07               9.684731e+05
    ## 78                  1.699137e+07               6.761377e+05
    ## 79                  4.482256e+08               2.212246e+07
    ## 80                  3.139168e+07               8.994105e+05
    ## 81                  3.415031e+08               1.831176e+07
    ## 82                  6.330467e+08               5.851905e+07
    ## 83                  2.213753e+08               1.087666e+07
    ## 84                  3.667237e+07               1.138268e+05
    ## 85                  4.138842e+07               1.525238e+06
    ## 86                  2.091110e+08               1.445166e+07
    ## 87                  8.445282e+07               4.964668e+06
    ## 88                  1.531082e+09               3.426866e+07
    ## 89                  1.446594e+08               3.515691e+07
    ## 90                  1.177900e+09               2.262488e+08
    ## 91                  2.005642e+09               8.584181e+07
    ## 92                  7.965374e+08               5.433546e+07
    ## 93                  2.962072e+08               4.247548e+07
    ## 94                  3.199405e+08               6.829770e+06
    ## 95                  3.100043e+09               2.832875e+08
    ## 96                  2.771336e+07               1.321950e+06
    ## 97                  9.075785e+08               9.405674e+07
    ## 98                  1.254117e+08               4.747879e+06
    ## 99                  1.279687e+08               1.066365e+06
    ## 100                 8.098507e+07               2.371928e+06
    ## 101                 4.772209e+07               7.511614e+06
    ## 102                 5.172269e+08               4.957236e+07
    ## 103                 1.952995e+09               3.466139e+08
    ## 104                 1.182522e+09               1.761297e+08
    ## 105                 1.345227e+08               7.542397e+05
    ## 106                 3.073089e+08               1.842630e+07
    ## 107                 2.337367e+08               1.424460e+07
    ## 108                 7.713746e+08               1.246747e+08
    ## 109                 1.017484e+08               1.828911e+06
    ## 110                 1.336414e+09               2.778302e+08
    ## 111                 3.351730e+07               1.040593e+06
    ## 112                 1.395216e+08               2.022301e+07
    ## 113                 2.812893e+09               2.278295e+08
    ## 114                 1.259171e+06               1.220525e+05
    ## 115                 6.698047e+08               1.381495e+07
    ## 116                 1.664928e+09               1.430835e+08
    ## 117                 8.932967e+09               7.658447e+07
    ## 118                 1.016413e+05               2.468085e+04
    ## 119                 6.964653e+06               1.072321e+06
    ## 120                 3.739496e+04               2.916149e+03
    ## 121                 8.414648e+07               8.910108e+05
    ## 122                 2.303957e+08               3.974122e+07
    ## 123                 2.803159e+07               2.394949e+06
    ##     pop_X2.3_vegan_l_blue_green_wf pop_X2.3_vegan_l_blue_wf_total
    ## 1                     4.377621e+11                   1.821752e+11
    ## 2                     1.274756e+11                   1.838528e+10
    ## 3                     9.083719e+11                   1.660124e+11
    ## 4                     1.695505e+12                   1.288501e+11
    ## 5                     8.678971e+10                   1.405906e+10
    ## 6                     1.222382e+12                   1.800728e+11
    ## 7                     1.734630e+11                   1.455832e+10
    ## 8                     1.926948e+11                   3.775786e+10
    ## 9                     9.949276e+07                   1.040209e+07
    ## 10                    2.427276e+11                   9.446633e+09
    ## 11                    2.448598e+11                   2.877107e+10
    ## 12                    5.329932e+09                   2.892493e+08
    ## 13                    1.753958e+10                   9.761845e+08
    ## 14                    6.189267e+06                   9.918840e+05
    ## 15                    7.788100e+10                   3.175238e+09
    ## 16                    3.866841e+10                   5.226050e+09
    ## 17                    8.459686e+12                   3.309905e+11
    ## 18                    8.533049e+09                   8.426858e+08
    ## 19                    2.034531e+11                   1.158294e+10
    ## 20                    1.105768e+11                   3.466365e+09
    ## 21                    2.486319e+11                   2.132310e+10
    ## 22                    3.480220e+11                   1.450838e+10
    ## 23                    9.019659e+11                   9.940457e+10
    ## 24                    1.404214e+10                   1.250857e+09
    ## 25                    5.648551e+11                   1.057828e+11
    ## 26                    1.693614e+13                   2.759354e+12
    ## 27                    1.560851e+12                   8.698781e+10
    ## 28                    5.939280e+11                   3.783056e+10
    ## 29                    1.651125e+11                   1.308355e+10
    ## 30                    1.957731e+11                   6.097518e+09
    ## 31                    1.335076e+11                   7.129794e+09
    ## 32                    3.587750e+10                   6.817802e+09
    ## 33                    1.707451e+11                   1.194003e+10
    ## 34                    1.362954e+11                   1.398145e+10
    ## 35                    7.264887e+11                   8.186606e+10
    ## 36                    1.956812e+12                   1.066516e+12
    ## 37                    1.466858e+11                   8.126874e+09
    ## 38                    3.265910e+10                   1.437093e+09
    ## 39                    7.723674e+11                   6.601681e+10
    ## 40                    1.815816e+08                   1.242743e+07
    ## 41                    1.575318e+11                   1.508754e+10
    ## 42                    1.105353e+12                   1.284450e+11
    ## 43                    1.767193e+11                   1.642715e+10
    ## 44                    1.456251e+12                   1.649233e+11
    ## 45                    3.517953e+11                   2.146801e+10
    ## 46                    5.795897e+11                   8.814129e+10
    ## 47                    2.314308e+11                   1.086231e+10
    ## 48                    2.506951e+11                   1.183795e+10
    ## 49                    1.824520e+11                   8.623252e+09
    ## 50                    8.676736e+09                   1.305616e+09
    ## 51                    3.574321e+13                   6.825016e+12
    ## 52                    6.470247e+12                   5.094738e+11
    ## 53                    3.637056e+12                   1.558086e+12
    ## 54                    9.127421e+10                   1.150414e+10
    ## 55                    2.460585e+11                   5.140361e+10
    ## 56                    1.354112e+12                   1.429327e+11
    ## 57                    3.004912e+10                   2.291647e+09
    ## 58                    1.842028e+12                   2.256402e+11
    ## 59                    2.491113e+11                   5.903636e+10
    ## 60                    7.522677e+11                   1.485068e+11
    ## 61                    5.648784e+11                   3.308139e+10
    ## 62                    1.502193e+11                   3.206819e+10
    ## 63                    6.985649e+10                   1.476677e+10
    ## 64                    5.188683e+10                   2.938060e+09
    ## 65                    1.968771e+11                   4.567438e+10
    ## 66                    7.972408e+10                   3.289923e+09
    ## 67                    3.326794e+09                   3.404136e+08
    ## 68                    2.036580e+11                   2.685035e+10
    ## 69                    1.619316e+08                   7.177305e+06
    ## 70                    6.990704e+11                   7.992438e+10
    ## 71                    9.414409e+09                   2.212718e+09
    ## 72                    1.396085e+11                   1.044961e+10
    ## 73                    8.321069e+09                   1.256798e+09
    ## 74                    2.097566e+10                   2.384326e+09
    ## 75                    2.501219e+10                   3.273859e+09
    ## 76                    3.868290e+12                   4.690611e+11
    ## 77                    6.269183e+10                   6.311559e+09
    ## 78                    1.945828e+10                   1.020898e+09
    ## 79                    1.100574e+12                   1.871319e+11
    ## 80                    4.652165e+10                   3.815036e+09
    ## 81                    5.588824e+11                   7.657567e+10
    ## 82                    4.625626e+11                   4.773652e+10
    ## 83                    1.550450e+11                   2.078125e+10
    ## 84                    2.334510e+11                   4.837480e+09
    ## 85                    4.266451e+10                   3.754071e+09
    ## 86                    1.522551e+11                   1.783976e+10
    ## 87                    1.196604e+11                   4.636941e+10
    ## 88                    1.920584e+12                   7.673717e+11
    ## 89                    1.330223e+11                   9.727048e+09
    ## 90                    1.425518e+12                   3.152456e+11
    ## 91                    3.466779e+12                   2.319018e+11
    ## 92                    8.440916e+11                   3.226941e+10
    ## 93                    3.333632e+11                   6.219859e+10
    ## 94                    4.225679e+11                   2.177013e+10
    ## 95                    4.404493e+12                   2.301279e+11
    ## 96                    1.051536e+11                   6.946527e+09
    ## 97                    1.309786e+12                   4.246799e+11
    ## 98                    2.380575e+11                   1.856860e+10
    ## 99                    1.965855e+11                   8.060852e+09
    ## 100                   8.460430e+10                   4.833644e+09
    ## 101                   3.825625e+10                   3.224359e+09
    ## 102                   6.192404e+11                   8.378421e+10
    ## 103                   1.581288e+12                   1.446720e+11
    ## 104                   1.460931e+12                   2.364239e+11
    ## 105                   2.855834e+11                   4.511866e+10
    ## 106                   2.053925e+11                   2.296770e+10
    ## 107                   1.600933e+11                   1.465824e+10
    ## 108                   9.112848e+11                   9.328214e+10
    ## 109                   1.935986e+11                   1.198380e+10
    ## 110                   2.229547e+12                   3.392966e+11
    ## 111                   1.437222e+11                   4.242455e+09
    ## 112                   2.814586e+11                   3.406522e+10
    ## 113                   3.905432e+12                   5.151199e+11
    ## 114                   2.945634e+09                   1.445501e+08
    ## 115                   1.068474e+12                   4.361535e+10
    ## 116                   9.826186e+11                   1.294787e+11
    ## 117                   8.966509e+12                   1.250901e+12
    ## 118                   1.013011e+08                   3.975516e+06
    ## 119                   5.250178e+09                   3.231653e+08
    ## 120                   6.286817e+07                   1.724218e+07
    ## 121                   1.321795e+11                   1.085384e+10
    ## 122                   2.159377e+11                   3.536626e+10
    ## 123                   2.942926e+10                   3.952968e+09
    ##     pop_X2.3_vegan_l_green_wf pop_vegan_kg_co2e_excl_luc
    ## 1                2.555869e+11               9.470968e+07
    ## 2                1.090903e+11               4.898697e+07
    ## 3                7.423596e+11               2.590103e+08
    ## 4                1.566655e+12               6.691837e+08
    ## 5                7.273064e+10               3.060113e+07
    ## 6                1.042309e+12               3.930263e+08
    ## 7                1.589047e+11               9.616192e+07
    ## 8                1.549369e+11               5.100144e+07
    ## 9                8.909067e+07               3.326717e+04
    ## 10               2.332810e+11               9.317047e+07
    ## 11               2.160887e+11               1.393007e+08
    ## 12               5.040682e+09               1.289284e+06
    ## 13               1.656339e+10               3.543988e+06
    ## 14               5.197383e+06               1.931097e+03
    ## 15               7.470576e+10               2.554371e+07
    ## 16               3.344236e+10               9.495039e+06
    ## 17               8.128695e+12               2.233862e+09
    ## 18               7.690363e+09               2.306886e+06
    ## 19               1.918702e+11               7.095127e+07
    ## 20               1.071105e+11               2.047783e+07
    ## 21               2.273088e+11               7.076967e+07
    ## 22               3.335136e+11               6.883289e+07
    ## 23               8.025614e+11               3.521345e+08
    ## 24               1.279129e+10               3.159423e+06
    ## 25               4.590723e+11               2.447971e+08
    ## 26               1.417679e+13               1.066582e+10
    ## 27               1.473863e+12               3.988945e+08
    ## 28               5.560974e+11               9.965278e+07
    ## 29               1.520289e+11               4.358211e+07
    ## 30               1.896755e+11               3.210298e+07
    ## 31               1.263778e+11               4.493408e+07
    ## 32               2.905970e+10               1.183775e+07
    ## 33               1.588051e+11               8.813476e+07
    ## 34               1.223140e+11               9.325666e+07
    ## 35               6.446226e+11               1.584215e+08
    ## 36               8.902965e+11               8.431708e+08
    ## 37               1.385589e+11               4.143997e+07
    ## 38               3.122200e+10               1.265583e+07
    ## 39               7.063506e+11               1.342566e+08
    ## 40               1.691542e+08               4.690421e+04
    ## 41               1.424443e+11               8.151807e+07
    ## 42               9.769076e+11               5.951143e+08
    ## 43               1.602921e+11               4.071776e+07
    ## 44               1.291328e+12               8.511931e+08
    ## 45               3.303273e+11               6.638624e+07
    ## 46               4.914484e+11               2.296924e+08
    ## 47               2.205685e+11               5.913973e+07
    ## 48               2.388571e+11               5.693523e+07
    ## 49               1.738287e+11               7.642719e+07
    ## 50               7.371120e+09               4.557182e+06
    ## 51               2.891819e+13               1.069376e+10
    ## 52               5.960773e+12               2.026897e+09
    ## 53               2.078970e+12               9.004515e+08
    ## 54               7.977007e+10               5.242351e+07
    ## 55               1.946549e+11               8.826177e+07
    ## 56               1.211179e+12               5.083743e+08
    ## 57               2.775747e+10               8.461595e+06
    ## 58               1.616387e+12               9.290502e+08
    ## 59               1.900750e+11               8.845636e+07
    ## 60               6.037609e+11               1.911644e+08
    ## 61               5.317971e+11               1.225119e+08
    ## 62               1.181511e+11               4.911323e+07
    ## 63               5.508972e+10               2.523776e+07
    ## 64               4.894877e+10               2.132990e+07
    ## 65               1.512027e+11               6.367708e+07
    ## 66               7.643416e+10               3.433557e+07
    ## 67               2.986380e+09               1.900508e+06
    ## 68               1.768076e+11               4.148955e+07
    ## 69               1.547543e+08               3.751552e+04
    ## 70               6.191460e+11               2.118528e+08
    ## 71               7.201691e+09               2.899063e+06
    ## 72               1.291589e+11               2.380405e+07
    ## 73               7.064271e+09               3.938443e+06
    ## 74               1.859134e+10               4.587073e+06
    ## 75               2.173833e+10               9.192007e+06
    ## 76               3.399229e+12               9.941536e+08
    ## 77               5.638027e+10               2.026568e+07
    ## 78               1.843738e+10               7.519947e+06
    ## 79               9.134419e+11               2.544939e+08
    ## 80               4.270662e+10               1.140737e+07
    ## 81               4.823067e+11               1.400477e+08
    ## 82               4.148261e+11               2.162813e+08
    ## 83               1.342637e+11               6.222781e+07
    ## 84               2.286135e+11               1.631919e+07
    ## 85               3.891044e+10               2.036650e+07
    ## 86               1.344153e+11               7.144901e+07
    ## 87               7.329102e+10               3.070056e+07
    ## 88               1.153212e+12               4.540244e+08
    ## 89               1.232953e+11               4.143594e+07
    ## 90               1.110272e+12               4.913131e+08
    ## 91               3.234877e+12               1.016602e+09
    ## 92               8.118222e+11               4.153255e+08
    ## 93               2.711646e+11               1.078338e+08
    ## 94               4.007978e+11               1.622219e+08
    ## 95               4.174365e+12               1.365889e+09
    ## 96               9.820704e+10               1.847050e+07
    ## 97               8.851058e+11               3.741035e+08
    ## 98               2.194889e+11               5.315259e+07
    ## 99               1.885247e+11               6.987691e+07
    ## 100              7.977065e+10               4.081304e+07
    ## 101              3.503189e+10               1.686069e+07
    ## 102              5.354562e+11               2.065226e+08
    ## 103              1.436616e+12               7.548003e+08
    ## 104              1.224507e+12               4.885993e+08
    ## 105              2.404647e+11               8.202795e+07
    ## 106              1.824248e+11               1.066613e+08
    ## 107              1.454350e+11               8.646919e+07
    ## 108              8.180027e+11               3.368717e+08
    ## 109              1.816148e+11               3.855271e+07
    ## 110              1.890250e+12               6.179558e+08
    ## 111              1.394797e+11               2.408072e+07
    ## 112              2.473933e+11               7.111134e+07
    ## 113              3.390313e+12               1.484706e+09
    ## 114              2.801084e+09               4.249672e+05
    ## 115              1.024859e+12               4.073485e+08
    ## 116              8.531400e+11               5.742279e+08
    ## 117              7.715609e+12               3.185333e+09
    ## 118              9.732563e+07               2.718788e+04
    ## 119              4.927013e+09               1.374623e+06
    ## 120              4.562599e+07               1.672129e+04
    ## 121              1.213257e+11               2.743059e+07
    ## 122              1.805715e+11               6.791226e+07
    ## 123              2.547629e+10               1.074571e+07
    ##     pop_vegan_kg_co2e_total pop_vegan_kg_co2e_luc pop_vegan_l_blue_green_wf
    ## 1              9.470968e+07          0.000000e+00              4.154027e+11
    ## 2              4.898709e+07          1.167735e+02              1.055427e+11
    ## 3              2.865154e+08          2.750505e+07              7.710394e+11
    ## 4              1.286682e+09          6.174982e+08              1.533055e+12
    ## 5              3.062097e+07          1.984478e+04              7.148935e+10
    ## 6              4.027691e+08          9.742816e+06              9.142872e+11
    ## 7              9.673225e+07          5.703261e+05              1.446408e+11
    ## 8              5.226852e+07          1.267079e+06              1.665779e+11
    ## 9              3.986006e+04          6.592889e+03              8.779544e+07
    ## 10             9.556741e+07          2.396947e+06              2.018413e+11
    ## 11             1.504103e+08          1.110959e+07              2.011762e+11
    ## 12             1.683643e+06          3.943591e+05              4.890020e+09
    ## 13             3.685804e+06          1.418152e+05              1.701818e+10
    ## 14             1.931640e+03          5.436338e-01              4.383449e+06
    ## 15             2.569865e+07          1.549389e+05              6.823901e+10
    ## 16             9.977798e+06          4.827593e+05              3.197262e+10
    ## 17             3.316318e+09          1.082456e+09              6.866369e+12
    ## 18             2.359830e+06          5.294395e+04              8.542778e+09
    ## 19             7.109942e+07          1.481559e+05              1.659226e+11
    ## 20             2.215749e+07          1.679660e+06              1.098642e+11
    ## 21             1.117631e+08          4.099344e+07              2.455112e+11
    ## 22             7.970694e+07          1.087404e+07              3.441758e+11
    ## 23             3.530420e+08          9.074983e+05              7.377533e+11
    ## 24             3.160892e+06          1.469209e+03              1.453999e+10
    ## 25             2.839758e+08          3.917877e+07              4.610108e+11
    ## 26             1.196928e+10          1.303463e+09              1.447298e+13
    ## 27             5.290993e+08          1.302048e+08              1.282781e+12
    ## 28             1.069625e+08          7.309717e+06              5.785190e+11
    ## 29             4.866144e+07          5.079334e+06              1.464550e+11
    ## 30             3.274326e+07          6.402819e+05              2.027428e+11
    ## 31             4.732650e+07          2.392420e+06              1.058617e+11
    ## 32             1.185266e+07          1.491091e+04              2.733189e+10
    ## 33             8.838398e+07          2.492231e+05              1.348823e+11
    ## 34             9.437448e+07          1.117820e+06              1.091290e+11
    ## 35             2.050424e+08          4.662086e+07              5.155881e+11
    ## 36             8.939490e+08          5.077816e+07              1.689762e+12
    ## 37             4.212374e+07          6.837711e+05              1.398070e+11
    ## 38             1.265787e+07          2.040289e+03              2.663071e+10
    ## 39             1.385425e+08          4.285893e+06              7.700061e+11
    ## 40             4.742752e+04          5.233145e+02              1.665682e+08
    ## 41             8.327478e+07          1.756713e+06              1.208490e+11
    ## 42             6.355173e+08          4.040301e+07              8.909132e+11
    ## 43             4.071817e+07          4.135098e+02              1.494749e+11
    ## 44             9.760491e+08          1.248561e+08              1.205536e+12
    ## 45             6.802271e+07          1.636469e+06              3.619252e+11
    ## 46             2.307045e+08          1.012090e+06              4.565345e+11
    ## 47             6.723386e+07          8.094126e+06              2.124014e+11
    ## 48             6.027792e+07          3.342692e+06              2.138689e+11
    ## 49             7.663113e+07          2.039417e+05              1.265562e+11
    ## 50             4.562698e+06          5.516004e+03              7.011311e+09
    ## 51             1.125879e+10          5.650280e+08              3.487238e+13
    ## 52             2.204755e+09          1.778581e+08              6.253314e+12
    ## 53             9.004515e+08          0.000000e+00              3.397519e+12
    ## 54             5.244732e+07          2.381242e+04              7.165115e+10
    ## 55             9.106728e+07          2.805508e+06              1.718265e+11
    ## 56             5.512784e+08          4.290419e+07              1.150863e+12
    ## 57             8.470521e+06          8.925455e+03              2.557708e+10
    ## 58             1.028814e+09          9.976427e+07              1.560360e+12
    ## 59             9.118503e+07          2.728665e+06              2.034002e+11
    ## 60             1.988222e+08          7.657795e+06              6.514398e+11
    ## 61             1.319963e+08          9.484310e+06              5.348228e+11
    ## 62             4.923203e+07          1.187915e+05              1.265205e+11
    ## 63             2.554645e+07          3.086889e+05              5.935462e+10
    ## 64             2.133655e+07          6.650598e+03              4.250958e+10
    ## 65             6.526541e+07          1.588325e+06              1.500024e+11
    ## 66             3.444516e+07          1.095922e+05              6.324712e+10
    ## 67             1.900533e+06          2.489282e+01              2.382670e+09
    ## 68             4.263574e+07          1.146195e+06              1.901799e+11
    ## 69             3.850852e+04          9.929952e+02              1.558359e+08
    ## 70             2.502298e+08          3.837704e+07              6.376177e+11
    ## 71             2.906202e+06          7.139279e+03              9.221540e+09
    ## 72             2.418827e+07          3.842250e+05              1.262196e+11
    ## 73             3.940577e+06          2.133546e+03              6.766658e+09
    ## 74             4.725455e+06          1.383822e+05              1.654791e+10
    ## 75             1.219561e+07          3.003602e+06              2.254659e+10
    ## 76             1.025807e+09          3.165326e+07              3.379413e+12
    ## 77             2.031800e+07          5.232051e+04              5.211370e+10
    ## 78             7.526277e+06          6.329099e+03              1.656600e+10
    ## 79             2.689964e+08          1.450247e+07              9.265244e+11
    ## 80             1.140906e+07          1.690238e+03              4.192064e+10
    ## 81             1.567567e+08          1.670906e+07              5.228794e+11
    ## 82             2.327094e+08          1.642809e+07              3.764502e+11
    ## 83             6.382133e+07          1.593520e+06              1.312166e+11
    ## 84             1.640655e+07          8.735714e+04              2.266856e+11
    ## 85             2.045104e+07          8.454206e+04              3.565957e+10
    ## 86             7.278442e+07          1.335407e+06              1.252505e+11
    ## 87             3.200490e+07          1.304342e+06              1.035235e+11
    ## 88             4.805772e+08          2.655277e+07              1.722443e+12
    ## 89             5.916156e+07          1.772562e+07              1.204540e+11
    ## 90             7.086815e+08          2.173685e+08              1.227162e+12
    ## 91             1.031163e+09          1.456033e+07              3.212373e+12
    ## 92             4.185946e+08          3.269139e+06              7.099709e+11
    ## 93             1.375130e+08          2.967921e+07              2.724386e+11
    ## 94             1.626526e+08          4.307653e+05              3.518218e+11
    ## 95             1.405373e+09          3.948451e+07              3.691083e+12
    ## 96             1.982618e+07          1.355683e+06              1.023302e+11
    ## 97             3.948926e+08          2.078909e+07              1.129005e+12
    ## 98             5.563791e+07          2.485322e+06              2.400914e+11
    ## 99             6.998042e+07          1.035044e+05              1.712491e+11
    ## 100            4.110629e+07          2.932452e+05              7.042331e+10
    ## 101            2.416389e+07          7.303191e+06              3.122013e+10
    ## 102            2.153580e+08          8.835402e+06              5.139256e+11
    ## 103            1.104555e+09          3.497552e+08              1.360961e+12
    ## 104            5.323834e+08          4.378409e+07              1.208894e+12
    ## 105            8.215425e+07          1.263013e+05              2.821196e+11
    ## 106            1.089023e+08          2.240995e+06              1.578255e+11
    ## 107            8.715720e+07          6.880064e+05              1.389106e+11
    ## 108            4.592798e+08          1.224081e+08              8.239121e+11
    ## 109            4.006545e+07          1.512733e+06              1.825879e+11
    ## 110            8.163765e+08          1.984208e+08              2.119852e+12
    ## 111            2.479826e+07          7.175350e+05              1.471475e+11
    ## 112            8.967722e+07          1.856588e+07              2.450995e+11
    ## 113            1.711024e+09          2.263181e+08              3.333714e+12
    ## 114            4.854881e+05          6.052090e+04              2.850549e+09
    ## 115            4.135066e+08          6.158096e+06              9.211941e+11
    ## 116            5.839650e+08          9.737072e+06              7.894390e+11
    ## 117            3.201453e+09          1.611916e+07              7.471910e+12
    ## 118            5.201551e+04          2.482762e+04              9.183784e+07
    ## 119            1.858751e+06          4.841271e+05              3.849324e+09
    ## 120            1.737860e+04          6.573186e+02              5.664947e+07
    ## 121            2.752015e+07          8.955886e+04              1.170276e+11
    ## 122            7.210436e+07          4.192098e+06              1.681716e+11
    ## 123            1.081651e+07          7.079694e+04              2.433485e+10
    ##     pop_vegan_l_blue_wf_total pop_vegan_l_green_wf
    ## 1                1.820594e+11         2.333434e+11
    ## 2                1.486252e+10         9.068014e+10
    ## 3                1.696918e+11         6.013476e+11
    ## 4                1.315875e+11         1.401467e+12
    ## 5                1.331252e+10         5.817683e+10
    ## 6                1.645880e+11         7.496992e+11
    ## 7                1.305326e+10         1.315875e+11
    ## 8                3.618836e+10         1.303895e+11
    ## 9                1.004575e+07         7.774969e+07
    ## 10               7.826034e+09         1.940153e+11
    ## 11               2.431250e+10         1.768637e+11
    ## 12               2.600455e+08         4.629975e+09
    ## 13               8.598198e+08         1.615836e+10
    ## 14               8.806585e+05         3.502791e+06
    ## 15               3.040196e+09         6.519881e+10
    ## 16               5.596273e+09         2.637634e+10
    ## 17               2.928717e+11         6.573497e+12
    ## 18               7.731910e+08         7.769587e+09
    ## 19               1.128516e+10         1.546374e+11
    ## 20               3.852960e+09         1.060113e+11
    ## 21               1.299155e+10         2.325196e+11
    ## 22               1.283678e+10         3.313390e+11
    ## 23               9.356120e+10         6.441921e+11
    ## 24               1.197917e+09         1.334207e+10
    ## 25               9.545494e+10         3.655559e+11
    ## 26               2.370387e+12         1.210259e+13
    ## 27               7.723248e+10         1.205548e+12
    ## 28               2.816322e+10         5.503558e+11
    ## 29               1.071896e+10         1.357360e+11
    ## 30               5.127319e+09         1.976155e+11
    ## 31               6.509080e+09         9.935265e+10
    ## 32               6.781278e+09         2.055062e+10
    ## 33               1.112760e+10         1.237547e+11
    ## 34               1.237588e+10         9.675310e+10
    ## 35               7.283081e+10         4.427572e+11
    ## 36               9.527825e+11         7.369800e+11
    ## 37               7.901996e+09         1.319050e+11
    ## 38               1.197114e+09         2.543360e+10
    ## 39               8.541552e+10         6.845905e+11
    ## 40               1.246988e+07         1.540983e+08
    ## 41               1.190382e+10         1.089452e+11
    ## 42               1.115229e+11         7.793903e+11
    ## 43               1.537542e+10         1.340995e+11
    ## 44               1.496092e+11         1.055927e+12
    ## 45               1.828309e+10         3.436421e+11
    ## 46               7.529787e+10         3.812366e+11
    ## 47               1.008973e+10         2.023116e+11
    ## 48               1.003542e+10         2.038335e+11
    ## 49               7.950116e+09         1.186061e+11
    ## 50               1.058171e+09         5.953140e+09
    ## 51               6.397402e+12         2.847498e+13
    ## 52               3.582397e+11         5.895074e+12
    ## 53               1.555820e+12         1.841699e+12
    ## 54               1.066260e+10         6.098854e+10
    ## 55               5.270966e+10         1.191169e+11
    ## 56               1.252198e+11         1.025643e+12
    ## 57               2.085370e+09         2.349171e+10
    ## 58               1.843884e+11         1.375971e+12
    ## 59               5.521038e+10         1.481898e+11
    ## 60               1.473064e+11         5.041334e+11
    ## 61               2.967835e+10         5.051445e+11
    ## 62               2.893663e+10         9.758384e+10
    ## 63               1.467253e+10         4.468209e+10
    ## 64               2.771047e+09         3.973853e+10
    ## 65               3.914481e+10         1.108576e+11
    ## 66               2.776638e+09         6.047048e+10
    ## 67               2.537427e+08         2.128927e+09
    ## 68               2.418603e+10         1.659938e+11
    ## 69               6.136939e+06         1.496990e+08
    ## 70               6.855907e+10         5.690586e+11
    ## 71               2.181503e+09         7.040037e+09
    ## 72               1.018581e+10         1.160338e+11
    ## 73               1.112441e+09         5.654217e+09
    ## 74               2.582889e+09         1.396502e+10
    ## 75               3.240310e+09         1.930628e+10
    ## 76               4.414689e+11         2.937944e+12
    ## 77               5.474633e+09         4.663906e+10
    ## 78               9.087523e+08         1.565725e+10
    ## 79               1.892868e+11         7.372376e+11
    ## 80               3.949627e+09         3.797101e+10
    ## 81               7.573752e+10         4.471419e+11
    ## 82               4.341216e+10         3.330380e+11
    ## 83               1.958202e+10         1.116346e+11
    ## 84               5.230469e+09         2.214551e+11
    ## 85               3.430880e+09         3.222869e+10
    ## 86               1.479173e+10         1.104588e+11
    ## 87               4.634998e+10         5.717356e+10
    ## 88               7.820160e+11         9.404275e+11
    ## 89               8.989132e+09         1.114649e+11
    ## 90               3.262333e+11         9.009292e+11
    ## 91               1.615867e+11         3.050786e+12
    ## 92               2.505537e+10         6.849156e+11
    ## 93               4.995050e+10         2.224881e+11
    ## 94               1.942251e+10         3.323993e+11
    ## 95               1.866602e+11         3.504423e+12
    ## 96               6.745730e+09         9.558444e+10
    ## 97               4.466259e+11         6.823787e+11
    ## 98               1.794438e+10         2.221470e+11
    ## 99               7.086648e+09         1.641625e+11
    ## 100              4.291703e+09         6.613161e+10
    ## 101              3.014490e+09         2.820564e+10
    ## 102              9.283657e+10         4.210891e+11
    ## 103              1.208899e+11         1.240071e+12
    ## 104              2.010085e+11         1.007885e+12
    ## 105              4.321078e+10         2.389088e+11
    ## 106              1.934268e+10         1.384828e+11
    ## 107              1.225856e+10         1.266520e+11
    ## 108              7.960923e+10         7.443029e+11
    ## 109              1.081070e+10         1.717772e+11
    ## 110              3.244293e+11         1.795423e+12
    ## 111              4.156938e+09         1.429906e+11
    ## 112              2.946935e+10         2.156301e+11
    ## 113              4.683563e+11         2.865358e+12
    ## 114              1.002710e+08         2.750278e+09
    ## 115              3.859125e+10         8.826028e+11
    ## 116              1.189544e+11         6.704846e+11
    ## 117              1.173554e+12         6.298356e+12
    ## 118              3.892934e+06         8.794490e+07
    ## 119              2.976990e+08         3.551625e+09
    ## 120              1.732423e+07         3.932524e+07
    ## 121              1.066402e+10         1.063636e+11
    ## 122              3.238481e+10         1.357868e+11
    ## 123              3.328899e+09         2.100595e+10
    ##     red_pop_meatless_day_kg_co2e_excl_luc
    ## 1                           -3.390886e+08
    ## 2                            4.056941e+07
    ## 3                           -8.358555e+07
    ## 4                            2.080295e+09
    ## 5                            3.800768e+06
    ## 6                            6.513061e+08
    ## 7                            1.058941e+08
    ## 8                           -1.288769e+07
    ## 9                            2.475968e+03
    ## 10                           4.925668e+07
    ## 11                           9.781574e+07
    ## 12                          -3.292473e+05
    ## 13                          -1.440301e+06
    ## 14                          -1.391852e+03
    ## 15                          -1.646222e+06
    ## 16                          -3.071386e+07
    ## 17                           6.498014e+09
    ## 18                           1.384846e+06
    ## 19                           9.175475e+06
    ## 20                           7.823627e+06
    ## 21                           1.690184e+07
    ## 22                           1.389012e+07
    ## 23                           4.324472e+08
    ## 24                          -1.630495e+06
    ## 25                           1.463260e+08
    ## 26                           2.752306e+09
    ## 27                          -1.005659e+09
    ## 28                          -1.626073e+08
    ## 29                          -5.462832e+07
    ## 30                          -5.561079e+07
    ## 31                          -1.204428e+07
    ## 32                          -6.004353e+06
    ## 33                          -8.517507e+06
    ## 34                           5.924621e+07
    ## 35                          -2.166131e+08
    ## 36                           5.938363e+08
    ## 37                          -2.432260e+07
    ## 38                           1.333723e+06
    ## 39                          -1.570511e+08
    ## 40                          -2.211321e+04
    ## 41                           4.786158e+07
    ## 42                           7.509189e+08
    ## 43                          -7.669633e+07
    ## 44                           6.875206e+08
    ## 45                          -1.210357e+07
    ## 46                           2.460899e+08
    ## 47                          -7.205528e+07
    ## 48                          -8.641469e+07
    ## 49                          -1.351659e+07
    ## 50                           6.233092e+06
    ## 51                          -1.724484e+10
    ## 52                          -1.212200e+09
    ## 53                           9.765655e+07
    ## 54                           8.373257e+07
    ## 55                           1.828846e+08
    ## 56                           4.596278e+08
    ## 57                          -5.551724e+06
    ## 58                          -1.396699e+07
    ## 59                          -5.671146e+07
    ## 60                           2.430417e+08
    ## 61                          -1.959802e+08
    ## 62                          -2.513526e+06
    ## 63                           5.300835e+07
    ## 64                           6.007976e+05
    ## 65                          -2.387960e+07
    ## 66                           1.674749e+07
    ## 67                           1.590883e+06
    ## 68                          -1.535565e+08
    ## 69                           4.615391e+03
    ## 70                          -1.945624e+07
    ## 71                          -1.802102e+05
    ## 72                          -1.503385e+07
    ## 73                           2.806479e+06
    ## 74                          -8.819257e+05
    ## 75                           4.196539e+06
    ## 76                           7.697219e+08
    ## 77                          -2.417610e+07
    ## 78                           6.490085e+06
    ## 79                           4.730103e+07
    ## 80                          -2.268615e+07
    ## 81                          -1.980750e+08
    ## 82                           1.857318e+08
    ## 83                           5.105249e+07
    ## 84                           1.322085e+07
    ## 85                          -1.112906e+07
    ## 86                           7.693101e+07
    ## 87                           5.766374e+06
    ## 88                          -1.092907e+09
    ## 89                          -8.022774e+06
    ## 90                          -3.266276e+07
    ## 91                          -7.650600e+08
    ## 92                           1.913673e+08
    ## 93                           7.065920e+07
    ## 94                           4.819740e+07
    ## 95                           7.663784e+08
    ## 96                          -9.336974e+06
    ## 97                           7.096196e+07
    ## 98                          -9.591508e+07
    ## 99                          -4.602315e+06
    ## 100                         -3.554702e+07
    ## 101                          1.381440e+07
    ## 102                          5.075734e+07
    ## 103                          6.047831e+08
    ## 104                          2.627784e+08
    ## 105                         -8.470840e+07
    ## 106                          9.670745e+07
    ## 107                          3.768306e+07
    ## 108                          1.273751e+08
    ## 109                         -8.190744e+07
    ## 110                         -2.094969e+08
    ## 111                         -7.429165e+06
    ## 112                          2.155333e+07
    ## 113                          9.729164e+08
    ## 114                         -1.012659e+06
    ## 115                          3.588363e+07
    ## 116                          5.400148e+08
    ## 117                          5.217189e+09
    ## 118                          4.922484e+04
    ## 119                         -9.071246e+05
    ## 120                         -2.824391e+04
    ## 121                         -1.020927e+08
    ## 122                          8.749262e+07
    ## 123                          1.149937e+06

Now, we will do the same for the per capita estimates.
