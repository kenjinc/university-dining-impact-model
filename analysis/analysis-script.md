Analysis and Visualization Script
================

## Required Packages

``` r
library(tidyverse)
```

## Data Loading

``` r
impact_modeling_data <- read.csv("/Users/kenjinchang/github/university-dining-impact-model/data/impact-modeling-data.csv")
impact_modeling_data %>%
  head(6)
```

    ##   X     long      lat group order     country isced6_enr isced6_ref_yr
    ## 1 1 74.89131 37.23164     2    12 Afghanistan     365982          2018
    ## 2 2 74.84023 37.22505     2    13 Afghanistan     365982          2018
    ## 3 3 74.76738 37.24917     2    14 Afghanistan     365982          2018
    ## 4 4 74.73896 37.28564     2    15 Afghanistan     365982          2018
    ## 5 5 74.72666 37.29072     2    16 Afghanistan     365982          2018
    ## 6 6 74.66895 37.26670     2    17 Afghanistan     365982          2018
    ##   isced7_enr isced7_ref_yr isced8_enr isced8_ref_yr natpop_est natpop_ref_yr
    ## 1       4600          2018         28          2018   38042000          2019
    ## 2       4600          2018         28          2018   38042000          2019
    ## 3       4600          2018         28          2018   38042000          2019
    ## 4       4600          2018         28          2018   38042000          2019
    ## 5       4600          2018         28          2018   38042000          2019
    ## 6       4600          2018         28          2018   38042000          2019
    ##   uni_enr_tot uni_enr_prop baseline_kg_co2e_excl_luc baseline_kg_co2e_total
    ## 1      370610  0.009742127                  894.9871               898.4078
    ## 2      370610  0.009742127                  894.9871               898.4078
    ## 3      370610  0.009742127                  894.9871               898.4078
    ## 4      370610  0.009742127                  894.9871               898.4078
    ## 5      370610  0.009742127                  894.9871               898.4078
    ## 6      370610  0.009742127                  894.9871               898.4078
    ##   baseline_l_blue_green_wf baseline_l_blue_wf_total baseline_l_green_wf
    ## 1                  1015577                 347776.8            667799.9
    ## 2                  1015577                 347776.8            667799.9
    ## 3                  1015577                 347776.8            667799.9
    ## 4                  1015577                 347776.8            667799.9
    ## 5                  1015577                 347776.8            667799.9
    ## 6                  1015577                 347776.8            667799.9
    ##   meatless_day_kg_co2e_excl_luc meatless_day_kg_co2e_total
    ## 1                      1809.934                   1817.495
    ## 2                      1809.934                   1817.495
    ## 3                      1809.934                   1817.495
    ## 4                      1809.934                   1817.495
    ## 5                      1809.934                   1817.495
    ## 6                      1809.934                   1817.495
    ##   meatless_day_l_blue_green_wf meatless_day_l_blue_wf_total
    ## 1                      1302835                     506810.4
    ## 2                      1302835                     506810.4
    ## 3                      1302835                     506810.4
    ## 4                      1302835                     506810.4
    ## 5                      1302835                     506810.4
    ## 6                      1302835                     506810.4
    ##   meatless_day_l_green_wf no_dairy_kg_co2e_excl_luc no_dairy_kg_co2e_total
    ## 1                796024.9                  503.0854               508.6838
    ## 2                796024.9                  503.0854               508.6838
    ## 3                796024.9                  503.0854               508.6838
    ## 4                796024.9                  503.0854               508.6838
    ## 5                796024.9                  503.0854               508.6838
    ## 6                796024.9                  503.0854               508.6838
    ##   no_dairy_l_blue_green_wf no_dairy_l_blue_wf_total no_dairy_l_green_wf
    ## 1                  1298970                 502284.9            796685.3
    ## 2                  1298970                 502284.9            796685.3
    ## 3                  1298970                 502284.9            796685.3
    ## 4                  1298970                 502284.9            796685.3
    ## 5                  1298970                 502284.9            796685.3
    ## 6                  1298970                 502284.9            796685.3
    ##   low_red_meat_kg_co2e_excl_luc low_red_meat_kg_co2e_total
    ## 1                      1827.645                    1835.41
    ## 2                      1827.645                    1835.41
    ## 3                      1827.645                    1835.41
    ## 4                      1827.645                    1835.41
    ## 5                      1827.645                    1835.41
    ## 6                      1827.645                    1835.41
    ##   low_red_meat_l_blue_green_wf low_red_meat_l_blue_wf_total
    ## 1                      1377793                     565371.3
    ## 2                      1377793                     565371.3
    ## 3                      1377793                     565371.3
    ## 4                      1377793                     565371.3
    ## 5                      1377793                     565371.3
    ## 6                      1377793                     565371.3
    ##   low_red_meat_l_green_wf no_red_meat_kg_co2e_excl_luc
    ## 1                  812422                     1946.329
    ## 2                  812422                     1946.329
    ## 3                  812422                     1946.329
    ## 4                  812422                     1946.329
    ## 5                  812422                     1946.329
    ## 6                  812422                     1946.329
    ##   no_red_meat_kg_co2e_total no_red_meat_l_blue_green_wf
    ## 1                  1954.949                     1288915
    ## 2                  1954.949                     1288915
    ## 3                  1954.949                     1288915
    ## 4                  1954.949                     1288915
    ## 5                  1954.949                     1288915
    ## 6                  1954.949                     1288915
    ##   no_red_meat_l_blue_wf_total no_red_meat_l_green_wf
    ## 1                    568457.3               720457.5
    ## 2                    568457.3               720457.5
    ## 3                    568457.3               720457.5
    ## 4                    568457.3               720457.5
    ## 5                    568457.3               720457.5
    ## 6                    568457.3               720457.5
    ##   pescetarian_kg_co2e_excl_luc pescetarian_kg_co2e_total
    ## 1                     879.0195                  881.5167
    ## 2                     879.0195                  881.5167
    ## 3                     879.0195                  881.5167
    ## 4                     879.0195                  881.5167
    ## 5                     879.0195                  881.5167
    ## 6                     879.0195                  881.5167
    ##   pescetarian_l_blue_green_wf pescetarian_l_blue_wf_total
    ## 1                     1168046                    512937.3
    ## 2                     1168046                    512937.3
    ## 3                     1168046                    512937.3
    ## 4                     1168046                    512937.3
    ## 5                     1168046                    512937.3
    ## 6                     1168046                    512937.3
    ##   pescetarian_l_green_wf lacto_ovo_vegetarian_kg_co2e_excl_luc
    ## 1               655108.3                              2170.739
    ## 2               655108.3                              2170.739
    ## 3               655108.3                              2170.739
    ## 4               655108.3                              2170.739
    ## 5               655108.3                              2170.739
    ## 6               655108.3                              2170.739
    ##   lacto_ovo_vegetarian_kg_co2e_total lacto_ovo_vegetarian_l_blue_green_wf
    ## 1                            2178.37                              1308713
    ## 2                            2178.37                              1308713
    ## 3                            2178.37                              1308713
    ## 4                            2178.37                              1308713
    ## 5                            2178.37                              1308713
    ## 6                            2178.37                              1308713
    ##   lacto_ovo_vegetarian_l_blue_wf_total lacto_ovo_vegetarian_l_green_wf
    ## 1                             594591.9                        714121.1
    ## 2                             594591.9                        714121.1
    ## 3                             594591.9                        714121.1
    ## 4                             594591.9                        714121.1
    ## 5                             594591.9                        714121.1
    ## 6                             594591.9                        714121.1
    ##   X2.3_vegan_kg_co2e_excl_luc X2.3_vegan_kg_co2e_total
    ## 1                     753.634                 756.1502
    ## 2                     753.634                 756.1502
    ## 3                     753.634                 756.1502
    ## 4                     753.634                 756.1502
    ## 5                     753.634                 756.1502
    ## 6                     753.634                 756.1502
    ##   X2.3_vegan_l_blue_green_wf X2.3_vegan_l_blue_wf_total X2.3_vegan_l_green_wf
    ## 1                    1181193                   491555.1              689638.3
    ## 2                    1181193                   491555.1              689638.3
    ## 3                    1181193                   491555.1              689638.3
    ## 4                    1181193                   491555.1              689638.3
    ## 5                    1181193                   491555.1              689638.3
    ## 6                    1181193                   491555.1              689638.3
    ##   vegan_kg_co2e_excl_luc vegan_kg_co2e_total vegan_l_blue_green_wf
    ## 1               255.5508            255.5508               1120862
    ## 2               255.5508            255.5508               1120862
    ## 3               255.5508            255.5508               1120862
    ## 4               255.5508            255.5508               1120862
    ## 5               255.5508            255.5508               1120862
    ## 6               255.5508            255.5508               1120862
    ##   vegan_l_blue_wf_total vegan_l_green_wf
    ## 1              491242.5         629619.7
    ## 2              491242.5         629619.7
    ## 3              491242.5         629619.7
    ## 4              491242.5         629619.7
    ## 5              491242.5         629619.7
    ## 6              491242.5         629619.7

## Included countries

To distinguish the 123 included countries from within the 135 designated
countries within the `world_map_iso` data without available university
enrollment and dietary footprint data, we need to construct a new column
that can aid with this need,

``` r
impact_modeling_data %>%
  mutate(inclusion=ifelse(uni_enr_tot>0,country)) %>%
  ggplot(aes(x=long,y=lat,fill=inclusion,group=group)) + 
  geom_polygon(color="black",linewidth=0.05) +
  scale_fill_discrete(na.value="white") +
  labs(fill="") +
  xlab("") + 
  ylab("") +
  theme(legend.position="none",panel.grid=element_blank(),panel.background=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
impact_modeling_data %>%
  distinct(country)
```

    ##                                 country
    ## 1                           Afghanistan
    ## 2                               Albania
    ## 3                               Algeria
    ## 4                        American Samoa
    ## 5                               Andorra
    ## 6                                Angola
    ## 7                              Anguilla
    ## 8                            Antarctica
    ## 9                               Antigua
    ## 10                  Antigua and Barbuda
    ## 11                            Argentina
    ## 12                              Armenia
    ## 13                                Aruba
    ## 14                     Ascension Island
    ## 15                            Australia
    ## 16                              Austria
    ## 17                           Azerbaijan
    ## 18                               Azores
    ## 19                              Bahamas
    ## 20                              Bahrain
    ## 21                           Bangladesh
    ## 22                             Barbados
    ## 23                              Barbuda
    ## 24                              Belarus
    ## 25                              Belgium
    ## 26                               Belize
    ## 27                                Benin
    ## 28                              Bermuda
    ## 29                               Bhutan
    ## 30                              Bolivia
    ## 31                              Bonaire
    ## 32               Bosnia and Herzegovina
    ## 33                             Botswana
    ## 34                               Brazil
    ## 35                               Brunei
    ## 36                             Bulgaria
    ## 37                         Burkina Faso
    ## 38                              Burundi
    ## 39                             Cambodia
    ## 40                             Cameroon
    ## 41                               Canada
    ## 42                       Canary Islands
    ## 43                           Cape Verde
    ## 44                       Cayman Islands
    ## 45             Central African Republic
    ## 46                                 Chad
    ## 47                   Chagos Archipelago
    ## 48                                Chile
    ## 49                                China
    ## 50                     Christmas Island
    ## 51                        Cocos Islands
    ## 52                             Colombia
    ## 53                              Comoros
    ## 54                     Congo, Dem. Rep.
    ## 55                          Congo, Rep.
    ## 56                         Cook Islands
    ## 57                           Costa Rica
    ## 58                        Cote d'Ivoire
    ## 59                              Croatia
    ## 60                                 Cuba
    ## 61                              Curacao
    ## 62                               Cyprus
    ## 63                       Czech Republic
    ## 64                              Denmark
    ## 65                             Djibouti
    ## 66                             Dominica
    ## 67                   Dominican Republic
    ## 68                              Ecuador
    ## 69                                Egypt
    ## 70                          El Salvador
    ## 71                    Equatorial Guinea
    ## 72                              Eritrea
    ## 73                              Estonia
    ## 74                             Eswatini
    ## 75                             Ethiopia
    ## 76                     Falkland Islands
    ## 77                        Faroe Islands
    ## 78                                 Fiji
    ## 79                              Finland
    ## 80                               France
    ## 81                        French Guiana
    ## 82                     French Polynesia
    ## 83  French Southern and Antarctic Lands
    ## 84                                Gabon
    ## 85                               Gambia
    ## 86                              Georgia
    ## 87                              Germany
    ## 88                                Ghana
    ## 89                               Greece
    ## 90                            Greenland
    ## 91                              Grenada
    ## 92                           Grenadines
    ## 93                           Guadeloupe
    ## 94                                 Guam
    ## 95                            Guatemala
    ## 96                             Guernsey
    ## 97                               Guinea
    ## 98                        Guinea-Bissau
    ## 99                               Guyana
    ## 100                               Haiti
    ## 101                        Heard Island
    ## 102                            Honduras
    ## 103                             Hungary
    ## 104                             Iceland
    ## 105                               India
    ## 106                           Indonesia
    ## 107                                Iran
    ## 108                                Iraq
    ## 109                             Ireland
    ## 110                         Isle of Man
    ## 111                              Israel
    ## 112                               Italy
    ## 113                             Jamaica
    ## 114                               Japan
    ## 115                              Jersey
    ## 116                              Jordan
    ## 117                          Kazakhstan
    ## 118                               Kenya
    ## 119                            Kiribati
    ## 120                              Kosovo
    ## 121                     Krygyz Republic
    ## 122                              Kuwait
    ## 123                                 Lao
    ## 124                              Latvia
    ## 125                             Lebanon
    ## 126                             Lesotho
    ## 127                             Liberia
    ## 128                               Libya
    ## 129                       Liechtenstein
    ## 130                           Lithuania
    ## 131                          Luxembourg
    ## 132                          Madagascar
    ## 133                     Madeira Islands
    ## 134                              Malawi
    ## 135                            Malaysia
    ## 136                            Maldives
    ## 137                                Mali
    ## 138                               Malta
    ## 139                    Marshall Islands
    ## 140                          Martinique
    ## 141                          Mauritania
    ## 142                           Mauritius
    ## 143                             Mayotte
    ## 144                              Mexico
    ## 145               Micronesia, Fed. Sts.
    ## 146                             Moldova
    ## 147                              Monaco
    ## 148                            Mongolia
    ## 149                          Montenegro
    ## 150                          Montserrat
    ## 151                             Morocco
    ## 152                          Mozambique
    ## 153                             Myanmar
    ## 154                             Namibia
    ## 155                               Nauru
    ## 156                               Nepal
    ## 157                         Netherlands
    ## 158                               Nevis
    ## 159                       New Caledonia
    ## 160                         New Zealand
    ## 161                           Nicaragua
    ## 162                               Niger
    ## 163                             Nigeria
    ## 164                                Niue
    ## 165                      Norfolk Island
    ## 166                         North Korea
    ## 167                     North Macedonia
    ## 168            Northern Mariana Islands
    ## 169                              Norway
    ## 170                                Oman
    ## 171                            Pakistan
    ## 172                               Palau
    ## 173                           Palestine
    ## 174                              Panama
    ## 175                    Papua New Guinea
    ## 176                            Paraguay
    ## 177                                Peru
    ## 178                         Philippines
    ## 179                    Pitcairn Islands
    ## 180                              Poland
    ## 181                            Portugal
    ## 182                         Puerto Rico
    ## 183                               Qatar
    ## 184                             Reunion
    ## 185                             Romania
    ## 186                              Russia
    ## 187                              Rwanda
    ## 188                                Saba
    ## 189                    Saint Barthelemy
    ## 190                        Saint Helena
    ## 191                         Saint Kitts
    ## 192                        Saint Martin
    ## 193           Saint Pierre and Miquelon
    ## 194                       Saint Vincent
    ## 195                               Samoa
    ## 196                          San Marino
    ## 197               Sao Tome and Principe
    ## 198                        Saudi Arabia
    ## 199                             Senegal
    ## 200                              Serbia
    ## 201                          Seychelles
    ## 202                     Siachen Glacier
    ## 203                        Sierra Leone
    ## 204                           Singapore
    ## 205                      Sint Eustatius
    ## 206                        Sint Maarten
    ## 207                     Slovak Republic
    ## 208                            Slovenia
    ## 209                     Solomon Islands
    ## 210                             Somalia
    ## 211                        South Africa
    ## 212                       South Georgia
    ## 213                         South Korea
    ## 214              South Sandwich Islands
    ## 215                         South Sudan
    ## 216                               Spain
    ## 217                           Sri Lanka
    ## 218                 St. Kitts and Nevis
    ## 219                           St. Lucia
    ## 220      St. Vincent and the Grenadines
    ## 221                               Sudan
    ## 222                            Suriname
    ## 223                              Sweden
    ## 224                         Switzerland
    ## 225                               Syria
    ## 226                              Taiwan
    ## 227                          Tajikistan
    ## 228                            Tanzania
    ## 229                            Thailand
    ## 230                         Timor-Leste
    ## 231                              Tobago
    ## 232                                Togo
    ## 233                               Tonga
    ## 234                            Trinidad
    ## 235                 Trinidad and Tobago
    ## 236                             Tunisia
    ## 237                              Turkey
    ## 238                        Turkmenistan
    ## 239            Turks and Caicos Islands
    ## 240                              Uganda
    ## 241                             Ukraine
    ## 242                United Arab Emirates
    ## 243                      United Kingdom
    ## 244                       United States
    ## 245                             Uruguay
    ## 246                          Uzbekistan
    ## 247                             Vanuatu
    ## 248                             Vatican
    ## 249                           Venezuela
    ## 250                             Vietnam
    ## 251               Virgin Islands (U.S.)
    ## 252                   Wallis and Futuna
    ## 253                      Western Sahara
    ## 254                               Yemen
    ## 255                              Zambia
    ## 256                            Zimbabwe
    ## 257                    Hong Kong, China
    ## 258                        Macao, China

Comparing university enrollment totals across countries.

``` r
ggplot(impact_modeling_data,aes(x=long,y=lat,fill=uni_enr_tot,group=group)) + 
  geom_polygon(color="black",linewidth=0.05) + 
  scale_fill_distiller(palette="Purples",trans="reverse",na.value="white") +
  labs(fill="") +
  xlab("") + 
  ylab("") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Comparing university enrollment proportions (university enrollees as a
function of the national population) across countries.

``` r
ggplot(impact_modeling_data,aes(x=long,y=lat,fill=uni_enr_prop,group=group)) + 
  geom_polygon(color="black",linewidth=0.05) + 
  scale_fill_distiller(palette="Purples",trans="reverse",na.value="white") +
  labs(fill="") +
  xlab("") + 
  ylab("") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Comparing per capita baseline carbon footprint (kg co2e) across
countries

``` r
ggplot(impact_modeling_data,aes(x=long,y=lat,fill=baseline_kg_co2e_total,group=group)) + 
  geom_polygon(color="black",linewidth=0.05) + 
  scale_fill_distiller(palette="Oranges",trans="reverse",na.value="white") +
  labs(fill="") +
  xlab("") + 
  ylab("") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
