library(pruatlas)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(sf)
#> Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE
library(ggplot2)
library(stringr)
library(readr)
library(purrr)

firs <- country_fir(pruatlas::firs_nm_406,
                    icao_id = "EB|ED|EE|EF|EH|EI|EK|EN|EP|ES|EV|EY|GC|LB|LC|LD|LE|LF|LG|LH|LI|LJ|LK|LM|LO|LP|LR|LS|LZ",
                    exclude = c("LPPOFIR"),
                    merge = FALSE,
                    fl = 200)

g <- plot_country_fir(firs = firs,
  icao_id = "E.|L.|UD|UG|UK|GC",
  name = "EUROCONTROL",
                 buffer = 0,
                  fl = 200)


g <- g + 
  theme(title = element_blank(),
        plot.margin = margin(-4, 0, -3, 0, "cm"))

g


