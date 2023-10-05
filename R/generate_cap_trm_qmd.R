#libraries
library(dplyr)
library(readxl)
library(openxlsx)
library(data.table)
library(tidyr)
library(stringr)

## parameters
source("R/parameters.R")

# define ranges and import data
sheet <- country

range <- "A3:U4"
cap_trm_1_1_r <- read_range(cap_trm_file, sheet, range)  %>% 
  mutate_all(~ str_replace_all(., "\r\n\r\n", "\r\n"))

range <- "A5:U6"
cap_trm_1_2_r <- read_range(cap_trm_file, sheet, range) 

range <- "A8:U9"
cap_trm_1_3_r <- read_range(cap_trm_file, sheet, range)  %>% 
  mutate_all(~ str_replace_all(., "\r\n\r\n", "\r\n"))

range <- "A11:U12"
cap_trm_1_4_r <- read_range(cap_trm_file, sheet, range)  %>% 
  mutate_all(~ str_replace_all(., "\r\n\r\n", "\r\n"))

range <- "A14:U15"
cap_trm_1_5_r <- read_range(cap_trm_file, sheet, range)  %>% 
  mutate_all(~ str_replace_all(., "\r\n\r\n", "\r\n"))

range <- "A16:U17"
cap_trm_1_6_r <- read_range(cap_trm_file, sheet, range)  %>% 
  mutate_all(~ str_replace_all(., "\r\n\r\n", "\r\n"))

range <- "A18:U19"
cap_trm_1_7_r <- read_range(cap_trm_file, sheet, range)  %>% 
  mutate_all(~ str_replace_all(., "\r\n\r\n", "\r\n"))

#---------------- sections definition
  # section cap_trm 1.1
cap_trm_1_1 <- paste0('## 3.2 Airports {.unnumbered}
### Overview
', cap_trm_1_1_r[1,1], '
'
)                

# section cap_trm 1.2
cap_trm_1_2 <- paste0('\n\n### Arrival ATFM Delay
:::: {.columns}
  
::: {.column width="48%" }
![](images/2022/', country, '/cap_trm_2_1.png)
:::

::: {.column width="2%" }
:::

::: {.column width="50%" }
', cap_trm_1_2_r[1,11]," 
:::
::::
"
)

# section cap_trm 1.3
cap_trm_1_3 <- paste0('\n\n### Arrival ATFM Delay – National Target
:::: {.columns}
  
::: {.column width="48%"}
![](images/2022/', country, '/cap_trm_3_1.png)
:::

::: {.column width="2%" }
:::

::: {.column width="50%"}
', cap_trm_1_3_r[1,11]," 
:::
::::
"
)

# section cap_trm 1.4
cap_trm_1_4 <- paste0('\n\n### ATFM Slot Adherence
:::: {.columns}
  
::: {.column width="48%"}
![](images/2022/', country, '/cap_trm_4_1.png)
:::

::: {.column width="2%" }
:::

::: {.column width="50%"}
', cap_trm_1_4_r[1,11]," 
:::
::::
"
)

# section cap_trm 5.1
cap_trm_1_5 <- paste0('\n\n### ATC Pre-departure Delay
', cap_trm_1_5_r[1,1], '
'
) 

# section cap_trm 6.1
cap_trm_1_6 <- paste0('\n\n### All Causes Pre-departure Delay
', cap_trm_1_6_r[1,1], '
'
) 

# section cap_trm 7.1
cap_trm_1_7 <- paste0('\n\n### Appendix
', cap_trm_1_7_r[1,1], '
![](images/2022/', country, '/cap_trm_7_1.png)

'
) 

# assemble all and create .qmd

cat(paste0(
  cap_trm_1_1,
  cap_trm_1_2,
  cap_trm_1_3,
  cap_trm_1_4,
  cap_trm_1_5,
  cap_trm_1_6,
  cap_trm_1_7
),
    file = "capacity_trm.qmd")

