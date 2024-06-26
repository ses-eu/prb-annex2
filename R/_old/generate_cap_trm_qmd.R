
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

# define ranges and import data
sheet <- country

range <- "A3:U4"
cap_trm_1_1_r <- read_range(cap_trm_file, sheet, range) 

range <- "A5:U7"
cap_trm_1_2_r <- read_range(cap_trm_file, sheet, range) 

range <- "A8:U10"
cap_trm_1_3_r <- read_range(cap_trm_file, sheet, range)  

range <- "A11:U13"
cap_trm_1_4_r <- read_range(cap_trm_file, sheet, range) 

range <- "A14:U15"
cap_trm_1_5_r <- read_range(cap_trm_file, sheet, range)  

range <- "A16:U17"
cap_trm_1_6_r <- read_range(cap_trm_file, sheet, range)  

range <- "A18:U19"
cap_trm_1_7_r <- read_range(cap_trm_file, sheet, range)  

#---------------- sections definition
  # section cap_trm 1.1
cap_trm_1_1 <- paste0(
'## Airports
### Overview
', cap_trm_1_1_r[1,1], '
'
)                

# section cap_trm 1.2
cap_trm_1_2 <- if_else(is.na(cap_trm_1_2_r[1,11]) == TRUE, 
                       paste0('\n\n### Arrival ATFM Delay
![](images/2022/', country, '/cap_trm_3_1.png)
', cap_trm_1_2_r[2,1], ' 
                      
' 
),
paste0('\n\n### Arrival ATFM Delay
:::: {.columns}
  
::: {.column width="48%" }
![](images/2022/', country, '/cap_trm_3_1.png)
:::

::: {.column width="2%" }
:::

::: {.column width="50%" }
', cap_trm_1_2_r[1,11]," 
:::
::::
"
), if_else(is.na(cap_trm_1_2_r[2,1]) == TRUE, "", pull(cap_trm_1_2_r[2,1])
) 
)

# section cap_trm 1.3
cap_trm_1_3 <- paste0('\n\n### Arrival ATFM Delay – National Target
:::: {.columns}
  
::: {.column width="48%"}
![](images/2022/', country, '/cap_trm_2_1.png)
:::

::: {.column width="2%" }
:::

::: {.column width="50%"}
', if_else(is.na(cap_trm_1_3_r[1,10]) == TRUE, "", pull(cap_trm_1_3_r[1,10]))
, if_else(is.na(cap_trm_1_3_r[1,11]) == TRUE, "", pull(cap_trm_1_3_r[1,11]))," 
:::
::::
"
)

# section cap_trm 1.4
cap_trm_1_4 <- if_else(is.na(cap_trm_1_4_r[1,11]) == TRUE, 
                       paste0('\n\n### ATFM Slot Adherence
![](images/2022/', country, '/cap_trm_1_1.png)
', cap_trm_1_4_r[2,1], ' 
                      
' 
),
paste0('\n\n### ATFM Slot Adherence
:::: {.columns}
  
::: {.column width="48%"}
![](images/2022/', country, '/cap_trm_1_1.png)
:::

::: {.column width="2%" }
:::

::: {.column width="50%"}
', if_else(is.na(cap_trm_1_4_r[1,10]) == TRUE, "", pull(cap_trm_1_4_r[1,10]))
, if_else(is.na(cap_trm_1_4_r[1,11]) == TRUE, "", pull(cap_trm_1_4_r[1,11])), 
',
:::
::::
'
), if_else(is.na(cap_trm_1_4_r[2,1]) == TRUE, "", pull(cap_trm_1_4_r[2,1])
) 
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
![](images/2022/', country, '/cap_trm_4.png)

'
) 

# assemble all and create .qmd

cat(paste0(
  cap_trm_1_1,
  cap_trm_1_2,
  cap_trm_1_3,
  cap_trm_1_4,
  cap_trm_1_5,
  cap_trm_1_6
  , cap_trm_1_7
),
    file = "_capacity_trm.qmd")

