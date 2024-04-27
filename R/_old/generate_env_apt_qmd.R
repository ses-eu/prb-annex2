
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

# define ranges and import data
sheet <- country

range <- "A3:P4"
env_apt_1_r <- read_range(env_apt_file, sheet, range)  

range <- "A5:P7"
env_apt_2_r <- read_range(env_apt_file, sheet, range) 

range <- "A8:P10"
env_apt_3_r <- read_range(env_apt_file, sheet, range) 

range <- "A11:P13"
env_apt_4_r <- read_range(env_apt_file, sheet, range) 

range <- "A14:P15"
env_apt_5_r <- read_range(env_apt_file, sheet, range) 

#---------------- sections definition
  # section 1
env_apt_1 <- paste0(
'## Airports
### Overview

', env_apt_1_r[1,1]   
)                

# section 2
env_apt_2 <- if_else(is.na(env_apt_2_r[1,1]) == FALSE,

paste0('\n\n### Additional Taxi-Out Time
', env_apt_2_r[1,1], '
'),

paste0('\n\n### Additional Taxi-Out Time															

:::: {.columns}
  
::: {.column width="48%"}
<br>
![](images/2022/', country, '/env_apt_1.png)

:::

::: {.column width="2%"}
:::

::: {.column width="50%"}
<br>
', env_apt_2_r[1,8], '   

:::
::::
                      
'
)
)

# section 3
env_apt_3 <- if_else(is.na(env_apt_3_r[1,1]) == FALSE,
                     
paste0('\n\n### Additional ASMA Time
', env_apt_3_r[1,1], '
'),

paste0('\n### Additional ASMA Time															

:::: {.columns}
  
::: {.column width="48%"}
<br>
![](images/2022/', country, '/env_apt_2.png)

:::

::: {.column width="2%"}
:::

::: {.column width="50%"}
<br>
', env_apt_3_r[1,8], '   

:::
::::
                      
' 
)
)

# section 4
env_apt_4 <- if_else(is.na(env_apt_4_r[1,8]) == TRUE, 
              paste0('\n\n### Share of arrivals applying CDO
<br>
![](images/2022/', country, if_else(is.na(env_apt_3_r[1,1]) == FALSE, 
                                    '/env_apt_1.png)', 
                                    '/env_apt_3.png)')
, env_apt_4_r[2,1], ' 
                      
' 
),
paste0('\n\n### Share of arrivals applying CDO

:::: {.columns}
  
::: {.column width="48%"}
<br>
![](images/2022/', country, if_else(is.na(env_apt_3_r[1,1]) == FALSE, 
                                    '/env_apt_1.png)', 
                                    '/env_apt_3.png)')
,
'
:::

::: {.column width="2%"}
:::

::: {.column width="50%"}
<br>
', env_apt_4_r[1,8], '   
:::
::::

', if_else(is.na(env_apt_4_r[2,1]) == TRUE, "", pull(env_apt_4_r[2,1])), ' 
                      
' 
)
)
# section 5
env_apt_5 <- paste0('\n### Appendix

', env_apt_5_r[1,1], '   

![](images/2022/', country, '/env_apt_4.png)

' 
)

# assemble all and create .qmd
cat(paste0(
  env_apt_1,
  env_apt_2,
  env_apt_3,
  env_apt_4,
  env_apt_5
),
    file = "_environment_apt.qmd")

