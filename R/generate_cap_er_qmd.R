
## parameters
source("R/parameters.R")

# define ranges and import data
sheet <- country

range <- "J4:J6"
cap_er_1_1_r <- read_range(cap_file, sheet, range)  

range <- "A9:O13"
cap_er_1_2_r <- read_range(cap_file, sheet, range)  

range <- "A14:O18"
cap_er_1_3_r <- read_range(cap_file, sheet, range)  

range <- "A19:O23"
cap_er_1_4_r <- read_range(cap_file, sheet, range)  

range <- "A49:O53"
cap_er_1_6_r <- read_range(cap_file, sheet, range)  

range <- "A54:O58"
cap_er_1_7_r <- read_range(cap_file, sheet, range)  

#---------------- sections definition
  # section cap_er 1.1
cap_er_1_1 <- paste0('[3 Capacity]{class="fakeh1"}

## En route
### Minutes of ATFM en-route delay

:::: {.columns}
  
::: {.column width="55%"}

```{r}
#| file: R/table_cap_er_1.R
#| out.width: "100%"
```
:::

::: {.column width="1%"}
:::

::: {.column width="44%"}

**Observations**
', cap_er_1_1_r[1,1], '
:::
::::
')                

# section cap_er 1.2
cap_er_1_2 <- paste0("\n\n### NSA's assessment of capacity performance
", cap_er_1_2_r[1,1]
)

# section cap_er 1.3
cap_er_1_3 <- paste0("\n\n### Monitoring process for capacity performance
", cap_er_1_3_r[1,1]
)

# section cap_er 1.4
cap_er_1_4 <- paste0("\n\n### Capacity Planning
", cap_er_1_4_r[1,1]
)

# section cap_er 1.5
cap_er_1_5 <- paste0('\n\n### ATCO in OPS (FTE)

```{r}
#| file: R/table_cap_er_2.R
```
```{r}
htmltools::tagList(t)
```

'
)

# section cap_er 1.6
cap_er_1_6 <- paste0("\n\n### Application of Corrective Measures for Capacity (if applicable)
", cap_er_1_6_r[1,1]
)

# section cap_er 1.7
cap_er_1_7 <- paste0("\n\n### Summary of capacity performance
", cap_er_1_7_r[1,1]
)
                   
# assemble all and create .qmd
cat(paste0(
  cap_er_1_1,
  cap_er_1_2,
  cap_er_1_3,
  cap_er_1_4,
  cap_er_1_5,
  cap_er_1_6,
  cap_er_1_7
),
    file = "capacity_er.qmd")

