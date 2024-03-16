
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

if (state_type == 3) {state_type = 1}

# define range
sheet <- c("7_TRM_ATSP", "7_TRM_ATSP (2)")


for (tz in 1:state_type) {
  # define ranges
  range <- "C8:C9"
  trm_2_1  <- read_range(file, sheet[tz], range)
  
  range <- "C11:M22"
  trm_2_10  <- read_range(file, sheet[tz], range)
  
  range <- "C66:M71"
  trm_2_13  <- read_range(file, sheet[tz], range)
    
#---------------- sections definition
  # section 2.10
ceff_5_10 <- paste0('## Terminal charging zone ', 
if_else(state_type == 2, as.character(tz), ""),' - main ANSP (', main_ansp, ')
### 10 Monitoring of the terminal ANSPs regulatory results (RR)
', trm_2_10[1,1]
)                

# section 2.11
ceff_5_11 <- paste0(
  '\n\n### 11 Net gain/loss for the main ANSP for the terminal activity at charging zone level

```{r, options=(tz = ', tz, ')}
#| file: R/table_ceff_tz_11.R
#| out.width: "100%"
```

'
)

# section 2.12
ceff_5_12 <- paste0(
  '\n\n### 12 Regulatory result (RR) for the main ANSP at charging zone level

```{r, options=(tz = ', tz, ')}
#| file: R/table_ceff_tz_12.R
#| out.width: "100%"
```

'
)

# section 2.13
ceff_5_13 <- paste0(
  '\n\n### 13 Focus on the main ANSP regulatory result on terminal activity

:::: {.columns}
  
::: {.column width="50%"}

![](images/2022/', country, '/7_TRM_ATSP_1.png)

:::

::: {.column width="50%"}

![](images/2022/', country, '/7_TRM_ATSP_2.png)

:::

::::
<p style = "margin-bottom: 0px;">**', trm_2_13[1,1], '**</p>

', trm_2_13[2,1], '

<p style = "margin-bottom: 0px;">**', trm_2_13[3,1], '**</p>

', trm_2_13[4,1]
  
)

                   
# assemble all and create .qmd
cat(paste0(
  ceff_5_10,
  ceff_5_11,
  ceff_5_12,
  ceff_5_13
  
),
  file = paste0("cost-efficiency-tz", tz, "-2.qmd")
)
}
