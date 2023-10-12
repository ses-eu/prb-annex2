
## parameters
source("R/parameters.R")

# define range
sheet <- "7_TRM_ATSP"

range <- "C11:M22"
trm_2_10  <- read_range(file, sheet, range)

range <- "C66:M71"
trm_2_13  <- read_range(file, sheet, range)

#---------------- sections definition
  # section 2.10
ceff_5_10 <- paste0('## 4.5 Terminal main ANSP (',
main_ansp,') 
### Monitoring of the terminal ANSPs regulatory results (RR)
', trm_2_10[1,1]
)                

# section 2.11
ceff_5_11 <- paste0(
  '\n\n### Net gain/loss for the main ANSP for the terminal activity at charging zone level

```{r}
#| file: R/table_ceff_tz_11.R
#| out.width: "100%"
```

'
)

# section 2.12
ceff_5_12 <- paste0(
  '\n\n### Regulatory result (RR) for the main ANSP at charging zone level

```{r}
#| file: R/table_ceff_tz_12.R
#| out.width: "100%"
```

'
)

# section 2.13
ceff_5_13 <- paste0(
  '\n\n### Focus on the main ANSP regulatory result on terminal activity

:::: {.columns}
  
::: {.column width="50%"}

![](images/2022/', country, '/ceff_5_13_1.png)

:::

::: {.column width="50%"}

![](images/2022/', country, '/ceff_5_13_2.png)

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
    file = "cost-efficiency5-tz.qmd")

