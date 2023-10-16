
## parameters
source("R/parameters.R")

# define range
sheet <- "3_ATSP"

range <- "C8:M9"
ert_2_1  <- read_range(file, sheet, range)

range <- "C11:M22"
ert_2_10  <- read_range(file, sheet, range)

range <- "C66:M71"
ert_2_13  <- read_range(file, sheet, range)

#---------------- sections definition
  # section 2.10
ceff_2_10 <- paste0('## En route main ANSP(', main_ansp, ')
### 10 Monitoring of the en route ANSPs regulatory results (RR)
', ert_2_10[1,1]
)                

# section 2.11
ceff_2_11 <- paste0(
  '\n\n### 11 Net gain/loss for the main ANSP for the en route activity at charging zone level

```{r}
#| file: R/table_ceff_11.R
#| out.width: "100%"
```

'
)

# section 2.12
ceff_2_12 <- paste0(
  '\n\n### 12 Regulatory result (RR) for the main ANSP at charging zone level

```{r}
#| file: R/table_ceff_12.R
#| out.width: "100%"
```

'
)

# section 2.13
ceff_2_13 <- paste0(
  '\n\n### 13 Focus on the main ANSP regulatory result on en route activity

:::: {.columns}
  
::: {.column width="50%"}

![](images/2022/', country, '/3_ATSP_1.png)

:::

::: {.column width="50%"}

![](images/2022/', country, '/3_ATSP_2.png)

:::

::::
<p style = "margin-bottom: 0px;">**', ert_2_13[1,1], '**</p>

', ert_2_13[2,1], '

<p style = "margin-bottom: 0px;">**', ert_2_13[3,1], '**</p>

', ert_2_13[4,1]
  
)

                   
# assemble all and create .qmd
cat(paste0(
  ceff_2_10,
  ceff_2_11,
  ceff_2_12,
  ceff_2_13
  
),
    file = "cost-efficiency-er1-2.qmd")

