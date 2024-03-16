
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

if (state_type == 3) {state_type = 1}

# define sheet
sheet <- c("5_TRM", "5_TRM (2)")


for (tz in 1:state_type) {

#---------------- sections definition
  # define ranges
  range <- "C8:C9"
  trm_1_1  <- read_range(file, sheet[tz], range)
  
  range <- "C16:M17"
  trm_1_2  <- read_range(file, sheet[tz], range)
  
  ## range 1.4
  range <- "C50:M71"
  trm_1_4  <- read_range(file, sheet[tz], range)
  
  ## range 1.6
  sheet <- c("6_TRM", "6_TRM (2)")
  range <- "J16:M33"
  trm_1_6  <- read_range(file, sheet[tz], range)
  
  range <- "C63:M70"
  trm_1_9  <- read_range(file, sheet[tz], range)  
  
  
    # section 1.1
ceff_4_1 <- paste0('## Terminal charging zone ', 
if_else(state_type == 2, as.character(tz), ""),'
### 1 Contextual economic information: terminal air navigation services

```{r, options=(tz = ', tz, ')}
#| file: R/table_ceff_tz_0.R
#| out.width: "100%"
```  
'
)

  # section 1.2
ceff_4_2 <- paste0(
  '\n\n### 2 Monitoring of the terminal determined unit cost (DUC) at charging zone level
  ', trm_1_2[1,1]
  )

  # section 1.3.1
ceff_4_3 <- paste0(
'\n\n### 3 Terminal actual unit cost (AUC) vs. terminal determined unit cost (DUC)

```{r, options=(tz = ', tz, ')}
#| file: R/table_ceff_tz_3_1.R
#| out.width: "100%"
```  
```{r, options=(tz = ', tz, ')}
#| file: R/table_ceff_tz_3_2.R
#| out.width: "100%"
```
```{r, options=(tz = ', tz, ')}
#| file: R/table_ceff_tz_3_3.R
#| out.width: "100%"
```  

')

# section 1.4
ceff_4_4 <- paste0(
  '\n\n### 4 Focus on terminal DUC monitoring at charging zone level
  
:::: {.columns}
  
::: {.column width="55%"}
  
<p style="margin-bottom: 0rem;">**', trm_1_4[1,1], '**</p>',
'\n\n', trm_1_4[2,1],
'\n\n<p style="margin-bottom: 0rem;">**', trm_1_4[5,1], '**</p>',
'\n\n', trm_1_4[6,1],
'\n\n<p style="margin-bottom: 0rem;">**', trm_1_4[9,1], '**</p>',
'\n\n', trm_1_4[10,1],
'\n\n<p style="margin-bottom: 0rem;">**', trm_1_4[13,1], '**</p>',
'\n\n', trm_1_4[14,1],
'\n\n:::
  
::: {.column width="45%"}
![](images/2022/', country, '/5_TRM_3.png)

![](images/2022/', country, '/5_TRM_1.png)


![](images/2022/', country, '/5_TRM_2.png)
:::
  
::::

'
)

# section 1.5
ceff_4_5 <- paste0(
  '\n\n### 5 Monitoring of the terminal actual unit cost for users (AUCU) at charging zone level
The **Actual Unit Cost for Users(AUCU)** reflects the price per service units that is charged *in fine* to users for the services provided in the year. It corresponds to the sum of the DUC for the year and of the different adjustments stemming from that year.
The monitoring of the AUCU is carried out in national currency in nominal terms.
')

# section 1.6
ceff_4_6 <- paste0(
  '\n\n### 6 Terminal actual unit cost for users (AUCU) at charging zone level
  
:::: {.columns}
  
::: {.column width="61%"}

![](images/2022/', country, '/6_TRM_1.png)

<small>* The traffic adjustment on adjustments is not considered to avoid double counting, as the related adjustments have already been taken into account in full in the AUCU for the current year or previous years.</small><br><small>** The difference in revenue due to the application of the temporary unit rates in 2022, if applicable, is already reflected in the DUC (part to be charged retroactively) and is therefore not considered in the total adjustments, in order to avoid double counting.</small>

:::

::: {.column width="1%"}
:::

::: {.column width="38%"}

```{r, options=(tz = ', tz, ')}
#| file: R/table_ceff_tz_6.R
#| out.width: "100%"
```  
:::

::::
'
)

# section 1.7
ceff_4_7 <- paste0(
  '\n\n### 7 Terminal costs exempt from cost sharing

```{r, options=(tz = ', tz, ')}
#| file: R/table_ceff_tz_7.R
#| out.width: "100%"
```  
Source: These data are taken from the June 2023 terminal Reporting Tables (for Eurocontrol costs and costs of competent authorities and qualified entities) and from the “NSA Report on the verification of cost risk sharing for the year 2022” submitted in accordance with Article 28 (7) of Regulation (EU) 2019/317 (for ANSPs costs).										

'
)

# section 1.7
ceff_4_8 <- paste0(
  '\n\n### 8 Terminal regulatory result at charging zone level

:::: {.columns}
  
::: {.column width="45%"}

![](images/2022/', country, '/6_TRM_2.png)

:::

::: {.column width="55%"}

```{r, options=(tz = ', tz, ')}
#| file: R/table_ceff_tz_8.R
#| out.width: "100%"
```

<small>*** before deduction of other revenues, as is the case for the regulatory results (see items 10 to 14)</small>
:::

::::
'
)

# section 1.7
ceff_4_9 <- paste0(
  '\n\n### 9 Focus on terminal AUCU monitoring at charging zone level
', trm_1_9[1,1]
)

# assemble all and create .qmd
cat(paste0(
  ceff_4_1,
  ceff_4_2,
  ceff_4_3,
  ceff_4_4,
  ceff_4_5,
  ceff_4_6,
  ceff_4_7,
  ceff_4_8,
  ceff_4_9
),
  file = paste0("cost-efficiency-tz", tz, "-1.qmd")
)

}