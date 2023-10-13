
## parameters
source("R/parameters.R")

# define range
sheet <- "1_ERT"

range <- "C11:M15"
ert_1_1  <- read_range(file, sheet, range)

range <- "C16:M17"
ert_1_2  <- read_range(file, sheet, range)

## range 1.4
range <- "C50:M71"
ert_1_4  <- read_range(file, sheet, range)

## range 1.6
sheet <- "2_ERT"
range <- "J16:M33"
ert_1_6  <- read_range(file, sheet, range)

## range 1.9
sheet <- "2_ERT"
range <- "C63:M70"
ert_1_9  <- read_range(file, sheet, range)

#---------------- sections definition
  # section 1.1
ceff_1_1 <- paste0('[4 Cost-efficiency]{class="fakeh1"}

## 4.1 En route charging zone
### Contextual economic information: en route air navigation services

```{r}
#| file: R/table_ceff_er_0.R
#| out.width: "100%"
```  
'
)

  # section 1.2
ceff_1_2 <- paste0(
  '\n\n### Monitoring of the en route determined unit cost (DUC) at charging zone level
  ', ert_1_2[1,1]
  )

  # section 1.3.1
ceff_1_3 <- paste0(
'\n\n### En route actual unit cost (AUC) vs. en route determined unit cost (DUC)

```{r}
#| file: R/table_ceff_3_1.R
#| out.width: "100%"
```  
```{r}
#| file: R/table_ceff_3_2.R
#| out.width: "100%"
```
```{r}
#| file: R/table_ceff_3_3.R
#| out.width: "100%"
```  

')

# section 1.4
ceff_1_4 <- paste0(
  '\n\n### Focus on en route DUC monitoring at charging zone level
  
:::: {.columns}
  
::: {.column width="55%"}
  
<p style="margin-bottom: 0rem;">**', ert_1_4[1,1], '**</p>',
'\n\n', ert_1_4[2,1],
'\n\n<p style="margin-bottom: 0rem;">**', ert_1_4[5,1], '**</p>',
'\n\n', ert_1_4[6,1],
'\n\n<p style="margin-bottom: 0rem;">**', ert_1_4[9,1], '**</p>',
'\n\n', ert_1_4[10,1],
'\n\n<p style="margin-bottom: 0rem;">**', ert_1_4[13,1], '**</p>',
'\n\n', ert_1_4[14,1],
'\n\n:::
  
::: {.column width="45%"}
![](images/2022/', country, '/1_ERT_3.png)

![](images/2022/', country, '/1_ERT_1.png)


![](images/2022/', country, '/1_ERT_2.png)
:::
  
::::

{{< pagebreak >}}'
)

# section 1.5
ceff_1_5 <- paste0(
  '\n\n### Monitoring of the en route actual unit cost for users (AUCU) at charging zone level
The **Actual Unit Cost for Users(AUCU)** reflects the price per service units that is charged *in fine* to users for the services provided in the year. It corresponds to the sum of the DUC for the year and of the different adjustments stemming from that year.
The monitoring of the AUCU is carried out in national currency in nominal terms.
')

# section 1.6
ceff_1_6 <- paste0(
  '\n\n### En route actual unit cost for users (AUCU) at charging zone level
  
:::: {.columns}
  
::: {.column width="61%"}

![](images/2022/', country, '/2_ERT_1.png)

<small>* The traffic adjustment on adjustments is not considered to avoid double counting, as the related adjustments have already been taken into account in full in the AUCU for the current year or previous years.</small><br><small>** The difference in revenue due to the application of the temporary unit rates in 2022, if applicable, is already reflected in the DUC (part to be charged retroactively) and is therefore not considered in the total adjustments, in order to avoid double counting.</small>

:::

::: {.column width="1%"}
:::

::: {.column width="38%"}

```{r}
#| file: R/table_ceff_6.R
#| out.width: "100%"
```  
:::

::::
'
)

# section 1.7
ceff_1_7 <- paste0(
  '\n\n### En route costs exempt from cost sharing

```{r}
#| file: R/table_ceff_7.R
#| out.width: "100%"
```  
Source: These data are taken from the June 2023 en route Reporting Tables (for Eurocontrol costs and costs of competent authorities and qualified entities) and from the “NSA Report on the verification of cost risk sharing for the year 2022” submitted in accordance with Article 28 (7) of Regulation (EU) 2019/317 (for ANSPs costs).										

'
)

# section 1.7
ceff_1_8 <- paste0(
  '\n\n### En route regulatory result at charging zone level

:::: {.columns}
  
::: {.column width="45%"}

![](images/2022/', country, '/2_ERT_2.png)

:::

::: {.column width="55%"}

```{r}
#| file: R/table_ceff_8.R
#| out.width: "100%"
```

<small>*** before deduction of other revenues, as is the case for the regulatory results (see items 10 to 14)</small>
:::

::::
'
)

# section 1.7
ceff_1_9 <- paste0(
  '\n\n### Focus on en route AUCU monitoring at charging zone level
', ert_1_9[1,1]
)

# assemble all and create .qmd
cat(paste0(
  ceff_1_1,
  ceff_1_2,
  ceff_1_3,
  ceff_1_4,
  ceff_1_5,
  ceff_1_6,
  ceff_1_7,
  ceff_1_8,
  ceff_1_9
),
    file = "cost-efficiency.qmd")

