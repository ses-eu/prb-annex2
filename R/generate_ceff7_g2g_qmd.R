
## parameters
source("R/parameters.R")

# define range
sheet <- "9_G2G"

range <- "I29:M43"
g2g_2  <- read_range(file, sheet, range)

range <- "C57:M71"
g2g_3  <- read_range(file, sheet, range)



#---------------- sections definition
  # section 
ceff_7_1 <- paste0('## 4.7 Gate-to-gate
### Monitoring of gate-to-gate ANS costs

```{r}
#| file: R/table_ceff_g2g_0.R
#| out.width: "100%"
```  
<br>
```{r}
#| file: R/table_ceff_g2g_1.R
#| out.width: "100%"
```  

```{r}
#| file: R/table_ceff_g2g_2.R
#| out.width: "100%"
```  

```{r}
#| file: R/table_ceff_g2g_3.R
#| out.width: "100%"
```  

'
)

  # section 
ceff_7_2 <- paste0(
'\n\n### Share of en route and terminal in gate-to-gate actual costs (', year_report, ')

:::: {.columns}
  
::: {.column width="50%"}
![](images/2022/', country, '/ceff_g2g_1.png)
:::

::: {.column width="2%"}
:::

::: {.column width="48%"}
', g2g_2[1,1],'
:::

::::

')

# section 
ceff_7_3 <- paste0(
  '### Gate-to-gate regulatory result (RR) 

```{r}
#| file: R/table_ceff_g2g_4.R
#| out.width: "100%"
```  
<br>

:::: {.columns}
  
::: {.column width="50%"}
![](images/2022/', country, '/ceff_g2g_2.png)
:::

::: {.column width="2%"}
:::

::: {.column width="48%"}
', g2g_3[1,1],'
:::

::::

'
)


# assemble all and create .qmd
cat(paste0(
  ceff_7_1,
  ceff_7_2,
  ceff_7_3
),
    file = "cost-efficiency7-g2g.qmd")

