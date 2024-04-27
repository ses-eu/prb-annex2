

# define ranges and import data
sheet <- country

range <- "A10:M18"
env_kea_3_r <- read_range(env_kea_file, sheet, range)  

#---------------- sections definition
  # section cap_er 1.1
env_kea_1 <- paste0(
'[2 Environment]{class="fakeh1"}

## Horizontal flight efficiency 
### KEA

```{r}
#| file: R/chart_level1_kea.R
#| out.width: "100%"
```

')                

# section cap_er 1.2
env_kea_2 <- paste0('\n### End of month indicators evolution in ', year_report,'												

```{r}
#| file: R/table_env_kea_2.R
#| out.width: "100%"
```
<br>

:::: {.columns}
  
::: {.column width="50%"}

![](images/2022/', country, '/env_kea_2.png)

:::

::: {.column width="50%"}

![](images/2022/', country, '/env_kea_4.png)

:::
::::
                      
', env_kea_3_r[7,1]                  
)


# assemble all and create .qmd
cat(paste0(
  env_kea_1,
  env_kea_2
),
    file = "_environment_kea.qmd")

