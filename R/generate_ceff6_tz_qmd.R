
## parameters
source("R/parameters.R")

# define range
sheet <- "8_TRM_ATSP"

range <- "C66:M71"
trm_2_14_e  <- read_range(file, sheet, range)

#---------------- sections definition
  # section 2.14[i]
ceff_6_14_1 <- paste0('## 4.6 Other terminal ANSPs/METSPs
### Other ANSP(s) / METSP(s) regulatory results for terminal activity

```{r}
#| file: R/table_ceff_tz_14.R
#| out.width: "100%"
```

'
)                

# section 2.14.e
ceff_6_14_e <- paste0('\n<p style = "margin-bottom: 0px;">**', 
str_replace(trm_2_14_e[1,1], "\n", ""), '**</p>
', trm_2_14_e[2,1]

)

                   
# assemble all and create .qmd
cat(paste0(
  ceff_6_14_1,
  ceff_6_14_e

  
),
    file = "cost-efficiency-tz1-3.qmd")

