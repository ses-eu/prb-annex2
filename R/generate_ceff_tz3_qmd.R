
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

# define range
sheet <- c("8_TRM_ATSP", "8_TRM_ATSP (2)")

if (state_type == 3) {state_type = 1}

for (tz in 1:state_type) {
  # define ranges
  range <- "C8:C9"
  trm_2_0  <- read_range(file, sheet[tz], range)
  
  range <- "C66:M71"
  trm_2_14_e  <- read_range(file, sheet[tz], range)

#---------------- sections definition
  # section 2.14[i]
ceff_6_14_1 <- paste0('## Terminal charging zone ', 
if_else(state_type == 2, as.character(tz), ""),' - Other terminal ANSPs/METSPs
### 14 Other ANSP(s) / METSP(s) regulatory results for terminal activity


```{r, options=(tz = ', tz, ')}
#| file: R/table_ceff_tz_14.R
```
```{r, options=(tz = ', tz, ')}
htmltools::tagList(t)
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
    file = paste0("cost-efficiency-tz", tz, "-3.qmd")
)
}
