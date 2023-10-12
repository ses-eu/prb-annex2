
## parameters
source("R/parameters.R")

# define range
sheet <- "4_ATSP"

range <- "C66:M71"
ert_2_14_e  <- read_range(file, sheet, range)

#---------------- sections definition
  # section 2.14[i]
ceff_2_14_1 <- paste0('## 4.3 Other en route ANSPs/METSPs
### Other ANSP(s) / METSP(s) regulatory results for en route activity

```{r}
#| file: R/table_ceff_14.R
#| out.width: "100%"
```
<br>
'
)                

# section 2.14.e
ceff_2_14_e <- paste0('\n<p style = "margin-bottom: 0px;">**', 
str_replace(ert_2_14_e[1,1], "\n", ""), '**</p>
', ert_2_14_e[2,1]

)

                   
# assemble all and create .qmd
cat(paste0(
  ceff_2_14_1,
  ceff_2_14_e

  
),
    file = "cost-efficiency3.qmd")

