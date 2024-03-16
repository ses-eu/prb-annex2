
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

# define ranges and import data
sheet <- country

range <- "A16:G17"
saf_2_r <- read_range(saf_eosm_file, sheet, range) 


#---------------- sections definition
# section 1
saf_1 <- paste0('# 1 Safety
### Effectiveness of Safety Management

```{r}
#| file: R/table_saf_eosm.R
#| out.width: "100%"
```

Note: EoSM questionnaire has been updated in RP3 using CANSO Standard of Excellence as the basis, maturity levels of study areas and calculation of the score have been updated too. A direct comparison with  maturity levels and scoring of EoSM used RP2 is not advisable.
'
)                

# section 2
saf_2 <- paste0('\n\n### Observations															
', saf_2_r[1,1]
)


# assemble all and create .qmd
cat(paste0(
  saf_1,
  saf_2
  
),
file = "safety.qmd")

