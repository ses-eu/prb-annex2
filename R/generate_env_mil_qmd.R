
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

# define ranges and import data
sheet <- country

range <- "A3:O7"
env_mil_1_r <- read_range(env_mil_file, sheet, range) 

range <- "A8:O12"
env_mil_2_r <- read_range(env_mil_file, sheet, range) 

range <- "A30:O34"
env_mil_5_r <- read_range(env_mil_file, sheet, range) 

range <- "A52:O56"
env_mil_8_r <- read_range(env_mil_file, sheet, range) 

range <- "A74:O78"
env_mil_11_r <- read_range(env_mil_file, sheet, range) 


#---------------- sections definition
  # section 1
env_mil_1 <- paste0('## ',
# if_else(state_type == 0, '2.2', '2.3'),
' Military dimension
### Update on Military dimension of the plan

', env_mil_1_r[1,1]
)                

# section 2
env_mil_2 <- paste0('\n\n### Military - related measures implemented or planned to improve capacity

', env_mil_2_r[1,1]                  
)

# section 3
env_mil_3 <- paste0('\n\n### PI#6 Effective use of reserved or segregated airspace - national level

```{r,  options=(range = "B15:N17")}
#| file: R/table_env_mil.R
#| out.width: "100%"
```

'
)

# section 4
env_mil_4 <- paste0('\n\n### PI#6 Effective use of reserved or segregated airspace (per ACC)

```{r,  options=(range = "B20:N29")}
#| file: R/table_env_mil.R
#| out.width: "100%"
```

'
)

# section 5
env_mil_5 <- paste0('\n### Initiatives implemented or planned to improve PI#6

', env_mil_5_r[1,1]                  
)

# section 6
env_mil_6 <- paste0('\n\n### PI#7 Rate of planning via available airspace structures - national level

```{r,  options=(range = "B37:N39")}
#| file: R/table_env_mil.R
#| out.width: "100%"
```

'
)

# section 7
env_mil_7 <- paste0('\n\n### PI#7 Rate of planning via available airspace structures (per ACC)

```{r,  options=(range = "B42:N51")}
#| file: R/table_env_mil.R
#| out.width: "100%"
```

'
)

# section 8
env_mil_8 <- paste0('\n### Initiatives implemented or planned to improve PI#7

', env_mil_8_r[1,1]                  
)

# section 9
env_mil_9 <- paste0('\n\n### PI#8 Rate of using available airspace structures - national level

```{r,  options=(range = "B59:N61")}
#| file: R/table_env_mil.R
#| out.width: "100%"
```

'
)

# section 10
env_mil_10 <- paste0('\n\n### PI#8 Rate of using available airspace structures (per ACC)

```{r,  options=(range = "B64:N73")}
#| file: R/table_env_mil.R
#| out.width: "100%"
```

'
)

# section 11
env_mil_11 <- paste0('\n### Initiatives implemented or planned to improve PI#8

', env_mil_11_r[1,1]                  
)

# assemble all and create .qmd
cat(paste0(
  env_mil_1,
  env_mil_2,
  env_mil_3,
  env_mil_4,
  env_mil_5,
  env_mil_6,
  env_mil_7,
  env_mil_8,
  env_mil_9,
  env_mil_10,
  env_mil_11
),
    file = "environment_mil.qmd")

