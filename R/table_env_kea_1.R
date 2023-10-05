## libraries
# library(tidyverse)
library(dplyr)
library(reactable)
library(janitor)
library(stringr)

## parameters
source("R/parameters.R")

#import data
sheet <- country
  range <- "A4:H6"
  df <- read_range(env_kea_file, sheet, range)  

## prepare data
data_for_table <- df %>% 
  select(1, 4:8) %>% 
  mutate(across(c(2:6), ~paste0(format(round(.*100,2)), "%"))) %>%  
  mutate_all(~ str_replace(., "NA%", "")) %>% 
  clean_names() %>% 
  rename(a = 1)

## plot table
t <- reactable(
  data_for_table,
  bordered = TRUE,
  pagination = FALSE,
  striped = FALSE,
  compact = TRUE,
  highlight = TRUE,
  defaultColDef = colDef(style = list(
                          "font-size" = "0.8rem",
                          "white-space"= "wrap"
                          ),
                         align = "center",
                         headerStyle = list(
                           background = "#D9D9D9", 
                           # color = "white", 
                           fontSize = "0.8rem",
                           style=list("white-space"= "wrap")
                           )
                         
  ),
  columns = list(
    a = colDef(name='', 
                                    minWidth = 40, 
                                    align = "left", 
                                    style = list(whiteSpace = "pre",
                                                 "font-size" = "0.8rem"
                                      # , "font-weight" = "bold"
                                      )
                         ), # to preserve whitespace,
    x2020 = colDef(name = '2020', minWidth = 12),
    x2021 = colDef(name = '2021', minWidth = 12),
    x2022 = colDef(name = '2022', minWidth = 12),
    x2023 = colDef(name = '2023', minWidth = 12),
    x2024 = colDef(name = '2024', minWidth = 12)
    
  )
)

t
