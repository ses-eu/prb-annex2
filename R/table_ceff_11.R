## libraries
# library(tidyverse)
library(dplyr)
library(reactable)
library(janitor)
library(stringr)

## parameters
source("R/parameters.R")

## import data
sheet <- "3_ATSP"
range <- range <- "C24:M28" 
ert_2_10_1  <- read_range(file, sheet, range)

range <- range <- "C29:M32" 
ert_2_10_2  <- read_range(file, sheet, range)

range <- range <- if_else(nat_curr == 'EUR', "C33:M35", "C33:M36") 
ert_2_10_3  <- read_range(file, sheet, range)


## prepare data
data_for_table1 <- ert_2_10_1 %>% 
  select(!c(2:7)) %>% 
  clean_names() %>% 
  mutate(across(c(2:5), round, 0)) %>% 
  rename(first = 1)

data_for_table2 <- ert_2_10_2 %>% 
  select(!c(2:7)) %>% 
  clean_names() %>% 
  mutate(across(c(2:5), round, 0)) %>% 
  rename(first = 1)

data_for_table3 <- ert_2_10_3 %>% 
  select(!c(2:7)) %>% 
  clean_names() %>% 
  mutate(across(c(2:5), round, 0)) %>% 
  rename(first = 1)


## plot table
t1 <- reactable(
  data_for_table1,
  bordered = TRUE,
  pagination = FALSE,
  striped = FALSE,
  compact = TRUE,
  highlight = TRUE,
  defaultColDef = colDef(style = list(
                          "font-size" = "0.75rem",
                          "white-space"= "wrap"
                          ),
                         align = "right",
                         headerStyle = list(
                           background = "#D9D9D9", 
                           # color = "white", 
                           fontSize = "0.75rem",
                           style=list("white-space"= "wrap")
                           )
                         
  ),
  columns = list(
    first = colDef(name=paste0("Cost sharing (",
                                          if_else(nat_curr == 'EUR', "€", nat_curr),
                                          " '000)"), 
                                    minWidth = 52, 
                                    align = "left"
                                      
                         ), # to preserve whitespace,
    x2020_2021 = colDef(name = "2020-2021", 
                minWidth = 12,
                format = colFormat(separators = TRUE)
                ),
    x2022 = colDef(name = "2022", 
                      minWidth = 12,
                      format = colFormat(separators = TRUE),
                ),
    x2023 = colDef(name = "2023", 
                  minWidth = 12,
                  format = colFormat(separators = TRUE),
                   ),
    x2024 = colDef(name = "2024", 
                   minWidth = 12,
                   format = colFormat(separators = TRUE)
    )
)
)

t2 <- reactable(
  data_for_table2,
  bordered = TRUE,
  pagination = FALSE,
  striped = FALSE,
  compact = TRUE,
  highlight = TRUE,
  defaultColDef = colDef(style = list(
    "font-size" = "0.75rem",
    "white-space"= "wrap"
  ),
  align = "right",
  headerStyle = list(
    background = "#D9D9D9", 
    # color = "white", 
    fontSize = "0.75rem",
    style=list("white-space"= "wrap")
  )
  
  ),
  columns = list(
    first = colDef(name=paste0("Traffic risk sharing (",
                                          if_else(nat_curr == 'EUR', "€", nat_curr),
                                          " '000)"), 
                              minWidth = 52, 
                              align = "left"
                              
    ), # to preserve whitespace,
    x2020_2021 = colDef(name = "2020-2021", 
                        minWidth = 12,
                        format = colFormat(separators = TRUE)
    ),
    x2022 = colDef(name = "2022", 
                   minWidth = 12,
                   format = colFormat(separators = TRUE),
    ),
    x2023 = colDef(name = "2023", 
                   minWidth = 12,
                   format = colFormat(separators = TRUE),
    ),
    x2024 = colDef(name = "2024", 
                   minWidth = 12,
                   format = colFormat(separators = TRUE)
    )
  )
)

t3 <- reactable(
  data_for_table3,
  bordered = TRUE,
  pagination = FALSE,
  striped = FALSE,
  compact = TRUE,
  highlight = TRUE,
  defaultColDef = colDef(style = list(
    "font-size" = "0.75rem",
    "white-space"= "wrap"
  ),
  align = "right",
  headerStyle = list(
    background = "#D9D9D9", 
    # color = "white", 
    fontSize = "0.75rem",
    style=list("white-space"= "wrap")
  )
  
  ),
  columns = list(
    first = colDef(name=paste0("Incentives (",
                                                  if_else(nat_curr == 'EUR', "€", nat_curr),
                                                  " '000)"), 
                                      minWidth = 52, 
                                      align = "left"
                                      
    ), # to preserve whitespace,
    x2020_2021 = colDef(name = "2020-2021", 
                        minWidth = 12,
                        format = colFormat(separators = TRUE)
    ),
    x2022 = colDef(name = "2022", 
                   minWidth = 12,
                   format = colFormat(separators = TRUE),
    ),
    x2023 = colDef(name = "2023", 
                   minWidth = 12,
                   format = colFormat(separators = TRUE),
    ),
    x2024 = colDef(name = "2024", 
                   minWidth = 12,
                   format = colFormat(separators = TRUE)
    )
  )
)

t1
t2
t3
