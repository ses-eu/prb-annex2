## libraries
# library(tidyverse)
library(dplyr)
library(reactable)
library(janitor)
library(stringr)

## parameters
source("R/parameters.R")

if (tz != 1 & tz !=2) {tz = 1}

## import data
sheet <- c("6_TRM", "6_TRM (2)")
range <- range <- "D38:M45" 
trm_1_7  <- read_range(file, sheet[tz], range)

## prepare data
trm_1_7_c <- trm_1_7 %>% 
  select(!c(2:6)) %>% 
  clean_names() %>% 
  mutate_at(c(-1), ~ as.numeric(.)) %>% 
  mutate(across(c(2:3), round, 0)) %>% 
  mutate(across(c(4:5), round, 2))  

trm_1_7_c[7,1] <- 'Total costs exempt from cost sharing'

if (nat_curr == 'EUR'){ 
data_for_table <- trm_1_7_c %>% 
  select(!c(3,5)) %>% 
  rename(second = 2, third = 3) 

## plot table
t <- reactable(
  data_for_table,
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
    x1 = colDef(name="by item", 
                                    minWidth = 60, 
                                    align = "left", 
                         ), 
    second = colDef(name = if_else(nat_curr == 'EUR', "€ '000", paste0(nat_curr, " '000" )), 
                    minWidth = 20,
                ),
    third = colDef(name = if_else(nat_curr == 'EUR', "€/SU", paste0(nat_curr, "/SU" )), 
                     minWidth = 20,
                    format = colFormat(digits = 2)
                  )
    
  )
)
} else{
  data_for_table <- trm_1_7_c %>% 
   rename(second = 2, third = 3, fourth = 4, fifth = 5) 
  
  ## plot table
  t <- reactable(
    data_for_table,
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
      x1 = colDef(name="by item", 
                  minWidth = 60, 
                  align = "left", 

      ), 
      second = colDef(name = if_else(nat_curr == 'EUR', "€ '000", paste0(nat_curr, " '000" )), 
                      minWidth = 11,
      ),
      third = colDef(name = "€ '000", 
                      minWidth = 11,
      ),
      fourth = colDef(name = if_else(nat_curr == 'EUR', "€/SU", paste0(nat_curr, "/SU" )), 
                     minWidth = 9,
                     format = colFormat(digits = 2)
      ),
      fifth = colDef(name = if_else(nat_curr == 'EUR', "€/SU", paste0(nat_curr, "/SU" )), 
                      minWidth = 9,
                      format = colFormat(digits = 2)
      
    )
  )
)
}

t

