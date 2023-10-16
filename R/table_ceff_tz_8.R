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
range <- range <- "H49:M53" 
trm_1_8_1  <- read_range(file, sheet[tz], range)

range <- range <- "H54:M61" 
trm_1_8_2  <- read_range(file, sheet[tz], range)

## prepare data
data_for_table1 <- trm_1_8_1 %>% 
  select(!c(2)) %>% 
  clean_names() %>% 
  mutate_at(c(-1), ~ as.numeric(.)) %>% 
  mutate(across(c(2:3), round, 0)) %>% 
  mutate(across(c(4:5), round, 2)) %>%
  rename(second = 2, third = 3, fourth = 4, fifth = 5) %>% 
  filter(is.na(ansp_s) == FALSE)

data_for_table2 <- trm_1_8_2 %>% 
  select(!c(2)) %>% 
  clean_names() %>% 
  mutate_at(c(-1), ~ as.numeric(.)) %>% 
  mutate(across(c(2:3), round, 0)) %>% 
  mutate(across(c(4:5), round, 2)) %>%   
  rename(second = 2, third = 3, fourth = 4, fifth = 5) %>% 
  filter(is.na(metsp_s) == FALSE)

if (nat_curr == 'EUR') {
data_for_table1 <- data_for_table1 %>% 
  select(!c(3,5)) 
data_for_table2 <- data_for_table2 %>% 
  select(!c(3,5))   

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
    ansp_s = colDef(name="ANSP(s)", 
                                    minWidth = 50, 
                                    align = "left"
                         ),
    second = colDef(name = if_else(nat_curr == 'EUR', "€ '000", paste0(nat_curr, " '000" )), 
                minWidth = 25,
                format = colFormat(separators = TRUE)
                ),
    fourth = colDef(name = if_else(nat_curr == 'EUR', "€/SU", paste0(nat_curr, "/SU" )), 
                     minWidth = 25,
                    format = colFormat(digits = 2)
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
    metsp_s = colDef(name="METSP(s)", 
                    minWidth = 50, 
                    align = "left" 
    ), 
    second = colDef(name = if_else(nat_curr == 'EUR', "€ '000", paste0(nat_curr, " '000" )), 
                    minWidth = 25,
                    format = colFormat(separators = TRUE)
    ),
    fourth = colDef(name = if_else(nat_curr == 'EUR', "€/SU", paste0(nat_curr, "/SU" )), 
                   minWidth = 25,
                  format = colFormat(digits = 2)
    )
    
  )
)
} else {
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
      ansp_s = colDef(name="ANSP(s)", 
                      minWidth = 40, 
                      align = "left" 
      ), 
      second = colDef(name = if_else(nat_curr == 'EUR', "€ '000", paste0(nat_curr, " '000" )), 
                      minWidth = 16,
                      format = colFormat(separators = TRUE)
      ),
      third = colDef(name = "€ '000", 
                      minWidth = 15,
                      format = colFormat(separators = TRUE)
      ),
      fourth = colDef(name = if_else(nat_curr == 'EUR', "€/SU", paste0(nat_curr, "/SU" )), 
                      minWidth = 15,
                      format = colFormat(digits = 2)
      ),
      fifth = colDef(name = "€/SU",  
                      minWidth = 14,
                      format = colFormat(digits = 2)
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
      metsp_s = colDef(name="METSP(s)", 
                       minWidth = 40, 
                       align = "left" 
      ), 
      second = colDef(name = if_else(nat_curr == 'EUR', "€ '000", paste0(nat_curr, " '000" )), 
                       minWidth = 16,
                      format = colFormat(separators = TRUE)
      ),
      third = colDef(name = "€ '000", 
                     minWidth = 15,
                     format = colFormat(separators = TRUE)
      ),
      fourth = colDef(name = if_else(nat_curr == 'EUR', "€/SU", paste0(nat_curr, "/SU" )), 
                      minWidth = 15,
                      format = colFormat(digits = 2)
      ),
      fifth = colDef(name = "€/SU",  
                     minWidth = 14,
                     format = colFormat(digits = 2)
      )   
    )
  )
  
}

t1
t2
