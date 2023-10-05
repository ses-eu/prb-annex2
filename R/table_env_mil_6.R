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
  range <- "B64:N73"
  df <- read_range(env_mil_file, sheet, range)  

mycolnames <- colnames(df)

## prepare data
data_for_table <- df %>% 
  select(1, 4, 6, 8, 10, 12)  %>% 
  rename(a = 1) %>% 
  filter(is.na(a) == FALSE) %>% 
  mutate(across(c(2:6), ~paste0(format(round(.*100,0)), "%"))) %>%  
  mutate_all(~ str_replace(., "NA%", "")) 

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
                         minWidth = 15,
                         headerStyle = list(
                           background = "#D9D9D9", 
                           # color = "white", 
                           fontSize = "0.8rem",
                           style=list("white-space"= "wrap")
                           )
                         
  ),
  columns = list(
    a = colDef(name=mycolnames[1], 
                                    minWidth = 25, 
                                    align = "left", 
                                    style = list(whiteSpace = "pre",
                                                 "font-size" = "0.8rem"
                                      # , "font-weight" = "bold"
                                      )
                         )
    
  )
)

t
