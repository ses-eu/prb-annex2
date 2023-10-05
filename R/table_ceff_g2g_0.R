## libraries
# library(tidyverse)
library(dplyr)
library(reactable)
library(janitor)
library(stringr)

## parameters
source("R/parameters.R")

#import data
sheet <- "9_G2G"
  range <- "C11:M14"
  df <- read_range(ceff_file, sheet, range)  

data_for_table <- df %>% 
  rename(a = 1) %>% 
  select(1, 3, 5,  7) %>% 
  mutate_all(~ str_replace(., "NA", "")) %>% 
  mutate_all(~ if_else(is.na(.) == TRUE, "", .)) %>% 
  clean_names()

if (data_for_table[2,4] == "") {data_for_table[,3] <- ""}

## plot table

t1 <- reactable(
  data_for_table,
  bordered = FALSE,
  pagination = FALSE,
  striped = FALSE,
  compact = TRUE,
  highlight = TRUE,
  class = "hidden-column-headers", #https://github.com/glin/reactable/issues/102
  defaultColDef = colDef(style = list(
    "font-size" = "0.8rem",
    "white-space"= "wrap"
  ),
  align = "left",
  headerStyle = list(
    # background = "#D9D9D9", 
    color = "white",
    fontSize = "0rem",
    style = list("white-space"= "wrap")
    )
  ),
  columns = list(
    a = colDef(name="", minWidth = 25),
    x3 = colDef(name = "", minWidth = 20),
    x5 = colDef(name = "", minWidth = 25),
    x7 = colDef(name = "", minWidth = 20)
  ) 
)

t1
