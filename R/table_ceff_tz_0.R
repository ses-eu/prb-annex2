## libraries
# library(tidyverse)
library(dplyr)
library(reactable)
library(janitor)
library(stringr)

## parameters
source("R/parameters.R")

#import data
sheet <- "5_TRM"
  range <- "C11:M15"
  df <- read_range(ceff_file, sheet, range)  

## prepare data
data_for_table1 <- df %>% 
  rename(a = 1) %>% 
  select(a) %>% 
  slice_head(., n = 1)

data_for_table2 <- df %>% 
  rename(a = 1) %>% 
  select(1, 3:4, 6:9) %>% 
  slice(., 3) %>% 
  mutate_all(~ str_replace(., "NA", "")) %>% 
  clean_names()

data_for_table3 <- df %>% 
  rename(a = 1) %>% 
  select(1, 3) %>% 
  slice(., 4) %>% 
  mutate_all(~ str_replace(., "NA", "")) %>% 
  clean_names()

data_for_table4 <- df %>% 
  rename(a = 1) %>% 
  select(1, 5:6,8, 11) %>% 
  slice(., 1:2)  %>% 
  mutate_all(~ str_replace(., "NA", "")) %>% 
  clean_names()

data_for_table4[1,1:3] = ""
data_for_table4[3,] = data_for_table4[1,]

data_for_table4 <- data_for_table4 %>% 
  slice(., 2:3)

## plot table
t1 <- reactable(
  data_for_table1,
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
                         minWidth = 100,
                         headerStyle = list(
                           # background = "#D9D9D9", 
                           color = "white",
                           fontSize = "0rem",
                           style = list("white-space"= "wrap")
                           )
                         
  )

)

t2 <- reactable(
  data_for_table2,
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
    a = colDef(name="", minWidth = 20),
    x3 = colDef(name = "", minWidth = 5, align = "left"),
    x4 = colDef(name = "", minWidth = 20),
    x6 = colDef(name = "", minWidth = 5),
    x7 = colDef(name = "", minWidth = 15),
    x8 = colDef(name = "", minWidth = 5),
    x9 = colDef(name = "", minWidth = 15)
  ) 
)

t3 <- reactable(
  data_for_table3,
  bordered = FALSE,
  borderless = TRUE,
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
    a = colDef(name="", minWidth = 20),
    x3 = colDef(name = "", minWidth = 80)
  ) 
)

t4 <- reactable(
  data_for_table4,
  bordered = FALSE,
  borderless = TRUE,
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
    a = colDef(name="", minWidth = 38),
    x5 = colDef(name = "", minWidth = 5),
    x6 = colDef(name = "", minWidth = 15),
    x8 = colDef(name = "", minWidth = 37),
    x11 = colDef(name = "", minWidth = 5)
  ) 
)


t1
t4
t2
t3
