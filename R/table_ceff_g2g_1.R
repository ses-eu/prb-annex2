## libraries
# library(tidyverse)
library(dplyr)
library(reactable)
library(data.table)
library(janitor)
library(stringr)

## parameters
source("R/parameters.R")

## import data
sheet <- "9_G2G"
range <- "C15:M19"
g2g_1_1  <- read_range(file, sheet, range)

myrownames <- g2g_1_1[1]
mycolnames <- colnames(g2g_1_1)

g2g_1_1_s <- g2g_1_1 %>% 
  select(6:11)
g2g_1_1_t <- transpose(g2g_1_1_s) %>% 
  mutate_all(~ as.numeric(.)) %>% 
  mutate(across(c(1:3), ~format(round(.,0), big.mark = ",", scientific = F))) %>% 
  mutate(across(c(4), ~paste0(format(round(.*100,1)), "%"))) %>%   
  mutate_all(~ str_replace(., "NA%", "NA")) %>% 
  mutate_all(~ str_replace(., "NA", ""))

colnames(myrownames) <- "a"

g2g_1_1_tt <- transpose(g2g_1_1_t) %>% as_tibble() %>% 
  mutate(myrownames,
         .before = V1) 

## prepare data
data_for_table <- g2g_1_1_tt 

## plot table
t <- reactable(
  data_for_table,
  bordered = TRUE,
  pagination = FALSE,
  striped = FALSE,
  compact = TRUE,
  highlight = TRUE,
  defaultColDef = colDef(style = list(
                          "font-size" = "0.72rem",
                          "white-space"= "wrap"
                          ),
                         align = "right",
                         headerStyle = list(
                           background = "#D9D9D9", 
                           # color = "white", 
                           fontSize = "0.72rem",
                           style=list("white-space"= "wrap")
                           )
                         
  ),
  columns = list(
    a = colDef(name=mycolnames[1], 
               minWidth = 33, 
               align = "left"
    ), 
    V1 = colDef(name = "2020D", minWidth = 11),
    V2 = colDef(name = "2021D", minWidth = 11),
    V3 = colDef(name = "2020-2021D", minWidth = 12),
    V4 = colDef(name = "2022D", minWidth = 11),
    V5 = colDef(name = "2023D", minWidth = 11),
    V6 = colDef(name = "2024D", minWidth = 11)
  )
)

t

