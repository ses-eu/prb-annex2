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

range <- "C46:M51"
g2g_3_1  <- read_range(file, sheet, range)
mycolnames <- colnames(g2g_3_1)

range <- "C52:M57"
g2g_3_2  <- read_range(file, sheet, range)
mycolnames2 <- colnames(g2g_3_2)


## prepare data
data_for_table1 <- g2g_3_1 %>% 
  select(1, 4:6, 8:10) %>% 
  rename(a = 1) %>% 
  filter(is.na(a) == FALSE) %>% 
  mutate_at(c(-1), ~ as.numeric(.)) %>% 
  mutate(across(c(2:3, 5:6), ~format(round(.,0), big.mark = ",", scientific = F))) %>% 
  mutate(across(c(4,7), ~paste0(format(round(.*100,1)), "%"))) %>% 
  clean_names()

## prepare data
data_for_table2 <- g2g_3_2 %>% 
  select(1, 4:6, 8:10) %>% 
  rename(a = 1) %>% 
  filter(is.na(a) == FALSE) %>% 
  mutate_at(c(-1), ~ as.numeric(.)) %>% 
  mutate(across(c(2:3, 5:6), ~format(round(.,0), big.mark = ",", scientific = F))) %>% 
  mutate(across(c(4,7), ~paste0(format(round(.*100,1)), "%"))) %>% 
  clean_names()

myheaderstyle <- list(
  background = "#D9D9D9", 
  # color = "white", 
  fontSize = "0.72rem",
  style=list("white-space"= "wrap")
)

## plot table
t1 <- reactable(
  data_for_table1,
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
                         headerStyle = myheaderstyle
                         
  ),
  columns = list(
    a = colDef(name=mycolnames[1], 
               minWidth = 22, 
               align = "left"
    ), 
    rr_4 = colDef(name = "RR", minWidth = 12),
    revenues_5 = colDef(name = 'Revenues', minWidth = 13),
    rr_percent_revenues_6 = colDef(name = 'RR % revenues', minWidth = 14),
    rr_8 = colDef(name = 'RR', minWidth = 12),
    revenues_9 = colDef(name = 'Revenues', minWidth = 13),
    rr_percent_revenues_10 = colDef(name = 'RR % revenues', minWidth = 14)
  ),
  columnGroups = list(
    colGroup(name = paste0("In ", nat_curr," '000"), columns = c("a"), 
             headerStyle = myheaderstyle, align = "left",
 ),
    colGroup(name = "Ex-ante", columns = c("rr_4", "revenues_5", "rr_percent_revenues_6"),
             headerStyle = myheaderstyle
    ),
    colGroup(name = "Ex-post", columns = c("rr_8", "revenues_9", "rr_percent_revenues_10"),
             headerStyle = myheaderstyle
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
    "font-size" = "0.72rem",
    "white-space"= "wrap"
  ),
  align = "right",
  headerStyle = myheaderstyle
  ),
  columns = list(
    a = colDef(name=mycolnames[1], 
               minWidth = 22, 
               align = "left"
    ), 
    rr_4 = colDef(name = "RR", minWidth = 12),
    revenues_5 = colDef(name = 'Revenues', minWidth = 13),
    rr_percent_revenues_6 = colDef(name = 'RR % revenues', minWidth = 14),
    rr_8 = colDef(name = 'RR', minWidth = 12),
    revenues_9 = colDef(name = 'Revenues', minWidth = 13),
    rr_percent_revenues_10 = colDef(name = 'RR % revenues', minWidth = 14)
  )
)


t1
t2
