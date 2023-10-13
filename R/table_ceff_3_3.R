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
sheet <- "1_ERT"
range <- if_else(nat_curr == 'EUR', "C37:M47", "C37:M49")
ert_1_3_3  <- read_range(file, sheet, range)
myrownames <- ert_1_3_3[1]
mycolnames <- colnames(ert_1_3_3)
if (nat_curr == 'EUR') {
  mycols <- c(9)
  mycols2 <- c(10)} else {mycols <- c(9,11)
  mycols2 <- c(10, 12)}

ert_1_3_3_s <- ert_1_3_3[,c(-1,-5)]  
ert_1_3_3_t <- transpose(ert_1_3_3_s) %>% 
  mutate(across(c(1,5,7), ~format(round(.,0), big.mark = ",", scientific = F))) %>%
  mutate(across(c(2,6,8), ~paste0(if_else(.>=0,"+",""),
    format(round(.*100,1)), "%"))) %>% 
  mutate(across(c(3,4), ~paste0(format(round(.,1)), " p.p."))) %>% 
  mutate(across(all_of(mycols), ~format(round(.,1), big.mark = ",", scientific = F))) %>% 
  mutate(across(all_of(mycols2), ~paste0(if_else(.>=0,"+",""),
                                         format(round(.*100,1)), "%")))

colnames(myrownames) <- "a"
mytypenames <- ert_1_3_3[5] 
colnames(mytypenames) <- "b"

ert_1_3_3_tt <- transpose(ert_1_3_3_t) %>% as_tibble() %>% 
  mutate(myrownames,
         .before = V1) %>% 
  mutate( mytypenames, .before = V4 )

## prepare data
data_for_table <- ert_1_3_3_tt %>% 
  select(!c(2:4)) %>% 
  mutate_all(~ str_replace(., "NA p.p.", "")) %>% 
  mutate_all(~ str_replace(., "NA%", "")) %>% 
  mutate_all(~ str_replace(., "NA", "")) 


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
               minWidth = 31, 
               align = "left"
    ), 
    b = colDef(name = "", minWidth = 8),
    V4 = colDef(name = "2020", minWidth = 10),
    V5 = colDef(name = "2021", minWidth = 10),
    V6 = colDef(name = "2020-2021", minWidth = 11),
    V7 = colDef(name = "2022", minWidth = 10),
    V8 = colDef(name = "2023", minWidth = 10),
    V9 = colDef(name = "2024", minWidth = 10)
  )
)

t
