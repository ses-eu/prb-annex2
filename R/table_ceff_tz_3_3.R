## libraries
# library(tidyverse)
library(dplyr)
library(reactable)
library(data.table)
library(janitor)
library(stringr)

## parameters
source("R/parameters.R")

if (exists("tz") == FALSE) {tz = 1}

## import data
sheet <- c("5_TRM", "5_TRM (2)")
range <- if_else(nat_curr == 'EUR', "C37:M47", "C37:M49")
trm_1_3_3  <- read_range(file, sheet[tz], range)

myrownames <- trm_1_3_3[1]
mycolnames <- colnames(trm_1_3_3)
if (nat_curr == 'EUR') {
  mycols <- c(9)
  mycols2 <- c(10)} else {mycols <- c(9,11)
  mycols2 <- c(10, 12)}

trm_1_3_3_s <- trm_1_3_3[,c(-1,-5)]  
trm_1_3_3_t <- transpose(trm_1_3_3_s) %>% 
  mutate_all(~ as.numeric(.)) %>% 
  mutate(across(c(1,5,7), ~format(round(.,0), big.mark = ",", scientific = F))) %>%
  mutate(across(c(2,6,8), ~paste0(if_else(.>=0,"+",""),
    format(round(.*100,1)), "%"))) %>% 
  mutate(across(c(3,4), ~paste0(format(round(.,1)), " p.p."))) %>% 
  mutate(across(all_of(mycols), ~format(round(.,1), big.mark = ",", scientific = F))) %>% 
  mutate(across(all_of(mycols2), ~paste0(if_else(.>=0,"+",""),
                                         format(round(.*100,1)), "%")))

colnames(myrownames) <- "a"
mytypenames <- trm_1_3_3[5] 
colnames(mytypenames) <- "b"

trm_1_3_3_tt <- transpose(trm_1_3_3_t) %>% as_tibble() %>% 
  mutate(myrownames,
         .before = V1) %>% 
  mutate( mytypenames, .before = V4 )

## prepare data
data_for_table <- trm_1_3_3_tt %>% 
  select(!c(2:4)) %>% 
  mutate_all(~ str_replace(., "NA p.p.", NA_character_)) %>% 
  mutate_all(~ str_replace(., "NA%", NA_character_)) %>% 
  mutate_all(~ str_replace(., "NA", NA_character_)) 


## plot table
t <- reactable(
  data_for_table,
  bordered = TRUE,
  pagination = FALSE,
  striped = FALSE,
  compact = TRUE,
  highlight = TRUE,
  defaultColDef = colDef( style = function(value, name) {
    color <- if (is.na(value)) {'#F2F2F2'}  
    list(background = color,
         "font-size" = "0.72rem",
         "white-space"= "wrap")
  } ,
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
               align = "left",
               style = list(background = 'white',
                            "font-size" = "0.72rem",
                            "white-space"= "wrap")
    ), 
    b = colDef(name = "", minWidth = 8),
    V4 = colDef(name = "2020", minWidth = 10),
    V5 = colDef(name = "2021", minWidth = 10),
    V6 = colDef(name = "2020-2021", minWidth = 11),
    V7 = colDef(name = "2022", minWidth = 10),
    V8 = colDef(name = "2023", minWidth = 10),
    V9 = colDef(name = "2024", minWidth = 10)
  ),
  borderless = TRUE,
  rowStyle = 
    function(index) {
      if (index == nrow(data_for_table)) list(fontWeight = "bold")
      else if (index == nrow(data_for_table) -1 ) list(fontWeight = "bold",
                                                       borderTop = "1px solid rgba(0, 0, 0, 0.1)")
    }
)

t

