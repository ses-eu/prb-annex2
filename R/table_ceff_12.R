## libraries
# library(tidyverse)
library(dplyr)
library(reactable)
library(janitor)
library(data.table)

library(stringr)

## parameters
source("R/parameters.R")

## import data
sheet <- "3_ATSP"
range <- range <- "C38:M46" 
ert_2_11_1  <- read_range(file, sheet, range)

range <- range <- "C47:M56" 
ert_2_11_2  <- read_range(file, sheet, range)

# data prep 1
myrownames1 <- ert_2_11_1[1]
mycolnames1 <- colnames(ert_2_11_1)

ert_2_11_1_s <- ert_2_11_1 %>% select(!c(1:5)) 
ert_2_11_1_t <- transpose(ert_2_11_1_s) %>% 
  mutate_all(~ as.numeric(.)) %>% 
  mutate(across(c(1,4,5,6), ~format(round(.,0), big.mark = ",", scientific = F))) %>%
  mutate(across(c(2), ~paste0(format(round(.*100,0)), "%"))) %>% 
  mutate(across(c(3,7,8), ~paste0(format(round(.*100,1)), "%")))

colnames(myrownames1) <- "a"

ert_2_11_1_tt <- transpose(ert_2_11_1_t) %>% as_tibble() %>% 
  mutate(myrownames1,
         .before = V1) 

data_for_table1 <- ert_2_11_1_tt %>% 
  mutate_all(~ str_replace(., "NA", ""))

# data prep 2
myrownames2 <- ert_2_11_2[1]
mycolnames2 <- colnames(ert_2_11_2)

ert_2_11_2_s <- ert_2_11_2 %>% select(!c(1:5)) 
ert_2_11_2_t <- transpose(ert_2_11_2_s) %>% 
  mutate_all(~ as.numeric(.)) %>% 
  mutate(across(c(1,4,5,6,7), ~format(round(.,0), big.mark = ",", scientific = F))) %>%
  mutate(across(c(2), ~paste0(format(round(.*100,0)), "%"))) %>% 
  mutate(across(c(3,8,9), ~paste0(format(round(.*100,1)), "%"))) %>% 
  as_tibble() %>% 
  mutate_all(~ str_replace(., "NA%", "")) %>% 
  mutate_all(~ str_replace(., "NA", ""))

colnames(myrownames2) <- "a"

ert_2_11_2_tt <- transpose(ert_2_11_2_t)  %>% 
  mutate(myrownames2,
         .before = V1) 

data_for_table2 <- ert_2_11_2_tt %>% 
  mutate_all(~ str_replace(., "NA", "")) 

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
    a = colDef(name = mycolnames1[1], 
                                    minWidth = 43, 
                                    align = "left"
                                      
                         ), # to preserve whitespace,
    V1 = colDef(name = "2020D", 
                minWidth = 9),
    V2 = colDef(name = "2021D", 
                minWidth = 10),
    V3 = colDef(name = "2020-2021D", 
                minWidth = 12),
    V4 = colDef(name = "2022D", 
                minWidth = 9),
    V5 = colDef(name = "2023D", 
                minWidth = 9),
    V6 = colDef(name = "2024D", 
                minWidth = 9)
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
    a = colDef(name = mycolnames2[1], 
               minWidth = 43, 
               align = "left"
               
    ), # to preserve whitespace,
    V1 = colDef(name = "2020A", 
                minWidth = 9),
    V2 = colDef(name = "2021A", 
                minWidth = 9),
    V3 = colDef(name = "2020-2021A", 
                minWidth = 12),
    V4 = colDef(name = "2022A", 
                minWidth = 9),
    V5 = colDef(name = "2023A", 
                minWidth = 9),
    V6 = colDef(name = "2024A", 
                minWidth = 9)
  )
)

t1
t2

