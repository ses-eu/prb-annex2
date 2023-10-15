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
sheet <- "8_TRM_ATSP"
range <- range <- "C13:M17" 
trm_2_14_1  <- read_range(file, sheet, range) 

range <- range <- "C18:M22" 
trm_2_14_2  <- read_range(file, sheet, range)

# data prep 1
myrownames <- trm_2_14_1[1]
mycolnames <- colnames(trm_2_14_1)

trm_2_14_1_s <- trm_2_14_1 %>% select(!c(1:5)) 
nacheck <- if_else(trm_2_14_1_s[4,1] == 'N/A', 1, 0)

if (nacheck == 1) {
trm_2_14_1_s <- trm_2_14_1_s %>% 
  mutate_all(~ str_replace(., "N/A", "0"))  %>% 
  mutate_all(~ as.numeric(.))
}

trm_2_14_1_t <- transpose(trm_2_14_1_s) %>% 
  mutate_all(~ as.numeric(.)) %>% 
  mutate(across(c(1,2), ~format(round(.,0), big.mark = ",", scientific = F))) %>%
  mutate(across(c(3,4), ~paste0(format(round(.*100,1)), "%")))

colnames(myrownames) <- "a"

trm_2_14_1_tt <- transpose(trm_2_14_1_t) %>% as_tibble() %>% 
  mutate(myrownames,
         .before = V1) 

data_for_table1 <- trm_2_14_1_tt %>% 
  mutate_all(~ str_replace(., "NA", ""))

if (nacheck == 1) {
  data_for_table1[4,] <- 'N/A'
  data_for_table1[4,1] <- 'Ex-post RoE pre-tax rate (in %)'
}

# data prep 2
myrownames <- trm_2_14_2[1]
mycolnames <- colnames(trm_2_14_1)

trm_2_14_2_s <- trm_2_14_2 %>% select(!c(1:5)) 
nacheck <- if_else(trm_2_14_2_s[4,1] == 'N/A', 1, 0)

if (nacheck == 1) {
  trm_2_14_2_s <- trm_2_14_2_s %>% 
    mutate_all(~ str_replace(., "N/A", "0"))  %>% 
    mutate_all(~ as.numeric(.))
}

trm_2_14_2_t <- transpose(trm_2_14_2_s) %>% 
  mutate_all(~ as.numeric(.)) %>% 
  mutate(across(c(1,2), ~format(round(.,0), big.mark = ",", scientific = F))) %>%
  mutate(across(c(3,4), ~paste0(format(round(.*100,1)), "%"))) %>% 
  as_tibble() %>% 
  mutate_all(~ str_replace(., "NA%", "")) %>% 
  mutate_all(~ str_replace(., "NA", ""))

colnames(myrownames) <- "a"

trm_2_14_2_tt <- transpose(trm_2_14_2_t)  %>% 
  mutate(myrownames,
         .before = V1) 

data_for_table2 <- trm_2_14_2_tt %>% 
  mutate_all(~ str_replace(., "NA", "")) 

if (nacheck == 1) {
  for (j in 1:(1+(year_report-2020))) {
    data_for_table2[4,j+1] <- 'N/A'
  }
  if (year_report>2020) {data_for_table2[4,j+2] <- 'N/A'}
  data_for_table2[4,1] <- 'Ex-post RoE pre-tax rate (in %)'
}

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
    a = colDef(name=paste0("Cost sharing (",
                                          if_else(nat_curr == 'EUR', "€", nat_curr),
                                          " '000)"), 
                                    minWidth = 43, 
                                    align = "left"
                                      
                         ), # to preserve whitespace,
    V1 = colDef(name = "2020D", 
                minWidth = 9),
    V2 = colDef(name = "2021D", 
                minWidth = 9),
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
    a = colDef(name=paste0("Cost sharing (",
                           if_else(nat_curr == 'EUR', "€", nat_curr),
                           " '000)"), 
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
