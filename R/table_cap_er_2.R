## libraries
# library(tidyverse)
library(dplyr)
library(reactable)
library(janitor)
library(stringr)

## parameters
source("R/parameters.R")

## count ACCs
sheet <- country
range <- "A24:A48"
countacc_r  <- read_range(cap_file, sheet, range)

no_acc = 0
for (i in 1:nrow(countacc_r)) {
  no_acc <- ifelse(countacc_r[i,1] != 'Planned (Perf Plan)' & 
                   countacc_r[i,1] != 'Actual' & is.na(countacc_r[i,1]) == FALSE,
                   no_acc + 1, no_acc) 
} 

no_acc <- no_acc[1] 

## import data
atcos <- list()
##https://stackoverflow.com/questions/75575583/interactive-ggiraph-objects-created-in-a-loop-does-not-show-in-quarto-html-outpu
t <- list()

for (i in 1:no_acc) {
  range[i] <- paste0("A", 25 + (i-1)*4, ":O", 27+ (i-1)*4)
  df <- read_range(cap_file, sheet, range[i])  
  atcos[[i]] <- df

## prepare data
data_for_table <- atcos[[i]] %>% 
  select(1, 5:10) %>% 
  mutate_all(~ as.character(.)) %>% 
  mutate_all(~ str_replace(., "NA", "")) %>% 
  clean_names() %>% 
  rename(a = 1)

mycolnames <- colnames(atcos[[i]])

## plot table
t[[i]] <- reactable(
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
                         headerStyle = list(
                           background = "#D9D9D9", 
                           # color = "white", 
                           fontSize = "0.8rem",
                           style=list("white-space"= "wrap")
                           )
                         
  ),
  columns = list(
    a = colDef(name=mycolnames[1], 
               minWidth = 32, 
               align = "left"
    ), 
    x2019 = colDef(name = '2019', minWidth = 10),
    x2020 = colDef(name = '2020', minWidth = 10),
    x2021 = colDef(name = '2021', minWidth = 10),
    x2022 = colDef(name = '2022', minWidth = 10),
    x2023 = colDef(name = '2023', minWidth = 10),
    x2024 = colDef(name = '2024', minWidth = 10)
    
  )
)

# print(t) 
}
