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
  range <- "B37:N39"
  df <- read_range(env_mil_file, sheet, range)  

mycolnames <- colnames(df)

## prepare data
data_for_table <- df %>% 
  mutate_at(c(-1), ~ as.numeric(.)) %>% 
  select(1, 4, 6, 8, 10, 12)  %>% 
  rename(a = 1) %>% 
  filter(is.na(a) == FALSE) 


# check if there is any non-numeric column
for (i in 2:ncol(data_for_table)) {
  if (class(data_for_table[[i]]) == "numeric") {
    data_for_table <- data_for_table %>%
      mutate(across(c(i), ~paste0(format(round(.*100,0)), "%")))   
  } 
}

data_for_table <- data_for_table %>% 
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
