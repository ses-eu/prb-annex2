## libraries
library(tidyr)
library(dplyr)
library(readxl)
library(reactable)
library(janitor)
# library(here)
# library(htmltools)

## parameters
source("R/parameters.R")

## import data
data_raw_target  <-  read_xlsx(
  paste0(data_folder, "targets.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "KEA",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

data_raw_actual  <-  read_xlsx(
  paste0(data_folder, "HFE_clean.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "Table_HFE",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 


## prepare data
data_prep_target <- data_raw_target %>% 
  filter(
    state == country,
    year_report == year_report) %>% 
  mutate(
    type = 'Target',
    kea = round(x321_kea_target * 100, 2)
  ) %>% 
  select(
    type,
    year,
    kea
  )

years <- data_prep_target %>% select(year)
  
data_prep_actual <- data_raw_actual %>% 
  filter(
    entity_name == country) %>% 
  mutate(
    kea = hfe_kpi
  ) %>% 
  select(
    year,
    kea
  ) 

data_prep_actual <-  merge(x = years, y = data_prep_actual, by="year", all.x = TRUE) %>% 
  mutate(
    type = 'Actual'
  )

data_for_table <- rbind(
  pivot_wider(data_prep_actual, names_from = year, values_from = kea),
  pivot_wider(data_prep_target, names_from = year, values_from = kea)
) %>% 
  arrange(desc(type)) %>% 
  clean_names()

## plot table
t <- reactable(
  data_for_table,
  bordered = TRUE,
  pagination = FALSE,
  striped = FALSE,
  compact = TRUE,
  highlight = TRUE,
  defaultColDef = colDef(style = list(
    # "font-size" = "11px", 
    "white-space"= "wrap"
  ),
  align = "center",
  headerStyle = list(
    background = "#D9D9D9", 
    # color = "white", 
    # fontSize = "11px", 
    style=list("white-space"= "wrap")
  )
  
  ),
  columns = list(
    type = colDef(name="", minWidth = 40, align = "left", 
                         style = list(whiteSpace = "pre"
                                      , "font-weight" = "bold"
                         )
    ), # to preserve whitespace,
    x2020 = colDef(name="2020", minWidth = 12),
    x2021 = colDef(name="2021", minWidth = 12),
    x2022 = colDef(name="2022", minWidth = 12),
    x2023 = colDef(name="2023", minWidth = 12),
    x2024 = colDef(name="2024", minWidth = 12)
  )
)

t

