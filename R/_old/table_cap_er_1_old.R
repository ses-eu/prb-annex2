
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

## import data
wb <- loadWorkbook(paste0(data_folder, "Lists.xlsx"))
tables <- getTables(wb, sheet = "Lists")
# get the range
table_range <- names(tables[tables == "Table_AUA"])
table_range_refs <- strsplit(table_range, ":")[[1]]

# use a regex to extract out the row numbers
table_range_row_num <- gsub("[^0-9.]", "", table_range_refs)
# extract out the column numbers
table_range_col_num <- convertFromExcelRef(table_range_refs)

# finally read it
aua <- read.xlsx(paste0(data_folder, "Lists.xlsx"),
                 sheet = "Lists",
                 cols = table_range_col_num[1]:table_range_col_num[2],
                 rows = table_range_row_num[1]:table_range_row_num[2]) %>% 
  clean_names() %>% 
  mutate(key = paste0(year, ansp_name)) %>% 
  select(key, state)

data_raw_target  <-  read_xlsx(
  paste0(data_folder, "targets.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "ER_CAP",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

data_raw_actual  <-  read_xlsx(
  paste0(data_folder, "AUA_export.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "AUA_export",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() %>% 
  mutate(key = paste0(year, entity_name))


## prepare data
data_prep_target <- data_raw_target %>% 
  filter(
    state == country,
    year_report == year_report) %>% 
  mutate(
    cap_er_target = x331_ert_delay_target,
    type = "Target"
  ) %>% 
  select(
    type,
    year,
    cap_er_target,
  )

data_prep_actual <- left_join(data_raw_actual, aua, by = "key") %>% 
  filter(
    state == country) %>% 
  select(
    year,
    avg_delay
  ) 

data_prep_actual <-  merge(x = data_prep_target, y = data_prep_actual, by="year", all.x = TRUE) %>% 
  select(year,
         avg_delay
  ) %>% 
  mutate(type = "Actual")

data_for_table <- rbind(
  pivot_wider(data_prep_actual, names_from = year, values_from = avg_delay),
  pivot_wider(data_prep_target, names_from = year, values_from = cap_er_target)
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
  defaultColDef = colDef(
    style = list(
    # "font-size" = "11px", 
      "font-size" = "0.8rem",
      "white-space"= "wrap"
    ),
    format = colFormat(digits = 2),
  align = "center",
  headerStyle = list(
    background = "#D9D9D9", 
    # color = "white", 
    fontSize = "0.8rem",
    style=list(
      "white-space"= "wrap")
  )
  
  ),
  columns = list(
    type = colDef(name="", minWidth = 35, align = "left", 
                         style = list(whiteSpace = "pre",
                                      "font-size" = "0.8rem"
                                      , "font-weight" = "bold"
                         )
    ), # to preserve whitespace,
    x2020 = colDef(name="2020", minWidth = 13),
    x2021 = colDef(name="2021", minWidth = 13),
    x2022 = colDef(name="2022", minWidth = 13),
    x2023 = colDef(name="2023", minWidth = 13),
    x2024 = colDef(name="2024", minWidth = 13)
  )
)

t

