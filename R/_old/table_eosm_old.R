
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

## import data
data_raw  <-  read_xlsx(
  paste0(data_folder, "SAF EoSM.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "EoSM",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

## prepare data
data_for_table <- data_raw %>% 
  filter(
    ms == country,
    year == year_report) %>% 
  mutate(
    eosm = round(eo_sm_score)
    ) %>% 
  select (
    entity_name,
    # year,
    eosm,
    actual_culture,
    actual_policy_and_objectives,
    actual_risk_management,
    actual_assurance,
    actual_promotion
  )

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
    entity_name = colDef(name="", minWidth = 25, align = "left", 
                         style = list(whiteSpace = "pre"
                                      , "font-weight" = "bold"
                                      )
                         ), # to preserve whitespace,
    eosm = colDef(name="Score", minWidth = 10),
    actual_culture = colDef(name="Safety Culture", minWidth = 12.5),
    actual_policy_and_objectives = colDef(name="Safety Policy and Objectives", minWidth = 12.5),
    actual_risk_management = colDef(name="Safety Risk Management", minWidth = 15),
    actual_assurance = colDef(name="Safety Assurance", minWidth = 12.5),
    actual_promotion = colDef(name="Safety Promotion", minWidth = 12.5)
  )
)

t

