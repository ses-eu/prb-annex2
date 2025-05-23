# to avoid quarto error when there are no airports
if (no_tcz >0 & country != "SES RP3") {

## import data  ----
data_raw_axot  <-  read_xlsx(
  paste0(data_folder, "ENV dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "Table_AXOT airports",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

data_raw_asma  <-  read_xlsx(
  paste0(data_folder, "ENV dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "Table_ASMA airports",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

data_raw_cdo  <-  read_xlsx(
  paste0(data_folder, "ENV dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "Table_CDO airports",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 


airports_table <- read_mytable("Lists.xlsx", "Lists", "Table_TCZs_RP3") %>%  clean_names()

## prepare data ----
data_prep_axot <- data_raw_axot  %>%
  filter(
  entity_name == .env$country,
  airport_code %in% airports_table$apt_code) %>%
  mutate_at(vars(-one_of(c('year', 'airport_code'))), ~ ifelse(year > year_report, NA, .)) %>%
  left_join(airports_table, by = c("airport_code" = "apt_code")) %>% 
  mutate(type = "Additional taxi-out time (PI#3)",
         mymetric = format(round(axot_airport_value_min_flight,2), decimals =2)
         ) %>%
  select(apt_name, year, type, mymetric)
# %>% 
  # filter(airport_icao %in% airports_table$apt_code)
  
data_prep_asma <- data_raw_asma  %>%
  filter(
    entity_name == .env$country,
    airport_code %in% airports_table$apt_code) %>%
  mutate_at(vars(-one_of(c('year', 'indicator_type', 'airport_code'))), ~ ifelse(year > year_report, NA, .)) %>%
  left_join(airports_table, by = c("airport_code" = "apt_code")) %>% 
  mutate(type = "Additional ASMA time (PI#4)",
         mymetric = format(round(asma_airport_value_min_flight,2), decimals =2 )
         ) %>%
  select(apt_name, year, type, mymetric)

data_prep_cdo <- data_raw_cdo  %>%
  filter(
    entity_name == .env$country,
    airport_code %in% airports_table$apt_code) %>%
  mutate_at(vars(-one_of(c('year', 'airport_code'))), ~ ifelse(year > year_report, NA, .)) %>%
  left_join(airports_table, by = c("airport_code" = "apt_code")) %>% 
  mutate(type = "Share of arrivals applying CDO (PI#5)",
         mymetric = if_else(is.na(cdo_airport_value) == TRUE, NA_character_, paste0(round(cdo_airport_value*100,0), '%'))
         ) %>%
  select(apt_name, year, type, mymetric) 

data_prep <- data_prep_axot %>% 
  rbind(data_prep_asma) %>% 
  rbind(data_prep_cdo) %>%
  pivot_wider(names_from = "type", values_from = "mymetric"
              # , names_glue = "{year}_{.value}" #suffix to prefix
              ) %>%
  pivot_wider(names_from = "year", values_from = c("Additional taxi-out time (PI#3)",
                                                   "Additional ASMA time (PI#4)",
                                                   "Share of arrivals applying CDO (PI#5)")
              # , names_glue = "{year}_{.value}" #suffix to prefix
  ) %>%
  rename("Airport Name" = apt_name)
  


# plot table ----

table1 <-mygtable(data_prep, myfont*0.9) %>% 
  tab_spanner_delim(
    delim = "_"
  ) |> 
  tab_header(
    title = md("**Airport level**")
  )

}  


# create latex table
if (knitr::is_latex_output()) {
  table_level2_env_apt_pis <- mylatex(table1)
  
} else {
  table1
}