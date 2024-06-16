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
  entity_name == .env$country) %>%
  mutate(type = "Additional taxi-out time",
         mymetric = format(round(axot_airport_value_min_flight,2), decimals =2)
         ) %>%
  select(airport, year, type, mymetric)
# %>% 
  # filter(airport_icao %in% airports_table$apt_code)
  
data_prep_asma <- data_raw_asma  %>%
  filter(
    entity_name == .env$country) %>%
  mutate(type = indicator_type,
         mymetric = format(round(asma_airport_value_min_flight,2), decimals =2 )
         ) %>%
  select(airport, year, type, mymetric)

data_prep_cdo <- data_raw_cdo  %>%
  filter(
    entity_name == .env$country,
    airport_code %in% airports_table$apt_code) %>%
  mutate(type = "Share of arrivals applying CDO",
         mymetric = paste0(round(cdo_airport_value*100,0), '%')
         ) %>%
  select(airport, year, type, mymetric) 

data_prep <- data_prep_axot %>% 
  rbind(data_prep_asma) %>% 
  rbind(data_prep_cdo) %>%
  pivot_wider(names_from = "type", values_from = "mymetric"
              # , names_glue = "{year}_{.value}" #suffix to prefix
              ) %>%
  pivot_wider(names_from = "year", values_from = c("Additional taxi-out time",
                                                   "Additional ASMA time",
                                                   "Share of arrivals applying CDO")
              # , names_glue = "{year}_{.value}" #suffix to prefix
  ) %>%
  rename("Airport Name" = airport)
  


## plot table

mygtable(data_prep, myfont*0.9) %>% 
  tab_spanner_delim(
    delim = "_"
  )
  


