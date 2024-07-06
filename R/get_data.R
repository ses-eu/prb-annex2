
# get data  ----
  
## main state params ----
params_table <- read_mytable("Lists.xlsx", "Lists", "Table_States") %>% clean_names()

state_list <- params_table %>% select(state) %>% unlist()

aua_entities_table <- read_mytable("Lists.xlsx", "Lists", "Table_AUA") %>% clean_names()

acc_list_table <- read_mytable("Lists.xlsx", "Lists", "Table_ACCs") %>% clean_names()

ecz_list_table <- read_mytable("Lists.xlsx", "Lists", "Table_ECZ") %>%
  left_join(
    read_mytable("Lists.xlsx", "Lists", "Table_forecast"),
    by = "forecast_id"
    ) %>% clean_names()

tcz_list_table <- read_mytable("Lists.xlsx", "Lists", "Table_TCZ") %>% clean_names()

context_data_table <- read_mytable("context_data.xlsx", "context", "Table_context") %>%  clean_names()

other_orgs_table <- read_mytable("Lists.xlsx", "Lists", "Table_PP_2023_ANSPs") %>%  clean_names()

## ceff raw data ----
# data_raw_t1_ert  <-  read_xlsx(
#   paste0(data_folder, "CEFF dataset master.xlsx"),
#   # here("data","hlsr2021_data.xlsx"),
#   sheet = "Enroute_T1",
#   range = cell_limits(c(1, 1), c(NA, NA))) %>%
#   as_tibble() %>% 
#   clean_names() 

# data_raw_t1_trm  <-  read_xlsx(
#   paste0(data_folder, "CEFF dataset master.xlsx"),
#   # here("data","hlsr2021_data.xlsx"),
#   sheet = "Terminal_T1",
#   range = cell_limits(c(1, 1), c(NA, NA))) %>%
#   as_tibble() %>% 
#   clean_names() 
