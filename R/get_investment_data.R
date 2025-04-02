if (exists("country") == FALSE) {country <- "Belgium"}

source("R/parameters.R")

# import data  ----
if (country != 'SES RP3') {
  ## State case ----
  data_new_major <-  read_xlsx(
    paste0(data_folder, "INVESTMNENTS DATA_master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "New Major Inv pivot",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_capex <-  read_xlsx(
    paste0(data_folder, "INVESTMNENTS DATA_master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "CAPEX per Main ANSP",
    range = cell_limits(c(2, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_category <-  read_xlsx(
    paste0(data_folder, "INVESTMNENTS DATA_master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Benefits | Investment category",
    range = cell_limits(c(2, NA), c(180, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_union_wide <-  read_xlsx(
    paste0(data_folder, "INVESTMNENTS DATA_master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Union-wide median",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_cost_inv <-  read_xlsx(
    paste0(data_folder, "INVESTMNENTS DATA_master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Costs of inv main ANSP (MR)",
    range = cell_limits(c(3, NA), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_cost_inv_rt <-  read_xlsx(
    paste0(data_folder, "INVESTMNENTS DATA_master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Costs of inv. (RT)-ANSP",
    range = cell_limits(c(3, NA), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
}  
