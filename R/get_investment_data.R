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
  
  data_new_major_detail <-  read_xlsx(
    paste0(data_folder, "INVESTMNENTS DATA_master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "New Major Investments",
    range = cell_limits(c(4, 1), c(NA, NA))) %>%
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
    range = cell_limits(c(3, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_cost_inv_rt <-  read_xlsx(
    paste0(data_folder, "INVESTMNENTS DATA_master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Costs of inv. (RT)-ANSP",
    range = cell_limits(c(3, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_impact <-  read_xlsx(
    paste0(data_folder, "INVESTMNENTS DATA_master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "IMPACT ANSP",
    range = cell_limits(c(2, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_funding_enr <-  read_xlsx(
    paste0(data_folder, "INVESTMNENTS DATA_master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Funding (2)",
    range = cell_limits(c(3, 1), c(NA, 7))) %>%
    as_tibble() %>% 
    clean_names() %>% 
    mutate(type = "enroute")
  
  data_funding_ter <-  read_xlsx(
    paste0(data_folder, "INVESTMNENTS DATA_master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Funding (2)",
    range = cell_limits(c(3, 9), c(NA, 15))) %>%
    as_tibble() %>% 
    clean_names() %>% 
    mutate(type = "terminal")
  
  data_funding_self <- rbind(data_funding_ter, data_funding_enr) %>% 
    pivot_longer(
      cols = -c(member_state, type),  # Pivot all columns
      names_to = c("year"),  # Create "type" and "year" columns
      values_to = "value"  # Store values in "value" column
    ) %>% 
    mutate(year = str_replace_all(year, "x", "")) %>% 
    group_by(member_state, year) %>% 
    summarise(value = sum(value, na.rm = TRUE), .groups = 'drop') %>% 
    ungroup() %>% 
    mutate(type = "Total self-declared funding")
    
  
  data_funding_sdm <-  read_xlsx(
    paste0(data_folder, "INVESTMNENTS DATA_master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Funding (2)",
    range = cell_limits(c(3, 17), c(NA, 23))) %>%
    as_tibble() %>% 
    clean_names() %>% 
    pivot_longer(
      cols = -c(member_state),  # Pivot all columns
      names_to = c("year"),  # Create "type" and "year" columns
      values_to = "value"  # Store values in "value" column
    ) %>% 
    mutate(year = str_replace_all(year, "x", ""),
           type = "SDM data")
  
  data_funding <- rbind(data_funding_self, data_funding_sdm) %>% 
    mutate(year = if_else(year == "total_rp3_to_date", "RP3", year))
}  
