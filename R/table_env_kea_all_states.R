# import data  ----
state_table <- state_list |> as_tibble() |> 
  filter(value != "MUAC", 
         value != "Luxembourg", 
         value != "SES RP3", 
         value != "Network Manager") |> 
  select(State = value)

data_raw_target  <-  read_xlsx(
  paste0(data_folder, "ENV dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "Table_KEA Targets",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

data_raw_actual  <-  read_xlsx(
  paste0(data_folder, "ENV dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "Table_HFE",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 
  
# prepare data ----
data_prep_target <- data_raw_target %>% 
  filter(year == year_report) |> 
  mutate(
    # type = 'Target',
    target = round(kea_reference_value_percent, 2)
  ) %>% 
  select(
    State = entity_name,
    Target = target
  ) 
  
data_prep_actual <- data_raw_actual %>% 
  filter(year == year_report) %>% 
  mutate (actual = hfe_kpi_percent) %>% 
  select(
    State = entity_name,
    Actual = hfe_all
  ) 

data_prep <- state_table |> 
  left_join(data_prep_target, by = "State") |> 
  left_join(data_prep_actual, by = "State") |> 
  mutate(Actual = paste0("<span style='font-size:0.8rem;'>",
                        format(Actual, nsmall = 2),
                        "</span>",
                        if_else(Actual <= Target,
                                "<span style='color:green; font-weight:bold; font-size:0.8rem;'>&nbsp;&nbsp;&#10003;</span>",
                                "<span style='color:red; font-size:0.8rem;'>&nbsp;&nbsp;&#10008;</span>")
                        )
         
         )
  
# plot table ----

table1 <- mygtable(data_prep, myfont)|> 
  tab_header(
    title = md("KEA (%)")
  ) |> 
  fmt_markdown(columns = Actual) 


# create latex table
if (knitr::is_latex_output()) {
  table_env_kea_all_states <- table1 %>% 
    mylatex(NA) 
  
} else {
  table1
}