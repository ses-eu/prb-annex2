# import data  ----
state_table <- state_list |> as_tibble() |> 
  filter(value != "MUAC", 
         value != "Luxembourg", 
         value != "SES RP3", 
         value != "Network Manager") |> 
  select(State = value)

data_raw_target  <-  read_xlsx(
  paste0(data_folder, "CAP dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "en route delay targets",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

data_raw_actual  <-  read_xlsx(
  paste0(data_folder, "CAP dataset master.xlsx"),
  sheet = "Avg en-route ATFM delay",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# prepare data ----
data_prep_target <- data_raw_target %>% 
  filter(year == .env$year_report) |> 
  select(
    State = state,
    Target = delay_target
  ) 
  
data_prep_actual <- data_raw_actual %>% 
  filter(year == .env$year_report) %>% 
  mutate(average_delay = if_else  (is.na(average_delay) == TRUE, 0, average_delay)) |> 
  select(
    State = state,
    Actual = average_delay
  ) 

data_prep <- state_table |> 
  left_join(data_prep_target, by = "State") |> 
  left_join(data_prep_actual, by = "State") |> 
  mutate(Actual = paste0("<span style='font-size:0.8rem;'>",
                        format(round(Actual,2), nsmall = 2),
                        "</span>",
                        if_else(Actual <= Target,
                                "<span style='color:green; font-weight:bold; font-size:0.8rem;'>&nbsp;&nbsp;&#10003;</span>",
                                "<span style='color:red; font-size:0.8rem;'>&nbsp;&nbsp;&#10008;</span>")
                        )
         
         )
  
# plot table ----

mygtable(data_prep, myfont)|> 
  tab_header(
    title = md("En route delay (min/flight)")
  ) |> 
  fmt_markdown(columns = Actual) 
