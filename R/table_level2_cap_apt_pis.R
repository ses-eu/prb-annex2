## import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "CAP dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "AirportPIs",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

airports_table <- read_mytable("Lists.xlsx", "Lists", "Table_TCZs_RP3") %>%  clean_names()

## prepare data ----
data_prep <- data_raw %>% 
  filter(
    state == .env$country) %>%
  mutate_at(vars(-one_of(c('year', 'airport_icao'))), ~ ifelse(year > year_report, NA, .)) %>% 
  filter(airport_icao %in% airports_table$apt_code) %>% 
  left_join(airports_table, by = c("airport_icao" = "apt_code")) %>% 
  arrange(apt_name) %>% 
  rename("Airport Name" = apt_name) %>% 
  mutate("Avg arrival ATFM delay" = format(round(terminal_delay, 2), decimals = 2),
         "Slot adherence" = paste0(format(round(slot_adherence*100, 1), decimals = 1), "%"),
         "ATC pre departure\ndelay" = format(round(atc_predep_dly, 2), decimals = 2),
         "All causes pre departure\ndelay" = format(round(all_cause_predep_dly, 1), decimals = 1),
  ) %>% 
  select(
    year,
    "Airport Name",
    "Avg arrival ATFM delay",
    "Slot adherence",
    "ATC pre departure\ndelay",
    "All causes pre departure\ndelay"
  ) %>% 
  pivot_wider(names_from = "year", values_from = c(    "Avg arrival ATFM delay",
                                                       "Slot adherence",
                                                       "ATC pre departure\ndelay",
                                                       "All causes pre departure\ndelay")
              # , names_glue = "{year}_{.value}" #suffix to prefix
              ) 
  ## order columns alphabetically 
  # select(order(colnames(.))) %>% 
  # select("Airport Name", everything())


## plot table

mygtable(data_prep, myfont*0.9) %>% 
  tab_spanner_delim(
    delim = "_"
  )
  


