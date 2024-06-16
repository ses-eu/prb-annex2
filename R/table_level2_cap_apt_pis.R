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
  filter(airport_icao %in% airports_table$apt_code) %>% 
  rename("Airport Name" = airport_name) %>% 
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

data_prep %>% gt() %>% 
  tab_options(
    table.width = '100%',
    table.font.size = myfont*0.9,
    # column_labels.vlines.color = "black", # don't know why this doesn't work
    column_labels.vlines.width = '1px',
    # column_labels.border.top.color = "#EAEAEA",
    column_labels.border.top.width = '1px',
    # row.striping.include_table_body = FALSE,
    table.border.top.width = '1px',
    table.border.bottom.width = '1px',
    data_row.padding = '3px') %>% 
  tab_spanner_delim(
    delim = "_"
  )
  


