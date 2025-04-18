if (!exists("country") | is.na(country)) {country <- "Poland"
source("R/parameters.R")
}


# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "SAF EoSM.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "SPI1d-SMI_ANSPs",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# process data ----
if (country != "SES RP3") {
  myentity_order <- data_raw %>% 
    filter(state == .env$country) %>% 
    group_by(entity) %>% 
    summarise(total_fh = sum(flight_hours, na.rm = TRUE)) %>% 
    arrange(desc(total_fh)) %>% 
    mutate(myentity = factor(entity, levels = entity)) %>% 
    select(myentity)
  
  data_calc <- data_raw %>% 
    filter(state == .env$country) %>% 
    mutate(myentity = entity)
} else {
  myentity_order <- data_raw %>% 
    group_by(state) %>% 
    summarise(total_fh = sum(flight_hours, na.rm = TRUE)) %>% 
    arrange(desc(total_fh)) %>% 
    mutate(myentity = factor(state, levels = state)) %>% 
    select(myentity)
  
  data_calc <- data_raw %>% 
    group_by(year, state) %>% 
    summarise(
      smi = sum(smi, na.rm = TRUE),
      flight_hours = sum(flight_hours, na.rm = TRUE),
      rate_per_100_000 = round(smi / flight_hours * 100000,2),
      .groups = "drop"
    ) %>% 
    mutate(myentity = state)
  
}

data_prep <- data_calc %>% 
  group_by(myentity) %>% 
  arrange(year) %>% 
  mutate(
    variation = if_else(lag(rate_per_100_000, 1) == 0, 0, rate_per_100_000/lag(rate_per_100_000, 1) -1)
  ) %>% 
  ungroup() %>% 
  select(
    myentity,
    year,
    "Flight hours" = flight_hours,
    "Number of SMIs" = smi,
    "Rate of SMI per 100,000 flight hours" = rate_per_100_000,
    "% variation in rate of SMIs" = variation
  ) %>% 
  pivot_wider(names_from = "year", values_from = c(3:6)) %>% 
  mutate(
    myentity = factor(myentity, levels = myentity_order$myentity),
  ) %>% 
  arrange(myentity) %>% 
  mutate(
    "#" = row_number()
  ) %>% 
  relocate(
    "#", .before = 1
  )
    

# plot table ----
table1 <-mygtable(data_prep, myfont*0.9) %>% 
  tab_spanner_delim(
    delim = "_"
  ) |> 
  cols_label(
    myentity = if_else(country =="SES RP3", "State", "ANSP")
  ) %>% 
  tab_header(
    title = md("**Rate of SMI with ANS contribution per 100,000 flight hours**")
  ) %>% 
  fmt_number(
    columns = c(3:10),  # Specify the columns to format
    decimals = 0,  # Number of decimal places
    use_seps = TRUE  # Use thousands separator
  ) %>%  
  fmt_number(
    columns = c(11:14),  # Specify the columns to format
    decimals = 2,  # Number of decimal places
  ) %>%  
  fmt(
    columns = 15:18,
    fns = function(x) {
      dplyr::case_when(
        x > 0 ~ paste0("+", scales::percent(x, accuracy = 1)),
        TRUE ~ scales::percent(x, accuracy = 1)
      )
    }
  )
  

table1
