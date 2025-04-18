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
ansp_order <- data_raw %>% 
  filter(state == .env$country) %>% 
  group_by(entity) %>% 
  summarise(total_fh = sum(flight_hours, na.rm = TRUE)) %>% 
  mutate(entity = factor(entity, levels = entity)) %>% 
  select(entity)

# mycolumns <- c(, "Number of SMIs", "Rate of SMI per 100,000 flight hours", "Rate")

data_prep <- data_raw %>% 
  filter(state == .env$country) %>% 
  group_by(entity) %>% 
  arrange(year) %>% 
  mutate(
    variation = if_else(lag(rate_per_100_000, 1) == 0, 0, rate_per_100_000/lag(rate_per_100_000, 1) -1)
  ) %>% 
  ungroup() %>% 
  select(
    ANSP = entity,
    year,
    "Flight hours" = flight_hours,
    "Number of SMIs" = smi,
    "Rate of SMI per 100,000 flight hours" = rate_per_100_000,
    "% variation in rate of SMIs" = variation
  ) %>% 
  pivot_wider(names_from = "year", values_from = c(3:6)) %>% 
  mutate(
    ANSP = factor(ANSP, levels = ansp_order$entity),
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
  tab_header(
    title = md("**Rate of SMI with ANS contribution per 100,000 flight hours**")
  ) %>% 
  fmt_number(
    columns = c(3:14),  # Specify the columns to format
    decimals = 0,  # Number of decimal places
    use_seps = TRUE  # Use thousands separator
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
