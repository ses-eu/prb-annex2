if (!exists("country") | is.na(country)) {country <- "Poland"
source("R/parameters.R")
}


# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "SAF EoSM.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "SPI1c-RI_Airport",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# process data ----
data_prep <- data_raw %>% 
  filter(state == .env$country & year == year_report) %>% 
  arrange(desc(ifr_movements)) %>% 
  mutate(
    rank = row_number(),
    ifr_movements = format(round(ifr_movements,0), big.mark   = ",", nsmall = 0),
    ri = format(round(ri,0), big.mark   = ",", nsmall = 0),
    rate_per_100_000 = format(round(rate_per_100_000,2), big.mark   = ",", nsmall = 2)
    
  ) %>% 
  select(
    "#" = rank,
    "Airport name" = name_apt,
    "APT movements" = ifr_movements,
    "Number of RI" = ri,
    "Rate RI  per 100,000" = rate_per_100_000
  )

# plot table ----
table1 <- mygtable(data_prep, myfont) %>% 
  tab_options(column_labels.background.color = "white",
              # column_labels.font.weight = 'bold',
              container.padding.y = 0) %>% 
  cols_align(columns = 2, align = "left") %>%
  # fmt_number(
  #   columns = c(3:4),  # Specify the columns to format
  #   decimals = 0,  # Number of decimal places
  #   use_seps = TRUE  # Use thousands separator
  # ) |> 
  # fmt_number(
  #   columns = c(5),  # Specify the columns to format
  #   decimals = 2,  # Number of decimal places
  #   use_seps = TRUE  # Use thousands separator
  # ) |> 
  tab_header(
    title = md(paste0("**Rate of RIs per 100,000 airport movements - ", country, "**"))
  )

if (!knitr::is_latex_output()) {
  table1
}

