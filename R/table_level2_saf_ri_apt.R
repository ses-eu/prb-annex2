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
  mutate(rank = row_number()) %>% 
  select(
    rank,
    name_apt,
    ifr_movements,
    ri,
    rate_per_100_000
  )

# plot table ----
table1 <- mygtable(data_prep, myfont) %>% 
  cols_label(
    rank = "#",
    name_apt = "Airport name",
    ifr_movements = "APT movements",
    ri = "Number of RI",
    rate_per_100_000 = "Rate RI  per 100,000"
    ) %>% 
  tab_options(column_labels.background.color = "white",
              column_labels.font.weight = 'bold',
              container.padding.y = 0) %>% 
  cols_align(columns = 2, align = "left") %>%
  # tab_style(
  #   style = cell_text(size = mytablefontsize),  # Set font size
  #   locations = list(
  #     cells_body(),                    # Apply to the table content
  #     cells_column_labels()         # Apply to column labels
  #   )
  # )|> 
  fmt_number(
    columns = c(3:4),  # Specify the columns to format
    decimals = 0,  # Number of decimal places
    use_seps = TRUE  # Use thousands separator
  ) |> 
  fmt_number(
    columns = c(5),  # Specify the columns to format
    decimals = 2,  # Number of decimal places
    use_seps = TRUE  # Use thousands separator
  ) |>   tab_header(
    title = html(paste0("<b>Rate of RIs per 100,000 airport movements - ", country), "</b>")
  )

table1
