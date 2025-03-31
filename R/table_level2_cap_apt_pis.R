if (country != "SES RP3") {
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
    rename("Airport name" = apt_name) %>% 
    mutate(year = factor(year, levels = 2020:2024),
           "Avg arrival\nATFM delay (KPI#2)" = format(round(terminal_delay, 2), decimals = 2),
           "Slot adherence (PI#1)" = paste0(format(round(slot_adherence*100, 1), decimals = 1), "%"),
           "ATC pre departure\ndelay (PI#2)" = format(round(atc_predep_dly, 2), decimals = 2),
           "All causes pre departure\ndelay (PI#3)" = format(round(all_cause_predep_dly, 1), decimals = 1),
    ) %>% 
    select(
      year,
      "Airport name",
      "Avg arrival\nATFM delay (KPI#2)",
      "Slot adherence (PI#1)",
      "ATC pre departure\ndelay (PI#2)",
      "All causes pre departure\ndelay (PI#3)"
    ) %>% 
    pivot_wider(names_from = "year", values_from = c(    "Avg arrival\nATFM delay (KPI#2)",
                                                         "Slot adherence (PI#1)",
                                                         "ATC pre departure\ndelay (PI#2)",
                                                         "All causes pre departure\ndelay (PI#3)")
                # , names_glue = "{year}_{.value}" #suffix to prefix
                ) 
    ## order columns alphabetically 
    # select(order(colnames(.))) %>% 
    # select("Airport Name", everything())
  
  
  ## plot table
  
  table1 <- mygtable(data_prep, myfont*0.9) %>% 
    tab_spanner_delim(
      delim = "_"
    )|> 
    tab_header(
      title = md("**Airport level**")
    )
}    
  
  
# create latex table
if (knitr::is_latex_output()) {
  table_level2_cap_apt_pis <- mylatex(table1)  
  
  # Step 1: Replace "\n" inside column headers with "\newline"
  table_level2_cap_apt_pis <- table_level2_cap_apt_pis %>% 
    str_replace_all(
    fixed("Avg arrival\nATFM delay (KPI\\#2)"), 
    "\\makecell{Avg arrival \\\\ ATFM delay (KPI\\#2)}") %>% 
    str_replace_all(
      fixed("ATC pre departure\ndelay (PI\\#2)"), 
      "\\makecell{ATC pre departure \\\\ delay (PI\\#2)}") %>% 
    str_replace_all(
      fixed("All causes pre departure\ndelay (PI\\#3)"), 
      "\\makecell{All causes pre departure \\\\ delay (PI\\#3)}")
    
  # "\\1\\makecell{\\2 \\\\ \\3}\\4"
} else {
  table1
}