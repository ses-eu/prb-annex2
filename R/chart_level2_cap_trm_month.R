
if (country == 'SES RP3') {
  # SES case ----
  ## import data  ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "SES.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "SES_ATFM_ERT_delay",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----
  data_prep_target <- data_raw %>% 
    filter(
      year_report == .env$year_report) %>% 
    mutate(
      er_cap_target = round(target, 2)
    ) %>% 
    select(
      year,
      er_cap_target
    ) %>% arrange(year)
  
  data_prep_actual <- data_raw %>% 
    filter(
      year_report == .env$year_report) %>% 
    select(-c(year_report,target)) %>% 
    pivot_longer(
      cols = c(capacity, staffing, disruptions, weather, other_non_atc),
      names_to = "type",
      values_to = "delay"
    ) %>% 
    mutate(
      type = case_when(
        type == "capacity" ~ "Capacity",
        type == "staffing" ~ "Staffing",
        type == "disruptions" ~ "Disruptions",
        type == "weather" ~ "Weather",
        type == "other_non_atc" ~ "Other non-ATC"
      ) 
    ) %>% 
    rename (average_delay = avg_er_atfm_delay) %>% 
    mutate(ifr = NA)
} else {
  # state case ----
  ## import data  ----
  data_raw_target  <-  read_xlsx(
    paste0(data_folder, "CAP dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "terminal delay targets",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_raw_actual  <-  read_xlsx(
    paste0(data_folder, "CAP dataset master.xlsx"),
    sheet = "terminal monthly delay",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----
  data_prep_target <- data_raw_target %>% 
    filter(
      state == .env$country,
      year == .env$year_report
      ) %>% 
    mutate(
      target = round(x332_state_arr_delay_target, 2)
    ) %>% 
    select(
      year,
      target)
  
  data_prep_actual <- data_raw_actual %>% 
    filter(
      state == .env$country,
      year == .env$year_report
      ) %>% 
    pivot_longer(
      cols = c(atc_capacity, atc_staffing, atc_disruptions, weather, other_non_atc),
      names_to = "type",
      values_to = "delay"
    ) %>% 
    mutate(
      type = case_when(
        type == "atc_capacity" ~ "Capacity",
        type == "atc_staffing" ~ "Staffing",
        type == "atc_disruptions" ~ "Disruptions",
        type == "weather" ~ "Weather",
        type == "other_non_atc" ~ "Other non-ATC"
      )
    )
}

# chart ----
## set parameters for chart ----

if (knitr::is_latex_output()) {
  if (country == 'SES RP3') {
    mymargin <- list (t = 20, r = 0, l = 30)
  } else {
    mymargin <- list (t = 20, r = 50, l = 10)
  }
  mylegend_x_pos <- 0.5
  mymargin <- list (t = 40, r = 0, l = 50)
  
  } 

mytitle <- paste0("Average monthly terminal ATFM delay per flight - ", year_report)

## plot chart ----
mycapchart_month(mywidth, myheight+20, myfont, mylinewidth, mymargin)



