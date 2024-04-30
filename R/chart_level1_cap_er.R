 
# parameters 
# mywidth = 300
# myheight = 220
# myfont = 8
# # mymargin = list (t = 20, l = 0)
# mylinewidth = 2

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
      target = round(target, 2)
    ) %>% 
    select(
      year,
      target
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
  
  ## prepare data ----
  data_prep_target <- data_raw_target %>% 
    filter(
      state == .env$country) %>% 
    mutate(
      target = round(delay_target, 2)
    ) %>% 
    select(
      year,
      target
    ) %>% arrange(year)
  
  data_prep_actual <- data_raw_actual %>% 
    filter(
      state == .env$country) %>% 
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
    ) %>% 
    rename(movements = ifr_movements)
}

# chart ----
## set parameters for chart ----

  if (knitr::is_latex_output()) {
    if (country == 'SES RP3') {
      mymargin <- list (t = 20, r = 0, l = 30)
    } else {
      mymargin <- list (t = 20, r = 50, l = 10)
    }
    mylegend_x_pos <- -0.12
  } else {
    mymargin <- list (t = 40, r = 70)
    mylegend_x_pos <- -0.12
  }
  
mytitle <- paste0("Average en route ATFM delay per flight - ", country)
myrightaxis <- "IFR flights ('000)"
mytrafficmetric <- "IFR mvts."

## plot chart ----
# function defined in utils
mycapchart(mywidth, myheight+20, myfont, mylinewidth, mymargin,
           data_prep_target,
           data_prep_actual,
           mytitle,
           myrightaxis,
           mytrafficmetric)


