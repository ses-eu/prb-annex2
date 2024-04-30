
#wrap script in function so it can be called from qmd
myfig <- function(){
  # import data  ----
  
  data_raw_target  <-  read_xlsx(
    paste0(data_folder, "CAP dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "terminal delay targets",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_raw_actual  <-  read_xlsx(
    paste0(data_folder, "CAP dataset master.xlsx"),
    sheet = "Avg terminal ATFM delay",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  # prepare data ----
  data_prep_target <- data_raw_target %>% 
    filter(
      state == .env$country) %>% 
    mutate(
      target = round(x332_state_arr_delay_target, 2)
    ) %>% 
    select(
      year,
      target
    ) %>% arrange(year)
  
  data_prep_actual <- data_raw_actual %>% 
    filter(
      state == .env$country,
      year >= 2020) %>% 
    mutate(movements = arrivals) %>%    #temporary line until the arrivals are added to the master file
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
  mytitle <- paste0("Average terminal ATFM delay per flight - ", country)
  myrightaxis <- "Arrival flights ('000)"
  mytrafficmetric <- "IFR arrivals"
  
  ## plot chart ----
  # function defined in utils
  return(mycapchart(mywidth, myheight+20, myfont, mylinewidth, mymargin,
                    data_prep_target,
                    data_prep_actual,
                    mytitle,
                    myrightaxis,
                    mytrafficmetric))
}

