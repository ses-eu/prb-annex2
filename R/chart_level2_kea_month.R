# parameters ----
mytooltip_decimals <- 2
targetcolor <- 'transparent'
mymarker_color <- 'transparent'

if (country == "Network Manager") {
  # NM case ----
  mymetric <- "KEP"
  mychart_title <- mymetric
  myaxis_title <- paste0(mymetric, " (%)")
  
  ## import data  ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "NM_data.xlsx"),
    sheet = "Environment",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----
  data_for_chart <- data_raw %>% 
    filter(year_report == .env$year_report) %>% 
    mutate(
      target = round(nm_target * 100, 2),
      actual = round(actual * 100, 2)
    ) %>% 
    select(year, target, actual)
  
} else if (country == "SES RP3"){
    # SES case ----
    mymetric <- "KEA"
    mychart_title <- paste0(mymetric, " - ", country)
    myaxis_title <- paste0(mymetric, " (%)")
    
    ## import data  ----
    data_raw  <-  read_xlsx(
      paste0(data_folder, "SES.xlsx"),
      sheet = "SES_KEA",
      range = cell_limits(c(1, 1), c(NA, NA))) %>%
      as_tibble() %>% 
      clean_names() 
    
    ## prepare data ----
    data_for_chart <- data_raw %>% 
      filter(year_report == .env$year_report) %>% 
      mutate ( status = str_to_lower(status)) %>% 
      select (-year_report) %>% 
      pivot_wider(names_from = status, values_from = kea_value) %>% 
      mutate(
        target = round(target * 100, 2),
        actual = round(actual * 100, 2)
      ) %>% 
      select(year, target, actual)
    
} else  {
  # State case ----
  mymetric <- "KEA"
  mychart_title <- paste0('Monthly ', mymetric, " - ", country)
  myaxis_title <- paste0(mymetric, " (%)")
  
  ## import data  ----
  data_raw_target  <-  read_xlsx(
    paste0(data_folder, "targets.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "KEA",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>%
    clean_names()
  # 
  # data_raw_actual  <-  read_xlsx(
  #   paste0(data_folder, "HFE_clean.xlsx"),
  #   # here("data","hlsr2021_data.xlsx"),
  #   sheet = "Table_HFE",
  #   range = cell_limits(c(1, 1), c(NA, NA))) %>%
  #   as_tibble() %>% 
  #   clean_names() 

  ## prepare data ----
  target_value <- data_raw_target %>%
    filter(
      state == .env$country,
      year == .env$year_report
    ) %>%
    mutate(
      # type = 'Target',
      target = round(x321_kea_target * 100, 2)
    ) %>%
    select(
      target
    ) %>% pull()
  # 
  # data_prep_actual <- data_raw_actual %>% 
  #   filter(
  #     entity_name == country) %>% 
  #   mutate (actual = hfe_kpi) %>% 
  #   select(
  #     year,
  #     actual
  #   ) 
  # 
  # data_for_chart <-  merge(x = data_prep_target, y = data_prep_actual, by="year", all.x = TRUE) 
  
  sheet <- country
  range <- "A11:M14"
  df <- read_range(env_kea_file, sheet, range)  
  
  ## prepare data
  data_for_chart <- df %>% 
    mutate_at(c(-1), ~ as.numeric(.)) %>% 
    mutate(across(c(2:13), ~paste0(format(round(.*100,2)), "%"))) %>%  
    mutate_all(~ str_replace(., "NA%", "")) %>% 
    rename(a = 1) %>% 
    filter(a == 'KEA') %>% 
    pivot_longer(-a, names_to = "month") %>% 
    mutate(actual = as.numeric(str_replace(value, '%', '')),
           target = target_value,
           year = seq(1:12))
  
    }


# plot chart ----
## function moved to utils  
mybarct(mywidth, myheight, myfont, mylinewidth, mymargin)

# # export to image 
# w = 1200
# h = 600
# export_fig(mybarct(w, h, 14 * w/900), paste0("env_", mymetric , "_main.png"), w, h)

