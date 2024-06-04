 
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
  ## import data  ----
  data_raw_kep  <-  read_xlsx(
    paste0(data_folder, "ENV dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Table_KEP",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_raw_scr  <-  read_xlsx(
    paste0(data_folder, "ENV dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Table_SCR",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----
  ## create sequence of years to ensure full series
  rp3_years <- 2020:2024
  rp3_years <- data.frame(rp3_years) %>% rename(year = rp3_years)
  
  data_raw_kep_p <- data_raw_kep %>% 
    rename(status = indicator_type,
           mymetric = kep_value) %>% 
    mutate(mymetric = round(mymetric, 2)) %>% 
    filter(
      entity_name == country,
    ) 
  
  data_raw_kep_p_f <- rp3_years %>% left_join(data_raw_kep_p, by= 'year') %>% 
    mutate(entity_name = country,
           status = 'KEP')
  
  data_raw_scr_p <- data_raw_scr %>% 
    rename(status = indicator_type,
           mymetric = scr_value) %>% 
    mutate(mymetric = round(mymetric * 100, 2)) %>% 
    filter(
      entity_name == country,
    ) 

  data_raw_scr_p_f <- rp3_years %>% left_join(data_raw_scr_p, by= 'year') %>% 
    mutate(entity_name = country,
           status = 'SCR')
  
  data_prep <- data_raw_kep_p_f %>% 
    rbind(data_raw_scr_p_f) %>% 
    mutate(
      mymetric = case_when(
        year > year_report ~ NA,
        year <= year_report ~ mymetric
        ),
      year_text = as.factor(year)
      ) %>% as_tibble()


  ## chart parameters ----
  mychart_title <- paste0("KEP and SCR - ", country)
  myaxis_title <- "KEP and SCR (%)"
  mylegend_y_position <- -0.1
  mycolors = c('#FFC000', '#5B9BD5' )
  
  mytextangle <- 0
  mytextposition <- "outside"
  mytextsize <- myfont
  mylabelposition <- 'middle'
  
  mydtick <- '1'
  mytickformat_x <- "0"
  # myrange <- list(2019, 2025)
  
  myticksuffix <- "%"
  mytickformat_y <- ",.0f"
  ###set up order of traces
  myfactor <- c("KEP", "SCR")
  
    }

# plot chart ----
## function moved to utils  
mybarc_nonst(mywidth, myheight, myfont, mymargin)

# # export to image 
# w = 1200
# h = 600
# export_fig(mybarct(w, h, 14 * w/900), paste0("env_", mymetric , "_main.png"), w, h)

