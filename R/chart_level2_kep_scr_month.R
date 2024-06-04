 
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
    sheet = "Table_KEP MM",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_raw_scr  <-  read_xlsx(
    paste0(data_folder, "ENV dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Table_SCR MM",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_raw_kea  <-  read_xlsx(
    paste0(data_folder, "ENV dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Table_HFE MM",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  
  ## prepare data ----
  data_raw_kep_p <- data_raw_kep %>% 
    rename(status = indicator_type,
           mymetric = kep_value) %>% 
    mutate(mymetric = round(mymetric, 2)) %>% 
    filter(
      entity_name == country,
      lubridate::year(month) == year_report
    ) 
  
  data_raw_scr_p <- data_raw_scr %>% 
    rename(status = indicator_type,
           mymetric = scr_value) %>% 
    mutate(mymetric = round(mymetric * 100, 2)) %>% 
    filter(
      entity_name == country,
      lubridate::year(month) == year_report
    ) 
  
  data_prep <- data_raw_kep_p %>% 
    rbind(data_raw_scr_p) %>% 
    mutate(
      year_text = lubridate::floor_date(month, unit = 'month') 
      ) %>% as_tibble()

  data_prep_kea <- data_raw_kea %>% 
    filter(
      entity_name == country,
      lubridate::year(month) == year_report) %>% 
    mutate (kea = hfe_kpi,
            year = lubridate::floor_date(month, unit = 'month' )) %>% 
    select(
      year,
      kea
    ) 
  
  ## chart parameters ----
  mychart_title <- paste0("Monthly KEA, KEP and SCR - ", country)
  myaxis_title <- "KEA, KEP and SCR (%)"
  mylegend_y_position <- -0.1
  mycolors <- c('#FFC000', '#5B9BD5')
  
  mytextangle <- -90
  mytextposition <- "inside"
  mytextsize <- myfont * 0.9
  mylabelposition <- 'top'

  mydtick <- 'M1'
  mytickformat_x <- "%b"
  myrange <- NA
  
  myticksuffix <- "%"
  mytickformat_y <- ",.0f"
  
  ###set up order of traces
  myfactor <- c("KEP", "SCR")
  
    }

# plot chart ----
## function moved to utils  
mybarc_nonst(mywidth, myheight, myfont, mymargin) %>% 
  add_trace(
    data = data_prep_kea,
    inherit = FALSE,
    x = ~ year,
    y = ~ kea,
    yaxis = "y1",
    type = 'scatter',  mode = 'markers',
    marker = list(size = mylinewidth * 3, color = '#FF0000'),
    name = "KEA",
    opacity = 1,
    hovertemplate = paste0('%{y:.2f}', myticksuffix),
    # hoverinfo = "none",
    showlegend = T
  )



