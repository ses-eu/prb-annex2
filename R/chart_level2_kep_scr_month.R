 
if (country == "Network Manager") {
  # NM case ----
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
    rename(type = indicator_type,
           mymetric = kep_value_percent,
           xlabel = month) %>% 
    mutate(mymetric = round(mymetric, 2)) %>% 
    filter(
      entity_name == country,
      lubridate::year(xlabel) == year_report
    ) %>% 
    select(xlabel, type, mymetric)
  
  data_raw_scr_p <- data_raw_scr %>% 
    rename(type = indicator_type,
           mymetric = scr_value,
           xlabel = month) %>% 
    mutate(mymetric = round(mymetric * 100, 2)) %>% 
    filter(
      entity_name == country,
      lubridate::year(xlabel) == year_report
    )  %>% 
    select(xlabel, type, mymetric)
  
  data_prep <- data_raw_kep_p %>% 
    rbind(data_raw_scr_p) %>% 
    mutate(
      xlabel = lubridate::floor_date(xlabel, unit = 'month') 
      ) %>% as_tibble()

  data_prep_kea <- data_raw_kea %>% 
    filter(
      entity_name == country,
      lubridate::year(month) == year_report) %>% 
    mutate (myothermetric = hfe_kpi_percent,
            type = indicator_type,
            xlabel = lubridate::floor_date(month, unit = 'month' )) %>% 
    select(
      xlabel,
      type,
      myothermetric
    ) 
  
  
    }

## chart parameters ----
mysuffix <- "%"
mydecimals <- 2

### trace parameters
mycolors = c('#FFC000', '#5B9BD5')
###set up order of traces
myfactor <- c("KEP", "SCR")

myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- -90
mytextposition <- "inside"
myinsidetextanchor <- 'top'
mytextfont_color <- 'black'
mytextfont_size <- myfont * 0.9

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- paste0("Monthly KEA, KEP and SCR - ", country)

#### xaxis
myxaxis_dtick <- 'M1'
myxaxis_tickformat <- "%b"

#### yaxis
myyaxis_title <- "KEA, KEP and SCR (%)"
myyaxis_ticksuffix <- "%"
myyaxis_tickformat <- ".1f"

#### legend

#### margin

#____additional trace parameters
myat_name <- "KEA"
myat_mode <- "markers"
myat_yaxis <- "y1"
myat_symbol <- NA
myat_marker_color <- '#FF0000'
myat_line_color <- 'transparent'
myat_line_width <- mylinewidth
myat_showlegend <- T

myat_textbold <- TRUE
myat_textangle <- 0
myat_textposition <- 'top'
myat_textfont_color <- 'transparent'
myat_textfont_size <- myfont

# plot chart ----
## function moved to utils  
mybarchart(data_prep, mywidth, myheight, myfont, mylocalmargin, mydecimals) %>% 
  add_line_trace(., data_prep_kea)
