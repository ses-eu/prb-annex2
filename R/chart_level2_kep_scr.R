 
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
      paste0(data_folder, "SES_OLD.xlsx"),
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
  rp3_years <- data.frame(rp3_years) %>% rename(xlabel = rp3_years)
  
  data_prep_kep <- data_raw_kep %>% 
    mutate(type = indicator_type,
           mymetric = round(kep_value_percent,2),
           xlabel = year) %>% 
    filter(
      entity_name == country,
      year <= year_report
    ) %>% 
    select(xlabel, type, mymetric)
  
  data_prep_kep_full <- data_prep_kep %>% 
    right_join(rp3_years, by = 'xlabel') 
  
  data_prep_scr <- data_raw_scr %>% 
    mutate(type = indicator_type,
           mymetric = round(scr_value*100,2),
           xlabel = year) %>% 
    filter(
      entity_name == country,
      year <= year_report
    ) %>% 
    select(xlabel, type, mymetric)
  
  data_prep_scr_full <- data_prep_scr %>% 
    right_join(rp3_years, by = 'xlabel') 
  
  
  data_prep <- data_prep_kep_full %>% 
    rbind(data_prep_scr_full)
  
  
    }

## chart parameters ----
mysuffix <- "%"
mydecimals <- 2

### trace parameters
mycolors = c('#FFC000', '#5B9BD5' )
###set up order of traces
myfactor <- c("KEP", "SCR")

myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- 0
mytextposition <- "outside"
myinsidetextanchor <- NA
mytextfont_color <- 'black'

### layout 
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <-  paste0("KEP & SCR – average horizontal flight efficiency of the last\nfiled flight plan (PI#1) & shortest constrained trajectory(PI#2)")
mytitle_y <- 0.95

#### xaxis

#### yaxis
myyaxis_title <- "KEP and SCR (%)"
myyaxis_ticksuffix <- "%"
myyaxis_tickformat <- ".1f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

#### margin
mylocalmargin <- list(t = 70)

# plot chart ----
mybarchart(data_prep, mywidth, myheight+30, myfont, mylocalmargin, mydecimals) %>% 
  add_empty_trace(., data_prep) 
