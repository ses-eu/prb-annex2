 
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
  data_prep <- data_raw %>% 
    filter(year_report == .env$year_report) %>% 
    mutate(
      xlabel = year
      # target = round(nm_target * 100, 2),
      # actual = round(actual * 100, 2)
    ) 
  
  data_prep_actual <- data_prep %>% 
    mutate(mymetric = case_when(
           year > year_report ~ NA,
           .default = round(actual * 100, 2)),
           type = "Actual"
           ) %>% 
    select(xlabel, mymetric, type)
  
  data_prep_target <- data_prep %>% 
    mutate(myothermetric = round(nm_target * 100, 2),
           type = "Target")
  
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
    data_prep <- data_raw %>% 
      filter(year_report == .env$year_report) %>% 
      mutate(
        xlabel = year,
        type = status
      ) 
    
    data_prep_actual <- data_prep %>% 
      filter(type == "Actual") %>% 
      mutate(mymetric = case_when(
        xlabel > year_report ~NA,
        .default = round(kea_value * 100, 2))
        ) %>% 
      select(xlabel, mymetric, type)
    
    data_prep_target <- data_prep %>% 
      filter(type == "Target") %>% 
      mutate(myothermetric = round(kea_value * 100, 2)) %>% 
      select(xlabel, myothermetric, type)

} else  {
  # State case ----

  ## import data  ----
  data_raw_target  <-  read_xlsx(
    paste0(data_folder, "ENV dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Table_KEA Targets",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_raw_actual  <-  read_xlsx(
    paste0(data_folder, "ENV dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Table_HFE",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----
  data_prep_target <- data_raw_target %>% 
    filter(
      entity_name == .env$country
    ) %>% 
    mutate(
      # type = 'Target',
      target = round(kea_reference_value_percent, 2)
    ) %>% 
    select(
      year,
      target
    ) %>% 
    mutate(
      xlabel = year,
      myothermetric = target,
      type = "Target"
    ) 
  
  data_prep_actual <- data_raw_actual %>% 
    filter(
      entity_name == country,
      year <= year_report) %>% 
    mutate (actual = hfe_kpi_percent) %>% 
    select(
      year,
      actual
    ) %>% 
    mutate(
      xlabel = year,
      mymetric = actual,
      type = "Actual"
    ) 
  
  # data_prep <- merge(x = data_prep_target, y = data_prep_actual, by="year", all.x = TRUE)
    
  
    }

## chart parameters ----
mysuffix <- "%"
mydecimals <- 2

### trace parameters
mycolors = c( '#FFC000')
###set up order of traces
myfactor <- "Actual"
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- 0
mytextposition <- "inside"
myinsidetextanchor <- 'middle'
mytextfont_color <- 'black'

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- paste0(if_else(country == "Network Manager", "KEP", "KEA - average horizontal flight efficiency\nof the actual trajectory (KPI#1)"))
mytitle_y <- 0.95

#### xaxis

#### yaxis
myyaxis_title <- paste0(if_else(country == "Network Manager", "KEP", "KEA"), " (%)")
myyaxis_ticksuffix <- "%"
myyaxis_tickformat <- ".1f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

#### margin
mylocalmargin <- list(t=60)

#____additional trace parameters
myat_name <- "Target"
myat_mode <- "line+markers"
myat_yaxis <- "y1"
myat_symbol <- NA
myat_marker_color <- '#FF0000'
myat_line_color <- '#FF0000'
myat_line_width <- mylinewidth
myat_showlegend <- T

myat_textbold <- FALSE
myat_textangle <- 0
myat_textposition <- 'top'
myat_textfont_color <- '#FF0000'
myat_textfont_size <- myfont


# plot chart ----
## function moved to utils  
mybarchart(data_prep_actual, mywidth, myheight+30, myfont, mylocalmargin, mydecimals) %>% 
  add_line_trace(., data_prep_target)

# %>% 
#   add_empty_trace(., data_prep) 

