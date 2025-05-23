if (!exists("country") | is.na(country)) {country = "SES RP3"}

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
  
} else {
  if (country == "SES RP3") {
    # SES case ----
    ## import data  ----
    data_raw_target  <-  read_xlsx(
      paste0(data_folder, "SES file.xlsx"),
      # here("data","hlsr2021_data.xlsx"),
      sheet = "Table_KEA Targets",
      range = cell_limits(c(1, 1), c(NA, NA))) %>%
      as_tibble() %>% 
      clean_names() |> 
      mutate(entity_name = "SES RP3")
    
    data_raw_actual  <-  read_xlsx(
      paste0(data_folder, "SES file.xlsx"),
      # here("data","hlsr2021_data.xlsx"),
      sheet = "Table_HFE MM",
      range = cell_limits(c(1, 1), c(NA, NA))) %>%
      as_tibble() %>% 
      clean_names() |> 
      mutate(entity_name = "SES RP3")
    
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
      sheet = "Table_HFE MM",
      range = cell_limits(c(1, 1), c(NA, NA))) %>%
      as_tibble() %>% 
      clean_names() 
    }
    ## prepare data ----
  target_value <- data_raw_target %>%
    filter(
      entity_name == .env$country,
      year == .env$year_report
    ) %>%
    mutate(
      # type = 'Target',
      target = round(kea_reference_value_percent, 2)
    ) %>%
    select(
      target
    ) %>% pull()

  data_prep_actual <- data_raw_actual %>% 
    filter(
      entity_name == country,
      lubridate::year(month) == year_report) %>% 
    mutate (mymetric = hfe_kpi_percent,
            xlabel = lubridate::floor_date(month, unit = 'month' )) %>% 
    select(
      xlabel,
      mymetric
    ) 

  ## prepare datasetfor chart
  data_prep <- data_prep_actual %>% 
    mutate(myothermetric = target_value,
           type = "Actual")
  
    }

## chart parameters ----
mysuffix <- "%"
mydecimals <- 2

### trace parameters
mycolors = c( '#FFC000')
###set up order of traces
myfactor <- "Actual"

myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- -90
mytextposition <- "inside"
myinsidetextanchor <- 'middle'
mytextfont_color <- 'black'

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- paste0(if_else(country == "Network Manager", "KEP (monthly)", "KEA (monthly)"))
mytitle_y <- 0.99

#### xaxis
myxaxis_dtick <- 'M1'
myxaxis_tickformat <- "%b"

#### yaxis
myyaxis_title <- paste0(if_else(country == "Network Manager", "KEP", "KEA"), " (%)")
myyaxis_ticksuffix <- "%"
myyaxis_tickformat <- ".2f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

if (knitr::is_latex_output()) {
  mylocallegend_y <- -0.18
  
} else {
  mylocallegend_y <- mylegend_y
  
}

#### margin
mylocalmargin <- mymargin


#____additional trace parameters
myat_name <- "Target"
myat_mode <- "line"
myat_yaxis <- "y1"
myat_symbol <- NA
myat_marker_color <- 'transparent'
myat_line_color <- '#FF0000'
myat_line_width <- mylinewidth
myat_showlegend <- T

myat_textbold <- TRUE
myat_textangle <- 0
myat_textposition <- 'top'
myat_textfont_color <- 'transparent'
myat_textfont_size <- myfont

# plot chart ----
p1 <- mybarchart(data_prep, mywidth, myheight+30, myfont, 
           mylocalmargin, mydecimals, mylocallegend_y) %>% 
  add_line_trace(., data_prep)


if (knitr::is_latex_output()) {
  p1 <- p1 %>% layout(xaxis = list(
                        tickangle = -90  # Rotate x-axis tick labels to -90 degrees
                      )) 
    p1
  
} else {
  # p1 <- p1 %>% layout(bargap = 0.05) 
  p1
  
}
