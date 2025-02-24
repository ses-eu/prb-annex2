if (!exists("country") | is.na(country)) {country = "SES RP3"}
if (exists("cztype") == FALSE) {cztype = "terminal"}

if (country == 'SES RP3') {
  # SES case ----
  ## import data  ----
  data_raw_actual  <-  read_xlsx(
    paste0(data_folder, "SES file.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = if_else(cztype == "enroute", "en route monthly delay",
                    "terminal monthly delay"),
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_raw_target  <-  read_xlsx(
    paste0(data_folder, "SES file.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "en route delay targets",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----
  data_prep_target <- data_raw_target %>% 
    filter(
      year == .env$year_report) %>% 
    right_join(filter(data_raw_actual, year == .env$year_report),
               by = "year") %>% 
    mutate(
      xlabel = month,
      myothermetric = round(delay_target, 2),
      type = "Target"
    ) %>% 
    select(
      xlabel,
      myothermetric,
      type
    )
  
  if(cztype == 'terminal') {data_prep_target$myothermetric = NA}

  data_prep_actual <- data_raw_actual %>% 
    filter(
      year == .env$year_report) %>% 
    select(c(month, atc_capacity, atc_staffing, atc_disruptions, weather, other_non_atc)) %>% 
    pivot_longer(
      cols = c(atc_capacity, atc_staffing, atc_disruptions, weather, other_non_atc),
      names_to = "type",
      values_to = "mymetric"
    ) %>% 
    mutate(
      xlabel = month,
      type = case_when(
        type == "atc_capacity" ~ "Capacity",
        type == "atc_staffing" ~ "Staffing",
        type == "atc_disruptions" ~ "Disruptions",
        type == "weather" ~ "Weather",
        type == "other_non_atc" ~ "Other non-ATC"
      ) 
    ) 

  data_prep_total <- data_raw_actual %>% 
    filter(year == .env$year_report) %>% 
    mutate(xlabel = month,
           myothermetric = format(round(average_delay,2), digits = 2),
           type = "Total") %>% 
    select(xlabel, myothermetric, type)
  
  
} else {
  # state case ----
  ## import data  ----
  data_raw_target  <-  read_xlsx(
    paste0(data_folder, "CAP dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = if_else(cztype == "enroute", "en route delay targets", "terminal delay targets"),
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_raw_actual  <-  read_xlsx(
    paste0(data_folder, "CAP dataset master.xlsx"),
    sheet = if_else(cztype == "enroute", "en route monthly delay", "terminal monthly delay"),
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----
  if(cztype == "terminal") {
    data_raw_target <- data_raw_target %>% 
      rename(delay_target = x332_state_arr_delay_target)
  }
  
  data_prep_target <- data_raw_target %>% 
    filter(
      state == .env$country,
      year == .env$year_report
      ) %>% 
    mutate(
      myothermetric = round(delay_target, 2),
      xlabel = year,
      type = "Target"
    ) %>% 
    select(
      xlabel,
      type,
      myothermetric)
  
  data_prep_actual <- data_raw_actual %>% 
    filter(
      state == .env$country,
      year == .env$year_report
      ) %>% 
    pivot_longer(
      cols = c(atc_capacity, atc_staffing, atc_disruptions, weather, other_non_atc),
      names_to = "type",
      values_to = "mymetric"
    ) %>% 
    mutate(
      mymetric = round(mymetric,2),
      xlabel = month,
      type = case_when(
        type == "atc_capacity" ~ "Capacity",
        type == "atc_staffing" ~ "Staffing",
        type == "atc_disruptions" ~ "Disruptions",
        type == "weather" ~ "Weather",
        type == "other_non_atc" ~ "Other non-ATC"
      )
    ) %>% 
    select(xlabel, type, mymetric)
  
  data_prep_total <- data_prep_actual %>% 
    select(xlabel, mymetric) %>% 
    group_by(xlabel) %>% 
    summarise(myothermetric = sum(mymetric)) %>%
    mutate(myothermetric = case_when(
      is.na(myothermetric) == TRUE ~ "",
      .default = format(round(myothermetric,2), digits = 2)
      )
      ) %>% 
    mutate(type = "Total")
}

# chart ----

## chart parameters ----
mysuffix <- ""
mydecimals <- 2

### trace parameters
mycolors = c('#EF7D22', '#F9CCAB', '#FDB014', '#70AD47', '#A0A0A0')
###set up order of traces
myfactor <- c("Capacity", "Staffing", 
              "Disruptions", "Weather",
              "Other non-ATC")
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- 0
mytextposition <- "inside"
myinsidetextanchor <- 'middle'
mytextfont_color <- 'transparent'
mytextfont_size <- 1


### layout parameters
mybargap <- 0.25
mybarmode <- 'stack'

#### title
if (knitr::is_latex_output()) {
  mylevel1_title <- " ATFM delay\nby delay groups  - "
  mytitle_y <- 0.95
  mylocallegend_y <- -0.2
  mylegend_x <- -0.18
  mylegend_xanchor <- 'left'
  mylocallegend_fontsize <- myfont-1
  mylocalmargin <- list(t = 50)
  
} else {
  mylevel1_title <- " ATFM delay by delay groups  - "
  mytitle_y <- 0.99
  mylocallegend_y <- mylegend_y
  mylegend_x <- -0.1
  mylegend_xanchor <- 'left'
  mylocallegend_fontsize <- myfont
  mylocalmargin <- mymargin
  
}

mytitle_text <- paste0("Monthly distribution of ", if_else(cztype == "enroute", "en route", "arrival"),
                       mylevel1_title, year_report)

#### xaxis
myxaxis_dtick <- 'M1'
myxaxis_tickformat <- "%b"

#### yaxis
myyaxis_title <- "ATFM delay (min/flight)"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ".2f"

#### legend

#### margin

#____additional trace parameters
## additional trace with text totals
myat_name <- "Total delay"
myat_mode <- "markers"
myat_yaxis <- "y1"
myat_symbol <- NA
myat_marker_color <- 'transaprent'
myat_line_color <- 'transparent'
myat_line_width <- 1
myat_showlegend <- F

myat_textbold <- FALSE
myat_textangle <- 0
myat_textposition <- 'top'
myat_textfont_color <- 'black'
myat_textfont_size <- myfont

# plot chart ----
p1 <- mybarchart(data_prep_actual, mywidth, myheight + 20, myfont, mylocalmargin, 
           mydecimals, mylocallegend_y, mylocallegend_fontsize) %>% 
  add_line_trace(., data_prep_total) %>% 
  layout(yaxis = list(rangemode = "tozero"))


if (knitr::is_latex_output()) {
  p1 <- p1 %>% layout(xaxis = list(
    tickangle = -90  # Rotate x-axis tick labels to -90 degrees
  )) 
  p1
  
} else {
  # p1 <- p1 %>% layout(bargap = 0.05) 
  p1
  
}
