
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
      er_cap_target = round(target, 2)
    ) %>% 
    select(
      year,
      er_cap_target
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
  
  data_prep <- data_raw_actual %>% 
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
  
  data_prep_total <- data_prep %>% 
    select(xlabel, mymetric) %>% 
    group_by(xlabel) %>% 
    summarise(myothermetric = sum(mymetric)) %>%
    mutate(myothermetric = format(round(myothermetric,2), digits = 2)) %>% 
    mutate(type = "Total")
}

# chart ----

## chart parameters ----
mysuffix <- ""
mydecimals <- 2

### trace parameters
mycolors = c('#ED7D31', '#F8CBAD', '#BF8F00', '#92D050', '#A5A5A5')
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
mytitle_text <- paste0("Monthly ", if_else(cztype == "enroute", "en route", "arrival"),
                       " ATFM delay per flight - ", year_report)

#### xaxis
myxaxis_dtick <- 'M1'
myxaxis_tickformat <- "%b"

#### yaxis
myyaxis_title <- "Average minutes of delay"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ".2f"

#### legend

#### margin

# if (knitr::is_latex_output()) {
#   if (country == 'SES RP3') {
#     mylocalmargin <- list (t = 20, r = 0, l = 30)
#   } else {
#     mylocalmargin <- list (t = 20, r = 50, l = 10)
#   }
# } else {
#   mylocalmargin <- list (t = 40, r = 70)
# }

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
## function moved to utils  
mybarchart(data_prep, mywidth, myheight + 20, myfont, mylocalmargin, mydecimals) %>% 
  add_line_trace(., data_prep_total)

