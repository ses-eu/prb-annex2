
if (exists("cztype") == FALSE) {cztype = "enroute"}

if (country == 'SES RP3') {
  # SES case ----
  ## import data  ----
  data_raw_actual  <-  read_xlsx(
    paste0(data_folder, "SES file.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = if_else(cztype == "enroute", "Avg en-route ATFM delay",
                    "Avg terminal ATFM delay"),
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
    mutate(
      xlabel = year,
      myothermetric = round(delay_target, 2),
      type = "Target"
    ) %>% 
    select(
      xlabel,
      type,
      myothermetric
    ) %>% arrange(xlabel)
  
  if(cztype == 'terminal') {data_prep_target$myothermetric = NA}

  data_prep <- data_raw_actual %>% 
    select(c(year, atc_capacity, atc_staffing, atc_disruptions, weather, other_non_atc)) %>% 
    pivot_longer(
      cols = c(atc_capacity, atc_staffing, atc_disruptions, weather, other_non_atc),
      names_to = "type",
      values_to = "mymetric"
    ) %>% 
    mutate(
      movements = NA,
      xlabel = year,
      type = case_when(
        type == "atc_capacity" ~ "Capacity",
        type == "atc_staffing" ~ "Staffing",
        type == "atc_disruptions" ~ "Disruptions",
        type == "weather" ~ "Weather",
        type == "other_non_atc" ~ "Other non-ATC"
      ),
      mymetric = case_when(
        xlabel > year_report ~ NA,
        .default = mymetric
      )
    ) 
  
  data_prep_total <- data_raw_actual %>% 
    mutate(xlabel = year,
           myothermetric = case_when(
             xlabel > year_report ~ NA,
             .default = format(round(average_delay,2), digits = 2)
           ),
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
    sheet = if_else(cztype == "enroute", "Avg en-route ATFM delay", "Avg terminal ATFM delay"),
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----
  if (cztype == "terminal") {
    data_raw_target <- data_raw_target %>% 
      rename(delay_target = x332_state_arr_delay_target)
  }
  data_prep_target <- data_raw_target %>% 
    filter(
      state == .env$country) %>% 
    mutate(
      myothermetric = round(delay_target, 2),
      type = 'Target',
      xlabel = year
    ) %>% 
    select(
      xlabel,
      type,
      myothermetric
    ) %>% arrange(xlabel)
  
  data_prep <- data_raw_actual %>% 
    filter(
      state == .env$country,
      year <= .env$year_report) %>% 
    rename(xlabel = year) %>% 
    # right_join(rp3_years, by = "xlabel") %>% 
    pivot_longer(
      cols = c(atc_capacity, atc_staffing, atc_disruptions, weather, other_non_atc),
      names_to = "type",
      values_to = "mymetric"
    ) %>% 
    mutate(
      mymetric = case_when(
        is.na(mymetric) == TRUE & xlabel == year_report ~ 0,
        .default = mymetric
      ),
      type = case_when(
        type == "atc_capacity" ~ "Capacity",
        type == "atc_staffing" ~ "Staffing",
        type == "atc_disruptions" ~ "Disruptions",
        type == "weather" ~ "Weather",
        type == "other_non_atc" ~ "Other non-ATC"
      )
    )
  
  data_prep_total <- data_prep %>% 
    select(xlabel, mymetric) %>% 
    group_by(xlabel) %>% 
    summarise(myothermetric = sum(mymetric)) %>%
    mutate(myothermetric = case_when(
      is.na(myothermetric) == TRUE ~ "",
      .default = format(round(myothermetric,2), digits = 2))
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
mytitle_text <- paste0("Average ", 
                       if_else(cztype == "enroute", "en route", "terminal"),
                       " ATFM delay per flight\nby delay groups")
mytitle_y <- 0.95

#### xaxis

#### yaxis
myyaxis_title <- "ATFM delay (min/flight)"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ".2f"

#### legend
mylegend_x <- -0.1
mylegend_xanchor <- 'left'

#### margin
mylocalmargin <- list(t = 60)

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

myat_textbold <- TRUE
myat_textangle <- 0
myat_textposition <- 'top'
myat_textfont_color <- 'black'
myat_textfont_size <- myfont

# plot chart ----
## function moved to utils  
myplot <- mybarchart(data_prep, mywidth, myheight + 20, myfont, mylocalmargin, mydecimals) %>% 
  add_line_trace(., data_prep_total)

## additional target trace
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

myplot %>%  add_line_trace(., data_prep_target) |> 
  add_empty_trace(data_prep)

