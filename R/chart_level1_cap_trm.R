# import data  ----

data_raw_target  <-  read_xlsx(
  paste0(data_folder, "CAP dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "terminal delay targets",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

data_raw_actual  <-  read_xlsx(
  paste0(data_folder, "CAP dataset master.xlsx"),
  sheet = "Avg terminal ATFM delay",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# prepare data ----
data_prep_target <- data_raw_target %>% 
  filter(
    state == .env$country) %>% 
  mutate(
    myothermetric = round(x332_state_arr_delay_target, 2),
    xlabel = year,
    type = "Target"
  ) %>% 
  select(
    xlabel,
    type,
    myothermetric
  ) %>% arrange(xlabel)

data_prep <- data_raw_actual %>% 
  filter(
    state == .env$country,
    year >= 2020,
    year <= year_report) %>% 
  pivot_longer(
    cols = c(atc_capacity, atc_staffing, atc_disruptions, weather, other_non_atc),
    names_to = "type",
    values_to = "mymetric"
  ) %>% 
  mutate(
    xlabel = year,
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

# chart ----
## set parameters for chart ----

# if (knitr::is_latex_output()) {
#   if (country == 'SES RP3') {
#     mymargin <- list (t = 20, r = 0, l = 30)
#   } else {
#     mymargin <- list (t = 20, r = 50, l = 10)
#   }
#   mylegend_x_pos <- -0.12
# } else {
#   mymargin <- list (t = 40, r = 70)
#   mylegend_x_pos <- -0.12
# }
mysuffix <- ""
mydecimals <- 2

### trace parameters
mycolors = c('#ED7D31', '#F8CBAD', '#BF8F00', '#92D050', '#A5A5A5')
###set up order of traces
myfactor <- c("Capacity", "Staffing", 
              "Disruptions", "Weather",
              "Other non-ATC")

mytextangle <- 0
mytextposition <- "inside"
myinsidetextanchor <- 'middle'
mytextfont_color <- 'transparent'
mytextfont_size <- 1

myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)
mytrace_showlegend <- T

### layout parameters
myfont_family <- "Roboto"
mybargap <- 0.25
mybarmode <- 'stack'
myhovermode <- "x unified"
myhoverlabel_bgcolor <- 'rgba(255,255,255,0.88)'
myminsize <- myfont*0.8

#### title
mytitle_text <- paste0("Average arrival ATFM delay per flight - ", country)
mytitle_x <- 0
mytitle_y <- 0.99
mytitle_xanchor <- 'left'
mytitle_yanchor <- 'top'
mytitle_font_size <- myfont * 20/15

#### xaxis
myxaxis_title <- ""
myxaxis_gridcolor <- 'rgb(255,255,255)'
myxaxis_showgrid <- TRUE
myxaxis_showline <- FALSE
myxaxis_showticklabels <- TRUE
myxaxis_dtick <- 1
myxaxis_tickformat <- "0"
myxaxis_zeroline <- TRUE
myxaxis_tickfont_size <- myfont

#### yaxis
myyaxis_title <- "Average minutes of delay"
myyaxis_gridcolor <- 'rgb(240,240,240)'
myyaxis_showgrid <- TRUE
myyaxis_showline <- FALSE
myyaxis_tickprefix <- ""
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ".2f"

myyaxis_zeroline <- TRUE
myyaxis_zerolinecolor <- 'rgb(255,255,255)'
myyaxis_titlefont_size <- myfont
myyaxis_tickfont_size <- myfont

#### legend
mylegend_traceorder <- 'normal'
mylegend_orientation <- 'h'
mylegend_xanchor <- "center"
mylegend_yanchor <- "center"
mylegend_x <- 0.5
mylegend_y <- -0.1
mylegend_font_size <- myfont

#### margin
mylocalmargin <- mymargin

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
myat_marker_color <- 'transaprent'
myat_line_color <- 'transparent'
myat_line_width <- 1
myat_showlegend <- F

myat_textbold <- TRUE
myat_textangle <- 0
myat_textposition <- 'top'
myat_textfont_color <- 'black'
myat_textfont_size <- myfont

## plot chart ----
# function moved to utils  
myplot <- mybarchart(data_prep, mywidth, myheight + 20, myfont, mylocalmargin) %>% 
  add_line_trace(., data_prep_total)

# additional target trace
myat_name <- "Target"
myat_mode <- "line+markers"
myat_marker_color <- '#FF0000'
myat_line_color <- '#FF0000'
myat_line_width <- mylinewidth
myat_showlegend <- T

myat_textbold <- TRUE
myat_textangle <- 0
myat_textposition <- 'top'
myat_textfont_color <- '#FF0000'
myat_textfont_size <- myfont

myplot %>%  add_line_trace(., data_prep_target)
  

