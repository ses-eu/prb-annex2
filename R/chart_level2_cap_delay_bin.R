## import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "CAP dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "DelayTimeBin",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

## prepare data ----
data_prep <- data_raw %>% 
  filter(
    state == .env$country,
    year <= .env$year_report) %>%
  mutate(
    x0_5mins_perc = x0_5mins /total_dlyflt *100,
    x5_15_mins_perc = x5_15_mins /total_dlyflt *100,
    x15_30_mins_perc = x15_30_mins /total_dlyflt *100,
    x30_60_mins_perc = x30_60_mins /total_dlyflt *100,
    x60_mins_perc = x60_mins /total_dlyflt  *100
    ) %>% 
  select(
    year, state, ansp,
    x0_5mins_perc,
    x5_15_mins_perc,
    x15_30_mins_perc,
    x30_60_mins_perc,
    x60_mins_perc
  ) %>% 
    pivot_longer(-c(year, state, ansp), names_to = 'type', values_to = 'mymetric') %>% 
  mutate(
    xlabel = year,
    mymetric = round(mymetric,0),
    type = case_when(
      type == 'x0_5mins_perc'  ~ '<5 min',
      type == 'x5_15_mins_perc' ~ '5 - 15 min',
      type == 'x15_30_mins_perc' ~ '15 - 30 min',
      type == 'x30_60_mins_perc' ~ '30 - 60 min',
      type == 'x60_mins_perc' ~ '>60 min'
    )
  ) %>%  
  select(
    xlabel,
    type,
    mymetric)

## chart parameters ----
mysuffix <- "%"
mydecimals <- 0

### trace parameters
mycolors = c('#4472C4', '#ED7D31', '#A5A5A5', '#FFC000', '#5B9BD5')
###set up order of traces
myfactor <- c('<5 min',
              '5 - 15 min',
              '15 - 30 min',
              '30 - 60 min',
              '>60 min') 

mytextangle <- 0
mytextposition <- "inside"
myinsidetextanchor <- 'middle'
mytextfont_color <- 'black'
mytextfont_size <- myfont

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
mytitle_text <- paste0("Delay time bin")
mytitle_x <- 0
mytitle_y <- 0.99
mytitle_xanchor <- 'left'
mytitle_yanchor <- 'top'
mytitle_font_size <- myfont * 20/15

#### xaxis
myxaxis_title <- ''
myxaxis_gridcolor <- 'rgb(255,255,255)'
myxaxis_showgrid <- TRUE
myxaxis_showline <- FALSE
myxaxis_showticklabels <- TRUE
myxaxis_tickformat <- "0"
myxaxis_dtick <- 1
myxaxis_zeroline <- TRUE
myxaxis_tickfont_size <- myfont

#### yaxis
myyaxis_title <- "To be added"
myyaxis_gridcolor <- 'rgb(240,240,240)'
myyaxis_showgrid <- TRUE
myyaxis_showline <- FALSE
myyaxis_tickprefix <- ""
myyaxis_ticksuffix <- "%"
myyaxis_tickformat <- ".0f"

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
mylocalmargin = mymargin


## define chart function ----
# function moved to utils

## plot chart  ----
mybarchart(data_prep, mywidth, myheight, myfont, mylocalmargin) %>% 
  layout(xaxis = list(range= c(2019.5,2024.5)))
