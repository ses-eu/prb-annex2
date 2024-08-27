## import data  ----
if (country == 'SES RP3') {
  data_raw  <-  read_xlsx(
    paste0(data_folder, "SES file.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "DTB",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() |> 
    #so it has the same columns as the state case
    mutate(ansp = "ansp",
           state = "SES RP3")
  
  } else {  
    
  data_raw  <-  read_xlsx(
    paste0(data_folder, "CAP dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "DelayTimeBin",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
}

## prepare data ----
data_prep <- data_raw %>% 
  filter(
    state == .env$country,
    year <= .env$year_report) %>%
  mutate(
    x0_5mins_perc = if_else(total_dlyflt ==0, 0, x0_5mins /total_dlyflt *100),
    x5_15_mins_perc = if_else(total_dlyflt ==0, 0, x5_15_mins /total_dlyflt *100),
    x15_30_mins_perc = if_else(total_dlyflt ==0, 0, x15_30_mins /total_dlyflt *100),
    x30_60_mins_perc = if_else(total_dlyflt ==0, 0, x30_60_mins /total_dlyflt *100),
    x60_mins_perc = if_else(total_dlyflt ==0, 0, x60_mins /total_dlyflt  *100)
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
mycolors = c('#4472C4', '#EF7D22', '#A0A0A0', '#FDB014', '#5B9BD5')
###set up order of traces
myfactor <- c('<5 min',
              '5 - 15 min',
              '15 - 30 min',
              '30 - 60 min',
              '>60 min') 
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- 0
mytextposition <- "inside"
myinsidetextanchor <- 'middle'
mytextfont_color <- 'black'

### layout parameters
mybargap <- 0.25
mybarmode <- 'stack'

#### title
mytitle_text <- paste0("Distribution of IFR flights per the duration of en route ATFM delay")
mytitle_y <- 0.99

#### xaxis

#### yaxis
myyaxis_title <- "Share of IFR flights (%)"
myyaxis_ticksuffix <- "%"
myyaxis_tickformat <- ".0f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

#### margin
mylocalmargin <- mymargin

## plot chart  ----
mybarchart(data_prep, mywidth, myheight, myfont, mylocalmargin, mydecimals) %>% 
  layout(xaxis = list(range= c(2019.5,2024.5)),
         yaxis = list(rangemode = "tozero"))
