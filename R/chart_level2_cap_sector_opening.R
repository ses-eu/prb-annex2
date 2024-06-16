## import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "CAP dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "Sectorhour",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

## prepare data ----
data_prep <- data_raw %>% 
  filter(
    state == .env$country,
    year <= .env$year_report) %>%
  mutate(
    xlabel = year,
    mymetric = round(total_soh,0),
    type = "Sector opening hours"
    ) %>% 
 select(
    xlabel,
    type,
    mymetric)

## chart parameters ----
mysuffix <- ""
mydecimals <- 0

### trace parameters
mycolors = c('#44546A')
###set up order of traces
myfactor <- data_prep$type %>% unique() 

mytextangle <- -90
mytextposition <- "inside"
myinsidetextanchor <- 'middle'
mytextfont_color <- 'white'
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
mytitle_text <- paste0("Sector opening hours")
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
myyaxis_title <- "Sector opening hours"
myyaxis_gridcolor <- 'rgb(240,240,240)'
myyaxis_showgrid <- TRUE
myyaxis_showline <- FALSE
myyaxis_tickprefix <- ""
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ",.0f"

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
