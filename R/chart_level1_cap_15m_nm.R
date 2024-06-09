# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "NM_data.xlsx"),
  sheet = "Capacity_PIs",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# prepare data ----
data_prep <- data_raw %>% 
  filter(
    year_report == .env$year_report) %>% 
  mutate(
    xlabel = year,
    mymetric = round(percent_above_15_min * 100, 1),
    type = "Actual"
      ) %>% 
  select(
    xlabel,
    type,
    mymetric
  )

# chart parameters ----
mysuffix <- "%"
mydecimals <- 1

### trace parameters
mycolors = c( '#ED7D31')
###set up order of traces
myfactor <- "Actual"

mytextangle <- 0
mytextposition <- "inside"
myinsidetextanchor <- 'middle'
mytextfont_color <- 'white'
mytextfont_size <- myfont

myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)
mytrace_showlegend <- T

### layout parameters
myfont_family <- "Roboto"
mybargap <- 0.25
mybarmode <- 'group'
myhovermode <- "x unified"
myhoverlabel_bgcolor <- 'rgba(255,255,255,0.88)'
myminsize <- myfont*0.8

#### title
mytitle_text <- "IFR flights with ATFM delay above 15 min."
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
myxaxis_dtick <- 1
myxaxis_tickformat <- "0"
myxaxis_zeroline <- TRUE
myxaxis_tickfont_size <- myfont

#### yaxis
myyaxis_title <- "IFR flights with ATFM delay > 15 min (%)"
myyaxis_gridcolor <- 'rgb(240,240,240)'
myyaxis_showgrid <- TRUE
myyaxis_showline <- FALSE
myyaxis_tickprefix <- ""
myyaxis_ticksuffix <- "%"
myyaxis_tickformat <- ".1f"

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

# plot chart ----
## function moved to utils  
mybarchart(data_prep, mywidth, myheight+10, myfont, mylocalmargin) %>% 
  add_empty_trace(., data_prep)
