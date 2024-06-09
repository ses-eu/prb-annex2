  
# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "NM_data.xlsx"),
  sheet = "Capacity_Terminal",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 
  
# prepare data ----
data_prep <- data_raw %>% 
  filter(year_report == .env$year_report) %>% 
  mutate(
    xlabel = year,
    Target = round(target * 100, 0),
    Actual = round(actual * 100, 1)
  ) %>% 
  select(xlabel, Target, Actual) %>% 
  pivot_longer(-xlabel, names_to = "type", values_to = "mymetric") %>% 
  mutate(myothermetric = mymetric)

# chart parameters ----
mysuffix <- "%"
mydecimals <- 1

### trace parameters
mycolors = c( '#FFC000')
###set up order of traces
myfactor <- "Actual"

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
mybarmode <- 'group'
myhovermode <- "x unified"
myhoverlabel_bgcolor <- 'rgba(255,255,255,0.88)'
myminsize <- myfont*0.8

#### title
mytitle_text <- "Percentage of arrival ATFM delay savings"
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
myyaxis_title <- "Arrival ATFM delay savings (%)"
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

#____additional trace parameters
myat_name <- "Target"
myat_mode <- "line+markers"
myat_yaxis <- "y1"
myat_symbol <- NA
myat_marker_color <- '#FF0000'
myat_line_color <- '#FF0000'
myat_line_width <- mylinewidth
myat_showlegend <- T

myat_textbold <- TRUE
myat_textangle <- 0
myat_textposition <- 'top'
myat_textfont_color <- 'transparent'
myat_textfont_size <- myfont

# plot chart ----
## function moved to utils  
mybarchart(filter(data_prep, type == "Actual"),
           mywidth, myheight, myfont, mylocalmargin) %>% 
  add_line_trace(., filter(data_prep, type == "Target"))