# import data  ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "NM_data.xlsx"),
    sheet = "Capacity_En route",
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
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- 0
mytextposition <- "inside"
myinsidetextanchor <- 'middle'
mytextfont_color <- 'black'

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- "Percentage of en route ATFM delay savings"
mytitle_y <- 0.99

#### xaxis

#### yaxis
myyaxis_title <- "En route ATFM delay savings (%)"
myyaxis_ticksuffix <- "%"
myyaxis_tickformat <- ".1f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

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
           mywidth, myheight, myfont, mylocalmargin, mydecimals) %>% 
  add_line_trace(., filter(data_prep, type == "Target"))
  