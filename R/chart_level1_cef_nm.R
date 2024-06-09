# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "NM_data.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "Cost-efficiency",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# prepare data ----
data_prep_wide <- data_raw %>% 
  filter(year_report == .env$year_report) %>% 
  select(-c(year_report, entity_name)) 

data_prep <- data_prep_wide %>%
  mutate(xlabel= year) %>% 
  select(xlabel, target, actual) %>% 
  pivot_longer(-xlabel, names_to = "type", values_to = "mymetric") %>% 
  mutate(type = if_else(type == "actual", "Actual CSU", "Determined CSU"))

data_prep_costs <- data_prep_wide %>%
  mutate(xlabel= year) %>% 
  select(xlabel, planned_costs, actual_costs) %>% 
  pivot_longer(-xlabel, names_to = "type", values_to = "myothermetric") %>% 
  mutate(type = if_else(type == "actual_costs", "Actual costs", "Planned costs"))

# chart parameters ----
mysuffix <- ""
mydecimals <- 2

### trace parameters
mycolors = c('#5B9BD5', '#FFC000', '#5B9BD5', '#FFC000')
###set up order of traces
myfactor <- data_prep %>% select(type) %>% unique() 
as.list(myfactor$type)
myfactor <- sort(myfactor$type, decreasing = TRUE)

mytextangle <- -90
mytextposition <- "inside"
myinsidetextanchor <- "middle"
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
mytitle_text <- paste0("Costs per service unit")
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
myyaxis_title <- "Costs per service unit (€2017)"
myyaxis_gridcolor <- 'rgb(240,240,240)'
myyaxis_showgrid <- TRUE
myyaxis_showline <- FALSE
myyaxis_tickprefix <- ""
myyaxis_ticksuffix <- ""
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
mylocalmargin = list(t = 60, b = 0, l = 60, r = 50)

# plot chart  ----
mybarchart(data_prep, mywidth, myheight, myfont, mylocalmargin) %>% 
  add_trace(
    inherit = FALSE,
    data = filter(data_prep_costs, type == "Planned costs"),
    x = ~ xlabel,
    y = ~ round(myothermetric/1000, 2),
    # name = "Actual costs",
    yaxis = "y2",
    type = 'scatter',
    mode = "line+markers",
    name = "Planned costs",
    text = "",
    line = list(width = mylinewidth, color = '#5B9BD5'),
    marker = list(size = mylinewidth * 3, color = '#5B9BD5'),
    showlegend = T
  )  %>%
  add_trace(
    inherit = FALSE,
    data = filter(data_prep_costs, type == "Actual costs"),
    x = ~ xlabel,
    y = ~ round(myothermetric/1000, 2),
    # name = "Actual costs",
    yaxis = "y2",
    type = 'scatter',
    mode = "line+markers",
    name = "Actual costs",
    text = "",
    line = list(width = mylinewidth, color = '#FFC000'),
    marker = list(size = mylinewidth * 3, color = '#FFC000'),
    showlegend = T
  )  %>% 
  layout(yaxis2 = list(title = "Total costs ('000 €2017)",
                     overlaying = "y",
                     side = "right",
                     showgrid = FALSE,
                     showline = FALSE,
                     tickformat = ",",
                     rangemode = "tozero",
                     zeroline = TRUE,
                     zerolinecolor = 'rgb(255,255,255)',
                     titlefont = list(size = myfont), tickfont = list(size = myfont)
  ))

