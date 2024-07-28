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
  mutate(mymetric = if_else(type == "actual" & xlabel > year_report, NA, mymetric),
    type = if_else(type == "actual", "Actual CSU", "Determined CSU"),
         )

data_prep_costs <- data_prep_wide %>%
  mutate(xlabel= year) %>% 
  select(xlabel, planned_costs, actual_costs) %>% 
  pivot_longer(-xlabel, names_to = "type", values_to = "myothermetric") %>% 
  mutate(myothermetric = if_else(type == "actual_costs" & xlabel > year_report, 
                                 NA, 
                                 round(myothermetric/1000, 2)),
    type = if_else(type == "actual_costs", "Actual costs", "Planned costs"),
  )

# chart parameters ----
mysuffix <- ""
mydecimals <- 2

### trace parameters
mycolors = c('#5B9BD5', '#FFC000', '#5B9BD5', '#FFC000')
###set up order of traces
myfactor <- c("Determined CSU", "Actual CSU")
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- -90
mytextposition <- "inside"
myinsidetextanchor <- "middle"
mytextfont_color <- 'black'

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'
myminsize <- myfont*0.95

#### title
mytitle_text <- paste0("Costs per service unit")
mytitle_y <- 0.99

#### xaxis

#### yaxis
myyaxis_title <- "Costs per service unit (€2017)"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ".0f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

#### margin
mylocalmargin = list(t = 60, b = 0, l = 40, r = 50)

#____additional trace parameters
myat_name <- "Planned costs"
myat_mode <- "line+markers"
myat_yaxis <- "y2"
myat_symbol <- NA
myat_marker_color <- '#5B9BD5'
myat_line_color <- '#5B9BD5'
myat_line_width <- mylinewidth
myat_showlegend <- T

myat_textbold <- FALSE
myat_textangle <- 0
myat_textposition <- 'top'
myat_textfont_color <- 'transparent'
myat_textfont_size <- myfont

# plot chart  ----
myplot_trace1 <- mybarchart(data_prep, mywidth, myheight, myfont, mylocalmargin, mydecimals) %>% 
  add_line_trace(., filter(data_prep_costs, type == "Planned costs"))

#____additional trace parameters
myat_name <- "Actual costs"
myat_mode <- "line+markers"
myat_yaxis <- "y2"
myat_symbol <- NA
myat_marker_color <- '#FFC000'
myat_line_color <- '#FFC000'
myat_line_width <- mylinewidth
myat_showlegend <- T

myat_textbold <- FALSE
myat_textangle <- 0
myat_textposition <- 'top'
myat_textfont_color <- 'transparent'
myat_textfont_size <- myfont


myplot_trace1 %>% add_line_trace(., filter(data_prep_costs, type == "Actual costs")) %>% 
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

