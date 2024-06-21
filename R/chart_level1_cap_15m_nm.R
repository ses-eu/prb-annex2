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
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- "IFR flights with ATFM delay above 15 min."

#### xaxis

#### yaxis
myyaxis_title <- "IFR flights with ATFM delay > 15 min (%)"
myyaxis_ticksuffix <- "%"
myyaxis_tickformat <- ".1f"

#### legend

#### margin

# plot chart ----
## function moved to utils  
mybarchart(data_prep, mywidth, myheight+10, myfont, mylocalmargin) %>% 
  add_empty_trace(., data_prep)
