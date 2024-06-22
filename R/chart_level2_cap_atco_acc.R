## import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "CAP dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "ATCOs",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

## prepare data ----
data_prep <- data_raw %>% 
  filter(
    state == .env$country,
    year == .env$year_report) %>% 
  select(acc, planned_atco_number, actual_atco_number) %>% 
  rename(xlabel = acc,
         Planned = planned_atco_number,
         Actual = actual_atco_number) %>% 
  pivot_longer(-xlabel, names_to = 'type', values_to = 'mymetric')

## chart parameters ----
mysuffix <- ""
mydecimals <- 0

### trace parameters
mycolors = c('#5B9BD5', '#FFC200')
###set up order of traces
myfactor <- c('Planned', 'Actual') 
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- 0
mytextposition <- "outside"
myinsidetextanchor <- NA
mytextfont_color <- 'black'

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- paste0("ATCOs in OPS per ACC - ", year_report)

#### xaxis

#### yaxis
myyaxis_title <- "ATCOs in OPS"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ".0f"

#### legend

#### margin

## define chart function ----
# function moved to utils

## plot chart  ----
mybarchart(data_prep, mywidth, myheight, myfont, mylocalmargin, mydecimals) %>% 
  layout(bargroupgap = 0.15)
