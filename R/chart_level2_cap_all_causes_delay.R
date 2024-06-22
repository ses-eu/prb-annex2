## import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "CAP dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "ACausePreDep",
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
    mymetric = round(all_cause_predep_dly,1),
    type = "Pre-departure delay per flight"
    ) %>% 
 select(
    xlabel,
    type,
    mymetric)

## chart parameters ----
mysuffix <- ""
mydecimals <- 1

### trace parameters
mycolors = c('#5B9BD5')
###set up order of traces
myfactor <- data_prep$type %>% unique() 
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- 0
mytextposition <- "outside"
myinsidetextanchor <- NA
mytextfont_color <- 'black'

### layout parameters
mybargap <- 0.25
mybarmode <- 'stack'

#### title
mytitle_text <- paste0("All causes pre departure delay")

#### xaxis

#### yaxis
myyaxis_title <- "Average minutes of\npre departure delay per flight"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ".0f"

#### legend

#### margin

## define chart function ----
# function moved to utils

## plot chart  ----
mybarchart(data_prep, mywidth, myheight, myfont, mylocalmargin, mydecimals) %>% 
  layout(xaxis = list(range= c(2019.5,2024.5)))
