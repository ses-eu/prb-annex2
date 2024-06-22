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
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- -90
mytextposition <- "inside"
myinsidetextanchor <- 'middle'
mytextfont_color <- 'white'

### layout parameters
mybargap <- 0.25
mybarmode <- 'stack'

#### title
mytitle_text <- paste0("Sector opening hours")

#### xaxis

#### yaxis
myyaxis_title <- "Sector opening hours"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ",.0f"

#### legend

#### margin

## plot chart  ----
mybarchart(data_prep, mywidth, myheight, myfont, mylocalmargin, mydecimals) %>% 
  layout(xaxis = list(range= c(2019.5,2024.5)))
