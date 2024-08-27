## import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "ENV dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "Table_MIL_PIs",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

## prepare data ----
data_prep <- data_raw %>% 
  filter(
    state == .env$country,
    year <= .env$year_report) %>% 
  select(
    xlabel = year,
    "RAI" = rai_cdr,
    "RAU" = rau_cdr
  ) |> 
  pivot_longer(-xlabel, names_to = 'type', values_to = 'mymetric') |> 
  mutate(mymetric = round(mymetric, 0))

## chart parameters ----
mysuffix <- "%"
mydecimals <- 0

### trace parameters
mycolors = c('#FFC200', '#5B9BD5' )
###set up order of traces
myfactor <- c("RAI", "RAU") 
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- 0
mytextposition <- "outside"
myinsidetextanchor <- NA
mytextfont_color <- 'black'

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- paste0("RAI & RAU via available conditional routes (PIs#7 & 8)")
mytitle_y <- 0.99

#### xaxis

#### yaxis
myyaxis_title <- "RAI & RAU (%)"
myyaxis_ticksuffix <- "%"
myyaxis_tickformat <- ".0f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

#### margin
mylocalmargin = list(t = 60)

## define chart function ----
# function moved to utils

## plot chart  ----
myplot <- mybarchart(data_prep, mywidth, myheight+30, myfont, mylocalmargin, mydecimals) %>% 
  layout(bargroupgap = 0.15,
         yaxis = list(range= c(0,100)))

# in case all values are NA
if (all(is.na(data_prep$mymetric)) == TRUE) {
  myplot <- myplot |> 
    layout(xaxis = list(range= c(2019.5,year_report + 0.5)))
  }

myplot