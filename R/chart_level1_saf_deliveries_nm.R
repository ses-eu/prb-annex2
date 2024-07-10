# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "NM_data.xlsx"),
  sheet = "PI_overdeliveries",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# prepare data ----
data_prep <- data_raw %>% 
  filter(
    year_report == .env$year_report) %>% 
  mutate(
    xlabel = year,
    mymetric = round(percentage_overdeliveries * 100, 1),
    type = "Actual"
      )

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
mytitle_text <- "Percentage of overdeliveries"
mytitle_y <- 0.99

#### xaxis

#### yaxis
myyaxis_title <- "Precentage of overdeliveries (%)"
myyaxis_ticksuffix <- "%"
myyaxis_tickformat <- ".1f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

#### margin
mylocalmargin = mymargin

# plot chart ----
## function moved to utils  
mybarchart(data_prep, mywidth, myheight, myfont, mylocalmargin, mydecimals) %>% 
  add_empty_trace(., data_prep)
