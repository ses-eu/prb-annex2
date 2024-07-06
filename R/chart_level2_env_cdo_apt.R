## import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "ENV dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "Table_CDO airports",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

airports_table <- read_mytable("Lists.xlsx", "Lists", "Table_TCZs_RP3") %>%  clean_names()

## prepare data ----
airports_country <- airports_table %>% 
  filter(country_name == .env$country) 

data_prep <- data_raw %>% 
  filter(
    entity_name == .env$country,
    year == .env$year_report,
    airport_code %in% airports_table$apt_code) %>%
  left_join(airports_table, by = c("airport_code" = "apt_code")) %>% 
  mutate(
    xlabel = apt_name,
    type = indicator_type,
    mymetric = round(cdo_airport_value * 100, 0)
  ) %>%  
  select(
    xlabel,
    type,
    mymetric)

## chart parameters ----
mysuffix <- "%"
mydecimals <- 0

### trace parameters
mycolors = c('#0070C0')
###set up order of traces
myfactor <- data_prep %>% select(type) %>% unique() 

myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- 0
mytextposition <- "outside"
myinsidetextanchor <- NA
mytextfont_color <- 'black'

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- paste0("CDO by airport - ", year_report)

#### xaxis

#### yaxis
myyaxis_title <- "Share of flights (%)"
myyaxis_ticksuffix <- "%"
myyaxis_tickformat <- ".0f"

#### legend

#### margin

## plot chart  ----
mybarchart(data_prep, mywidth, myheight, myfont, mylocalmargin, mydecimals)
