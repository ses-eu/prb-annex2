## import data  ----
if (country == 'SES RP3') {
  data_raw  <-  read_xlsx(
    paste0(data_folder, "SES file.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Sectorhour",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() |> 
    #so it has the same columns as the state case
    mutate(ansp = "ansp",
           state = "SES RP3")
  
} else {
  
  data_raw  <-  read_xlsx(
    paste0(data_folder, "CAP dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Sectorhour",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
}

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
mycolors = c('#8497B0')
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
mytitle_text <- paste0("Sector opening hours",
                       if_else(country != "SES RP3",
                               paste0(" - ", main_ansp), "")
                       )
mytitle_y <- 0.99

#### xaxis

#### yaxis
myyaxis_title <- "Sector opening hours"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ",.0f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

#### margin
mylocalmargin = mymargin

## plot chart  ----
mybarchart(data_prep, mywidth, myheight, myfont, mylocalmargin, mydecimals) %>% 
  layout(xaxis = list(range= c(2019.5,2024.5), tickformat = '.0f'))
