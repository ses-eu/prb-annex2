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
    "Hours allocated for activity" = ersa_allocated,
    "Hours used for activity" = ersa_used
  ) |> 
  pivot_longer(-xlabel, names_to = 'type', values_to = 'mymetric') |> 
  mutate(mymetric = round(mymetric/1000, 1))

## chart parameters ----
mysuffix <- ""
mydecimals <- 1

### trace parameters
mycolors = c('#FFC200', '#5B9BD5' )
###set up order of traces
myfactor <- c("Hours allocated for activity", "Hours used for activity") 
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- 0
mytextposition <- "outside"
myinsidetextanchor <- NA
mytextfont_color <- 'black'

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
if (knitr::is_latex_output()) {
  mytitle_text <- paste0("Effective use of reserved or segregated\nairspace (ERSA)(PI#6)")
  mytitle_y <- 0.95
  mylocalmargin = list(t = 60)
  
} else {
  mytitle_text <- paste0("Effective use of reserved or segregated airspace (ERSA)(PI#6)")
  mytitle_y <- 0.99
}

#### xaxis

#### yaxis
myyaxis_title <- "ERSA ('000 hours)"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ".1f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

#### margin
mylocalmargin = mymargin

## define chart function ----
# function moved to utils

## plot chart  ----
mybarchart(data_prep, mywidth, myheight, myfont, mylocalmargin, mydecimals) %>% 
  layout(bargroupgap = 0.15,
         yaxis=list(rangemode = "tozero"),
         xaxis=list(range = c(2019.5, 2024.5),
                    tickformat = '.0f')
  ) 
