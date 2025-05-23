if (!exists("country") | is.na(country)) {country = "SES RP3"}

# import data  ---- 
if (country == "SES RP3"){
  ## SES case ----
  data_raw_kep  <-  read_xlsx(
    paste0(data_folder, "SES file.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Table_KEP MM",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() |> 
    #so it has the same structure as the state case
    mutate(entity_name = "SES RP3") 
  
  data_raw_scr  <-  read_xlsx(
    paste0(data_folder, "SES file.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Table_SCR MM",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() |> 
    #so it has the same structure as the state case
    mutate(entity_name = "SES RP3") 
  
  data_raw_kea  <-  read_xlsx(
    paste0(data_folder, "SES file.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Table_HFE MM",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() |> 
    #so it has the same structure as the state case
    mutate(entity_name = "SES RP3") 

} else  {
  ## State case ----
  data_raw_kep  <-  read_xlsx(
    paste0(data_folder, "ENV dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Table_KEP MM",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_raw_scr  <-  read_xlsx(
    paste0(data_folder, "ENV dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Table_SCR MM",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_raw_kea  <-  read_xlsx(
    paste0(data_folder, "ENV dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Table_HFE MM",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  }

# prepare data ----
data_raw_kep_p <- data_raw_kep %>% 
  rename(type = indicator_type,
         mymetric = kep_value_percent,
         xlabel = month) %>% 
  mutate(mymetric = round(mymetric, 2)) %>% 
  filter(
    entity_name == country,
    lubridate::year(xlabel) == year_report
  ) %>% 
  select(xlabel, type, mymetric)

data_raw_scr_p <- data_raw_scr %>% 
  rename(type = indicator_type,
         mymetric = scr_value,
         xlabel = month) %>% 
  mutate(mymetric = round(mymetric * 100, 2)) %>% 
  filter(
    entity_name == country,
    lubridate::year(xlabel) == year_report
  )  %>% 
  select(xlabel, type, mymetric)

data_prep <- data_raw_kep_p %>% 
  rbind(data_raw_scr_p) %>% 
  mutate(
    xlabel = lubridate::floor_date(xlabel, unit = 'month') 
  ) %>% as_tibble()

data_prep_kea <- data_raw_kea %>% 
  filter(
    entity_name == country,
    lubridate::year(month) == year_report) %>% 
  mutate (myothermetric = hfe_kpi_percent,
          type = indicator_type,
          xlabel = lubridate::floor_date(month, unit = 'month' )) %>% 
  select(
    xlabel,
    type,
    myothermetric
  ) 

# chart parameters ----
mysuffix <- "%"
mydecimals <- 2

### trace parameters
mycolors = c('#FFC000', '#5B9BD5')
###set up order of traces
myfactor <- c("KEP", "SCR")

myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- -90
mytextposition <- "inside"
myinsidetextanchor <- 'middle'
mytextfont_color <- 'black'

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- paste0("KEP & SCR (monthly, compared to KEA)")
mytitle_y <- 0.99

#### xaxis
myxaxis_dtick <- 'M1'
myxaxis_tickformat <- "%b"

#### yaxis
myyaxis_title <- "KEA, KEP and SCR (%)"
myyaxis_ticksuffix <- "%"
myyaxis_tickformat <- ".2f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

if (knitr::is_latex_output()) {
  mytextfont_size <- myfont * 0.8
  mylocallegend_y <- -0.18
  
  
} else {
  mylocallegend_y <- mylegend_y
  mytextfont_size <- myfont * 0.9
  
}

#### margin
mylocalmargin = mymargin

#____additional trace parameters
myat_name <- "KEA"
myat_mode <- "markers"
myat_yaxis <- "y1"
myat_symbol <- NA
myat_marker_color <- '#FF0000'
myat_line_color <- 'transparent'
myat_line_width <- mylinewidth
myat_showlegend <- T

myat_textbold <- TRUE
myat_textangle <- 0
myat_textposition <- 'top'
myat_textfont_color <- 'transparent'
myat_textfont_size <- myfont

# plot chart ----
## function moved to utils  
p1 <- mybarchart(data_prep, mywidth, myheight+30, myfont, mylocalmargin, 
           mydecimals, mylocallegend_y) %>% 
  add_line_trace(., data_prep_kea)



if (knitr::is_latex_output()) {
  p1 <- p1 %>% layout(bargap = 0.05,
                      xaxis = list(
                        tickangle = -90  # Rotate x-axis tick labels to -90 degrees
                      ))  
  p1
  
} else {
  # p1 <- p1 %>% layout(bargap = 0.05) 
  p1
  
}