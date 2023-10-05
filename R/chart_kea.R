## libraries
library(tidyr)
library(dplyr)
library(readxl)
library(plotly)
library(stringr)
library(janitor)

## parameters
source("R/parameters.R")

## import data
data_raw_target  <-  read_xlsx(
  paste0(data_folder, "targets.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "KEA",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

data_raw_actual  <-  read_xlsx(
  paste0(data_folder, "HFE_clean.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "Table_HFE",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 


## prepare data
data_prep_target <- data_raw_target %>% 
  filter(
    state == country,
    year_report == year_report) %>% 
  mutate(
    # type = 'Target',
    kea_target = round(x321_kea_target * 100, 2)
  ) %>% 
  select(
    year,
    kea_target
  )

data_prep_actual <- data_raw_actual %>% 
  filter(
    entity_name == country) %>% 
  select(
    year,
    hfe_kpi
  ) 

data_for_chart <-  merge(x = data_prep_target, y = data_prep_actual, by="year", all.x = TRUE) 

## plot chart
c <-   data_for_chart %>% 
  plot_ly(
  x = ~ year,
  y = ~ hfe_kpi,
  yaxis = "y1",
  marker = list(color =('#FFC000')),
  text = ~ paste0(hfe_kpi,'%'),
  # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
  # textangle = -90,
  textposition = "inside", 
  cliponaxis = FALSE,
  insidetextanchor =  "middle",
  name = "KEA",
  textfont = list(color = 'black'),
  type = "bar",
  # hoverinfo = "none",
  # domain = list(x = c(0, 1), y = c(0, 1)),
  showlegend = T
) %>% 
  add_trace(
    inherit = FALSE,
    x = ~ year,
    y = ~ kea_target,
    yaxis = "y1",
    type = 'scatter',  mode = 'lines+markers',
    line = list(color = '#FF0000', width = 2),
    marker = list(color = '#FF0000'),
    name = "Target",
    opacity = 1,
    # hoverinfo = "none",
    showlegend = T
  ) %>%
  add_trace(
    inherit = FALSE,
    x = ~ year,
    y = ~ kea_target + 0.05,
    yaxis = "y1",
    mode = 'text',
    text = ~ paste0('<b>', kea_target,'%', '</b>'),
    textposition = "top", cliponaxis = FALSE,
    textfont = list(color = '#FF0000'),
    hoverinfo = "none",
    showlegend = F
  ) %>%
  config( responsive = TRUE,
          displaylogo = FALSE,
          displayModeBar = F
          # modeBarButtons = list(list("toImage")),
  ) %>% 
  layout(
    font = list(family = "Roboto"),
    bargap = 0.25,
    hovermode = "x unified",
    xaxis = list(title = "",
                 gridcolor = 'rgb(255,255,255)',
                 showgrid = FALSE,
                 showline = FALSE,
                 showticklabels = TRUE,
                 # tickcolor = 'rgb(127,127,127)',
                 # ticks = 'outside',
                 zeroline = TRUE
                 ),
    yaxis = list(title = "KEA (%)",
                 # gridcolor = 'rgb(255,255,255)',
                 showgrid = TRUE,
                 showline = FALSE,
                 ticksuffix = "% ",
                 tickformat = ".1f",
                 # showticklabels = TRUE,
                 # tickcolor = 'rgb(127,127,127)',
                 # ticks = 'outside',
                 zeroline = TRUE,
                 zerolinecolor = 'rgb(255,255,255)'
                 ),
    # showlegend = FALSE
    legend = list(
      orientation = 'h', 
      xanchor = "center",
      x = 0.5, 
      y =-0.1
      )
    
  )

c

