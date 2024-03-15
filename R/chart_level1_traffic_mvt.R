# libraries ----
library(tidyr)
library(dplyr)
library(readxl)
library(plotly)
library(stringr)
library(janitor)
library(webshot)
library(magick)

# parameters ----
source("R/parameters.R")

# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "STATFOR_forecast_en-route_MVT.xlsx"),
  sheet = "data",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# prepare data ----
max_actual_year <- as.numeric(substrRight(forecast, 4))-1

data_prep <- data_raw %>% 
  filter(
    tz == statfor_zone, 
    daio == "T",
    yr < 2025
  ) %>% 
  mutate(rank = paste0(rank, ' forecast'))

data_prep_forecast <-  data_prep %>%
  filter(
    forecast_id == forecastid
    ) %>%
  mutate(mvts = case_when (
    yr > max_actual_year ~ mvts,
    TRUE ~ NA
  )
  )

data_prep_actual <-  data_prep %>%
  filter(
    forecast_id == 5,
    rank == 'Base forecast'
  ) %>%
  mutate(
    forecast_id = forecastid,
    rank = 'Actual',
    mvts = case_when (
    yr <= year_report ~ mvts,
    TRUE ~ NA
    )
    )

# plot chart ----
c <-   data_prep_forecast %>% 
  plot_ly(
  x = ~ yr,
  y = ~ round(mvts/1000,0),
  yaxis = "y1",
  cliponaxis = FALSE,
  yaxis = "y1",
  type = 'scatter',  mode = 'lines+markers',
  line = list(width = 3, dash = 'dash'),
  marker = list(size = 9),
  color = ~ rank,
  colors = c('#1969B4','#044598', '#229FDD'),
  opacity = 1,
  # hovertemplate = paste('Target: %{y:.2f}%<extra></extra>'),
  # hoverinfo = "none",
  showlegend = T
) %>% 
  add_trace(
    data = data_prep_actual,
   inherit = FALSE,
   x = ~ yr,
   y = ~ round(mvts/1000,0),
   yaxis = "y1",
   cliponaxis = FALSE,
   yaxis = "y1",
   type = 'scatter',  mode = 'lines+markers',
   line = list(width = 3, dash = 'solid', color = '#FFC000'),
   marker = list(size = 9, color = '#FFC000'),
   color = ~ rank,
   opacity = 1,
   # hovertemplate = paste('Target: %{y:.2f}%<extra></extra>'),
   # hoverinfo = "none",
   showlegend = T
  ) %>% 
  config( responsive = TRUE,
          displaylogo = FALSE,
          displayModeBar = F
          # modeBarButtons = list(list("toImage")),
  ) %>% 
  layout(
    font = list(family = "Roboto"),
    title = list(text=paste0("IFR movements - ", forecast, " - ", country),
                 y = 1, 
                 x = 0, 
                 xanchor = 'left', 
                 yanchor =  'top'),
    hovermode = "x unified",
    hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
    xaxis = list(title = "",
                 gridcolor = 'rgb(255,255,255)',
                 showgrid = FALSE,
                 showline = FALSE,
                 showticklabels = TRUE,
                 dtick = 1,
                 # tickcolor = 'rgb(127,127,127)',
                 # ticks = 'outside',
                 zeroline = TRUE
                 ),
    yaxis = list(title = "IFR movements ('000)",
                 # gridcolor = 'rgb(255,255,255)',
                 showgrid = TRUE,
                 showline = FALSE,
                 tickformat = ",",
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

# export to image ----
export_fig(c,"traffic_mvt_main.png")