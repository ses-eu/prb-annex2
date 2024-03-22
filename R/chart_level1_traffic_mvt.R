
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "STATFOR_forecast_en-route_MVT.xlsx"),
  sheet = "data",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

data_raw_planned  <-  read_xlsx(
  paste0(data_folder, "targets.xlsx"),
  sheet = "IFR_MVTS",
  range = cell_limits(c(3, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# prepare data ----
max_actual_year <- as.numeric(substrRight(forecast, 4))-1

data_prep <- data_raw %>% 
  filter(
    tz == statfor_zone, 
    daio == "T",
    yr < 2025,
    yr >= 2019
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

data_prep_planned <- data_raw_planned %>% 
  filter(state == country,
         year > 2020) %>% 
  select(state, year, x121_ecz_ifr_mvt)  %>%
  rename(mvts = x121_ecz_ifr_mvt) %>% 
  mutate(rank = 'Planned')

# plot chart ----
mycolors <-  c('#1969B4','#044598', '#229FDD')

myc <- function (mywidth, myheight, myfont) {
  plot_ly(
    width = mywidth,
    height = myheight,
    data = data_prep_forecast,
  x = ~ yr,
  y = ~ round(mvts/1000,0),
  yaxis = "y1",
  cliponaxis = FALSE,
  yaxis = "y1",
  type = 'scatter',  mode = 'lines+markers',
  line = list(width = 3, dash = 'dash'),
  marker = list(size = 9),
  color = ~ rank,
  colors = mycolors,
  opacity = 1,
  # hovertemplate = paste('Target: %{y:.2f}%<extra></extra>'),
  # hoverinfo = "none",
  showlegend = T
) %>% 
  add_trace(
      data = data_prep_planned,
      inherit = FALSE,
      x = ~ year,
      y = ~ round(mvts,0),
      yaxis = "y1",
      cliponaxis = FALSE,
      yaxis = "y1",
      type = 'scatter',  mode = 'lines+markers',
      line = list(width = 3, dash = 'solid', color = '#5B9BD5'),
      marker = list(size = 9, color = '#5B9BD5'),
      color = ~ rank,
      opacity = 1,
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
    title = list(text=paste0("IFR movements - ", forecast, " - ", 
                             if_else(country == "Spain", 
                                     country, ecz_list$ecz_name[1])
                    ),
                 y = 0.99, 
                 x = 0, 
                 xanchor = 'left', 
                 yanchor =  'top',
                 font = list(size = myfont * 20/15)
                 ),
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
                 zeroline = TRUE,
                 tickfont = list(size = myfont)
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
                 zerolinecolor = 'rgb(255,255,255)',
                 titlefont = list(size = myfont), tickfont = list(size = myfont)
                 ),
    # showlegend = FALSE
    legend = list(
      orientation = 'h', 
      xanchor = "left",
      x = -0.1, 
      y =-0.1,
      font = list(size = myfont*0.95)
      ),
    margin = list (t = 40)
    
  )
}

myc(NULL, 280, 14)

# export to image ----
w = 1200
h = 600
export_fig(myc(w, h, 14 * w/900),"traffic_mvt_main.png", w, h)