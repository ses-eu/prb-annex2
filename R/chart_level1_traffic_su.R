# 
# # parameters ----
# if (exists("data_folder") == FALSE) {
#   source("R/parameters.R")
# }

# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "STATFOR_forecast_en-route_TSU.xlsx"),
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

data_spain <- data_raw %>% 
  filter(tz_id %like% "Spain") %>% 
  group_by(forecast_id, forecast_name, rank, year) %>% 
  summarise(tsu = sum(tsu)) %>% ungroup() %>% 
  mutate(tz_id = "Spain") %>% 
  relocate(tz_id, .before = year)

data_prep <- rbind(data_raw, data_spain) %>% 
  filter(
    tz_id == statfor_zone, 
    year < 2025,
    year >= 2019
    
  ) %>% 
  mutate(rank = paste0(rank, ' forecast'))


data_prep_forecast <-  data_prep %>%
  filter(
    forecast_id == .env$forecast_id
  ) %>%
  mutate(tsu = case_when (
    year > max_actual_year ~ tsu,
    TRUE ~ NA
  )
  )

data_prep_actual <-  data_prep %>%
  filter(
    forecast_id == 5,
    rank == 'Base forecast'
  ) %>%
  mutate(
    forecast_id = .env$forecast_id,
    rank = 'Actual',
    tsu = case_when (
    year <= year_report ~ tsu,
    TRUE ~ NA
    )
    )

data_prep_planned <- data_raw_planned %>% 
  filter(state == country,
         status == 'D',
         year > 2020) %>% 
  select(state, year, x121_ecz_su)  %>% 
  group_by(year) %>% summarise (tsu = sum(x121_ecz_su, na.rm=TRUE)) %>% 
  mutate(rank = 'Determined')

if (country == 'SES RP3') {
  data_prep_planned <- data_raw_planned %>% 
    filter(status == 'D',
           year > 2020) %>% 
    select(state, year, x121_ecz_su)  %>% 
    group_by(year) %>% summarise (tsu = sum(x121_ecz_su, na.rm=TRUE)) %>% 
    mutate(rank = 'Determined')
}

mycolors <-  c('#1969B4','#044598', '#229FDD')

# plot chart ----
myc <- function (mywidth, myheight, myfont) {
  plot_ly(
    width = mywidth,
    height = myheight,
    data = data_prep_forecast,
  x = ~ year,
  y = ~ round(tsu/1000,0),
  yaxis = "y1",
  cliponaxis = FALSE,
  yaxis = "y1",
  type = 'scatter',  mode = 'lines+markers',
  line = list(width = 3, dash = 'dash'),
  marker = list(size = 9),
  color = ~ rank,
  colors = mycolors,
  opacity = 1,
  showlegend = T
) %>% 
  add_trace(
    data = data_prep_planned,
    inherit = FALSE,
    x = ~ year,
    y = ~ round(tsu,0),
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
   x = ~ year,
   y = ~ round(tsu/1000,0),
   yaxis = "y1",
   cliponaxis = FALSE,
   yaxis = "y1",
   type = 'scatter',  mode = 'lines+markers',
   line = list(width = 3, dash = 'solid', color = '#FFC000'),
   marker = list(size = 9, color = '#FFC000'),
   color = ~ rank,
   opacity = 1,
   showlegend = T
  ) %>%
  config( responsive = TRUE,
          displaylogo = FALSE,
          displayModeBar = F
          # modeBarButtons = list(list("toImage")),
  ) %>% 
  layout(
    font = list(family = "Roboto"),
    title = list(text=paste0("En route service units - ", forecast, " - ", 
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
    yaxis = list(title = "En route service units ('000)",
                 # gridcolor = 'rgb(255,255,255)',
                 showgrid = TRUE,
                 showline = FALSE,
                 tickprefix = " ",
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

myc(NA, 280, 14)

# export to image ----
w = 1200
h = 600
export_fig(myc(w, h, 14 * w/900),"traffic_su_main.png", w, h)
