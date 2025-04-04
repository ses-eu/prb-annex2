
# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "STATFOR_forecast_en-route_TSU.xlsx"),
  sheet = "data",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

data_raw_rts  <- read_xlsx(
  paste0(data_folder, "CEFF dataset master.xlsx"),
  sheet = "Enroute_T1",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names()

data_raw_rts_ses  <- read_xlsx(
  paste0(data_folder, "SES CEFF.xlsx"),
  sheet = "SES_ERT_all",
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

## rts data
data_prep_rts_ses <- data_raw_rts_ses |> 
  mutate(x5_4_total_su = su_cz) |> 
  select(year, status, x5_4_total_su)

data_prep_rts <- data_raw_rts |> 
  filter(entity_code == ecz_list$ecz_id[1],
         year != 20202021) |> 
  select(year, status, x5_4_total_su)

if (country == "Spain") {
  data_canarias_rts <- data_raw_rts |> 
    filter(entity_code == ecz_list$ecz_id[2],
           year != 20202021) |> 
    select(year, status, x5_4_total_su) 
    
  data_prep_rts <- data_prep_rts |> 
    rbind(data_canarias_rts) |> 
    group_by(year, status) |> 
    summarise(x5_4_total_su = sum(x5_4_total_su, na.rm = TRUE)) 
} else if (country == "SES RP3") {
  
  data_prep_rts <- data_prep_rts_ses
} 
  
## statfor data
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

## we take the actuals from the rts
data_prep_actual_rts <- data_prep_rts |>
  filter(status == "A") |>
  mutate(tsu = case_when(
    year > year_report ~ NA,
    .default = x5_4_total_su
    ),
    rank = 'Actual'
    ) |> 
  select(year, rank, tsu)

## but we need 2019 from statfor
data_prep_actual_statfor <-  data_prep %>%
  filter(
    forecast_id == max(forecast_id),
    rank == 'Base forecast'
  ) %>%
  mutate(
    forecast_id = .env$forecast_id,
    rank = 'Actual'
    # , tsu = case_when (
    # year <= year_report ~ tsu,
    # TRUE ~ NA
    # )
  ) |> 
  filter(year == 2019) |> 
  select(year, rank, tsu)

## merge actual tables
data_prep_actual <- data_prep_actual_rts |> rbind(data_prep_actual_statfor) |> 
  arrange(year)

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

# chart ----
## set parameters for chart ----
  mycolors <-  c('#1969B4','#044598', '#229FDD')
  
  if (knitr::is_latex_output()) {
    mytitle <- paste0("En route service units - ", forecast, " -\n",
                      if_else(country == "Spain",
                              country, ecz_list$ecz_name[1]))
    mytitle_y <- 0.95
    mylocalmargin = list (t = 40, l = 0)
    mylegend_x <- -0.1
  } else {
    mytitle <- paste0("En route service units - ", forecast, " - ", 
                      if_else(country == "Spain", 
                              country, ecz_list$ecz_name[1]))
    mylegend_x <- 0
  }

## define chart function ----
  myc <- function (mywidth, myheight, myfont, mylinewidth, mymargin) {
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
    line = list(width = mylinewidth, dash = 'dash'),
    marker = list(size = mylinewidth * 3),
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
      line = list(width = mylinewidth, dash = 'solid', color = '#5B9BD5'),
      marker = list(size = mylinewidth * 3, color = '#5B9BD5'),
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
     type = 'scatter',  mode = 'markers',
     line = list(width = mylinewidth, dash = 'solid', color = '#FFC000'),
     marker = list(size = mylinewidth * 3, color = '#FFC000'),
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
      title = list(text = mytitle,
                   y = mytitle_y, 
                   x = mytitle_x, 
                   xanchor = mytitle_xanchor, 
                   yanchor = mytitle_yanchor,
                   font = list(size = mytitle_font_size)
                   ),
      dragmode = FALSE,
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
                   zerolinecolor = 'rgb(240,240,240)',
                   titlefont = list(size = myfont), tickfont = list(size = myfont)
                   ),
      # showlegend = FALSE
      legend = list(
        orientation = 'h', 
        xanchor = "left",
        x = mylegend_x, 
        y = -0.1,
        font = list(size = myfont)
        ),
      margin = mymargin
      
      
    )
  }
  
## plot chart ----
  myc(mywidth, myheight+20, myfont, mylinewidth, mymargin)
