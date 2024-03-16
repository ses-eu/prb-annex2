
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

# import data  ----
data_raw_target  <-  read_xlsx(
  paste0(data_folder, "targets.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "ER_CAP",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

data_raw_actual  <-  read_xlsx(
  paste0(data_folder, "AUA_export.xlsx"),
  sheet = "AUA_export",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# prepare data ----
data_prep_target <- data_raw_target %>% 
  filter(
    state == country) %>% 
  mutate(
    er_cap_target = round(x331_ert_delay_target, 2)
  ) %>% 
  select(
    year,
    er_cap_target
  )

data_prep_actual <- data_raw_actual %>% 
  filter(
    entity_name == main_ansp) %>% 
  pivot_longer(
    cols = c(avg_atc_cap, avg_atc_stff, avg_atc_dsrptn, avg_weather, avg_other_nonatc),
    names_to = "type",
    values_to = "delay"
  ) %>% 
  mutate(
    type = case_when(
      type == "avg_atc_cap" ~ "Capacity",
      type == "avg_atc_stff" ~ "Staffing",
      type == "avg_atc_dsrptn" ~ "Disruptions",
      type == "avg_weather" ~ "Weather",
      type == "avg_other_nonatc" ~ "Other non-ATC"
    )
  )

# data_for_chart <-  merge(x = data_prep_target, y = data_prep_actual, by="year", all.x = TRUE) 

# plot chart ----
myc <-  function(mywidth, myheight, myfont, mymargin) {
  plot_ly(
    data = data_prep_actual,
    width = mywidth,
    height = myheight,
    x = ~ year,
    y = ~ delay,
    yaxis = "y1",
    color = ~ factor(type, levels = c("Capacity", "Staffing", 
                                      "Disruptions", "Weather",
                                      "Other non-ATC")
    ),   
    colors = c('#ED7D31', '#F8CBAD', '#BF8F00', '#92D050', '#A5A5A5'),
    cliponaxis = FALSE,
    type = "bar",
    # hovertemplate = paste('KEA: %{y:.2f}%<extra></extra>'),
    # hoverinfo = "none",
    showlegend = T
    ) %>% 
    add_trace(
      data = data_prep_actual,
      inherit = FALSE,
      x = ~ year,
      y = ~ avg_delay,
      yaxis = "y1",
      name = "Total delay",
      type = 'scatter',  mode = 'lines',
      line = list(color = 'transparent', width = 0),
      text = ~ format(round(avg_delay, 2), digits = 2),
      textfont = list(color = 'black', size = myfont),
      textposition = "top center",
      hovertemplate = paste('Total delay: %{y:.2f}<extra></extra>'),
      opacity = 1,
      showlegend = F
    ) %>%
    add_trace(
    data = data_prep_target,
    inherit = FALSE,
    x = ~ year,
    y = ~ er_cap_target,
    yaxis = "y1",
    type = 'scatter',  mode = 'lines+markers',
    line = list(color = '#FF0000', width = 3),
    marker = list(size = 9, color = '#FF0000'),
    name = "Target",
    text = ~ paste0 ("<b>", format(er_cap_target, digits = 2), "</b>"),
    textfont = list(color = '#FF0000', size = myfont),
    textposition = "top center",
    hovertemplate = paste('Target: %{y:.2f}<extra></extra>'),
    opacity = 1,
    showlegend = T
  ) %>%
    add_trace(
      data = data_prep_actual,
      inherit = FALSE,
      x = ~ year,
      y = ~ round(ifr/1000, 0),
      yaxis = "y2",
      type = 'scatter',  mode = 'lines+markers',
      line = list(color = '#FFC000', width = 3),
      marker = list(size = 9, color = '#FFC000'),
      name = "IFR movements",
      hovertemplate = paste("IFR mvts ('000): %{y:,}<extra></extra>"),
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
    title = list(text=paste0("Average en route ATFM delay per flight ", country),
                 y = 1, 
                 x = 0, 
                 xanchor = 'left', 
                 yanchor =  'top',
                 font = list(size = myfont * 20/14)
            ),
    bargap = 0.25,
    barmode = 'stack',
    hovermode = "x unified",
    hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
    xaxis = list(title = "",
                 gridcolor = 'rgb(255,255,255)',
                 showgrid = FALSE,
                 showline = FALSE,
                 showticklabels = TRUE,
                 dtick = 1,
                 zeroline = TRUE,
                 tickfont = list(size = myfont)
                 ),
    yaxis = list(title = "Average minutes of delay",
                 showgrid = TRUE,
                 showline = FALSE,
                 tickformat = ".2f",
                 zeroline = TRUE,
                 zerolinecolor = 'rgb(255,255,255)',
                 titlefont = list(size = myfont), tickfont = list(size = myfont)
                 ),
    yaxis2 = list(title = "\n IFR flights ('000)",
                  overlaying = "y",
                  side = "right",
                  showgrid = FALSE,
                 showline = FALSE,
                 tickformat = ",",
                 zeroline = TRUE,
                 zerolinecolor = 'rgb(255,255,255)',
                 titlefont = list(size = myfont), tickfont = list(size = myfont)
    ),
    # showlegend = FALSE
    legend = list(
      orientation = 'h', 
      xanchor = "center",
      x = 0.5, 
      y =-0.1,
      traceorder= 'reversed',
      font = list(size = myfont)
      ),
    margin = list(t = mymargin/2, r = mymargin)
    
  )
}
  
myc(NA, NA, 14, 70)

# export to image ----
w = 1200
h = 600
export_fig(myc(w, h, 14 * w/900, 70 * w/1000),"cap_er_main.png", w, h)

