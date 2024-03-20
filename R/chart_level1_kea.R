
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

# import data  ----
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

# prepare data ----
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

# plot chart ----
myc <-  function(mywidth, myheight, myfont) {
  data_for_chart %>% 
  plot_ly(
    width = mywidth,
    height = myheight,
  x = ~ year,
  y = ~ kea_target,
  yaxis = "y1",
  cliponaxis = FALSE,
  name = "",
  textfont = list(color = 'transparent'),
  type = 'scatter',  mode = 'lines',
  line = list(color = 'transparent', width = 0),
  hovertemplate = paste('%{x}:<extra></extra>'),
  # hoverinfo = "none",
  showlegend = F
) %>% 
  add_trace(
    inherit = FALSE,
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
    textfont = list(color = 'black', size = myfont),
    type = "bar",
    hovertemplate = paste('KEA: %{y:.2f}%<extra></extra>'),
    # hoverinfo = "none",
    showlegend = T
  ) %>%
  add_trace(
    inherit = FALSE,
    x = ~ year,
    y = ~ kea_target,
    yaxis = "y1",
    type = 'scatter',  mode = 'lines+markers',
    line = list(color = '#FF0000', width = 3),
    marker = list(size = 9, color = '#FF0000'),
    name = "Target",
    opacity = 1,
    hovertemplate = paste('Target: %{y:.2f}%<extra></extra>'),
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
    textfont = list(color = '#FF0000', size = myfont),
    # hovertemplate = paste('<extra></extra>'),
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
    title = list(text=paste0("KEA - ", country),
                 y = 1, 
                 x = 0, 
                 xanchor = 'left', 
                 yanchor =  'top',
                 font = list(size = myfont * 20/15)
            ),
    bargap = 0.25,
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
                 zerolinecolor = 'rgb(255,255,255)',
                 titlefont = list(size = myfont), tickfont = list(size = myfont)
                 ),
    # showlegend = FALSE
    legend = list(
      orientation = 'h', 
      xanchor = "center",
      x = 0.5, 
      y =-0.1,
      font = list(size = myfont)
      ),
    margin = list (t = 40)
    
  )
}
  
myc(NA, 300, 14)

# export to image ----
w = 1200
h = 600
export_fig(myc(w, h, 14 * w/900),"env_kea_main.png", w, h)

