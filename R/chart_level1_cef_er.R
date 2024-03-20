
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

# fix tz if script not executed from qmd file
if (exists("ez") == FALSE) {ez = 1}
# tz=1

# initialise list to store plots
myplot = list()

for (ez in 1:nrow(ecz_list)) {

# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "CEFF.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "ERT_CZ",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# prepare data ----
##xxx this will have to be parametrised

data_prep <- data_raw %>% 
  filter(
    entity_code == ecz_list$ecz_id[ez]) %>% 
  mutate(
    unit_cost_er = round(x5_5_unit_cost_nc2017/x2017_xrate, 2)
  ) %>% 
  select(
    year,
    status,
    unit_cost_er
  ) %>% 
  filter(year > 2021) %>% 
  mutate(year_text = as.character(year),
         year_text = str_replace(year_text, "20202021", "2020-2021"),
         status = str_replace(status, "A", "Actual unit cost"),
         status = str_replace(status, "D", "Determined unit cost")
  ) %>% 
  arrange(year_text)

# plot chart ----
myc <-  function(mywidth, myheight, myfont) {
  data_prep %>% 
    plot_ly(
      width = mywidth,
      height = myheight,
      x = ~ year_text,
      y = ~ unit_cost_er,
      yaxis = "y1",
      text = ~ format(unit_cost_er, nsmall = 2),
      textangle = -90,
      textposition = "inside", 
      cliponaxis = FALSE,
      insidetextanchor =  "middle",
      textfont = list(color = 'black', size = myfont),
      type = "bar",
      color = ~ factor(status, levels = c("Determined unit cost", 
                                          "Actual unit cost")),
      colors = c('#5B9BD5', '#FFC000'),
      # hovertemplate = paste('%{y:.2f}<extra></extra>'),
      hoverinfo = "none",
      showlegend = T
    ) %>%
    add_trace(
      inherit = FALSE,
      data = data_prep,
      x = ~ year_text,
      y = ~ unit_cost_er/2,
      yaxis = "y1",
      type = 'scatter',
      mode = "markers",
      text = ~ paste0(substr(status,1,1), ": ", format(unit_cost_er, nsmall = 2)),
      color = ~ factor(status, levels = c("Determined unit cost",
                                          "Actual unit cost")),
      colors = c('#5B9BD5', '#FFC000'),
      # line = list(width = 0),
      marker = list(color = 'transparent'),
      hovertemplate = paste('%{text}<extra></extra>'),
      # hoverinfo = "none",
      showlegend = F
    ) %>% 
    config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
            # modeBarButtons = list(list("toImage")),
    ) %>% 
    layout(
      font = list(family = "Roboto"),
      title = list(text=paste0("En route unit costs - ", ecz_list$ecz_name[ez]),
                   y = 0.99, 
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
      yaxis = list(title = "En route unit costs (€2017)",
                   # gridcolor = 'rgb(255,255,255)',
                   showgrid = TRUE,
                   showline = FALSE,
                   ticksuffix = "",
                   tickformat = ",.0f",
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
      )
      
    )
  
}

myplot[[ez]] <- myc(NA, 280, 14)

# export to image ----
w = 1200
h = 600
export_fig(myc(w, h, 14 * w/900), paste0("cef_er", ez, "_main.png"), w, h)

}

htmltools::tagList(myplot)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document
