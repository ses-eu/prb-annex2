# parameters 
# mywidth = 300
# myheight = 220
# myfont = 8
# mymargin = list (t = 20, l = 0)
# mylinewidth = 2

# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "SES.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "SES_DUC",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# prepare data ----
data_prep_all <- data_raw %>% 
  filter(
    year_report == .env$year_report) %>% 
  mutate(
    unit_cost_er = round(duc_value, 3)
  ) %>% 
  select(
    year,
    status,
    unit_cost_er
  ) %>% 
  mutate(year_text = as.character(year),
         status = str_replace(status, "Actual", "Actual unit cost"),
         status = str_replace(status, "Determined", "Determined unit cost")
  ) %>% 
  arrange(year_text) %>% 
  select(-year)

data_actual_trend <- data_prep_all %>% 
  filter(status %like% "Actual unit cost") %>% 
  pivot_wider(names_from = 'status', values_from = 'unit_cost_er' ) %>% 
  clean_names()

data_target_trend <- data_prep_all %>% 
  filter(status %like% "Target") %>% 
  pivot_wider(names_from = 'status', values_from = 'unit_cost_er' ) %>% 
  clean_names()

# chart ----
## set parameters for chart ----

## define chart function ----
myc <-  function(mywidth, myheight, myfont, mylinewidth, mymargin) {
    plotly::plot_ly(
      width = mywidth,
      height = myheight,
      data = data_prep_all,
      x = ~ year_text,
      y = ~ unit_cost_er,
      yaxis = "y1",
      text = ~ format(round(unit_cost_er,2), nsmall = 2),
      textangle = -90,
      textposition = "inside", 
      cliponaxis = FALSE,
      insidetextanchor =  "middle",
      textfont = list(color = 'white', size = myfont),
      type = "bar",
      color = ~ factor(status, levels = c("Determined unit cost", 
                                          "Actual unit cost")),
      colors = c('#5B9BD5', '#FFC000'),
      hovertemplate = paste0('%{xother} %{y:.2f}'),
      # hovertemplate = paste('%{y:.2f}<extra></extra>'),
      # hoverinfo = "x+y",
      showlegend = T
    ) %>%
    plotly::add_trace(
      inherit = FALSE,
      data = data_target_trend,
      x = ~ year_text,
      y = ~ target,
      yaxis = "y1",
      type = 'scatter',
      mode = "line+markers",
      name = 'Target trend',
      text = ~ paste0('<b>', format(target_trend * 100, nsmall = 1), '%</b>'),
      textposition = "top center",
      cliponaxis = FALSE,
      textfont = list(color = '#FF0000', size = myfont),
      line = list(width = mylinewidth, color = '#FF0000'),
      marker = list(size = mylinewidth * 3, color = '#FF0000'),
      hovertemplate = paste('Target trend: %{text}<extra></extra>'),
      # hoverinfo = "none",
      showlegend = T
    ) %>%
    plotly::add_trace(
      inherit = FALSE,
      data = data_actual_trend,
      x = ~ year_text,
      y = ~ actual_unit_cost,
      yaxis = "y1",
      type = 'scatter',
      mode = "line+markers",
      name = 'Actual trend',
      text = ~ paste0('<b>', format(actual_unit_cost_trend * 100, nsmall = 1), '%</b>'),
      textposition = "bottom center",
      cliponaxis = FALSE,
      textfont = list(color = 'black', size = myfont),
      line = list(width = mylinewidth, color = '#ED7D31'),
      marker = list(size = mylinewidth * 3, color = '#ED7D31'),
      hovertemplate = paste('Actual trend: %{text}<extra></extra>'),
      # hoverinfo = "none",
      showlegend = T
    ) %>%
    plotly::config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
            # modeBarButtons = list(list("toImage")),
    ) %>% 
    plotly::layout(
      font = list(family = "Roboto"),
      title = list(text=paste0("En route unit costs - SES RP3"),
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
        font = list(size = myfont*0.9)
      ),
      margin = mymargin
    )
  
}

## plot chart ----
myc(mywidth, myheight+20, myfont, mylinewidth, mymargin)

# # export to image
# w = 1200
# h = 600
# export_fig(myc(w, h, 14 * w/900), paste0("cef_er_main.png"), w, h)

