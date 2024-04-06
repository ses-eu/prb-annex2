
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "NM_data.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "Cost-efficiency",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# prepare data ----
data_prep <- data_raw %>% 
  filter(year_report == .env$year_report) %>% 
  select(-c(year_report, entity_name)) 


# plot chart ----
myc <-  function(mywidth, myheight, myfont, mymargin) {
  data_prep %>% 
    plot_ly(
      width = mywidth,
      height = myheight,
      x = ~ year,
      y = ~ target,
      name = "Determined CSU",
      yaxis = "y1",
      text = ~ format(target, nsmall = 2),
      textangle = -90,
      textposition = "inside", 
      cliponaxis = FALSE,
      insidetextanchor =  "middle",
      textfont = list(color = 'white', size = myfont),
      type = "bar",
      marker = list(color = '#5B9BD5'),
      hovertemplate = paste('Determined CSU: %{y:.2f}<extra></extra>'),
      # hoverinfo = "none",
      showlegend = T
    ) %>%
    add_trace(
      inherit = FALSE,
      x = ~ year,
      y = ~ actual,
      name = "Actual CSU",
      yaxis = "y1",
      text = ~ format(actual, nsmall = 2),
      textangle = -90,
      textposition = "inside", 
      cliponaxis = FALSE,
      insidetextanchor =  "middle",
      textfont = list(color = 'black', size = myfont),
      type = "bar",
      marker = list(color = '#FFC000'),
      hovertemplate = paste('Actual CSU: %{y:.2f}<extra></extra>'),
      # hoverinfo = "none",
      showlegend = T
    ) %>%
    add_trace(
      inherit = FALSE,
      data = data_prep,
      x = ~ year,
      y = ~ round(planned_costs/1000, 2),
      name = "Planned costs",
      yaxis = "y2",
      type = 'scatter',
      mode = "line+markers",
      text = "",
      line = list(width = 3, color = '#5B9BD5'),
      marker = list(size = 9, color = '#5B9BD5'),
      hovertemplate = paste('Planned costs: %{y:.2f}<extra></extra>'),
      showlegend = T
    ) %>%
    add_trace(
        inherit = FALSE,
        data = data_prep,
        x = ~ year,
        y = ~ round(actual_costs/1000, 2),
        name = "Actual costs",
        yaxis = "y2",
        type = 'scatter',
        mode = "line+markers",
        text = "",
        line = list(width = 3, color = '#FFC000'),
        marker = list(size = 9, color = '#FFC000'),
        hovertemplate = paste('Actual costs: %{y:.2f}<extra></extra>'),
        showlegend = T
      ) %>%
    config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
            # modeBarButtons = list(list("toImage")),
    ) %>% 
    layout(
      font = list(family = "Roboto"),
      title = list(text=paste0("Costs per service units"),
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
      yaxis = list(title = "Costs per service units (€2017)",
                   # gridcolor = 'rgb(255,255,255)',
                   showgrid = TRUE,
                   showline = FALSE,
                   # tickprefix = " ",
                   # showtickprefix = 'all',
                   ticksuffix = "",
                   tickformat = ".1f",
                   # showticklabels = TRUE,
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = TRUE,
                   zerolinecolor = 'rgb(255,255,255)',
                   titlefont = list(size = myfont), tickfont = list(size = myfont)
      ),
      yaxis2 = list(title = "Total costs ('000 €2017)",
                    overlaying = "y",
                    side = "right",
                    showgrid = FALSE,
                    showline = FALSE,
                    tickformat = ",",
                    rangemode = "tozero",
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
      margin = list(t = mymargin/2, r = mymargin)
      
    )
  
}

myc(NA, 280, 14, 70)

# export to image ----
w = 1200
h = 600
export_fig(myc(w, h, 14 * w/900, 70 * w/1000), paste0("cef_nm_main.png"), w, h)




