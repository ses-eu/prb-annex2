# 
# # parameters
# if (exists("data_folder") == FALSE) {
#   source("R/parameters.R")
# }

# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "SES.xlsx"),
  sheet = "SES_EoSM",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() %>% 
  mutate(management_objectives = str_replace_all(management_objectives, 'Other Mos' , 'Other MOs'))

data_prep <- data_raw %>% 
  filter(year_report == .env$year_report) %>% 
  select(-year_report) %>% 
  mutate(
    management_objectives = paste(management_objectives, str_to_lower(status)),
    mytextpos = case_when(
      management_objectives == 'Other MOs planned' ~ "top center",
      TRUE ~ 'bottom center'
    ),
    ) 

data_prep_actual <- data_prep %>% 
  filter(status == 'Actual')

data_prep_planned <- data_prep %>% 
  filter(status == 'Planned')

# chart ----
  ## set parameters for chart ----
  mycolors <-  c('#FFC000', '#FFC000','#5B9BD5', '#5B9BD5')

  ## define chart function ----
  myc <- function (mywidth, myheight, myfont, mylinewidth) {
    plot_ly(
      width = mywidth,
      height = myheight,
      data = data_prep_planned,
      x = ~ year,
      y = ~ number_of_ans_ps,
      yaxis = "y1",
      cliponaxis = FALSE,
      yaxis = "y1",
      type = 'scatter',  mode = 'lines+markers',
      line = list(width = mylinewidth, dash = 'dot'),
      marker = list(size = mylinewidth * 3),
      color = ~ management_objectives,
      colors = mycolors,
      opacity = 1,
      text = ~ number_of_ans_ps,
      textposition = ~ mytextpos,
      textfont = list(color = 'black', size = myfont),
      hovertemplate = paste0('%{xother} %{y:.0f}'),
      showlegend = T
    ) %>% 
    add_trace(
      data = data_prep_actual,
      x = ~ year,
      y = ~ number_of_ans_ps,
      yaxis = "y1",
      cliponaxis = FALSE,
      yaxis = "y1",
      type = 'scatter',  mode = 'lines',
      line = list(width = mylinewidth, dash = 'solid'),
      color = ~ management_objectives,
      colors = mycolors,
      opacity = 1,
      hovertemplate = paste0('%{xother} %{y:.0f}'),
      showlegend = T
      ) %>%
    config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
            # modeBarButtons = list(list("toImage")),
    ) %>% 
    layout(
      font = list(family = "Roboto"),
      title = list(text = paste0("Number of ANSPs on or above target"),
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
      yaxis = list(title = "Number of ANSPs on or above target",
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
        x = -0.05, 
        y =-0.1,
        font = list(size = myfont*0.95)
        ),
      margin = list (t = 40)
      
      
    )
  }

## plot chart ----
  myc(mywidth, myheight, myfont, mylinewidth)

# # export to image
# w = 1200
# h = 600
# export_fig(myc(w, h, 14 * w/900),"saf_main.png", w, h)