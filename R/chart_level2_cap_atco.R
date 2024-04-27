# # parameters

## import data
sheet <- country
range <- "A24:A48"
countacc_r  <- read_range(cap_file, sheet, range)

atcos <- list()

for (i in 1:acc_no) {
  range[i] <- paste0("A", 25 + (i-1)*4, ":O", 27+ (i-1)*4)
  df <- read_range(cap_file, sheet, range[i])  
  atcos[[i]] <- df
}

t <- list()

  ## prepare data
  data_for_chart <- atcos[[1]] %>% 
    select(1, 5:10) %>% 
    clean_names() %>% 
    rename(type = 1) %>% 
    mutate(type = str_replace_all(type, fixed(' (Perf Plan)'), '')) %>% 
    mutate_all(~ str_replace_all(., "-", NA_character_)) %>% 
    mutate_at(c(-1), ~ as.numeric(.)) %>% 
    pivot_longer(-type, names_to = 'year') %>% 
    mutate_at('year', ~ str_replace_all(., "x", '')) %>% 
    mutate_at('year', ~ as.numeric(.)) %>% 
    mutate(acc_ = acc_list$acc_full_name[1])
    


# chart ----
## set parameters for chart ----
  mycolors <-  c('#FFC000','#5B9BD5')
  
  if (knitr::is_latex_output()) {
    mytitle <- paste0("ATCO evolution - ", acc_list$acc_full_name[1])
    mytitle_pos <- 0.95
  } else {
    mytitle <- paste0("ATCO evolution - ", acc_list$acc_full_name[1])
    mytitle_pos <- 0.99
  }

## define chart function ----
  myc <- function (mywidth, myheight, myfont, mylinewidth, mymargin) {
    plotly::plot_ly(
      width = mywidth,
      height = myheight,
      data = data_for_chart,
    x = ~ year,
    y = ~ value,
    yaxis = "y1",
    cliponaxis = FALSE,
    yaxis = "y1",
    type = 'scatter',  mode = 'lines+markers',
    line = list(width = mylinewidth, dash = 'solid'),
    marker = list(size = mylinewidth * 3),
    color = ~ type,
    colors = mycolors,
    opacity = 1,
    # hovertemplate = paste('Target: %{y:.2f}%<extra></extra>'),
    # hoverinfo = "none",
    showlegend = T
  ) %>% 
      # plotly::add_trace(
      #   data = data_prep_planned,
      #   inherit = FALSE,
      #   x = ~ year,
      #   y = ~ round(mvts,0),
      #   yaxis = "y1",
      #   cliponaxis = FALSE,
      #   yaxis = "y1",
      #   type = 'scatter',  mode = 'lines+markers',
      #   line = list(width = mylinewidth, dash = 'solid', color = '#5B9BD5'),
      #   marker = list(size = mylinewidth * 3, color = '#5B9BD5'),
      #   color = ~ rank,
      #   opacity = 1,
      #   showlegend = T
      # ) %>%
      # plotly::add_trace(
      #   data = data_prep_actual,
      #   inherit = FALSE,
      #   x = ~ yr,
      #   y = ~ round(mvts/1000,0),
      #   yaxis = "y1",
      #   cliponaxis = FALSE,
      #   yaxis = "y1",
      #   type = 'scatter',  mode = 'lines+markers',
      #   line = list(width = mylinewidth, dash = 'solid', color = '#FFC000'),
      #   marker = list(size = mylinewidth * 3, color = '#FFC000'),
      #   color = ~ rank,
      #   opacity = 1,
      #   # hovertemplate = paste('Target: %{y:.2f}%<extra></extra>'),
      #   # hoverinfo = "none",
      #   showlegend = T
      # ) %>%
      plotly::config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
            # modeBarButtons = list(list("toImage")),
    ) %>% 
    plotly::layout(
      font = list(family = "Roboto"),
      title = list(text = mytitle,
                   y = mytitle_pos, 
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
      yaxis = list(title = "ATCOs in OPS (FTEs)",
                   # gridcolor = 'rgb(255,255,255)',
                   showgrid = TRUE,
                   showline = FALSE,
                   tickformat = ",",
                   tickprefix = " ",
                   rangemode = "tozero",
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
        x = 0, 
        y =-0.1,
        font = list(size = myfont)
        ),
      margin = mymargin
      
    )
  }

## plot chart ----
  myc(mywidth, myheight, myfont, mylinewidth, mymargin)

# # export to image
# w = 1200
# h = 600
# export_fig(myc(w, h, 14 * w/900),"traffic_mvt_main.png", w, h)
