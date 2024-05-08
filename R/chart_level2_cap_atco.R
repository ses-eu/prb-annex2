# # parameters

## import data
sheet <- country
range <- "A24:A48"
countacc_r  <- read_range(cap_file, sheet, range)

# data_for_chart <- list()

## prepare data
prepare_data <- function(i, acc_name) {
  range[i] <- paste0("A", 25 + (i-1)*4, ":O", 27+ (i-1)*4)
  df <- read_range(cap_file, sheet, range[i])  
  df <- df %>% 
    select(1, 5:10) %>% 
    clean_names() %>% 
    rename(type = 1) %>% 
    mutate(type = str_replace_all(type, fixed(' (Perf Plan)'), '')) %>% 
    mutate_all(~ str_replace_all(., "-", NA_character_)) %>% 
    mutate_at(c(-1), ~ as.numeric(.)) %>% 
    pivot_longer(-type, names_to = 'year') %>% 
    mutate_at('year', ~ str_replace_all(., "x", '')) %>% 
    mutate_at('year', ~ as.numeric(.)) %>% 
    mutate(acc = acc_name) %>% 
    arrange(type, year)
}

data_acc <- map2_dfr(seq(1:acc_no), acc_list$acc_full_name, prepare_data)

data_ansp <- data_acc %>% group_by(type, year) %>% 
  summarise(value = sum(value)) %>% 
  mutate(acc = main_ansp) %>% ungroup()

data_for_chart <- rbind(data_ansp, data_acc) %>% 
  group_by(acc) %>% 
  mutate(min_y_axis = min(value, na.rm=T)/1.5)

acc_list_for_chart <- unique(data_for_chart$acc)

# chart ----
## set parameters for chart ----
mycolors <-  c('#FFC000','#5B9BD5')

if (knitr::is_latex_output()) {
  mytitle <- paste0("ATCO evolution")
  mytitle_pos <- 0.94
} else {
  mytitle <- paste0("ATCO evolution")
  mytitle_pos <- 0.93
}

## define chart function ----
myc <- function (mywidth, myheight, myfont, mylinewidth, mymargin) {
  myplot <- plotly::plot_ly( 
    width = mywidth,
    height = myheight,
  )
  for (i in 1:length(acc_list_for_chart)) {
    df <- data_for_chart %>% filter(acc == acc_list_for_chart[i])
    myplot <- myplot %>% 
      plotly::add_trace(
        data = df,
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
        visible = ifelse(i == 1, TRUE, FALSE),  # Set the initial visibility
        # visible = c(rep(TRUE, 2), rep(FALSE, length(acc_list_for_chart)*2 - 2)),  # Set the initial visibility
        # hovertemplate = paste('Target: %{y:.2f}%<extra></extra>'),
        # hoverinfo = "none",
        showlegend = T
      ) %>% 
      plotly::add_trace(
        x = ~ year,
        y = ~ min_y_axis,
        yaxis = "y1",
        cliponaxis = FALSE,
        yaxis = "y1",
        type = 'scatter',  mode = 'lines',
        line = list(width = 0, dash = 'solid', color = 'transparent'),
        opacity = 1,
        visible = ifelse(i == 1, TRUE, FALSE),  # Set the initial visibility
        showlegend = F,
        hoverinfo = 'none'
      )
  }
  
  # Create updatemenu buttons for selecting accs
  updatemenus = list(list(
    xref = 'paper',
    xanchor = 'left',
    yanchor = "top",
    x = -0.2,
    y = 1.2,
    font = list(family = "Arial", color="black", size=myfont),
    pad = 0,
    bgcolor = 'white', 
    bordercolor = '#e0e0e0', 
    active = 0,
    buttons = lapply(1:length(acc_list_for_chart), function(i) {
      list(
        label = acc_list_for_chart[i],
        method = "update",
        args = list(
          list(visible = replace(
            rep(FALSE, length(acc_list_for_chart) * 3), seq((i - 1) * 3 + 1, i * 3, by = 1), TRUE
          ))
          # list(title = list(
          #   text = paste0("<b>EUROCONTROL 7-year forecast for ", tz_values[i], " 2024-2030 </b> <br><span style='font-size:0.8em;color:grey'>Actual and future IFR movements</span>")
          # ))
        )
      )
    })
  ))
  
  
  myplot <- myplot %>% 
    plotly::config( responsive = TRUE,
                    displaylogo = FALSE,
                    displayModeBar = F
    ) %>% 
    plotly::layout(
      updatemenus = updatemenus,
      font = list(family = "Roboto"),
      title = list(text = mytitle,
                   y = mytitle_pos, 
                   x = 0.5, 
                   xanchor = 'center', 
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
                   # rangemode = "tozero",
                   # autorange = 'max',
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
      margin = mymargin
    )
  myplot
}

## plot chart ----
myc(mywidth, myheight, myfont, mylinewidth, mymargin)

# # export to image
# w = 1200
# h = 600
# export_fig(myc(w, h, 14 * w/900),"traffic_mvt_main.png", w, h)
