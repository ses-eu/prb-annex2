
# fix ez if script not executed from qmd file ----
if (exists("ez") == FALSE) {ez = 1}
# ez=1

# initialise list to store plots ----
myplot = list()

# loop through czs ----
for (ez in 1:no_ecz) {
  
  data_prep <- aucu(ez)
  
  ## select relevant values for chart
  data_for_chart <- data_prep %>% 
    select(
      year_text,
      new_duc,
      total_adjustments_aucu,
      aucu
    ) 

  ## chart parameters ----
  mychart_title <- paste0("AUCU")
  myaxis_title <- "AUCU (€/SU)"
  mybarcolor <- c( '#5B9BD5', 'transparent', '#BFBFBF', '#9DC3E6')
  mytextcolor <- 'black'
  mylegend_y_position <- -0.28
  mymargin = list (t = 60, b = 80)
  
  ## define chart function ----
  mybarc_aucu <-  function(mywidth, myheight, myfont, mymargin) {
    mychart <- list()
    data_for_chart_filtered <- list()
    
    ### loop through years ----
    for (i in 1:nrow(data_for_chart)) {
       # i=2
      ### prepare data for chart ----
      data_for_chart_filtered[[i]] <- data_for_chart %>% 
        slice(i:i) %>% 
        pivot_longer(-c(year_text), names_to = 'type', values_to = 'metric')  %>% 
        mutate(
          mydatalabel = case_when(
            type == 'total_adjustments_aucu' ~ if_else(is.na(metric) == TRUE, 
                                                    NA, 
                                                    paste0(if_else(metric >= 0, '+', ''),
                                                           trimws(format(round(metric, 2), big.mark = ",", nsmall = 2)))
                                                    ),
            .default = if_else(is.na(metric) == TRUE, NA, format(round(metric, 2), big.mark = ",", nsmall = 2))
            ),
          new_duc = case_when(
              type == 'new_duc' ~ metric,
              .default = NA),
          total_adjustments_aucu = case_when(
              type == 'total_adjustments_aucu' ~ abs(metric),
              .default = NA),      
          aucu = case_when(
              type == 'aucu' ~ metric,
              .default = NA),
          fake_series = case_when(
              type == 'total_adjustments_aucu' ~ min(c(aucu, new_duc), na.rm = TRUE),
              .default = NA)
          ) %>% 
        relocate(fake_series, .after = new_duc) %>% 
        select(-metric) %>% 
        pivot_longer(-c(year_text, type, mydatalabel), names_to ='subtype', values_to = 'metric') %>% 
        mutate(mydatalabel = case_when(
          subtype == 'fake_series' ~ NA,
          .default = mydatalabel
            ))
    
      ### plot indivicual year charts ----
      mychart[[i]] <- data_for_chart_filtered[[i]] %>% 
      plot_ly(
        width = mywidth,
        height = myheight,
        y = ~ round(metric, 2),
        x = ~ factor(type, levels = c('new_duc', 'total_adjustments_aucu',
                                      'aucu')),
        yaxis = "y1",
        type = 'bar',
        color = ~ factor(subtype, levels = c('new_duc', 'fake_series',
                                                       'total_adjustments_aucu',
                                                       'aucu')),
        colors = mybarcolor,
        text = ~ mydatalabel,
        textangle = -90,
        textposition = "outside", 
        cliponaxis = FALSE,
        # insidetextanchor =  "middle",
        # name = mymetric,
        textfont = list(color = mytextcolor, size = myfont * 0.8),
        # hovertemplate = paste0('%{y} (A-D): %{x:+0,}<extra></extra>'),
        hoverinfo = "none",
        showlegend = FALSE
      ) %>% 
      # add_trace(
      #   # data = filter(data_for_chart_filtered[[i]], subtype != 'fake_series'),
      #   y = 0,
      #   x = 'total_adjustments_aucu',
      #   yaxis = "y1",
      #   type = 'scatter',  mode = 'markers',
      #   marker = list(size = mylinewidth, color = 'transparent'),
      #   # color = ~ factor(subtype, levels = c('new_duc', 'fake_series',
      #   #                                      'total_adjustments_aucu',
      #   #                                      'aucu')),
      #   # text = ~ mylabel,
      #   # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
      #   # textangle = -90,
      #   textposition = "none",
      #   cliponaxis = FALSE,
      #   # insidetextanchor =  "middle",
      #   # name = mymetric,
      #   textfont = list(color = 'transparent'),
      #   hovertemplate = paste0('%{x}: %{y}<extra></extra>'),
      #   # hoverinfo = "none",
      #   showlegend = F
      # ) %>%
      layout(
        showlegend = F,  
        barmode = "stack",
        bargap = '0',
        xaxis = list(title = '',
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = FALSE,
                     showline = TRUE,
                     showticklabels = FALSE,
                     # dtick = 1,
                     # tickcolor = 'rgb(127,127,127)',
                     # ticks = 'outside',
                     # font = list(size = 1, color = 'transparent'),
                     zeroline = TRUE
        )
      )
      
    }

    ### group year charts ----
    subplot(mychart[[1]], mychart[[2]], mychart[[3]], mychart[[4]],
            titleX = TRUE, shareY = T) %>% 
      config( responsive = TRUE,
              displaylogo = FALSE,
              displayModeBar = F
              # modeBarButtons = list(list("toImage")),
      ) %>% 
      layout(
        font = list(family = "Roboto"),
        title = list(text = mychart_title,
                     y = 0.99, 
                     x = 0, 
                     xanchor = 'left', 
                     yanchor =  'top',
                     font = list(size = myfont * 20/15)
        ),
        hovermode = "x",
        hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
        # uniformtext = list(minsize = myfont, mode='show'),
        yaxis = list(title = myaxis_title,
                     # gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     # tickprefix = if_else(" ",
                     # ticksuffix = "% ",
                     tickformat = "0, ",
                     tickcolor='white',     # to increase space between tick and plot
                     ticklen = 7,
                     # showticklabels = TRUE,
                     # ticks = 'outside',
                     zeroline = TRUE,
                     zerolinecolor = 'rgb(255,255,255)',
                     titlefont = list(size = myfont), tickfont = list(size = myfont)
        ),
        # legend = list(
        #   orientation = 'h', 
        #   xanchor = "left",
        #   x = -0.1, 
        #   y = -0.5,
        #   font = list(size = myfont*0.9)
        #   ),
        # couldn't get the legend working so I had to resort to this
        annotations = list(
          list (
            xanchor = "center",
            x = 0.125,
            y = -0.15,
            text = '2020-2021',
            font = list(size = myfont),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          ),
          list (
            xanchor = "center",
            x = 0.375,
            y = -0.15,
            text = '2022',
            font = list(size = myfont),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          ), 
          list (
            xanchor = "center",
            x = 0.625,
            y = -0.15,
            text = '2023',
            font = list(size = myfont),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          ), 
          list (
            xanchor = "center",
            x = 0.875,
            y = -0.15,
            text = '2024',
            font = list(size = myfont),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          ), 
          list (
            xanchor = "left",
            x = 0.22,
            y = mylegend_y_position,
            text = '■',
            font = list(size = myfont * 1.2, color = '#5B9BD5'),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          ), 
          list (
            xanchor = "left",
            x = 0.26,
            y = mylegend_y_position,
            text = 'DUC',
            font = list(size = myfont),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          ),
          list (
            xanchor = "left",
            x = 0.35,
            y = mylegend_y_position,
            text = '■',
            font = list(size = myfont * 1.2, color = '#9DC3E6'),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          ), 
          list (
            xanchor = "left",
            x = 0.39,
            y = mylegend_y_position,
            text = 'AUCU',
            font = list(size = myfont),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          ),
          list (
            xanchor = "left",
            x = 0.50,
            y = mylegend_y_position,
            text = '■',
            font = list(size = myfont * 1.2, color = '#BFBFBF'),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          ), 
          list (
            xanchor = "left",
            x = 0.54,
            y = mylegend_y_position,
            text = 'Total adjustments',
            font = list(size = myfont),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          )
          ),
        
        margin = mymargin
        
      )
  }
  
  ## plot chart  ----
  myplot[[ez]] <- mybarc_aucu(mywidth, myheight+40, myfont, mymargin)
}

# create html plotlist ----
htmltools::tagList(myplot)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document



