
# fix ez if script not executed from qmd file ----
if (exists("ez") == FALSE) {ez = 1}
# ez=1

# initialise list to store plots ----
myplot = list()

# loop through czs ----
for (ez in 1:no_ecz) {
  
  data_prep <- regulatory_result(ez)
  
  ## select relevant values for chart
  data_for_chart <- data_prep %>% 
    filter(xlabel == 'Main ANSP') %>% 
    mutate(
      share_rr_act_rev_expost = regulatory_result/actual_revenues * 100,
      share_rr_act_rev_exante = ex_ante_roe/actual_revenues * 100
      ) %>% 
    select(-xlabel, -x5_4_total_su, -actual_revenues)

  
  ## chart parameters ----
  mychart_title <- paste0("Regulatory Result - ", main_ansp)
  myaxis_title <- "Regulatory result (€M)"
  mybarcolor <- c( '#CEE0EA', '#4B8DB1')
  mytextcolor <- 'black'
  mylegend_y_position <- -0.28
  mylocalmargin = list (t = 60, b = 80)
  
  mytickformat_y <- ",.0f"
  mytooltipformat <- ",.2f"
  
  
  ## define chart function ----
  mybarc_reg_res_ansp <-  function(mywidth, myheight, myfont, mymargin) {
    mychart <- list()
    data_for_chart_value <- list()
    data_for_chart_share <- list()
    
    ### loop through years ----
    for (i in 1:nrow(data_for_chart)) {
       # i=2
      ### prepare data for chart ----
      data_for_chart_value[[i]] <- data_for_chart %>% 
        select(-c(share_rr_act_rev_expost, share_rr_act_rev_exante)) %>% 
        slice(i:i) %>% 
        pivot_longer(-year_text, names_to = "type", values_to = 'metric') %>% 
        mutate(type = case_when(
          type == 'regulatory_result'  ~ 'Ex-post',
          type == 'ex_ante_roe'  ~ 'Ex-ante')
        )
    
      data_for_chart_share[[i]] <- data_for_chart %>% 
        select(-c(regulatory_result, ex_ante_roe)) %>% 
        slice(i:i) %>% 
        pivot_longer(-year_text, names_to = "type", values_to = 'share') %>% 
        mutate(type = case_when(
          type == 'share_rr_act_rev_expost'  ~ 'Ex-post',
          type == 'share_rr_act_rev_exante'  ~ 'Ex-ante')
        )
      
      ### plot indivicual year charts ----
      mychart[[i]] <-  data_for_chart_value[[i]] %>% 
      plot_ly(
        width = mywidth,
        height = myheight+40,
        y = ~ round(metric, 2)/1000,
        # y = ~ if_else(is.na(metric) == TRUE, 0, round(metric, 2)/1000),
        x = ~ factor(type, levels = c('Ex-post', 'Ex-ante')),
        yaxis = "y1",
        type = 'bar',
        color = ~ factor(type, levels = c('Ex-post', 'Ex-ante')),
        colors = mybarcolor,
        text = '',
        # textangle = -90,
        textposition = "outside", 
        cliponaxis = FALSE,
        # insidetextanchor =  "middle",
        # name = mymetric,
        # textfont = list(color = mytextcolor, size = myfont * 0.8),
        hovertemplate = paste('%{y:,.2f}'),
        hoverinfo = "none",
        showlegend = T
      ) %>% 
        add_trace(
          data =  data_for_chart_value[[i]],
          x = ~ factor(type, levels = c('Ex-post', 'Ex-ante')),
          y = ~ '',
          name = "Fake series to force all years in x axis",
          yaxis = "y1",
          type = 'bar',
          color = ~ factor(type, levels = c('Ex-post', 'Ex-ante')),
          colors = mybarcolor,
          text = '',
          marker = list(color = 'transparent'),
          showlegend = F,
          # hovertemplate = '',
          hoverinfo = 'none'
        ) %>% 
        layout(
        showlegend = F,  
        barmode = "stack",
        bargap = '20',
        xaxis = list(title = '',
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = FALSE,
                     showline = FALSE,
                     showticklabels = TRUE,
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
        hovermode = "x unified",
        hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
        # uniformtext = list(minsize = myfont, mode='show'),
        yaxis = list(title = myaxis_title,
                     # gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     # tickprefix = if_else(" ",
                     # ticksuffix = "% ",
                     tickformat = mytickformat_y,
                     # tickcolor='black',
                     # ticklen = 7,
                     # showticklabels = TRUE,
                     # ticks = 'outside',
                     zeroline = TRUE,
                     zerolinecolor = 'rgb(255,255,255)',
                     titlefont = list(size = myfont), 
                     tickfont = list(size = myfont)
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
            y = -0.18,
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
            y = -0.18,
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
            y = -0.18,
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
            y = -0.18,
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
            x = 0.35,
            y = mylegend_y_position,
            text = '■',
            font = list(size = myfont * 1.2, color = '#CEE0EA'),
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
            text = 'Ex-ante RR (in value)',
            font = list(size = myfont*0.9),
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
            font = list(size = myfont * 1.2, color = '#4B8DB1'),
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
            text = 'Ex-post RR (in value)',
            font = list(size = myfont*0.9),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          )
          # list (
          #   xanchor = "left",
          #   x = 0.22,
          #   y = mylegend_y_position,
          #   text = '■',
          #   font = list(size = myfont * 1.2, color = '#5B9BD5'),
          #   xref = "paper",
          #   yref = "paper",
          #   showarrow = FALSE,
          #   # arrowhead = 7,
          #   ax = 0,
          #   ay = 0
          # ),
          # list (
          #   xanchor = "left",
          #   x = 0.26,
          #   y = mylegend_y_position,
          #   text = 'DUC',
          #   font = list(size = myfont),
          #   xref = "paper",
          #   yref = "paper",
          #   showarrow = FALSE,
          #   # arrowhead = 7,
          #   ax = 0,
          #   ay = 0
          # )
        ),
        
        margin = mylocalmargin
        
      )
  }
  
  ## plot chart  ----
  myplot[[ez]] <- mybarc_reg_res_ansp (mywidth, myheight+40, myfont, mylocalmargin)
}

# create html plotlist ----
htmltools::tagList(myplot)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document



