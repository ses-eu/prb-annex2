# libraries ----
library(tidyr)
library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)
library(janitor)
library(webshot)
library(data.table)
library(here)
library(fs)
library(purrr)
library(plotly)

# functions ----
## right x characters function ----
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }  

## read range function ----
  read_range <- function(file, sheet, range){
    read_excel(
      file,
      sheet = sheet,
      range = range) %>% 
      mutate_all(~ str_replace_all(., "\r\n\r\n", "\r\n")) %>% 
      mutate_all(~ str_replace_all(., "<br><br><br><br>", "<br><br>"))   
  }

## read table range function ----
read_mytable <- function(file, sheet, table){
  wb <- loadWorkbook(paste0(data_folder, file))
  tables <- getTables(wb, sheet = sheet)
  # get the range
  table_range <- names(tables[tables == table])
  # read range
  read_range(paste0(data_folder, file), sheet, table_range) 
  }

## add columns to df if they don't exist ----
  add_cols <- function(df, cols) {
    add <- cols[!cols %in% names(df)]
    if (length(add) != 0) {
      df[add] <- NA
    }
    return(df)
  }

## get yearly exchange rates ----
  get_xrates <- function() {
    data_raw_xrates  <-  read_xlsx(
      paste0(data_folder, "CEFF dataset master.xlsx"),
      # here("data","hlsr2021_data.xlsx"),
      sheet = "ERT_XRATE2017",
      range = cell_limits(c(1, 1), c(NA, NA))) %>%
      as_tibble() %>% 
      clean_names()  
    
    yearly_xrates <- data_raw_xrates %>% 
      filter(
        entity_code %in% ecz_list$ecz_id == TRUE
      ) %>% 
      select(entity_code, contains('pp_exchangerate_' )) %>% 
      pivot_longer(cols = starts_with("pp_exchangerate_"),
                   names_to = "year",
                   values_to = 'pp_exchangerate') %>% 
      mutate(year = str_replace_all(year, 'pp_exchangerate_', ''),
             year = as.numeric(year),
             pp_exchangerate = if_else(pp_exchangerate == 0, NA, pp_exchangerate),
      )
    return(yearly_xrates)
  }

## export figure function ----
  # the export function needs webshot and PhantomJS. Install PhantomJS with 'webshot::install_phantomjs()' and then cut the folder from wherever is installed and paste it in C:\Users\[username]\dev\r\win-library\4.2\webshot\PhantomJS

  export_fig <- function (fig, fig_name, width, height) {
    fig_dir <- paste0('images/', year_report, '/', country,'/')
    invisible(export(fig, paste0(fig_dir, fig_name)))
    invisible(figure <- image_read(paste0(fig_dir,fig_name)))
    invisible(cropped <- image_crop(figure, paste0(width, "x", height)))
    invisible(image_write(cropped, paste0(fig_dir, fig_name)))
  }


## plot bar chart with target  ----
  mybarct <-  function(mywidth, myheight, myfont, mylinewidth, mymargin) {
    data_for_chart %>% 
      plot_ly(
        width = mywidth,
        height = myheight,
        x = ~ year,
        y = ~ target,
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
        y = ~ actual,
        yaxis = "y1",
        marker = list(color =('#FFC000')),
        text = ~ paste0(format(actual, nsmall = mytooltip_decimals),'%'),
        # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
        textangle = mytextangle,
        textposition = "inside", 
        cliponaxis = FALSE,
        insidetextanchor =  "middle",
        name = "Actual",
        textfont = list(color = 'black', size = myfont),
        type = "bar",
        hovertemplate = paste0(mymetric, ': %{y:.', mytooltip_decimals, 'f}%<extra></extra>'),
        # hoverinfo = "none",
        showlegend = T
      ) %>%
      add_trace(
        inherit = FALSE,
        x = ~ year,
        y = ~ target,
        yaxis = "y1",
        type = 'scatter',  mode = 'lines+markers',
        line = list(color = '#FF0000', width = mylinewidth),
        marker = list(size = mylinewidth * 3, color = mymarker_color),
        name = "Target",
        opacity = 1,
        hovertemplate = paste0('Target: %{y:.', mytooltip_decimals ,'f}%<extra></extra>'),
        # hoverinfo = "none",
        showlegend = T
      ) %>%
      add_trace(
        inherit = FALSE,
        x = ~ year,
        y = ~ target + 0.025 * max(data_for_chart$target),
        yaxis = "y1",
        mode = 'text',
        text = ~ paste0('<b>', format(target, nsmall = mytooltip_decimals) ,'%', '</b>'),
        textposition = "top", cliponaxis = FALSE,
        textfont = list(color = targetcolor, size = myfont),
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
        title = list(text = mychart_title,
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
                     dtick = mydtick,
                     tickformat = mytickformat,
                     xperiod0 = min(data_for_chart$year),
                     # tickcolor = 'rgb(127,127,127)',
                     # ticks = 'outside',
                     zeroline = TRUE,
                     tickfont = list(size = myfont)
        ),
        yaxis = list(title = myaxis_title,
                     # gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     tickprefix = " ",
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
        margin = mymargin
        
      )
  }

## plot bar chart without target  ----
  mybarc <-  function(mywidth, myheight, myfont, mymargin) {
    data_for_chart %>% 
      plot_ly(
        width = mywidth,
        height = myheight,
        x = ~ year,
        y = 0, # to force all years in x axis
        yaxis = "y1",
        cliponaxis = FALSE,
        name = "",
        textfont = list(color = 'transparent'),
        type = "bar",
        marker = list(color =('transparent')),
        # hovertemplate = '',
        hoverinfo = "none",
        showlegend = F
      ) %>%
      add_trace(
        x = ~ year,
        y = ~ actual,
        yaxis = "y1",
        marker = list(color = mybarcolor),
        text = ~ paste0(format(actual,  big.mark  = ",", nsmall = mytooltip_decimals), myticksuffix),
        # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
        textangle = mytextangle,   #auto
        textposition = mytextposition,   #"inside", 
        cliponaxis = FALSE,
        insidetextanchor = mylabelposition, # "middle",
        name = mymetric,
        textfont = list(color = mytextcolor, size = myfont),
        type = "bar",
        hovertemplate = paste0(mymetric, ': %{y:,.', mytooltip_decimals, 'f}', myticksuffix ,'<extra></extra>'),
        # hoverinfo = "none",
        showlegend = T
        
      ) %>% 
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
        bargap = 0.25,
        barmode = 'stack',
        hovermode = "x unified",
        hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
        xaxis = list(title = "",
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = FALSE,
                     showline = FALSE,
                     showticklabels = TRUE,
                     dtick = mydtick,  # 1,
                     # tickcolor = 'rgb(127,127,127)',
                     # ticks = 'outside',
                     zeroline = TRUE,
                     tickfont = list(size = myfont)
        ),
        yaxis = list(title = myaxis_title,
                     # gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     tickprefix = " ",
                     ticksuffix = myticksuffix,  #"% ",
                     tickformat = mytickformat_y,  #".1f",
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
  }

  ## plot capacity annual chart  ----
  mycapchart <-  function(mywidth, myheight, myfont, mylinewidth, mymargin,
                          data_prep_target,
                          data_prep_actual,
                          mytitle,
                          myrightaxis,
                          mytrafficmetric) {
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
        y = ~ average_delay,
        yaxis = "y1",
        cliponaxis = FALSE,
        name = "Total delay",
        type = 'scatter',  mode = 'lines',
        line = list(color = 'transparent', width = 0),
        text = ~ format(round(average_delay, 2), digits = 2),
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
        y = ~ target,
        cliponaxis = FALSE,
        yaxis = "y1",
        type = 'scatter',  mode = 'lines+markers',
        line = list(color = '#FF0000', width = mylinewidth),
        marker = list(size = mylinewidth * 3, color = '#FF0000'),
        name = "Target",
        text = ~ paste0 ("<b>", format(target, digits = 2), "</b>"),
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
        y = ~ round(movements/1000, 0),
        yaxis = "y2",
        type = 'scatter',  mode = 'lines+markers',
        line = list(color = '#FFC000', width = mylinewidth),
        marker = list(size = mylinewidth * 3, color = '#FFC000'),
        name = mytrafficmetric,
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
        title = list(text = mytitle,
                     y = 0.99, 
                     x = 0, 
                     xanchor = 'left', 
                     yanchor =  'top',
                     font = list(size = myfont * 20/15)
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
                     rangemode = "nonnegative",
                     zeroline = TRUE,
                     zerolinecolor = 'rgb(255,255,255)',
                     titlefont = list(size = myfont), tickfont = list(size = myfont)
        ),
        yaxis2 = list(title = myrightaxis,
                      overlaying = "y",
                      side = "right",
                      showgrid = FALSE,
                      showline = FALSE,
                      tickformat = ",",
                      rangemode = "nonnegative",
                      zeroline = TRUE,
                      zerolinecolor = 'rgb(255,255,255)',
                      titlefont = list(size = if_else(country == 'SES RP3', 1, myfont), 
                                       color = if_else(country == 'SES RP3', 'transparent', 'black')
                      ), 
                      tickfont = list(size = if_else(country == 'SES RP3', 1, myfont),
                                      color = if_else(country == 'SES RP3', 'transparent', 'black')
                      )
        ),
        # showlegend = FALSE
        legend = list(
          orientation = 'h', 
          xanchor = "left",
          x = mylegend_x_pos, 
          y =-0.1,
          font = list(size = myfont*0.9)
        ),
        margin = mymargin
        
      )
  }
 
  ## plot capacity monthly chart  ----
  
  mycapchart_month <-  function(mywidth, myheight, myfont, mylinewidth, mymargin) {
    plot_ly(
      data = data_prep_actual,
      width = mywidth,
      height = myheight,
      x = ~ month,
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
        x = ~ month,
        y = ~ average_delay,
        yaxis = "y1",
        cliponaxis = FALSE,
        name = "Total delay",
        type = 'scatter',  mode = 'lines',
        line = list(color = 'transparent', width = 0),
        text = ~ format(round(average_delay, 2), digits = 2),
        textfont = list(color = 'black', size = myfont*0.9),
        textposition = "top center",
        hovertemplate = paste('Total delay: %{y:.2f}<extra></extra>'),
        opacity = 1,
        showlegend = F
      ) %>%
      config( responsive = TRUE,
                      displaylogo = FALSE,
                      displayModeBar = F
                      # modeBarButtons = list(list("toImage")),
      ) %>%
      layout(
        font = list(family = "Roboto"),
        title = list(text = mytitle,
                     y = 0.99, 
                     x = 0, 
                     xanchor = 'left', 
                     yanchor =  'top',
                     font = list(size = myfont * 20/15)
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
                     tickformat = "%b",
                     tick0 = min(data_prep_actual$month),
                     dtick = "M1",
                     zeroline = TRUE,
                     tickfont = list(size = myfont)
        ),
        yaxis = list(title = "Average minutes of delay",
                     showgrid = TRUE,
                     showline = FALSE,
                     tickformat = ".2f",
                     rangemode = "nonnegative",
                     zeroline = TRUE,
                     zerolinecolor = 'rgb(255,255,255)',
                     titlefont = list(size = myfont), tickfont = list(size = myfont)
        ),
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
  
  ## plot CEF non-stacked bar chart  ----
  mybarc_nonst <-  function(mywidth, myheight, myfont, mymargin) {
    data_prep %>% 
      plot_ly(
        width = mywidth,
        height = myheight,
        x = ~ year_text,
        y = ~ mymetric,
        yaxis = "y1",
        text = ~ paste0(format(mymetric, nsmall = 2), myticksuffix),
        textangle = mytextangle,
        textposition = mytextposition, 
        cliponaxis = FALSE,
        insidetextanchor =  mylabelposition,
        textfont = list(color = 'black', size = mytextsize),
        type = "bar",
        color = ~ factor(status, levels = myfactor),
        colors = mycolors,
        hovertemplate = paste0('%{y:.2f}', myticksuffix),
        # hoverinfo = "none",
        showlegend = T
      ) %>%
    add_trace(           ### series to force the full x series of years
      inherit = FALSE,
      data = data_prep,
      x = ~ year_text,
      y = 0,
      yaxis = "y1",
      type = 'scatter',
      mode = "markers",
      # line = list(width = 0),
      marker = list(color = 'transparent'),
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
        title = list(text = mychart_title,
                     y = 0.99, 
                     x = 0, 
                     xanchor = 'left', 
                     yanchor =  'top',
                     font = list(size = myfont * 20/15)
        ),
        uniformtext=list(minsize = myfont*0.8, mode='show'),
        bargap = 0.25,
        hovermode = "x unified",
        hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
        xaxis = list(title = "",
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = FALSE,
                     showline = FALSE,
                     showticklabels = TRUE,
                     dtick = mydtick,
                     tickformat = mytickformat_x,
                     # range = myrange,
                     # tickvals = myticktext,
                     # tickcolor = 'rgb(127,127,127)',
                     # ticks = 'outside',
                     zeroline = TRUE,
                     tickfont = list(size = myfont)
        ),
        yaxis = list(title = myaxis_title,
                     # gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     ticksuffix = myticksuffix,
                     tickformat = mytickformat_y,
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
          y = mylegend_y_position,
          font = list(size = myfont)
        ),
        margin = mymargin
      )
    
  }
  
  ## plot CEF horizontal barchart A-D  ----
  myhbarc <-  function(mywidth, myheight, myfont, mymargin) {
    data_prep %>% 
      plot_ly(
        width = mywidth,
        height = myheight,
        x = ~ round(mymetric, 0),
        y = ~ factor(ylabel, levels = myfactor),
        yaxis = "y1",
        marker = list(color = mybarcolor),
        text = ~ mylabel,
        # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
        # textangle = -90,
        textposition = "auto", 
        cliponaxis = FALSE,
        orientation = 'h',
        # insidetextanchor =  "middle",
        # name = mymetric,
        textfont = list(color = mytextcolor, size = myfont),
        type = "bar",
        hovertemplate = paste0('%{y} (A-D): %{x:+0,}<extra></extra>'),
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
        title = list(text = mychart_title,
                     y = 0.99, 
                     x = 0, 
                     xanchor = 'left', 
                     yanchor =  'top',
                     font = list(size = myfont * 20/15)
        ),
        bargap = 0.25,
        barmode = 'stack',
        hovermode = "y",
        hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
        yaxis = list(title = "",
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = FALSE,
                     showline = FALSE,
                     showticklabels = TRUE,
                     # dtick = 1,
                     # tickcolor = 'rgb(127,127,127)',
                     # ticks = 'outside',
                     zeroline = TRUE,
                     tickfont = list(size = myfont)
        ),
        xaxis = list(title = myaxis_title,
                     # gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     # tickprefix = if_else(" ",
                     # ticksuffix = "% ",
                     tickformat = "+0,",
                     # showticklabels = TRUE,
                     # tickcolor = 'rgb(127,127,127)',
                     # ticks = 'outside',
                     zeroline = TRUE,
                     zerolinecolor = 'rgb(255,255,255)',
                     titlefont = list(size = myfont), tickfont = list(size = myfont)
        ),
        showlegend = FALSE,
        margin = mymargin
        
      )
  }
  
  ## plot grouped/stacked barchart  ----
  mybarc_group <-  function(mywidth, myheight, myfont, mymargin) {
    data_prep %>% 
      plot_ly(
        width = mywidth,
        height = myheight,
        y = ~ round(mymetric/1000, mytooltip_decimals),
        x = ~ year_text,
        yaxis = "y1",
        colors = mybarcolor,
        # color = ~ xlabel,
        color = ~ factor(xlabel, levels = myfactor),
        # text = ~ mylabel,
        # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
        # textangle = -90,
        textposition = "auto", 
        cliponaxis = FALSE,
        # insidetextanchor =  "middle",
        # name = mymetric,
        textfont = list(color = mytextcolor, size = myfont),
        type = "bar",
        # hovertemplate = paste0('%{y} (A-D): %{x:+0,}<extra></extra>'),
        # hoverinfo = "none",
        showlegend = T
      ) %>% 
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
        bargap = 0.25,
        barmode = 'stack',
        hovermode = "x unified",
        hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
        xaxis = list(title = "",
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = FALSE,
                     showline = FALSE,
                     showticklabels = TRUE,
                     # dtick = 1,
                     # tickcolor = 'rgb(127,127,127)',
                     # ticks = 'outside',
                     zeroline = TRUE,
                     tickfont = list(size = myfont)
        ),
        yaxis = list(title = myaxis_title,
                     # gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     # tickprefix = if_else(" ",
                     # ticksuffix = "% ",
                     tickformat = "0,",
                     # showticklabels = TRUE,
                     # tickcolor = 'rgb(127,127,127)',
                     # ticks = 'outside',
                     zeroline = TRUE,
                     zerolinecolor = 'rgb(200,200,200)',
                     titlefont = list(size = myfont), tickfont = list(size = myfont)
        ),
        legend = list(
          orientation = 'h', 
          traceorder= 'normal',
          xanchor = mylegend_x_anchor ,  # "left"
          x = mylegend_x_position,       # 0
          y = mylegend_y_position,
          font = list(size = myfont)
        ),
        margin = mymargin
        
      )
  }
  