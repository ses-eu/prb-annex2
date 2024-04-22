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
      plotly::plot_ly(
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
      plotly::add_trace(
        inherit = FALSE,
        x = ~ year,
        y = ~ actual,
        yaxis = "y1",
        marker = list(color =('#FFC000')),
        text = ~ paste0(format(actual, nsmall = mytooltip_decimals),'%'),
        # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
        # textangle = -90,
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
      plotly::add_trace(
        inherit = FALSE,
        x = ~ year,
        y = ~ target,
        yaxis = "y1",
        type = 'scatter',  mode = 'lines+markers',
        line = list(color = '#FF0000', width = mylinewidth),
        marker = list(size = mylinewidth * 3, color = '#FF0000'),
        name = "Target",
        opacity = 1,
        hovertemplate = paste0('Target: %{y:.', mytooltip_decimals ,'f}%<extra></extra>'),
        # hoverinfo = "none",
        showlegend = T
      ) %>%
      plotly::add_trace(
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
      plotly::config( responsive = TRUE,
              displaylogo = FALSE,
              displayModeBar = F
              # modeBarButtons = list(list("toImage")),
      ) %>% 
      plotly::layout(
        font = list(family = "Roboto"),
        title = list(text = mychart_title,
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
  mybarc <-  function(mywidth, myheight, myfont) {
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
        text = ~ paste0(format(actual, nsmall = mytooltip_decimals),'%'),
        # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
        # textangle = -90,
        textposition = "inside", 
        cliponaxis = FALSE,
        insidetextanchor =  "middle",
        name = mymetric,
        textfont = list(color = mytextcolor, size = myfont),
        type = "bar",
        hovertemplate = paste0(mymetric, ': %{y:.', mytooltip_decimals, 'f}%<extra></extra>'),
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
                     y = 1, 
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
        margin = list (t = 40)
        
      )
  }
  