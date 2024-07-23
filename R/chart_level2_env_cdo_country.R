## import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "ENV dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "Table_CDO MS",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

## prepare data ----
data_prep <- data_raw %>% 
  filter(
    entity_name == .env$country
    ) %>% 
  mutate(
    xlabel = year,
    type = indicator_type,
    mymetric = case_when(
      year <= year_report ~ round(cdo_ms_value * 100, 0),
      .default = NA)
  ) %>%  
  select(
    xlabel,
    type,
    mymetric)

## chart parameters ----
mysuffix <- "%"
mydecimals <- 0

### trace parameters
mycolors = c('#0070C0')
###set up order of traces
myfactor <- data_prep %>% select(type) %>% unique() 

myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- 0
mytextposition <- "top"
myinsidetextanchor <- NA
mytextfont_color <- 'black'

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- paste0("Share of arrivals applying\nContinuous Descent Operations (CDOs) (PI#5)")
mytitle_y <- 0.95

#### xaxis

#### yaxis
myyaxis_title <- "Share of flights (%)"
myyaxis_ticksuffix <- "%"
myyaxis_tickformat <- ".0f"
myyaxis_rangemode <- NA
myyaxis_range <- c((floor(min(data_prep$mymetric, na.rm = TRUE)/5)*5)-5, (ceiling(max(data_prep$mymetric, na.rm = TRUE)/5)*5)+5)

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

#### margin
mylocalmargin = list(t=60)

## define chart function ----
mylinechart <-  function(df, mywidth, myheight, myfont, mymargin) {
  df %>% 
    plot_ly(
      width = mywidth,
      height = myheight,
      x = ~ xlabel,
      y = ~ mymetric,
      yaxis = "y1",
      line = list(color = mycolors, width = mylinewidth),
      marker = list(size = mylinewidth * 3, 
                    color = mycolors,
                    symbol = NA),
      # colors = mycolors,
      color = ~ factor(type, levels = myfactor),
      text = ~ paste0(format(mymetric,  big.mark  = ",", nsmall = mydecimals), mysuffix),
      # text = ~ mymetric,
      textangle = mytextangle,
      textposition = mytextposition, 
      insidetextanchor = myinsidetextanchor,
      textfont = list(color = mytextfont_color, size = mytextfont_size),
      cliponaxis = FALSE,
      type = 'scatter',
      mode = "line+markers",
      hovertemplate = myhovertemplate,
      # hoverinfo = "none",
      showlegend = mytrace_showlegend
    ) %>% 
    config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
            # modeBarButtons = list(list("toImage")),
    ) %>% 
    layout(
      uniformtext=list(minsize = myminsize, mode='show'),
      font = list(family = myfont_family),
      title = list(text = mytitle_text,
                   x = mytitle_x, 
                   y = mytitle_y, 
                   xanchor = mytitle_xanchor, 
                   yanchor = mytitle_yanchor,
                   font = list(size = mytitle_font_size)
      ),
      bargap = mybargap,
      barmode = mybarmode,
      hovermode = myhovermode,
      hoverlabel = list(bgcolor = myhoverlabel_bgcolor),
      xaxis = list(title = myxaxis_title,
                   gridcolor = myxaxis_gridcolor,
                   showgrid = myxaxis_showgrid,
                   showline = myxaxis_showline,
                   showticklabels = myxaxis_showticklabels,
                   dtick = myxaxis_dtick,
                   tickformat = myxaxis_tickformat,
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = myxaxis_zeroline, 
                   tickfont = list(size = myxaxis_tickfont_size)
      ),
      yaxis = list(title = myyaxis_title,
                   gridcolor = myyaxis_gridcolor,
                   showgrid = myyaxis_showgrid,
                   showline = myyaxis_showline,
                   tickprefix = myyaxis_tickprefix,
                   ticksuffix = myyaxis_ticksuffix, 
                   tickformat = myyaxis_tickformat,
                   # showticklabels = TRUE,
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   range = myyaxis_range,
                   rangemode = myyaxis_rangemode,
                   zeroline = myyaxis_zeroline,
                   zerolinecolor = myyaxis_zerolinecolor,
                   titlefont = list(size = myyaxis_titlefont_size), 
                   tickfont = list(size = myyaxis_tickfont_size)
      ),
      legend = list(
        traceorder= mylegend_traceorder,
        orientation = mylegend_orientation, 
        xanchor = mylegend_xanchor,
        yanchor = mylegend_yanchor,
        x = mylegend_x,  
        y = mylegend_y, 
        font = list(size = mylegend_font_size)
      ),
      margin = mymargin
    )
}


## plot chart  ----
mylinechart(data_prep, mywidth, myheight, myfont, mylocalmargin) %>% 
  add_empty_trace(data_prep)
