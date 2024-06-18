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

mytextangle <- 0
mytextposition <- "top"
myinsidetextanchor <- NA
mytextfont_color <- 'black'
mytextfont_size <- myfont

myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)
mytrace_showlegend <- T

### layout parameters
myfont_family <- "Roboto"
mybargap <- 0.25
mybarmode <- 'group'
myhovermode <- "x unified"
myhoverlabel_bgcolor <- 'rgba(255,255,255,0.88)'
myminsize <- myfont*0.8

#### title
mytitle_text <- paste0("CDO - ", country)
mytitle_x <- 0
mytitle_y <- 0.99
mytitle_xanchor <- 'left'
mytitle_yanchor <- 'top'
mytitle_font_size <- myfont * 20/15

#### xaxis
myxaxis_title <- ''
myxaxis_gridcolor <- 'rgb(255,255,255)'
myxaxis_showgrid <- TRUE
myxaxis_showline <- FALSE
myxaxis_showticklabels <- TRUE
myxaxis_tickformat <- "0"
myxaxis_dtick <- 1
myxaxis_zeroline <- TRUE
myxaxis_tickfont_size <- myfont

#### yaxis
myyaxis_title <- "To be added (%)"
myyaxis_gridcolor <- 'rgb(240,240,240)'
myyaxis_showgrid <- TRUE
myyaxis_showline <- FALSE
myyaxis_tickprefix <- ""
myyaxis_ticksuffix <- "%"
myyaxis_tickformat <- ".0f"
myyaxis_rangemode <- NA
myyaxis_range <- c(floor(min(data_prep$mymetric, na.rm = TRUE)/5)*5, ceiling(max(data_prep$mymetric, na.rm = TRUE)/5)*5)

myyaxis_zeroline <- TRUE
myyaxis_zerolinecolor <- 'rgb(255,255,255)'
myyaxis_titlefont_size <- myfont
myyaxis_tickfont_size <- myfont

#### legend
mylegend_traceorder <- 'normal'
mylegend_orientation <- 'h'
mylegend_xanchor <- "center"
mylegend_yanchor <- "center"
mylegend_x <- 0.5
mylegend_y <- -0.1
mylegend_font_size <- myfont

#### margin
mylocalmargin = mymargin


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
