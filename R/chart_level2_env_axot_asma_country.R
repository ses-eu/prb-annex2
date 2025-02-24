# to avoid error when processing the .qmd file
if (country != "SES RP3") {

## import data  ----
data_raw_axot  <-  read_xlsx(
  paste0(data_folder, "ENV dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "Table_AXOT MS",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

data_raw_asma  <-  read_xlsx(
  paste0(data_folder, "ENV dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "Table_ASMA MS",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

rp3_years <- 2020:2024 %>% as_tibble %>% rename(xlabel = value)

## prepare data ----

data_prep_asma <- data_raw_asma %>% 
  rename(mymetric = asma_value_min_flight)  %>% 
  mutate(indicator_type = "ASMA")

data_prep_axot <- data_raw_axot %>% 
  rename(mymetric = axot_value_min_flight) 
  
data_prep <- data_prep_asma %>% 
  rbind(data_prep_axot) %>% 
  filter(
    entity_name == .env$country
    ) %>% 
  rename(
    type = indicator_type,
    xlabel = year
  ) %>% 
  mutate(
    mymetric = case_when(
      xlabel <= year_report ~ mymetric,
      .default = NA
    )
  ) %>%
  right_join(rp3_years, by = 'xlabel') %>% 
  mutate(type = if_else(is.na(type) == TRUE, "ASMA", type))
  
## chart parameters ----
mysuffix <- ""
mydecimals <- 2

### trace parameters
mycolors = c('#0070C050', '#FFC00050')
###set up order of traces
myfactor <- data_prep %>% select(type) %>% unique() 
invisible(as.list(myfactor$type))
myfactor <- sort(myfactor$type, decreasing = TRUE)
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- 0
mytextposition <- "bottom"
myinsidetextanchor <- NA
mytextfont_color <- 'black'

### layout 
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- paste0("ASMA & AXOT")
mytitle_y <- 0.99

#### xaxis

#### yaxis
myyaxis_title <- "ASMA & AXOT (min/flight)"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ".2f"
myyaxis_rangemode <- NA
myyaxis_range <- c(round(min(data_prep$mymetric, na.rm = TRUE)/5)*5, round(max(data_prep$mymetric, na.rm = TRUE)/5)*5)

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

#### margin
mylocalmargin <- list(t=60)

## define chart function ----
myareachart <-  function(mywidth, myheight, myfont, mymargin) {
    plot_ly(
      data = filter(data_prep,type ==  myfactor[[1]]),
      width = mywidth,
      height = myheight,
      x = ~ xlabel,
      y = ~ mymetric,
      yaxis = "y1",
      line = list(width = mylinewidth),
      # colors = mycolors,
      fillcolor = mycolors[[1]],
      name = myfactor[[1]],
      # color = ~ factor(type, levels = myfactor),
      text = ~ paste0('\n',format(mymetric,  big.mark  = ",", nsmall = mydecimals), mysuffix),
      # text = ~ mymetric,
      textangle = mytextangle,
      textposition = mytextposition, 
      insidetextanchor = myinsidetextanchor,
      textfont = list(color = mytextfont_color, size = mytextfont_size),
      cliponaxis = FALSE,
      type = 'scatter',
      stackgroup = 'one',
      mode = "none",
      hovertemplate = myhovertemplate,
      # hoverinfo = "none",
      showlegend = mytrace_showlegend
    ) %>% 
    add_trace(
      data = filter(data_prep,type == myfactor[[2]]),
      fillcolor = mycolors[[2]],
      name = myfactor[[2]]
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
      dragmode = FALSE,
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
                   range = c(2019.7, 2024),
                   tickformat = '.0f',
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
myareachart(mywidth, myheight, myfont, mylocalmargin) 
}