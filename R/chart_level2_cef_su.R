
# fix ez if script not executed from qmd file ----
if (exists("ez") == FALSE) {ez = 1}
# ez=1

# initialise list to store plots ----
myplot = list()

# loop through czs ----
for (ez in 1:no_ecz) {
## import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "CEFF dataset master.xlsx"),
  sheet = "Enroute_T1",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

## prepare data ----
data_prep <- data_raw %>% 
  filter(
    entity_code == ecz_list$ecz_id[ez]) %>% 
  mutate(
    mymetric = round(x5_4_total_su/1000, 0)
  ) %>%  
  select(
    year,
    status,
    mymetric
  ) %>%  
  filter(year > 2021) %>% 
  mutate(year_text = as.character(year),
         year_text = str_replace(year_text, "20202021", "2020-2021"),
         status = str_replace(status, "A", "Actual SUs"),
         status = str_replace(status, "D", "Planned SUs")
  ) %>% 
  arrange(year_text)

## replace 0 by NAs so they are not plotted
data_prep[data_prep == 0] <- NA

## add a column to determine the minimum y axis value
data_prep <- data_prep %>% mutate(min_y_axis = min(mymetric, na.rm=T)/1.5)

## create dfs for series 
data_prep_planned <- data_prep %>% 
  filter(status == "Planned SUs") 

data_prep_actual <- data_prep %>% 
  filter(status == "Actual SUs")

## set parameters for chart ----
myaxis_title <- "En route TSUs '000"
mychart_title <- 'En route service units'
mytitle_pos <- 0.98

## define chart function ----
myc <- function (mywidth, myheight, myfont, mylinewidth, mymargin) {
  plot_ly( 
    width = mywidth,
    height = myheight
  ) %>% 
    add_trace(
      data = data_prep_planned,
      x = ~ year_text,
      y = ~ mymetric,
      text = ~ format(round(mymetric*.02, 0), nsmall = 0,  big.mark = ','),
      yaxis = "y1",
      type = 'scatter',  mode = 'lines',
      line = list(width = 0, dash = 'solid', color = 'transparent'),
      error_y = list(type = "data", array = ~mymetric*0.02, color= 'grey'),
      opacity = 1,
      hoverinfo = 'none',
      # hovertemplate = paste('±2%: ±%{text}<extra></extra>'),
      showlegend = F
    ) %>%
    add_trace(
      data = data_prep_planned,
      x = ~ year_text,
      y = ~ mymetric,
      text = ~ format(round(mymetric*.1, 0), nsmall = 0,  big.mark = ','),
      yaxis = "y1",
      type = 'scatter',  mode = 'lines',
      line = list(width = 0, dash = 'solid', color = 'transparent'),
      error_y = list(type = "data", array = ~mymetric*0.1, color= 'black'),
      opacity = 1,
      hoverinfo = 'none',
      # hovertemplate = paste('±10%: ±%{text}<extra></extra>'),
      showlegend = F
    ) %>%
    add_trace(
      data = data_prep_planned,
      x = ~ year_text,
      y = ~ mymetric,
      name = ~status,
      text = ~status, 
      yaxis = "y1",
      type = 'scatter',  mode = 'lines+markers',
      line = list(width = mylinewidth, dash = 'solid', color = '#5B9BD5'),
      marker = list(size = mylinewidth * 3, color = '#5B9BD5'),
      # error_y = list(type = "data", array = ~mymetric*0.1, color= 'black'),
      opacity = 1,
      hovertemplate = paste('%{text}: %{y:,.0f}<extra></extra>'),
      showlegend = T
      # hoverinfo = 'none'
    ) %>%
    add_trace(                                  #we add them again for the tooltip order
      data = data_prep_planned,
      x = ~ year_text,
      y = ~ mymetric,
      text = ~ format(round(mymetric*.02, 0), nsmall = 0,  big.mark = ','),
      yaxis = "y1",
      type = 'scatter',  mode = 'lines',
      line = list(width = 0, dash = 'solid', color = 'transparent'),
      opacity = 1,
      # hoverinfo = 'none',
      hovertemplate = paste('±2%: ±%{text}<extra></extra>'),
      showlegend = F
    ) %>%
    add_trace(
      data = data_prep_planned,
      x = ~ year_text,
      y = ~ mymetric,
      text = ~ format(round(mymetric*.1, 0), nsmall = 0,  big.mark = ','),
      yaxis = "y1",
      type = 'scatter',  mode = 'lines',
      line = list(width = 0, dash = 'solid', color = 'transparent'),
      opacity = 1,
      # hoverinfo = 'none',
      hovertemplate = paste('±10%: ±%{text}<extra></extra>'),
      showlegend = F
    ) %>%
    add_trace(
      inherit = FALSE,
      data = data_prep_actual,
      x = ~ year_text,
      y = ~ mymetric,
      name = ~status,
      text = ~status, 
      yaxis = "y1",
      cliponaxis = FALSE,
      yaxis = "y1",
      type = 'scatter',  mode = 'lines+markers',
      line = list(width = mylinewidth, dash = 'solid', color = '#FFC000'),
      marker = list(size = mylinewidth * 3, color = '#FFC000'),
      opacity = 1,
      hovertemplate = paste('%{text}: %{y:,.0f}<extra></extra>'),
      # hoverinfo = "none",
      showlegend = T
    ) %>% 
    add_trace(      ## to push the y axis down
      x = ~ year_text,
      y = ~ min_y_axis,
      yaxis = "y1",
      cliponaxis = FALSE,
      yaxis = "y1",
      type = 'scatter',  mode = 'lines',
      line = list(width = 0, dash = 'solid', color = 'transparent'),
      opacity = 1,
      hoverinfo = 'none',
      showlegend = F
    ) %>% 
    config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
    ) %>% 
    layout(
      font = list(family = "Roboto"),
      title = list(text = mychart_title,
                   y = mytitle_pos, 
                   x = 0.02, 
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
      yaxis = list(title = myaxis_title,
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
}

## plot chart ----
myplot[[ez]] <- myc(mywidth, myheight, myfont, mylinewidth, mymargin)

}

# create html plotlist ----
htmltools::tagList(myplot)

