
# parameters 
# mywidth = 300
# myheight = 220
# myfont = 8
# # mymargin = list (t = 20, l = 0)
# mylinewidth = 2

if (country == 'SES RP3') {
  # SES case ----
  ## import data  ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "SES.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "SES_ATFM_ERT_delay",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----
  data_prep_target <- data_raw %>% 
    filter(
      year_report == .env$year_report) %>% 
    mutate(
      er_cap_target = round(target, 2)
    ) %>% 
    select(
      year,
      er_cap_target
    ) %>% arrange(year)
  
  data_prep_actual <- data_raw %>% 
    filter(
      year_report == .env$year_report) %>% 
    select(-c(year_report,target)) %>% 
    pivot_longer(
      cols = c(capacity, staffing, disruptions, weather, other_non_atc),
      names_to = "type",
      values_to = "delay"
    ) %>% 
    mutate(
      type = case_when(
        type == "capacity" ~ "Capacity",
        type == "staffing" ~ "Staffing",
        type == "disruptions" ~ "Disruptions",
        type == "weather" ~ "Weather",
        type == "other_non_atc" ~ "Other non-ATC"
      ) 
    ) %>% 
    rename (average_delay = avg_er_atfm_delay) %>% 
    mutate(ifr = NA)
} else {
  # state case ----
  ## import data  ----
  data_raw_target  <-  read_xlsx(
    paste0(data_folder, "CAP dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "en route delay targets",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_raw_actual  <-  read_xlsx(
    paste0(data_folder, "CAP dataset master.xlsx"),
    sheet = "en route monthly delay",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----
  data_prep_target <- data_raw_target %>% 
    filter(
      state == .env$country,
      year == .env$year_report
      ) %>% 
    mutate(
      er_cap_target = round(delay_target, 2)
    ) %>% 
    select(
      year,
      er_cap_target)
  
  data_prep_actual <- data_raw_actual %>% 
    filter(
      state == .env$country,
      year == .env$year_report
      ) %>% 
    pivot_longer(
      cols = c(atc_capacity, atc_staffing, atc_disruptions, weather, other_non_atc),
      names_to = "type",
      values_to = "delay"
    ) %>% 
    mutate(
      type = case_when(
        type == "atc_capacity" ~ "Capacity",
        type == "atc_staffing" ~ "Staffing",
        type == "atc_disruptions" ~ "Disruptions",
        type == "weather" ~ "Weather",
        type == "other_non_atc" ~ "Other non-ATC"
      )
    )
}

# chart ----
## set parameters for chart ----

if (knitr::is_latex_output()) {
  if (country == 'SES RP3') {
    mymargin <- list (t = 20, r = 0, l = 30)
  } else {
    mymargin <- list (t = 20, r = 50, l = 10)
  }
  mylegend_x_pos <- 0.5
} 

## define chart function ----
myc <-  function(mywidth, myheight, myfont, mylinewidth, mymargin) {
  plotly::plot_ly(
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
    plotly::add_trace(
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
    # plotly::add_trace(
    #   data = data_prep_target,
    #   inherit = FALSE,
    #   x = ~ month,
    #   y = ~ er_cap_target,
    #   cliponaxis = FALSE,
    #   yaxis = "y1",
    #   type = 'scatter',  mode = 'lines+markers',
    #   line = list(color = '#FF0000', width = mylinewidth),
    #   marker = list(size = mylinewidth * 3, color = '#FF0000'),
    #   name = "Target",
    #   text = ~ paste0 ("<b>", format(er_cap_target, digits = 2), "</b>"),
    #   textfont = list(color = '#FF0000', size = myfont),
    #   textposition = "top center",
    #   hovertemplate = paste('Target: %{y:.2f}<extra></extra>'),
    #   opacity = 1,
    #   showlegend = T
    # ) %>%
    # plotly::add_trace(
    #   data = data_prep_actual,
    #   inherit = FALSE,
    #   x = ~ year,
    #   y = ~ round(ifr_movements/1000, 0),
    #   yaxis = "y2",
    #   type = 'scatter',  mode = 'lines+markers',
    #   line = list(color = '#FFC000', width = mylinewidth),
    #   marker = list(size = mylinewidth * 3, color = '#FFC000'),
    #   name = "IFR mvts.",
    #   hovertemplate = paste("IFR mvts ('000): %{y:,}<extra></extra>"),
    #   opacity = 1,
    #   showlegend = T
    # ) %>%
    plotly::config( responsive = TRUE,
                    displaylogo = FALSE,
                    displayModeBar = F
                    # modeBarButtons = list(list("toImage")),
    ) %>%
    plotly::layout(
      font = list(family = "Roboto"),
      title = list(text=paste0("Average monthly en route ATFM delay per flight - ", year_report),
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
      # yaxis2 = list(title = "IFR flights ('000)",
      #               overlaying = "y",
      #               side = "right",
      #               showgrid = FALSE,
      #               showline = FALSE,
      #               tickformat = ",",
      #               rangemode = "nonnegative",
      #               zeroline = TRUE,
      #               zerolinecolor = 'rgb(255,255,255)',
      #               titlefont = list(size = if_else(country == 'SES RP3', 1, myfont), 
      #                                color = if_else(country == 'SES RP3', 'transparent', 'black')
      #               ), 
      #               tickfont = list(size = if_else(country == 'SES RP3', 1, myfont),
      #                               color = if_else(country == 'SES RP3', 'transparent', 'black')
      #               )
      # ),
      # showlegend = FALSE
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

## plot chart ----
myc(mywidth, myheight+20, myfont, mylinewidth, mymargin)


## export to image 
# w = 1200
# h = 600
# export_fig(myc(w, h, 14 * w/900, 70 * w/1000),"cap_er_main.png", w, h)

