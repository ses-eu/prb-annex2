
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
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Enroute_T1",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----
  data_prep <- data_raw %>% 
    filter(
      charging_zone_code == ecz_list$ecz_id[ez],
      entity_type_id == "ANSP1",
      year == .env$year_report
    ) %>% 
    select(
      year,
      entity_name,
      status,
      x1_1_staff,
      x1_2_other_operating_cost,
      x1_3_depreciation,
      x1_4_cost_of_capital,
      x1_5_exceptional_items,
      x4_1_cost_for_vfr_exempted
    ) %>% 
    pivot_longer(cols = -c(year, entity_name, status),
      names_to = 'type', 
      values_to = 'value') %>% 
    pivot_wider(names_from = 'status', values_from = 'value') %>% 
    arrange(desc(type)) %>% 
    mutate(mymetric = (A-D)/1000,
           mylabel = if_else(A == 0, '-', 
                             paste0(
                               if_else(mymetric > 0, '+', ''),
                               round((A/D-1) *100, 0), 
                               '%')),
           ylabel = as.factor(c("VFR exempted", 
                          "Exceptional items",
                          "Cost of capital",
                          "Depreciation costs",
                          "Other operating costs",
                          "Staff costs"))) 
  

  ## chart parameters ----
  mychart_title <- paste0(main_ansp,", Actual v Determined costs - ", year_report)
  myaxis_title <- "Costs (€2017'000)"
  mybarcolor <- '#A5A5A5'
  mytextcolor <- 'black'
  
  ## define chart function ----
  myhbarc <-  function(mywidth, myheight, myfont, mymargin) {
    data_prep %>% 
      plot_ly(
        width = mywidth,
        height = myheight,
        x = ~ round(mymetric, 0),
        y = ~ factor(ylabel, levels = c("VFR exempted", 
                                       "Exceptional items",
                                       "Cost of capital",
                                       "Depreciation costs",
                                       "Other operating costs",
                                       "Staff costs")),
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
                     y = 1, 
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
  
  ## plot chart  ----
  myplot[[ez]] <- myhbarc(mywidth, myheight, myfont, mymargin)
  
}

# create html plotlist ----
htmltools::tagList(myplot)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document
