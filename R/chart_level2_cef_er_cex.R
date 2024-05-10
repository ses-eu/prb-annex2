
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
    sheet = "Enroute_T2",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----
  data_prep <- data_raw %>% 
    filter(
      entity_code == ecz_list$ecz_id[ez],
    ) %>% 
    select(
      year,
      x3_1_investment,
      x3_3_cost_authority_qes,
      x3_4_ectl_cost_eur,
      x3_5_pension_cost,
      x3_6_interest_loan,
      x3_7_change_in_law
    ) %>% 
    pivot_longer(cols = -c(year),
                 names_to = 'type', 
                 values_to = 'mymetric') %>% 
    mutate(xlabel = rep(c("New and existing investments", 
                          "Competent authorities\nand qualified entities costs",
                          "Eurocontrol costs",
                          "Pension costs",
                          "Interest on loans",
                          "Changes in law"), 4),
           year_text = as.character(year),
           year_text = str_replace(year_text, "20202021", "2020-2021"),
    )
  
  
  ## chart parameters ----
  mychart_title <- paste0("Cost exempt - ", year_report)
  myaxis_title <- "Cost exempt from cost sharing\n(€2017'000)"
  mybarcolor <- c( '#C9C9C9', '#B4C7E7', '#FFC000', '#A9D18E', '#F4B183', '#8497B0')
  mytextcolor <- 'black'
  mylegend_y_position <- -0.2
  
  ## define chart function ----
  mybarc_group <-  function(mywidth, myheight, myfont, mymargin) {
    data_prep %>% 
      plot_ly(
        width = mywidth,
        height = myheight,
        y = ~ round(mymetric/1000, 0),
        x = ~ year_text,
        yaxis = "y1",
        colors = mybarcolor,
        # color = ~ xlabel,
        color = ~ factor(xlabel, levels = c("Changes in law",
                                            "Interest on loans",
                                            "Pension costs",
                                            "Eurocontrol costs",
                                            "Competent authorities\nand qualified entities costs",
                                            "New and existing investments"
        )),
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
          xanchor = "left",
          x = 0, 
          y = mylegend_y_position,
          font = list(size = myfont)
        ),
        margin = mymargin
        
      )
  }
  
  ## plot chart  ----
  myplot[[ez]] <- mybarc_group(mywidth, myheight+30, myfont, mymargin)
  
}

# create html plotlist ----
htmltools::tagList(myplot)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document
