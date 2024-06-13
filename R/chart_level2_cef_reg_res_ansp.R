
# fix ez if script not executed from qmd file ----
if (exists("ez") == FALSE) {ez = 1}
# ez=1

# initialise list to store plots ----
myplot = list()

# loop through czs ----
for (ez in 1:no_ecz) {
  
  data_reg_res <- regulatory_result(ez)
  
  ## select relevant values for chart
  data_for_chart_wide <- data_reg_res %>% 
    filter(type == 'Main ANSP') %>% 
    mutate(
      share_rr_act_rev_expost = regulatory_result/actual_revenues * 100,
      share_rr_act_rev_exante = ex_ante_roe/actual_revenues * 100
      ) %>% 
    select(-type, -x5_4_total_su, -actual_revenues)
  
  ## separate in two tables for pivoting 
  data_for_chart_value <- data_for_chart_wide %>% 
    select(-c(share_rr_act_rev_expost, share_rr_act_rev_exante)) %>% 
    pivot_longer(-year_text, names_to = "xlabel", values_to = 'mymetric') %>% 
    mutate(xlabel = case_when(
      xlabel == 'regulatory_result'  ~ 'Ex-post',
      xlabel == 'ex_ante_roe'  ~ 'Ex-ante'),
      mymetric = mymetric/1000
    )
  data_for_chart_share <- data_for_chart_wide %>% 
    select(-c(regulatory_result, ex_ante_roe)) %>% 
    pivot_longer(-year_text, names_to = "xlabel", values_to = 'share') %>% 
    mutate(xlabel = case_when(
      xlabel == 'share_rr_act_rev_expost'  ~ 'Ex-post',
      xlabel == 'share_rr_act_rev_exante'  ~ 'Ex-ante')
    )
  
  data_prep <- data_for_chart_value %>% 
    left_join(data_for_chart_share, by = c('year_text', 'xlabel')) %>% 
    mutate(type = xlabel,
      xlabel = year_text,
      year = as.numeric(str_replace_all(xlabel, "2020-2021", '2021')),
      xlabel = year)
  
  #

  # chart parameters ----
  mysuffix <- ""
  mydecimals <- 2
  
  ### trace parameters
  mycolors = c( '#CEE0EA', '#4B8DB1')
  ###set up order of traces
  myfactor <- c("Ex-ante",
                "Ex-post")
  
  mytextangle <- -90
  mytextposition <- "inside"
  myinsidetextanchor <- "middle"
  mytextfont_color <- 'transparent'
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
  mytitle_text <- paste0("Regulatory Result - ", main_ansp)
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
  myyaxis_title <- "Regulatory result (€M)"
  myyaxis_gridcolor <- 'rgb(240,240,240)'
  myyaxis_showgrid <- TRUE
  myyaxis_showline <- FALSE
  myyaxis_tickprefix <- ""
  myyaxis_ticksuffix <- ""
  myyaxis_tickformat <- ".0f"
  
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
  mylegend_y <- -0.28
  mylegend_font_size <- myfont
  
  #### margin
  mylocalmargin = list(t = 60, b = 0, l = 60, r = 40)
  
  
  # store plot chart in list ----
  myplot[[ez]] <- mybarchart(data_prep, mywidth, myheight + 30, myfont, mylocalmargin) %>% 
  
  add_trace(
      inherit = FALSE,
      data = filter(data_prep, type == "Ex-ante"),
      x = c(2020.8, 2021.8, 2022.8, 2023.8),
      y = ~share,
      # offset = c(1, 1,1,1),
      name = "RR in percent of en-route revenues",
      yaxis = "y2",
      mode = "markers",
      type = 'scatter',
      # color = ~ factor(type, levels = myfactor),
      marker = list(size = mylinewidth * 3, color = '#E46C0A'),
      showlegend = T
    )   %>% 
    layout(yaxis2 = list(
      title = 'Regulatory result as a % of revenues',
      overlaying = "y",
      ticksuffix = '%',
      side = 'right',
      showgrid = FALSE
    ))

    
  # myplot[[ez]]
}

# create html plotlist ----
htmltools::tagList(myplot)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document



