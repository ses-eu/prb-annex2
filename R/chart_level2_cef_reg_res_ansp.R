
# fix ez if script not executed from qmd file ----
if (exists("ez") == FALSE) {ez = 1}
# ez=1

# initialise list to store plots ----
myplot = list()

# loop through czs ----
for (ez in 1:no_ecz) {
  
  data_prep <- regulatory_result(ez)
  
  ## select relevant values for chart
  data_for_chart_wide <- data_prep %>% 
    filter(xlabel == 'Main ANSP') %>% 
    mutate(
      share_rr_act_rev_expost = regulatory_result/actual_revenues * 100,
      share_rr_act_rev_exante = ex_ante_roe/actual_revenues * 100
      ) %>% 
    select(-xlabel, -x5_4_total_su, -actual_revenues)
  
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
    left_join(data_for_chart_share, by = c('year_text', 'xlabel'))
  
  ## chart parameters ----
  mylocalmargin = list(t = 60, b = 0, l = 60, r = 40)
  mychart_title <- paste0("Regulatory Result - ", main_ansp)
  myaxis_title <- "Regulatory result (€M)"

  mybarcolor <- c( '#CEE0EA', '#4B8DB1')
  mybarmode <- 'group'
  
  mytextcolor <- 'black'
  
  mylegend_fontsize <- myfont*0.9
  mylegend_x_anchor <- 'center'
  mylegend_x_position <- 0.5
  mylegend_y_position <- -0.28

  mytickformat_y <- ",.0f"
  mytooltipformat <- ",.2f"

  myfactor <- c("Ex-ante",
                "Ex-post")
  
  mytooltip_decimals <- 2  
  
  ## define chart function ----

  
  ## plot chart  ----
  myplot[[ez]] <- mybarc_group(mywidth, myheight+30, myfont, mylocalmargin) %>% 
    add_trace(
      # data = data_prep,
      x = list(1,2,3,4,5,6,7,8),
      y = c(1,2,3,4,5,6,7,8),
      name = "RR in percent of en-route revenues",
      yaxis = "y2",
      mode = "markers",
      type = 'scatter',
      # color = ~ factor(xlabel, levels = myfactor),
      marker = list(size = mylinewidth * 3, color = '#E46C0A'),
      showlegend = T
    ) %>% 
    add_trace(
      data = data_prep,
      x = ~year_text,
      y = ~ '',
      name = "Fake series to force all years in x axis",
      yaxis = "y1",
      mode = "markers",
      type = 'scatter',
      marker = list(size = mylinewidth, color = 'transparent'),
      showlegend = F,
      # hovertemplate = '',
      hoverinfo = 'none'
    ) 
    
  myplot[[ez]]
}

# create html plotlist ----
htmltools::tagList(myplot)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document



