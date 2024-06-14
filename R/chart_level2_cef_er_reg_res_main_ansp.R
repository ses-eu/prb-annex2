
# fix ez if script not executed from qmd file ----
if (exists("ez") == FALSE) {ez = 1}
# ez=1

# initialise list to store plots ----
myplot = list()

# loop through czs ----
for (ez in 1:no_ecz) {
  ## import data  ----
  data_raw  <-  regulatory_result(ez)
  
  ## prepare data ----
  data_prep <- data_raw %>% 
    filter(type == "Main ANSP",
           year_text == if_else(year_report == 2021 | year_report == 2020, "2020-2021", as.character(year_report))
           )  %>% 
    select(year_text, atsp_gain_loss_cost_sharing, trs, financial_incentive, ex_post_roe) %>% 
    pivot_longer(-year_text, names_to = "status", values_to = "mymetric") %>% 
    mutate (ylabel = case_when (
      status == 'atsp_gain_loss_cost_sharing' ~ "Cost sharing", 
      status == 'trs' ~ "Traffic risk sharing",
      status == 'financial_incentive' ~ "Incentives",
      status == 'ex_post_roe' ~ "Actual RoE in value"),
      mylabel = format(round(mymetric,0), big.mark = ",", nsmall =0)
    )
    
  ## chart parameters ----
  mychart_title <- paste0(main_ansp,", TBD - ", year_report)
  myaxis_title <- ""
  mybarcolor_pos <- '#9ECF8D'
  mybarcolor_neg <- '#F87474'
  mytextcolor <- 'black'
  myhovertemplate <- paste0('%{x:,.0f}<extra></extra>')
  myxaxis_tickformat <- "0,"
  
  ###set up order of traces
  myfactor <- c("Actual RoE in value",
                "Incentives",
                "Traffic risk sharing",
                "Cost sharing")
  
  mylocalmargin <- list (t = 40, b = 60)
  
  ## define chart function ----
  ### function moved to utils

  ## plot chart  ----
  
  ### calculate x range, and annotation and image position
  myroundup <- max(floor((log10(abs(max(data_prep$mymetric))))), floor((log10(abs(min(data_prep$mymetric))))))
  range_min <- floor(min(data_prep$mymetric)/10^myroundup) * 10^myroundup
  range_max <- ceiling(max(data_prep$mymetric)/10^myroundup) * 10^myroundup
  
  annotation_x <- (range_max - range_min) /20
  
  ### plot chart and add annotations
  myplot[[ez]] <- myhbarc(mywidth, myheight+30, myfont, mylocalmargin) %>% 
    layout(
      xaxis = list(
        range = c(range_min, range_max)
        ),
      images = list(
        list(
          # Add images
          source =  base64enc::dataURI(file = here("images","arrow_right.png")),  
          xref = "paper",  
          yref = "paper",  
          x = if_else(range_min >=0,0, abs(range_min)/(range_max-range_min)),  
          y = -0.12,  
          sizex = 0.18,  
          sizey = 0.18,  
          layer = "above",
          xanchor="left",  
          yanchor="bottom" 
        ),
        list(
          # Add images
          source =  base64enc::dataURI(file = here("images","arrow_left.png")),  
          xref = "paper",  
          yref = "paper",  
          x = if_else(range_min >=0,0, abs(range_min)/(range_max-range_min)),  
          y = -0.12,  
          sizex = 0.18,  
          sizey = 0.18,  
          layer = "above",
          xanchor="right",  
          yanchor="bottom" 
        )
        )
      ,
      annotations = list(
        list(
        xanchor = "left",
        x = annotation_x,
        y = -0.2,
        text = '<b>ANSP gain</b>',
        font = list(size = myfont*0.85),
        xref = "x",
        yref = "paper",
        showarrow = FALSE,
        # arrowhead = 7,
        ax = 0,
        ay = 0
      ),
      list(
        xanchor = "right",
        x = -annotation_x,
        y = -0.2,
        text = '<b>ANSP loss</b>',
        font = list(size = myfont*0.85),
        xref = "x",
        yref = "paper",
        showarrow = FALSE,
        # arrowhead = 7,
        ax = 0,
        ay = 0
      )
      )
    )
  
  myplot[[ez]]
  
}

# create html plotlist ----
htmltools::tagList(myplot)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document
  