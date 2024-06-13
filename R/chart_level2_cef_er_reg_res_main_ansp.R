
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
    select(year_text, atsp_gain_loss_cost_sharing, trs, financial_incentive, regulatory_result) %>% 
    pivot_longer(-year_text, names_to = "status", values_to = "mymetric") %>% 
    mutate (ylabel = case_when (
      status == 'atsp_gain_loss_cost_sharing' ~ "Cost sharing", 
      status == 'trs' ~ "Traffic risk sharing",
      status == 'financial_incentive' ~ "Incentives",
      status == 'regulatory_result' ~ "Actual RoE in value"),
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
  
  
  ## define chart function ----
  ### function moved to utils

  ## plot chart  ----
  
  ### calculate annotation position
  annotation_x <- (max(data_prep$mymetric) - min(data_prep$mymetric)) /10

  ### plot chart and add annotations
  myplot[[ez]] <- myhbarc(mywidth, myheight+30, myfont, mymargin) %>% 
    layout(
      annotations = list(
        list(
        xanchor = "left",
        x = annotation_x,
        y = -0.18,
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
        y = -0.18,
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
  
  ### add arrows
  image_folder <- here("images")
  library(webshot)
  
  myimages <- list(
    list(
      source = base64enc::dataURI(file = paste0(image_folder,"/arrow_right.png")),
      x = 0, 
      y = 0,
      # sizex = 0.18, 
      # sizey = 0.18,
      xref = "x", 
      yref = "paper",
      xanchor = "left", 
      yanchor = "center"
    ),
    list(
      source = base64enc::dataURI(file = paste0(image_folder,"/arrow_left.png")),
      # source =raster2uri(as.raster(arrow_right)), #https://plotly-r.com/embedding-images.html
      x = 0,
      y = 0,
      sizex = 0.18, sizey = 0.18,
      xref = "x", 
      yref = "paper",
      xanchor = "right", 
      yanchor = "center"
    )
  )
      
  ### update plot with images
  myplot[[ez]] <- myplot[[ez]] %>% 
    layout(
      annotations = myimages
    )
  
}

# create html plotlist ----
htmltools::tagList(myplot)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document
  