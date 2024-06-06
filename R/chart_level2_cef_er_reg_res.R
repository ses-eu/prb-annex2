
# fix ez if script not executed from qmd file ----
if (exists("ez") == FALSE) {ez = 1}
# ez=1

# initialise list to store plots ----
myplot = list()

# loop through czs ----
for (ez in 1:no_ecz) {

  ## get data ----
  data_prep <- regulatory_result(ez)
  data_prep <- data_prep %>% 
    mutate(mymetric = mymetric / 1000)
    
  ## chart parameters ----
  mychart_title <- paste0("Regulatory result at CZ level")
  myaxis_title <- "Regulatory result (€M)"
  mybarcolor <- c( '#5B9BD5', '#FFC000', '#BFBFBF')
  mytextcolor <- 'black'
  
  mylegend_fontsize <- myfont
  mylegend_y_position <- -0.15
  mylegend_x_position <- 0.5
  mylegend_x_anchor <- 'center'
  myfactor <- c("Main ANSP",
    "Other ANSP",
    "MET")
  
  mytooltip_decimals <- 2
  
  ## define chart function ----
  ### function moved to utils
  
  ## plot chart  ----
  myplot[[ez]] <- mybarc_group(mywidth, myheight+30, myfont, mymargin)
  
}

# create html plotlist ----
htmltools::tagList(myplot)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document
