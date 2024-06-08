
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
    mutate(mymetric = regulatory_result / 1000) %>% 
    rename(xlabel = year_text)
    
  ## chart parameters ----
  mysuffix <- ""
  mydecimals <- 2
  
  ### trace parameters
  mycolors = c( '#5B9BD5', '#FFC000', '#BFBFBF')
  ###set up order of traces
  myfactor <- c("Main ANSP",
                "Other ANSP",
                "MET")
  
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
  mybarmode <- 'stack'
  myhovermode <- "x unified"
  myhoverlabel_bgcolor <- 'rgba(255,255,255,0.88)'
  myminsize <- myfont*0.8
  
  #### title
  mytitle_text <- paste0("Regulatory result at CZ level")
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
  mylegend_y <- -0.1
  mylegend_font_size <- myfont
  
  #### margin
  mylocalmargin = mymargin
  
  ## define chart function ----
  ### function moved to utils
  
  ## plot chart  ----
  myplot[[ez]] <- mybarchart(data_prep, mywidth, myheight+10, myfont, mylocalmargin) %>% 
    add_empty_trace(., data_prep)  
}

# create html plotlist ----
htmltools::tagList(myplot)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document
