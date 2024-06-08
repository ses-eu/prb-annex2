
# fix ez if script not executed from qmd file ----
if (exists("ez") == FALSE) {ez = 1}
# ez=1

# initialise list to store plots ----
myplot = list()

# loop through czs ----
for (ez in 1:no_ecz) {

  ## get data ----
  # regulatory result
  data_prep_reg_all <- regulatory_result(ez)
  data_prep_reg <- data_prep_reg_all %>% 
    group_by(year_text, x5_4_total_su) %>% 
    summarise(reg_res = sum(regulatory_result)) %>% 
    mutate(reg_res_per_su = reg_res * 1000 / x5_4_total_su) %>% 
    select(year_text, reg_res, reg_res_per_su)
  
  # aucu
  data_prep_aucu_all <- aucu(ez) 
  data_prep_aucu <- data_prep_aucu_all %>% 
    select(year_text, aucu_excluding_or)
  
  # join tables
  data_prep <- data_prep_reg %>% 
    left_join(data_prep_aucu, by = 'year_text') %>% 
    select(-reg_res) %>% 
    mutate(rr_as_perc_aucu = reg_res_per_su / aucu_excluding_or * 100) %>% 
    pivot_longer(-c(year_text, rr_as_perc_aucu), names_to = 'type', values_to = 'mymetric') %>% 
    mutate(type = case_when(
      type == 'reg_res_per_su'  ~ 'Regulatory result per SU',
      type == 'aucu_excluding_or'  ~ 'AUCU (before other revenues)'
      ),
      xlabel = year_text
    
    )
    
  ## chart parameters ----
  mysuffix <- ""
  mydecimals <- 2
  
  ### trace parameters
  mycolors = c( '#9DC3E6', '#FFC000')
  ###set up order of traces
  myfactor <- c("AUCU (before other revenues)",
                "Regulatory result per SU")
  
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
  mytitle_text <- paste0("AUCU and Regulatory result")
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
  myxaxis_dtick <- 1
  myxaxis_zeroline <- TRUE
  myxaxis_tickfont_size <- myfont
  
  #### yaxis
  myyaxis_title <- "AUCU and Regulatory result (€/SU)"
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
  mylocalmargin = list(t = 60, b = 0, l = 60, r = 40)
  
  ## define chart function ----
  ### function moved to utils
  
  ## plot chart  ----
  myplot[[ez]] <- mybarchart(data_prep, mywidth, myheight+30, myfont, mylocalmargin) %>% 
    add_trace(
      data = filter(data_prep, type == 'Regulatory result per SU'),
      x = ~ xlabel,
      y = ~ round(rr_as_perc_aucu, 2),
      name = "Share of RR in AUCU",
      yaxis = "y2",
      mode = "markers",
      type = 'scatter',
      marker = list(size = mylinewidth * 3, color = '#E46C0A'),
      showlegend = T
    ) %>% 
    add_empty_trace(., data_prep) %>% 
    layout(yaxis2 = list(
      title = 'Regulatory result as a % of AUCU',
      overlaying = "y",
      ticksuffix = '%',
      side = 'right',
      showgrid = FALSE
    ))
  
}

# create html plotlist ----
htmltools::tagList(myplot)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document
