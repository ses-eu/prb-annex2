
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
    summarise(reg_res = sum(mymetric)) %>% 
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
    pivot_longer(-year_text, names_to = 'xlabel', values_to = 'mymetric') %>% 
    mutate(xlabel = case_when(
      xlabel == 'reg_res_per_su'  ~ 'Regulatory result per SU',
      xlabel == 'aucu_excluding_or'  ~ 'AUCU (before other revenues)',
    ))
    

  ## chart parameters ----
  mychart_title <- paste0("AUCU and Regulatory result")
  myaxis_title <- "AUCU and Regulatory result (€/SU)"
  mybarcolor <- c( '#9DC3E6', '#FFC000')
  mytextcolor <- 'black'
  
  mylegend_fontsize <- myfont*0.9
  mylegend_y_position <- -0.15
  mylegend_x_position <- 0.5
  mylegend_x_anchor <- 'center'
  myfactor <- c("AUCU (before other revenues)",
    "Regulatory result per SU")
  
  mytooltip_decimals <- 2
  
  ## define chart function ----
  ### function moved to utils
  
  ## plot chart  ----
  myplot[[ez]] <- mybarc_group(mywidth, myheight+30, myfont, mymargin)
  
}

# create html plotlist ----
htmltools::tagList(myplot)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document
