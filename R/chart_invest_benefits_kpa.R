if (exists("country") == FALSE) {country <- "Bulgaria"}

# source("R/parameters.R")

# import data  ----
if (!exists("data_cost_inv")) {
  source("R/get_investment_data.R")
}


# process data  ----
if (country == "SES RP3") {
  
  data_prep <- data_benefit_ses_forchart %>% 
    filter(union_wide_median == 'Union-wide ave') %>% 
    filter(variable %in% c('SAF', 'ENV', 'CAP', 'CEF')) %>% 
    mutate(
      type = "Union-wide average",
      xlabel = factor(variable, levels = c('SAF', 'ENV', 'CAP', 'CEF')),
      mymetric = percent * 100
    )
  
} else {
  
  data_prep_uw <- data_union_wide %>% 
    filter(variable == "SAF" | variable == "ENV" |
             variable == "CAP" | variable == "CEF") %>% 
    mutate(mymetric =percent *100) %>% 
    select(type = union_wide_median,
           xlabel = variable,
           mymetric) %>% 
    filter(type == "Union-wide median")
      
  data_prep_ansp <- data_impact %>% 
    filter(state == .env$country) %>% 
    filter(state != "SES RP3") %>% 
    mutate(
      type = "ANSP",
      SAF = if_else(nmajor_rp3 == 0, 0, saf_rp3/nmajor_rp3)*100,
      ENV = if_else(nmajor_rp3 == 0, 0, env_rp3/nmajor_rp3)*100,
      CAP = if_else(nmajor_rp3 == 0, 0, cap_rp3/nmajor_rp3)*100,
      CEF = if_else(nmajor_rp3 == 0, 0, cef_rp3/nmajor_rp3)*100
      ) %>% 
    select(type, SAF, ENV, CAP, CEF) %>% 
    pivot_longer(-c(type), names_to = "xlabel", values_to = "mymetric")
  
  data_prep <- rbind(data_prep_ansp, data_prep_uw) %>% 
    mutate(xlabel = factor(xlabel, levels = c("SAF", "ENV", "CAP", "CEF")))

}
  
# chart ----
## chart parameters ----
local_suffix <- "%"
local_decimals <- 0

###set up order of traces
local_hovertemplate <- paste0('%{y:,.', local_decimals, 'f}', local_suffix)

#### legend
if (knitr::is_latex_output()) {
  local_legend_y <- mylegend_y
  local_legend_x <- -0.18
  local_legend_xanchor <- 'left'
  local_legend_fontsize <- myfont-1
  
} else {
  local_legend_y <- -0.12
  local_legend_x <- 0.5
  local_legend_xanchor <- 'center'
  local_legend_fontsize <- myfont
  
}

mylocalfactor <- if (country == 'SES RP3') c("Union-wide average") else c("ANSP", "Union-wide median", NULL)
mylocalcolors <- if (country == 'SES RP3') c('#58595B')else c('#FFC000', '#58595B')

# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight,
                      colors = mylocalcolors,
                      local_factor = mylocalfactor,
                      shape = c( "", "", "", "", "/", "/", "/", "/"),
                      
                      suffix = local_suffix,
                      decimals = local_decimals,
                      
                      hovertemplate = local_hovertemplate,
                      hovermode = "x unified",
                      
                      textangle = 0,
                      textposition = "outside",
                      textfont_color = 'black',
                      insidetextanchor = 'middle',
                      
                      bargap = 0.25,
                      barmode = 'group',
                      
                      title_text = "Expected benefits of investments by KPA - RP3",
                      title_y = 0.99,
                      
                      yaxis_title = "% of RP3 actual costs of investments\nwith expected benefits per KPA",
                      yaxis_titlefont_size = myfont,
                      yaxis_ticksuffix = local_suffix,
                      yaxis_tickformat = ".0f",
                      yaxis_standoff = 10,
                      
                      legend_y = local_legend_y, 
                      legend_x = local_legend_x,
                      legend_xanchor = local_legend_xanchor,
                      legend_fontsize = local_legend_fontsize)

myplot




