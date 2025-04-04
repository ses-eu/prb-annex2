if (exists("country") == FALSE) {country <- "Bulgaria"}

source("R/parameters.R")

# import data  ----
if (!exists("data_cost_inv")) {
  source("R/get_investment_data.R")
}


# process data  ----
data_prep_uw <- data_union_wide %>% 
  filter(variable == "SES mandated" | variable == "Partnership" |
           variable == "CP/MP investment") %>% 
  mutate(mymetric =percent *100) %>% 
  select(type = union_wide_median,
         xlabel = variable,
         mymetric)
    
data_prep_ansp <- data_impact %>% 
  filter(state == .env$country) %>% 
  mutate(
    type = "ANSP",
    "SES mandated" = if_else(nmajor_rp3 == 0, 0, nw_rp3/nmajor_rp3)*100,
    "Partnership" = if_else(nmajor_rp3 == 0, 0, local_rp3/nmajor_rp3)*100,
    "CP/MP investment" = if_else(nmajor_rp3 == 0, 0, np_rp3/nmajor_rp3)*100
    ) %>% 
  select(type, "SES mandated", "Partnership", "CP/MP investment") %>% 
  pivot_longer(-c(type), names_to = "xlabel", values_to = "mymetric")

data_prep <- rbind(data_prep_uw, data_prep_ansp) %>% 
  mutate(xlabel = factor(xlabel, levels = c("SES mandated", "Partnership", "CP/MP investment")))

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

# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight,
                      colors = c('#58595B', '#FFC000'),
                      local_factor = c("Union-wide median",
                                       "ANSP",
                                        NULL),
                      shape = c("/", "/", "/", "", "", ""),
                      
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
                      
                      title_text = "Costs by type of investments - impact",
                      title_y = 0.99,
                      
                      yaxis_title = "% of RP3 actual costs of new\ninvestments by type of investment",
                      yaxis_titlefont_size = myfont,
                      yaxis_ticksuffix = local_suffix,
                      yaxis_tickformat = ".0f",
                      yaxis_standoff = 10,
                      
                      legend_y = local_legend_y, 
                      legend_x = local_legend_x,
                      legend_xanchor = local_legend_xanchor,
                      legend_fontsize = local_legend_fontsize)

myplot




