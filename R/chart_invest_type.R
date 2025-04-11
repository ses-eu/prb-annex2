if (exists("country") == FALSE) {country <- "Bulgaria"}

# source("R/parameters.R")

# import data  ----
if (!exists("data_cost_inv")) {
  source("R/get_investment_data.R")
}


# process data  ----
data_prep <- data_impact %>% 
  filter(state == .env$country) %>%
  select(
    ses_rp3,
    par_rp3,
    cpmp_rp3,
    sesdet_rp3,
    pardet_rp3,
    cpmpdet_rp3
  ) %>% 
  pivot_longer(everything(), names_to = "xlabel", values_to = "mymetric") %>% 
  mutate(
    type = if_else(str_detect(xlabel, "det"), "Determined", "Actual"),
    xlabel = str_replace_all(xlabel, "det", ""),
    xlabel = case_when(
      xlabel == "ses_rp3" ~ "SES mandated",
      xlabel == "par_rp3" ~ "Partnership",
      xlabel == "cpmp_rp3" ~ "CP/MP investments"
    ),
    xlabel = factor(xlabel, levels = c("SES mandated", "Partnership", "CP/MP investments"))
  )


# chart ----
## chart parameters ----
local_suffix <- ""
local_decimals <- 1

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
                      colors = c('#22A0E7', '#FFC000'),
                      local_factor = c("Determined",
                                       "Actual",
                                        NULL),
                      # shape = c("/", "/", "/", "/", "", "", "", ""),
                      
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
                      
                      title_text = "Costs by type of investments - actual and determined",
                      title_y = 0.99,
                      
                      yaxis_title = "RP3 determined versus actual\ncost of investments (M€<sub>2017</sub>)",
                      yaxis_titlefont_size = myfont,
                      yaxis_ticksuffix = local_suffix,
                      yaxis_tickformat = ".0f",
                      yaxis_standoff = 10,
                      
                      legend_y = local_legend_y, 
                      legend_x = local_legend_x,
                      legend_xanchor = local_legend_xanchor,
                      legend_fontsize = local_legend_fontsize)

myplot




