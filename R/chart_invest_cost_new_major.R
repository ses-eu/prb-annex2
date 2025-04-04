if (exists("country") == FALSE) {country <- "Bulgaria"}

source("R/parameters.R")

# import data  ----
if (!exists("data_cost_inv")) {
  source("R/get_investment_data.R")
}


# process data  ----
data_prep <- data_new_major_detail %>% 
  filter(member_state == .env$country) %>% 
  select(investment_name,
    determined = total_rp3_18,
    actual = total_rp3_24
  ) %>% 
  pivot_longer(
    cols = -c(investment_name),  # Pivot all columns
    names_to = c("type"),  # Create "type" and "year" columns
    values_to = "value"  # Store values in "value" column
  ) %>% 
  mutate(
    xlabel = investment_name,
    type = if_else(type == "determined", "Determined\ncosts", "Actual\ncosts"),
    type = factor(type, levels = c("Determined\ncosts", "Actual\ncosts")),
    mymetric = round(value/10^6, 2)
  ) %>%
  select(
    xlabel,
    type,
    mymetric)   

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
  local_legend_y <- 0.5
  local_legend_x <- 1.1
  local_legend_xanchor <- 'center'
  local_legend_fontsize <- myfont-1
  
}

# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight+70,
                      colors = c('#5B9BD5', '#FFC000'),
                      local_factor = c("Determined\ncosts",
                                       "Actual\ncosts",
                                       NULL),
                      # shape = c("/", "", "/", "", "/", "", "/", "", "/", ""),
                      
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
                      
                      title_text = "Total costs of major investments - RP3",
                      title_y = 0.99,
                      
                      textfont_size = myfont-2,
                      xaxis_tickfont_size = myfont -1,
                      
                      yaxis_title = "Total costs of investments in RP3 (M€<sub>2017</sub>)",
                      yaxis_ticksuffix = local_suffix,
                      yaxis_tickformat = ".0f",
                      yaxis_titlefont_size = myyaxis_titlefont_size -1,
                      
                      legend_y = local_legend_y, 
                      legend_x = local_legend_x,
                      legend_xanchor = local_legend_xanchor,
                      legend_fontsize = local_legend_fontsize,
                      legend_orientation = "v",
                      
                      margin = list(t = 40, r = 80))

myplot




