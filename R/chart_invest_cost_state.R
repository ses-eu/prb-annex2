if (exists("country") == FALSE) {country <- "Bulgaria"}

# source("R/parameters.R")

# import data  ----
if (!exists("data_cost_inv")) {
  source("R/get_investment_data.R")
}


# process data  ----
data_prep_state <- data_cost_inv %>% 
  mutate(
    Determined = nm_d_total_rp3 + on_d_total_rp3 + e_d_total_rp3,
    Actual = nm_a_total_rp3 + on_a_total_rp3 + e_a_total_rp3,
  ) %>% 
  select(
    member_state, Determined, Actual
  ) %>% 
  pivot_longer(
    -member_state, names_to = "type", values_to = "value"
  )

data_prep_ses <- data_prep_state %>% filter(member_state == "SES RP3") %>% 
  select(type,
         value_ses = value)

data_prep1 <- data_prep_state %>% 
  left_join(data_prep_ses, by = "type") %>% 
  mutate(
    mymetric = value/value_ses * 100
  ) %>% 
  select(
    xlabel = member_state,
    type, 
    mymetric
  ) %>% 
  arrange(desc(mymetric)) %>% 
  filter(xlabel != "SES RP3") 

states_factor <- unique(data_prep1$xlabel)

data_prep <- data_prep1 %>% 
    mutate(xlabel = factor(xlabel, levels = states_factor ))

# chart ----
## chart parameters ----
local_suffix <- "%"
local_decimals <- 1

###set up order of traces
local_hovertemplate <- paste0('%{y:.', local_decimals, 'f}', local_suffix)

#### legend
if (knitr::is_latex_output()) {
  local_legend_y <- mylegend_y
  local_legend_x <- -0.18
  local_legend_xanchor <- 'left'
  local_legend_fontsize <- myfont-1
  
} else {
  local_legend_y <- 1
  local_legend_x <- 1
  local_legend_xanchor <- 'right'
  local_legend_fontsize <- myfont-2
  
}

# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight+40,
                      colors = c('#5B9BD5','#FFC000'),
                      local_factor = c("Determined",
                                       "Actual",
                                       NULL),
                      # shape = c("/", "", "/", "", "/", "", "/", "", "/", ""),
                      
                      suffix = local_suffix,
                      decimals = local_decimals,
                      
                      hovertemplate = local_hovertemplate,
                      hovermode = "x unified",
                      
                      textangle = 0,
                      textposition = "none",
                      textfont_color = 'black',
                      insidetextanchor = 'middle',
                      
                      bargap = 0.25,
                      barmode = 'group',
                      
                      title_text = "",
                      title_y = 0.99,
                      
                      xaxis_tickangle = -90,
                      
                      yaxis_title = "Share of costs of investments\nin total costs (%)",
                      yaxis_standoff = 5,
                      yaxis_ticksuffix = local_suffix,
                      yaxis_tickformat = ",.0f",
                      yaxis_tickfont_size = myfont,
                      
                      legend_orientation = "v",
                      legend_y = local_legend_y, 
                      legend_x = local_legend_x,
                      legend_xanchor = local_legend_xanchor,
                      legend_fontsize = local_legend_fontsize)

myplot




