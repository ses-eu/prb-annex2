if (exists("country") == FALSE) {country <- "Bulgaria"}

# source("R/parameters.R")

# import data  ----
if (!exists("data_cost_inv")) {
  source("R/get_investment_data.R")
}


# process data  ----
data_prep1 <- data_capex %>% 
  filter(member_state != "SES RP3") %>% 
  select(
    member_state,
    new_major_investments_as_per_pp,
    other_new_investments_as_per_pp,
    additional_new_major_investments
  ) %>% 
  pivot_longer(-member_state, names_to = "type", values_to = "value") %>% 
  mutate(
    mymetric = value/10^6,
    type = case_when(
      type == "new_major_investments_as_per_pp" ~ "New major investments",
      type == "other_new_investments_as_per_pp" ~ "Other new investments",
      type == "additional_new_major_investments" ~ "Additional new major investments"
    )
  ) %>% 
  select(
    xlabel = member_state,
    type,
    mymetric
  ) 

data_prep_total <- data_prep1 %>% 
  group_by(xlabel) %>% 
  summarise(mymetric = sum(mymetric, na.rm = TRUE)) %>% 
  arrange(desc(mymetric)) %>% select(xlabel) %>% pull()

data_prep <- data_prep1 %>% 
  mutate(xlabel = factor(xlabel, levels = data_prep_total))
  

# chart ----
## chart parameters ----
local_suffix <- ""
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
  local_legend_y <- 1.1
  local_legend_x <- 1
  local_legend_xanchor <- 'right'
  local_legend_fontsize <- myfont
  
}

# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight+40,
                      colors = c('#5B9BD5','#044598', '#FFC000'),
                      local_factor = c("New major investments",
                                       "Additional new major investments",
                                       "Other new investments",
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
                      barmode = 'stack',
                      
                      title_text = "",
                      title_y = 0.99,
                      
                      xaxis_tickangle = -90,
                      
                      yaxis_title = "CAPEX per Member State (M€<sub>2017</sub>)",
                      yaxis_ticksuffix = local_suffix,
                      yaxis_tickformat = ",.0f",
                      
                      legend_y = local_legend_y, 
                      legend_x = local_legend_x,
                      legend_xanchor = local_legend_xanchor,
                      legend_fontsize = local_legend_fontsize)

myplot




