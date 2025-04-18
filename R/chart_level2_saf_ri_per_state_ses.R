if (!exists("doclevel")) {doclevel = "level1"}

# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "SAF EoSM.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "RI - occurrences",
  range = cell_limits(c(1, 1), c(NA, 6))) %>%
  as_tibble() %>% 
  clean_names() 



# prepare data ----
data_prep <- data_raw %>% 
  filter(
    type == "RI",
    year == year_report
  ) %>% 
  mutate(
    rate_per_100_000 = if_else(is.na(rate_per_100_000), 0, rate_per_100_000),
    type = "Rate of RI with safety impact by State"
    ) %>% 
  arrange (desc(rate_per_100_000)) %>% 
  mutate(xlabel = factor(state, levels = state)) %>% 
  select(
    -year, - reference_period,
    xlabel,
    mymetric = rate_per_100_000,
    myothermetric = eu_wide_average
  ) 

eu_average <- data_prep$myothermetric[1]


# plot chart ----
# chart ----
## chart parameters ----
local_suffix <- " "
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
  local_legend_y <- 1.35
  local_legend_x <- 0.5
  local_legend_xanchor <- 'center'
  local_legend_fontsize <- myfont
  
}

# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight+60,
                      colors = c('#FFC000'),
                      local_factor = c("Rate of RI with safety impact by State",
                                       NULL),
                      # shape = c("/", "", "/", "", "/", "", "/", "", "/", ""),
                      
                      suffix = local_suffix,
                      decimals = local_decimals,
                      
                      hovertemplate = local_hovertemplate,
                      hovermode = "x unified",
                      
                      textangle = 0,
                      textposition = "outside",
                      textfont_color = 'black',
                      textfont_size = myfont -1,
                      insidetextanchor = 'middle',
                      
                      bargap = 0.4,
                      barmode = 'group',
                      
                      title_text = "",
                      title_y = 0.99,
                      
                      xaxis_tickangle =  -90,
                      
                      yaxis_title = "Rate of RIs per 100,000 airport movements",
                      yaxis_titlefont_size = myfont-1,
                      yaxis_ticksuffix = local_suffix,
                      yaxis_tickformat = ".0f",

                      legend_y = local_legend_y, 
                      legend_x = local_legend_x,
                      legend_xanchor = local_legend_xanchor,
                      legend_fontsize = local_legend_fontsize) %>%
  add_trace(
    inherit = FALSE,
    data = data_prep,
    x = ~ xlabel,
    y = ~ myothermetric,
    yaxis = "y1",
    type = 'scatter',
    mode = "line",
    name = paste0("Union-wide average: ", eu_average),
    line = list (color = '#FFC000', width = 2, dash = 'dot'),
    hoverinfo = 'none',
    opacity = 0.6,
    showlegend = T
  ) 



myplot 




