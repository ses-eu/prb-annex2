if (exists("country") == FALSE) {country <- "SES RP3"}

# source("R/parameters.R")

# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "SES file.xlsx"),
  sheet = "EoSM target #ANSPs",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# process data  ----
data_prep <- data_raw %>% 
  filter(year == year_report) %>% 
  select(
    - c(state)
  ) %>% 
  mutate(
    xlabel = case_when (
      management_objectives == "Safety culture" ~ "Culture",
      management_objectives == "Safety policy & objectives" ~ "Policy\n& objectives",
      management_objectives == "Safety risk management" ~ "Risk\nmanagement",
      management_objectives == "Safety assurance" ~ "Assurance",
      management_objectives == "Safety promotion" ~ "Promotion"
    ),
    xlabel = factor (xlabel, levels = c(
      "Culture",
      "Policy\n& objectives",
      "Risk\nmanagement",
      "Assurance",
      "Promotion")
    ),
  ) %>% 
  select(
    xlabel,
    type = level,
    mymetric = number_of_ans_ps
  ) 

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
  local_legend_y <- 1.35
  local_legend_x <- 0.5
  local_legend_xanchor <- 'center'
  local_legend_fontsize <- myfont
  
}

# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight,
                      colors = c('#585858',  '#FFC000', '#00B0F0', '#196AB4'),
                      local_factor = c("A",
                                       "B",
                                       'C', 
                                       'D',
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
                      
                      title_text = "",
                      title_y = 0.99,
                      
                      xaxis_tickfont_size = myfont ,
                      yaxis_title = "Number of ANSPs",
                      yaxis_ticksuffix = local_suffix,
                      yaxis_tickformat = "",
                      
                      trace_showlegend = FALSE,
                      
                      legend_y = local_legend_y, 
                      legend_x = local_legend_x,
                      legend_xanchor = local_legend_xanchor,
                      legend_fontsize = local_legend_fontsize) %>% 
  layout(
    showlegend = TRUE,
    yaxis = list(
      showticklabels = FALSE,
      range = c (0,30)
    )
  ) %>% 
  # to force full legend
  add_trace(
    data = data.frame(
      xlabel = "Promotion",
      type = "A",
      mymetric = 100
    ),
    x = ~xlabel, y = ~mymetric,
    type = "scatter",
    mode = "markers",
    name = "A",
    marker = list(color = '#585858',
                  symbol = "square",
                  size = 11),
    # showlegend = TRUE,
    hoverinfo = "skip",
    inherit = FALSE
  ) %>% 
  add_trace(
    data = data.frame(
      xlabel = "Promotion",
      type = "B",
      mymetric = 100
    ),
    x = ~xlabel, y = ~mymetric,
    type = "scatter",
    mode = "markers",
    name = "B",
    marker = list(color = '#FFC000',
                  symbol = "square",
                  size = 11),
    # showlegend = TRUE,
    hoverinfo = "skip",
    inherit = FALSE
  ) %>% 
  add_trace(
    data = data.frame(
      xlabel = "Promotion",
      type = "C",
      mymetric = 100
    ),
    x = ~xlabel, y = ~mymetric,
    type = "scatter",
    mode = "markers",
    name = "C",
    marker = list(color = '#00B0F0',
                  symbol = "square",
                  size = 11),
    # showlegend = TRUE,
    hoverinfo = "skip",
    inherit = FALSE
  ) %>% 
  add_trace(
    data = data.frame(
      xlabel = "Promotion",
      type = "D",
      mymetric = 100
    ),
    x = ~xlabel, y = ~mymetric,
    type = "scatter",
    mode = "markers",
    name = "D",
    marker = list(color = '#196AB4',
                  symbol = "square",
                  size = 11),
    # showlegend = TRUE,
    hoverinfo = "skip",
    inherit = FALSE
  )

# colors = c('#585858',  '#FFC000', '#00B0F0', '#196AB4'),



myplot




