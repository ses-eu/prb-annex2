if (exists("country") == FALSE) {country <- "Bulgaria"}

# source("R/parameters.R")

# import data  ----
data_raw_maturity  <-  read_xlsx(
  paste0(data_folder, "SAF EoSM.xlsx"),
  sheet = "A>P",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# process data  ----
data_prep <- data_raw_maturity %>% 
  select(
    - c(ms, entity, reference_period, ansp_meeting_targets_yearly)
  ) %>% 
  pivot_longer(-c(entity_name, year), names_to = "type", values_to = "score") %>%  
  separate_wider_delim(type, delim = "_", names = c("status", "type"),
                       too_many = "merge") %>% 
  mutate(type = str_replace_all(type, "_", " "),
         type = str_to_sentence(type)
  ) %>% 
  filter(status == "actual",
         year == year_report) %>% 
  group_by(type, score) %>% 
  summarise(ansp_count = n(), .groups = "drop") %>%
  mutate(
    score_text = case_when (
      score == 80 ~ 'D',
      score == 60 ~ 'C',
      score == 40 ~ 'B',
      score == 20 ~ 'A',
      .default = as.character(score)
    ),
    type = factor (type, levels = c("Culture",
                                    "Policy and objectives",
                                    "Risk management",
                                    "Assurance",
                                    "Promotion")
                   )
  ) %>% 
  select(
    xlabel = type,
    type = score_text,
    mymetric = ansp_count
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
      range = c (0,20)
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




