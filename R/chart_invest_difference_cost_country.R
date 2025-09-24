if (!exists("country")) {country <- "Bulgaria"}
if (!exists("cost_type")) {cost_type <- "terminal"}

# source("R/parameters.R")

# import data  ----
if (!exists("data_cost_inv")) {
  source("R/get_investment_data.R")
}

# process data  ----
if (cost_type == "en route") {
  data_filtered <- data_cost_ses %>% 
    select(
      member_state = state,
      dif = difference_a_d_value_en_route,
      dif_perc = difference_a_d_percent_en_route
    ) %>% 
    filter(member_state != "Luxembourg")
} else {
  data_filtered <- data_cost_ses %>% 
    select(
      member_state = state,
      dif = difference_a_d_value_terminal,
      dif_perc = difference_a_d_percent_terminal
    ) %>% 
    filter(!(is.na(dif)))
}

data_calc <- data_filtered %>% 
  mutate(
    dif = dif / 10^6,
    dif_perc = dif_perc  * 100
  ) %>% 
  select(
    member_state,
    mymetric = dif,
    myothermetric = dif_perc
  ) %>% 
  arrange(desc(mymetric)) %>% 
  mutate(xlabel = factor(member_state, levels = member_state))

data_prep <- data_calc %>% select (xlabel, mymetric) %>%
  mutate(type = "Difference (magnigude)")

data_prep2 <- data_calc %>% select (xlabel, myothermetric) %>%
  mutate(type = "Difference %")

# chart ----
## chart parameters ----
local_suffix <- ""
local_decimals <- 1

local_hovertemplate <- "%{y}"

#### legend
if (knitr::is_latex_output()) {
  local_legend_y <- mylegend_y
  local_legend_x <- -0.18
  local_legend_xanchor <- 'left'
  local_legend_fontsize <- myfont-1
  
} else {
  local_legend_y <- 1.5
  local_legend_x <- 0.5
  local_legend_xanchor <- 'center'
  local_legend_fontsize <- myfont
  
}

mylocalcolors <- c('#044598')

# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight+20,
                      colors = mylocalcolors,
                      local_factor = c("Difference (magnigude)"),

                      suffix = local_suffix,
                      decimals = local_decimals,
                      
                      # text = ~textlabel,
                      hovertemplate = paste0('%{y:,.', local_decimals, 'f}', local_suffix),
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
                      
                      yaxis_title = paste0("Difference between ", 
                                           cost_type,
                                           " actual and\ndetermined investment costs (M€<sub>2017</sub>)"),
                      yaxis_titlefont_size = myfont-1,
                      yaxis_ticksuffix = local_suffix,
                      yaxis_tickformat = ".0f",
                      yaxis_tickfont_size = myfont-1,
                      
                      legend_y = local_legend_y, 
                      legend_x = local_legend_x,
                      legend_xanchor = local_legend_xanchor,
                      legend_fontsize = local_legend_fontsize,
                      # trace_showlegend = FALSE
                      margin = list(t = 0, r= 50)
                      
                      ) %>% 
  layout(
      yaxis = list(
        zeroline = TRUE,
        zerolinecolor = "#808080",   # darker line at 0
        zerolinewidth = 1
    )
  ) %>% 
  add_trace(
    data = data_prep2,
    x = ~xlabel,
    y = ~myothermetric,
    name = "Difference (%)",
    mode = "markers",
    type = "scatter",
    yaxis = "y2",
    marker = list(
      size = 5,
      color = '#FFC000'
    ),
    inherit = FALSE
  ) %>% 
  layout(
    yaxis2 = list(title = list(text = "Difference (%)", standoff = -50),
                 showgrid = FALSE,
                 ticksuffix = "%", 
                 tickformat = ".0f",
                 zeroline = FALSE,
                 titlefont = list(size = myfont-1), 
                 tickfont = list(size = myfont-1),
                 overlaying = "y",
                 side = "right"
    )
  )


myplot
