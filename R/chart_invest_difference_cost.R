if (!exists("country")) {country <- "France"}
if (!exists("cost_type")) {cost_type <- "enroute"}

# source("R/parameters.R")

# import data  ----
if (!exists("data_cost_inv")) {
  source("R/get_investment_data.R")
}

# process data  ----
if (cost_type == "en route") {
  data_filtered <- data_cost_inv_rt %>% 
    filter(member_state == .env$country) %>% 
    select(member_state,
           d_2020 = d_enr_2020,
           d_2021 = d_enr_2021,
           d_2022 = d_enr_2022,
           d_2023 = d_enr_2023,
           d_2024 = d_enr_2024,

           a_2020 = a_enr_2020,
           a_2021 = a_enr_2021,
           a_2022 = a_enr_2022,
           a_2023 = a_enr_2023,
           a_2024 = a_enr_2024
    )
} else {
  data_filtered <- data_cost_inv_rt %>% 
    filter(member_state == .env$country) %>% 
    select(member_state,
           d_2020 = d_ter_2020,
           d_2021 = d_ter_2021,
           d_2022 = d_ter_2022,
           d_2023 = d_ter_2023,
           d_2024 = d_ter_2024,
           
           a_2020 = a_ter_2020,
           a_2021 = a_ter_2021,
           a_2022 = a_ter_2022,
           a_2023 = a_ter_2023,
           a_2024 = a_ter_2024
    )
}

data_prep_year <- data_filtered %>% 
  group_by(member_state) %>% 
  summarise(
    d_2020 = sum(d_2020, na.rm = TRUE),
    d_2021 = sum(d_2021, na.rm = TRUE),
    d_2022 = sum(d_2022, na.rm = TRUE),
    d_2023 = sum(d_2023, na.rm = TRUE),
    d_2024 = sum(d_2024, na.rm = TRUE),

    a_2020 = sum(a_2020, na.rm = TRUE),
    a_2021 = sum(a_2021, na.rm = TRUE),
    a_2022 = sum(a_2022, na.rm = TRUE),
    a_2023 = sum(a_2023, na.rm = TRUE),
    a_2024 = sum(a_2024, na.rm = TRUE)
  )%>% 
  select(-member_state) %>% 
  pivot_longer(
    cols = everything(),  # Pivot all columns
    names_to = c("type", "year"),  # Create "type" and "year" columns
    names_pattern = "(.+?)_(\\d{4})",  # Regex: Extract "type" + 4-digit year
    values_to = "value"  # Store values in "value" column
  ) %>% 
  mutate(value = if_else(year>year_report, NA, value))

data_prep_total <- data_prep_year %>% 
  group_by(type) %>% 
  summarise(value = sum(if_else(as.numeric(year) > year_report, 0, value), na.rm = TRUE), .groups = "drop") %>% 
  ungroup() %>% 
  mutate(year = "RP3") %>% select(type, year, value)

data_prep <- rbind(data_prep_year, data_prep_total) %>%
  pivot_wider( names_from = "type", values_from = "value" ) %>% 
  mutate(
    value = if_else(a == 0, 0, a/d-1),
    cost_difference = (a-d)/1000
  ) %>% 
  select(year, value, cost_difference) %>% 
  mutate(
    split_flag = value > 0.05
    ) %>% 
  mutate(weights = if_else(is.na(split_flag), 1L, if_else(split_flag, 2L, 1L))) %>%
  uncount(weights) %>%
  group_by(year) %>%
  mutate(value = if (n() == 2) c(0.05, first(value) - 0.05) else first(value)) %>% 
  mutate(type = case_when(
           row_number() == 1 & value >0 ~ "Overspending < 5%",
           row_number() > 1 & value > 0 ~ "Overspending > 5%",
           value < 0 ~ "Underspending"
         )
  ) %>%
  ungroup() %>%
  select(-split_flag) %>% 
  mutate(
    mymetric = round(value*100,1),
    myothermetric = 5,
    textlabel = if_else(mymetric == 0, "",
                        paste0(format(mymetric, nsmall = 0), 
                               "% (",
                               format(round(cost_difference,2) , nsmall = 2),
                               "M€)"))
  ) %>% 
  select(
    xlabel = year,
    type,
    mymetric,
    myothermetric,
    textlabel)   



# chart ----
## chart parameters ----
local_suffix <- "%"
local_decimals <- 0

local_hovertemplate <- "%{y}"

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

mylocalfactor <- c("Underspending",
  "Overspending < 5%",
  "Overspending > 5%"
  )

mylocalcolors <- c('#044598', '#22A0DD', '#58595B')

# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight+20,
                      colors = mylocalcolors,
                      local_factor = mylocalfactor,

                      suffix = local_suffix,
                      decimals = local_decimals,
                      
                      text = ~textlabel,
                      hovertemplate = "<b>%{x}</b><br>%{meta}: %{text}<extra></extra>",
                      hovermode = "x",
                      
                      textangle = 0,
                      textposition = "none",
                      textfont_color = 'black',
                      insidetextanchor = 'middle',
                      
                      bargap = 0.25,
                      barmode = 'stack',
                      
                      title_text = paste0("Difference in investment costs - ", cost_type),
                      title_y = 0.99,
                      
                      yaxis_title = "",
                      yaxis_ticksuffix = local_suffix,
                      yaxis_tickformat = ".0f",
                      
                      legend_y = local_legend_y, 
                      legend_x = local_legend_x,
                      legend_xanchor = local_legend_xanchor,
                      legend_fontsize = local_legend_fontsize,
                      trace_showlegend = FALSE,
                      margin = list(t = 40, b= 60)
                      
                      ) %>% 
  layout(
      yaxis = list(
        zeroline = TRUE,
        zerolinecolor = "#808080",   # darker line at 0
        zerolinewidth = 1
    )
  ) %>% 
  ## to force the legend
  add_trace(
    data = data_prep,
    x = ~xlabel,
    y = 0,
    # mode = "lines+markers",
    type = "bar",
    name = "Underspending",                   
    hoverinfo = "skip",
    marker = list(color = '#044598'),
    textposition = "none",
    # textfont = list(color = "transparent", size = 1),
    showlegend = TRUE,
    inherit = FALSE
  ) %>% 
  add_trace(
    data = data_prep,
    x = ~xlabel,
    y = 0,
    # mode = "lines+markers",
    type = "bar",
    name = "Overspending < 5%",                   
    hoverinfo = "skip",
    marker = list(color = '#22A0DD'),
    textposition = "none",
    # textfont = list(color = "transparent", size = 1),
    showlegend = TRUE,
    inherit = FALSE
  ) %>% 
  add_trace(
    data = data_prep,
    x = ~xlabel,
    y = 0,
    # mode = "lines+markers",
    type = "bar",
    name = "Overspending > 5%",                   
    hoverinfo = "skip",
    marker = list(color = '#58595B'),
    textposition = "none",
    # textfont = list(color = "transparent", size = 1),
    showlegend = TRUE,
    inherit = FALSE
  ) %>% 
  add_trace(
    data = data_prep,
    x = ~xlabel,
    y = ~myothermetric,
    mode = "lines+markers",
    type = "scatter",
    name = "Threshold",                  
    hoverinfo = "skip",
    line = list(
      color = '#FF0000',
      width = 2,
      dash = "dash"
    ),
    marker = list(
      size = 1, 
      color = "transparent"
    ),
    textposition = "none",
    textfont = list(color = "transparent", size = 1),
    showlegend = TRUE,
    inherit = FALSE
  )

myplot


# force all legend items to appear

# add_fake_legend_split <- function(p, labels, colors, x = 0.07, y = -0.17, spacing = 0.28, font_size = myfont) {
#   for (i in seq_along(labels)) {
#     # Colored square
#     p <- p %>%
#       add_annotations(
#         x = x + (i - 1) * spacing, y = y,
#         text = "▇",
#         xref = "paper", yref = "paper",
#         xanchor = "left", yanchor = "middle",
#         showarrow = FALSE,
#         font = list(size = font_size, color = colors[i])
#       ) %>%
#       # Text label (default Plotly font color)
#       add_annotations(
#         x = x + (i - 1) * spacing + 0.04,
#         y = y,
#         text = labels[i],
#         xref = "paper", yref = "paper",
#         xanchor = "left", yanchor = "middle",
#         showarrow = FALSE,
#         font = list(size = font_size, color = "#444")
#       )
#   }
#   p
# }
# 
# p <- myplot %>%
#   layout(showlegend = FALSE) %>%  # hide real legend
#   add_fake_legend_split(labels = mylocalfactor, colors = mylocalcolors)
# p

