if (exists("country") == FALSE) {country <- "Bulgaria"}

source("R/parameters.R")

# import data  ----
if (!exists("data_cost_inv")) {
  source("R/get_investment_data.R")
}

# process data  ----
data_prep_year <- data_cost_inv_rt %>% 
  filter(member_state == .env$country) %>% 
  select(member_state,
    d_enr_2020, d_enr_2021, d_enr_2022, d_enr_2023, d_enr_2024,
    a_enr_2020, a_enr_2021, a_enr_2022, a_enr_2023, a_enr_2024
    ) %>% 
  group_by(member_state) %>% 
  summarise(
    d_enr_2020 = sum(d_enr_2020, na.rm = TRUE),
    d_enr_2021 = sum(d_enr_2021, na.rm = TRUE),
    d_enr_2022 = sum(d_enr_2022, na.rm = TRUE),
    d_enr_2023 = sum(d_enr_2023, na.rm = TRUE),
    d_enr_2024 = sum(d_enr_2024, na.rm = TRUE),

    a_enr_2020 = sum(a_enr_2020, na.rm = TRUE),
    a_enr_2021 = sum(a_enr_2021, na.rm = TRUE),
    a_enr_2022 = sum(a_enr_2022, na.rm = TRUE),
    a_enr_2023 = sum(a_enr_2023, na.rm = TRUE),
    a_enr_2024 = sum(a_enr_2024, na.rm = TRUE)
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
    value = if_else(a_enr == 0, 0, a_enr/d_enr-1)
  ) %>% 
  select(year, value) %>% 
  mutate(
    split_flag = value > 0.05
    ) %>% 
  mutate(weights = if_else(is.na(split_flag), 1L, if_else(split_flag, 2L, 1L))) %>%
  uncount(weights) %>%
  group_by(year) %>%
  mutate(value = if (n() == 2) c(0.05, first(value) - 0.05) else first(value)) %>%
  ungroup() %>%
  select(-split_flag) %>% 
  mutate(
    type = case_when(
      value > 0.05 ~ "Overspending > 5%",
      value <= 0.05 & value >=0 ~ "Overspending ≤ 5%",
      value < 0 ~ "Underspending"
    ),
    mymetric = round(value*100,1),
    myothermetric = 5,
    textlabel = if_else(mymetric == 0, "", paste0(format(mymetric, nsmall = 0), "%"))
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

# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight+20,
                      colors = c('#044598', '#22A0DD', '#58595B'),
                      local_factor = c("Underspending",
                                       "Overspending ≤ 5%",
                                       "Overspending > 5%",
                                        NULL),

                      suffix = local_suffix,
                      decimals = local_decimals,
                      
                      text = ~textlabel,
                      hovertemplate = "<b>%{x}</b><br>%{meta}: %{y:.1f}%<extra></extra>",
                      hovermode = "x",
                      
                      textangle = 0,
                      textposition = "none",
                      textfont_color = 'black',
                      insidetextanchor = 'middle',
                      
                      bargap = 0.25,
                      barmode = 'stack',
                      
                      title_text = "Difference in investment costs - en route",
                      title_y = 0.99,
                      
                      yaxis_title = "",
                      yaxis_ticksuffix = local_suffix,
                      yaxis_tickformat = ".0f",
                      
                      legend_y = local_legend_y, 
                      legend_x = local_legend_x,
                      legend_xanchor = local_legend_xanchor,
                      legend_fontsize = local_legend_fontsize) %>% 
  add_trace(
    data = data_prep,
    x = ~xlabel,
    y = ~myothermetric,
    mode = "lines+markers",
    type = "scatter",
    name = " ",                   # Suppresses label row
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
    showlegend = FALSE,
    inherit = FALSE
  )

myplot 



