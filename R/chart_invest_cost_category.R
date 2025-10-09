if (exists("country") == FALSE) {country <- "Bulgaria"}

# source("R/parameters.R")

# import data  ----
if (!exists("data_cost_inv")) {
  source("R/get_investment_data.R")
}


# process data  ----
data_prep <- data_cost_inv %>% 
  filter(member_state == .env$country) %>% 
  select(nm_d_2020, nm_d_2021, nm_d_2022, nm_d_2023, nm_d_2024,
         on_d_2020, on_d_2021, on_d_2022, on_d_2023, on_d_2024,
         e_d_2020, e_d_2021, e_d_2022, e_d_2023, e_d_2024,
         
         nm_a_2020, nm_a_2021, nm_a_2022, nm_a_2023, nm_a_2024,
         on_a_2020, on_a_2021, on_a_2022, on_a_2023, on_a_2024,
         e_a_2020, e_a_2021, e_a_2022, e_a_2023, e_a_2024,
  ) %>% 
  pivot_longer(
    cols = everything(),  # Pivot all columns
    names_to = c("xlabel", "year"),  # Create "type" and "year" columns
    names_pattern = "(.+?)_(\\d{4})",  # Regex: Extract "type" + 4-digit year
    values_to = "value"  # Store values in "value" column
  ) %>% 
  mutate(
    type = if_else(str_detect(xlabel,"_d"), "Determined", "Actual"),
    xlabel = factor(case_when(
      str_detect(xlabel,"nm_") ~ "New major\ninvestments",
      str_detect(xlabel,"on_") ~ "Other new\ninvestments",
      str_detect(xlabel,"e_") ~ "Existing\ninvestments"
      ), levels = c("New major\ninvestments", "Other new\ninvestments", "Existing\ninvestments"))
    ) %>%
  group_by(xlabel, type) %>% 
  summarise(total = sum(value, na.rm = TRUE)/10^6) %>% 
  ungroup() %>% 
  mutate(total = if_else(total == 0, NA, total)) %>% 
  select(
    xlabel,
    type,
    mymetric = total)   

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
  local_legend_y <- -0.2
  local_legend_x <- 0.5
  local_legend_xanchor <- 'center'
  local_legend_fontsize <- myfont
  
}

# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight,
                      colors = c('#5B9BD5', '#FFC000'),
                      local_factor = c("Determined",
                                       "Actual",
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
                      
                      title_text = "Total costs of investments by category - RP3",
                      title_y = 0.99,
                      
                      yaxis_title = "Total costs of investments in RP3 (M€<sub>2017</sub>)",
                      yaxis_ticksuffix = local_suffix,
                      yaxis_tickformat = ".0f",
                      yaxis_titlefont_size = myyaxis_titlefont_size -1,
                      
                      legend_y = local_legend_y, 
                      legend_x = local_legend_x,
                      legend_xanchor = local_legend_xanchor,
                      legend_fontsize = local_legend_fontsize)

myplot




