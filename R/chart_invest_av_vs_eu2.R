
if (exists("country") == FALSE) {country <- "Belgium"}

source("R/parameters.R")

# import data  ----
if (!exists("data_capex")) {
  source("R/get_investment_data.R")
}

# process data  ----
data_prep <- data_capex %>% 
  filter(member_state == .env$country | member_state == "SES RP3") %>% 
  select(member_state, total) %>% 
  mutate(
    type = case_when(
      member_state == .env$country ~ "ANSP",
      member_state == "SES RP3" ~ "Union-wide median"),
    mymetric = total / lead(total, 1)*100,
    mymetric = case_when(
      type == "Union-wide median" ~ 100-lag(mymetric,1),
      .default = mymetric
      ),
    textposition = if_else(mymetric == 0 | mymetric > 2, "inside", "outside"),
    textlabel = if_else(mymetric == 0, " ", paste0(format(round(mymetric,0), nsmall = 0), "%"))
  )  %>% 
  select(type, mymetric, textlabel, textposition) 

  

# chart ----
## legend
if (knitr::is_latex_output()) {
  local_legend_x <- 1
  local_legend_y <- 0.5  
} else {
  local_legend_x <- 0.5
  local_legend_y <- -0.05
  local_legend_xanchor <- 'center'
}



# plot chart ----
mydonutchart(data_prep, 
             colors = c('#FFF000', '#22A0DD'),
             shape = c("/", ""), # not supported by plotly on donut charts
             hovertemplate = "%{label}: %{value:.0f}%<extra></extra>",
             title_text = "Asset value of new investments RP3 (%)",
             minsize = 14,
             legend_x = local_legend_x,
             legend_y = local_legend_y,
             legend_xanchor = local_legend_xanchor,
             legend_orientation = "h")


