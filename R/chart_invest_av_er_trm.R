if (exists("country") == FALSE) {country <- "Belgium"}

# source("R/parameters.R")

# import data  ----
if (!exists("data_new_major")) {
  source("R/get_investment_data.R")
}

# process data  ----
data_prep <- data_new_major %>% 
  filter(member_state == .env$country) %>% 
  mutate(
    enroute_share = total_value_of_the_asset_nominal_euros_en_route / total_value_of_the_asset_nominal_euros * 100,
    terminal_share = total_value_of_the_asset_nominal_euros_terminal / total_value_of_the_asset_nominal_euros * 100,
    NULL
    ) %>% 
  select(
    enroute_share,
    terminal_share,
    NULL
  ) %>% 
  gather() %>% 
  mutate(
    type = case_when(
      key == "enroute_share" ~ "En route",
      key == "terminal_share" ~ "Terminal"
    ),
    xlabel = "Asset value",
    mymetric = round(value,0),
    textposition = if_else(mymetric == 0 | mymetric > 2, "inside", "outside"),
    textlabel = if_else(mymetric == 0, " ", paste0(format(mymetric, nsmall = 0), "%"))
  )  %>% 
  select(xlabel, type, mymetric, textlabel, textposition) 

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
             height = myheight-110,
             colors = c( '#22A0DD', '#044598'),
             hovertemplate = "%{label}: %{value}%",
             title_text = "Asset value: en route and terminal",
             minsize = 14,
             legend_x = local_legend_x,
             legend_y = local_legend_y,
             legend_xanchor = local_legend_xanchor,
             legend_orientation = "h",
             margin = list(t = 25))


