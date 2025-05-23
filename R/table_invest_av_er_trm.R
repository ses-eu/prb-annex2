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
    enroute_value = total_value_of_the_asset_nominal_euros_en_route/10^6,
    terminal_value = total_value_of_the_asset_nominal_euros_terminal/10^6,
    enroute_share = total_value_of_the_asset_nominal_euros_en_route / total_value_of_the_asset_nominal_euros,
    terminal_share = total_value_of_the_asset_nominal_euros_terminal / total_value_of_the_asset_nominal_euros,
    NULL
  ) %>% 
  select(
    enroute_value,
    terminal_value,
    enroute_share,
    terminal_share,
    NULL
  ) %>% 
  gather() %>% 
  mutate(
    type = case_when(
      str_detect(key, "enroute") ~ "En route",
      str_detect(key, "terminal") ~ "Terminal"
    ),
    key = str_remove_all(key, "enroute_"),
    key = str_remove_all(key, "terminal_")
    
  ) %>% 
  pivot_wider(id_cols = type, names_from = "key", values_from = "value")

total_value <- format(round(sum(data_prep$value, na.rm = TRUE),2), nsmall = 2, big.mark = ",")

table1 <- mygtable(data_prep, myfont) %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold',
              container.padding.y = 0) %>% 
  cols_align(columns = 1, align = "left") %>%
  cols_label(
    type = html("Total value of the asset for new major investments (M€<sub>2017</sub>)"),
    value = total_value,
    share = "% of total"
  ) %>% 
  fmt_number(
    columns = 2,   # replace with your actual column name
    decimals = 2,
    use_seps = TRUE,
    sep_mark = ",",
    dec_mark = "."
  ) %>% 
  fmt_percent(
    columns = 3,   # replace with your actual column name
    decimals = 0
  ) %>% 
  tab_style(
    style = list(
      # cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 1
    ))
 

# create latex table
if (knitr::is_latex_output()) {
  table_level2_cef_cost_infl <- mylatex(table1)
  
} else {
  table1
}
