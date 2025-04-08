if (exists("country") == FALSE) {country <- "Belgium"}

# source("R/parameters.R")

# import data  ----
if (!exists("data_new_major")) {
  source("R/get_investment_data.R")
}

# process data  ----
data_prep <- data_capex %>% 
  filter(member_state == .env$country) %>% 
  mutate(
    pp_new_major_value = new_major_investments_as_per_pp/10^6,
    pp_other_new_value = other_new_investments_as_per_pp/10^6,
    add_new_major_value = additional_new_major_investments/10^6,
    new_major_value = pp_new_major_value + add_new_major_value,
      
    pp_new_major_share = new_major_investments_as_per_pp / total,
    pp_other_new_share = other_new_investments_as_per_pp / total,
    add_new_major_share = additional_new_major_investments / total,
    new_major_share = pp_new_major_share + add_new_major_share,
    NULL
  ) %>% 
  select(
    pp_new_major_value,
    pp_other_new_value,
    add_new_major_value,
    new_major_value,
    pp_new_major_share,
    pp_other_new_share,
    add_new_major_share,
    new_major_share,
    NULL
  ) %>% 
  gather() %>% 
  mutate(
    type = case_when(
      str_detect(key, "pp_new_major") ~ "Included in the performance plan",
      str_detect(key, "add") ~ "Additional",
      str_detect(key, "new_major") ~ "New major investments (above 5M€ each)",
      str_detect(key, "other") ~ "Other new investments (below 5M€ each)"
    ),
    key = str_remove_all(key, "pp_new_major_|add_new_major_|new_major_|pp_other_new_")
    
  ) %>% 
  pivot_wider(id_cols = type, names_from = "key", values_from = "value") %>% 
  mutate(type = factor (type, levels = c(
    "New major investments (above 5M€ each)",
    "Included in the performance plan",
    "Additional",
    "Other new investments (below 5M€ each)")
    )
    ) %>% 
  arrange(type)

total_value <- data_prep %>% 
  filter(data_prep$type != "New major investments (above 5M€ each)") %>% 
  select(value) %>% 
  sum(., na.rm = TRUE) %>% 
  round(.,2) %>% 
  format(., nsmall = 2, big.mark = ",")

table1 <- mygtable(data_prep, myfont) %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold',
              container.padding.y = 0) %>% 
  cols_align(columns = 1, align = "left") %>%
  cols_label(
    type = html("Total value of the asset for new investments (M€<sub>2017</sub>)"),
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
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = c(1,4)
    )) %>% 
  tab_style(
    style = cell_text(indent = px(20)),
    locations = cells_body(
      columns = c(type),
      rows = type == "Additional" | type == "Included in the performance plan" 
    )
  )
 

# create latex table
if (knitr::is_latex_output()) {
  table_level2_cef_cost_infl <- mylatex(table1)
  
} else {
  table1
}
