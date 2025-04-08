if (exists("country") == FALSE) {country <- "Belgium"}

# source("R/parameters.R")

# import data  ----
if (!exists("data_new_major")) {
  source("R/get_investment_data.R")
}

# process data  ----
data_prep <- data_category %>% 
  filter(member_state_1 == .env$country) %>% 
  select(atm, cns, infra, ancillary, unknown, other) %>% 
  summarise (atm_value = sum(atm, na.rm=TRUE),
             cns_value = sum(cns, na.rm=TRUE), 
             infra_value = sum(infra, na.rm=TRUE), 
             unknown_value = sum(unknown, na.rm=TRUE),
             other_value = sum(other, na.rm=TRUE) + sum(ancillary, na.rm=TRUE)) %>% 
  mutate(total = rowSums(across(everything())),
         atm_share = atm_value/total,
         cns_share = cns_value/total,
         infra_share = infra_value/total,
         unknown_share = unknown_value/total,
         other_share = other_value/total
         )%>% 
  select(
    atm_value,
    cns_value,
    infra_value,
    unknown_value,
    other_value,
    
    atm_share,
    cns_share,
    infra_share,
    unknown_share,
    other_share,
    NULL
  ) %>% 
  gather() %>% 
  mutate(
    type = case_when(
      str_detect(key, "atm") ~ "ATM systems",
      str_detect(key, "cns") ~ "CNS systems",
      str_detect(key, "infra") ~ "Infrastructure",
      str_detect(key, "other") ~ "Other",
      str_detect(key, "unknown") ~ "Unknown"
    ),
    key = str_remove_all(key, "atm_|cns_|infra_|unknown_|other_")
  ) %>% 
  pivot_wider(id_cols = type, names_from = "key", values_from = "value") %>% 
  arrange(type)

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
