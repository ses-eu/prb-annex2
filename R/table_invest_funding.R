if (exists("country") == FALSE) {country <- "Belgium"}

# source("R/parameters.R")

# import data  ----
if (!exists("data_new_major")) {
  source("R/get_investment_data.R")
}

# process data  ----
data_prep <- data_funding %>% 
  filter(member_state == .env$country) %>% 
  mutate(
    value = if_else(as.numeric(year) > year_report & year != "RP3", NA, value/10^6)
  ) %>% 
  pivot_wider(id_cols = c(year), names_from ="type", values_from = "value") %>% 
 mutate (
    Difference = `Total self-declared funding` - `SDM data`
    ) %>% 
  pivot_longer(-c(year), names_to = "type", values_to = "value") %>% 
  mutate (
    type = case_when(
      tolower(type) == "total self-declared funding" ~ "Total included in the funding declaration (reporting tables)",
      tolower(type) == "sdm data" ~ "SDM Payment",
      tolower(type) == "difference" ~ "Difference between total funding declared vs received"
      
    )
  ) %>% 
  arrange(year) %>% 
  pivot_wider(id_cols = c(type), names_from = "year", values_from = "value") %>% 
  rowwise() %>%
  mutate(across(2:7, ~ if_else(
    str_detect(type, "Difference") & !is.na(type),
    format_parens(.x),
    format(round(.x, 2), nsmall =2)
    ))) %>%
  ungroup() 

# render table ----

table1 <- mygtable(data_prep, myfont) %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold',
              container.padding.y = 0) %>% 
  cols_align(columns = 1, align = "left") %>%
  cols_label(
    type = html("Funding declared (M€<sub>2017</sub>)")
  ) %>%
  fmt_number(
    columns = 2:7,   
    decimals = 2,
    use_seps = TRUE,
    sep_mark = ",",
    dec_mark = "."
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = 7)
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(rows = nrow(data_prep))
  ) %>% 
  # tab_style(
  #   style = cell_text(indent = px(20)),
  #   locations = cells_body(
  #     columns = 1,
  #     rows = 2:nrow(data_prep)
  #   )
  # ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_body(
      columns = "RP3"
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_column_labels(
      columns = "RP3"
    )
  )

  

# create latex table
if (knitr::is_latex_output()) {
  table_level2_cef_cost_infl <- mylatex(table1)
  
} else {
  table1
}

