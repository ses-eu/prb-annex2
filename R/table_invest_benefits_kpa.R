if (exists("country") == FALSE) {country <- "Belgium"}

source("R/parameters.R")

# import data  ----
if (!exists("data_new_major")) {
  source("R/get_investment_data.R")
}

# process data  ----
data_prep <- data_category %>% 
  filter(member_state_1 == .env$country) %>% 
  mutate(empty_col = "",
         SAF = if_else(saf ==1, "X", ""),
         ENV = if_else(env ==1, "X", ""),
         CAP = if_else(cap ==1, "X", ""),
         CEF = if_else(cef ==1, "X", ""),
  ) %>% 
  select(
    investment_name, empty_col, SAF, ENV, CAP, CEF)

# render table ----
table1 <- mygtable(data_prep, myfont) %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold',
              container.padding.y = 0) %>% 
  cols_align(columns = 1, align = "left") %>%
  cols_label(
    investment_name = "New major investments",
    empty_col = "Expected benefit per KPA"
  ) %>% 
  cols_align(
    align = "center",
    columns = 3:ncol(data_prep)  # center all except first column
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = 3:ncol(data_prep))
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
