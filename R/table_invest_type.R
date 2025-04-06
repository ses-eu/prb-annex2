if (exists("country") == FALSE) {country <- "Belgium"}

source("R/parameters.R")

# import data  ----
if (!exists("data_new_major")) {
  source("R/get_investment_data.R")
}

# process data  ----
data_prep <- data_category %>% 
  filter(member_state_1 == .env$country) %>% 
  mutate(
    "SES mandated" = if_else(tolower(investment_mandated_by_ses_regulation_i_e_pcp_interoperability_y_n) == "yes",
                  "X", ""),
    "Partnership" = if_else(tolower(joint_investment_partnership_y_n) == "yes",
                             "X", ""),
    "ATM system" = if_else(tolower(investment_in_atm_systems_y_n) == "yes",
                           "X", ""),
    "CP/MP" = if_else(tolower(if_investment_in_atm_system_reference_to_european_atm_master_plan_pcp) == "pcp" |
                        tolower(if_investment_in_atm_system_reference_to_european_atm_master_plan_pcp) == "master plan (non-pcp)",
                           "X", ""),
  ) %>% 
  select(
    investment_name, "SES mandated", "Partnership", "ATM system", "CP/MP")

# render table ----
table1 <- mygtable(data_prep, myfont) %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold',
              container.padding.y = 0) %>% 
  cols_align(columns = 1, align = "left") %>%
  cols_label(
    investment_name = "New major investments"
  ) %>% 
  cols_align(
    align = "center",
    columns = 2:ncol(data_prep)  # center all except first column
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = 2:ncol(data_prep))
  ) %>% 
  tab_style(
    style = list(
      # cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 1
    )) %>%
  tab_spanner(
    label = "ATM systems",
    columns = c(`ATM system`, `CP/MP`)
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_body(
      columns = "ATM system"
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_column_labels(
      columns = "ATM system"
    )
  )
 

# create latex table
if (knitr::is_latex_output()) {
  table_level2_cef_cost_infl <- mylatex(table1)
  
} else {
  table1
}
