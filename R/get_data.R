
# data folders ----
  data_folder <- 'G:/HQ/dgof-pru/Data/SES Monitoring Dashboard/PBI files/'
  data_folder_a2 <- 'G:/HQ/dgof-pru/Data/SES Monitoring Dashboard/Annex 2/data/'

# get data  ----
  params_table <- read_mytable("Lists.xlsx", "Lists", "Table_States") %>% clean_names()

  state_list <- params_table %>% select(state) %>% unlist()

  aua_entities_table <- read_mytable("Lists.xlsx", "Lists", "Table_AUA") %>% clean_names()

  acc_list_table <- read_mytable("Lists.xlsx", "Lists", "Table_ACCs") %>% clean_names()

  ecz_list_table <- read_mytable("Lists.xlsx", "Lists", "Table_ECZ") %>%
    left_join(
      read_mytable("Lists.xlsx", "Lists", "Table_forecast"),
      by = "forecast_id"
      ) %>% clean_names()

  tcz_list_table <- read_mytable("Lists.xlsx", "Lists", "Table_TCZ") %>% clean_names()

  context_data_table <- read_mytable("context_data.xlsx", "context", "Table_context") %>%  clean_names()

  other_orgs_table <- read_mytable("Lists.xlsx", "Lists", "Table_PP_2023_ANSPs") %>%  clean_names()
