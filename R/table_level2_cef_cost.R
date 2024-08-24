
# fix ez if script not executed from qmd file ----
if (exists("cz") == FALSE) {cz = c("1", "enroute")}
# ez=1

# define cz ----
ez <- as.numeric(cz[[1]])
cztype <- cz[[2]]
# cztype <- "terminal"
mycz <- if_else(cztype == "terminal",
                tcz_list$tcz_id[ez],
                ecz_list$ecz_id[ez])
mycz_name <- if_else(cztype == "terminal",
                     tcz_list$tcz_name[ez],
                     ecz_list$ecz_name[ez])

# import data  ----
if (country == "SES RP3") {
  ## SES  ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "SES CEFF.xlsx"),
    sheet = if_else(cztype == "terminal", "SES_TRM_all", "SES_ERT_all"),
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() |> 
    # so the field name is the same as for state
    mutate(x4_2_cost_excl_vfr = costs_eur2017nominal_cz,
           xrate2017 = 1)
    
  
} else {
  ## State  ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "CEFF dataset master.xlsx"),
    sheet = if_else(cztype == "terminal", "Terminal_T1", "Enroute_T1"),
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
}


# prepare data ----
data_prep_split <- data_raw %>% 
  filter(
    year != 20202021,
    entity_code == mycz) %>% 
  mutate(
    mymetric = case_when (
      status == 'A' & year > max(.env$year_report, 2021) ~ NA,
      .default = x4_2_cost_excl_vfr/xrate2017
    ),
    xlabel = as.character(year)
  ) %>%  
  select(
    year,
    status,
    mymetric,
    xlabel
  ) 

data_prep2020_2021 <- data_prep_split %>% 
  filter(
    year < 2022) %>% 
  group_by(status) |> 
  summarise(mymetric = sum(mymetric, na.rm = TRUE)) |> 
  mutate(xlabel = "2020-2021")

data_prep <- data_prep_split |> 
  filter(year > 2021) |> 
  select(-year) |>
  rbind(data_prep2020_2021) |> 
  mutate(mymetric = round(mymetric/10^6, 2),
         status = str_replace(status, "A", "Actual costs"),
         status = str_replace(status, "D", "Determined costs")
  ) %>% 
  arrange(xlabel) %>% 
  pivot_wider(values_from = 'mymetric', names_from = 'status') %>% 
  mutate('Difference costs' = case_when(
    xlabel<= .env$year_report ~ .[[2]] - .[[3]],
    .default = NA)
  ) %>% 
  mutate_at(c(2:4), ~round(.,0)) %>% 
  pivot_longer(-xlabel, names_to = "name", values_to = 'mymetric') %>% 
  pivot_wider(values_from = 'mymetric', names_from = 'xlabel')

# plot table ----
mygtable(data_prep, myfont) %>% 
  cols_label(name = html("Total costs - nominal EURO (M€)")) %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold',
              container.padding.y = 0) %>% 
  cols_align(columns = 1, align = "left") %>%
  tab_style(
    style = list(
      # cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 1
      )
    )|> 
  fmt_number(
    columns = c(2:5),  # Specify the columns to format
    decimals = 0,  # Number of decimal places
    use_seps = TRUE  # Use thousands separator
  ) |> 
  tab_header(
    title = md(paste0(if_else(cztype == "terminal", "**TCZ", "**ECZ"),
                      " actual and planned data**"))
  )

  
