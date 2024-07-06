
# fix ez if script not executed from qmd file ----
if (exists("cz") == FALSE) {cz = c("1", "terminal")}
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
# if(cztype == "terminal") {data_raw <- data_raw_t1_trm} else {data_raw <- data_raw_t1_ert}

data_raw  <-  read_xlsx(
  paste0(data_folder, "CEFF dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = if_else(cztype == "terminal", "Terminal_T1", "Enroute_T1"),
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# prepare data ----
data_prep <- data_raw %>% 
  filter(
    entity_code == mycz) %>% 
  mutate(
    mymetric = round(x4_2_cost_excl_vfr/xrate2017/10^6,2),
    mymetric = case_when( 
      year > year_report & year != 20202021 & status == "A" ~ NA,
      .default = mymetric)
  ) %>%  
  select(
    year,
    status,
    mymetric
  ) %>%  
  filter(year > 2021) %>% 
  mutate(year = as.character(year),
         year = str_replace(year, "20202021", "2020-2021"),
         status = str_replace(status, "A", "Actual costs"),
         status = str_replace(status, "D", "Determined costs")
  ) %>% 
  arrange(year) %>% 
  pivot_wider(values_from = 'mymetric', names_from = 'status') %>% 
  mutate('Difference costs' = case_when(
    year<= .env$year_report ~ .[[2]] - .[[3]],
    .default = NA)
    ) %>% 
  mutate_at(c(2:4), ~round(.,0)) %>% 
  pivot_longer(-year, names_to = "Total costs - nominal EURO ('000)", values_to = 'mymetric') %>% 
  pivot_wider(values_from = 'mymetric', names_from = 'year')

mygtable(data_prep, myfont) %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold') %>% 
  cols_align(columns = 1, align = "left") %>%
  tab_style(
    style = list(
      # cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 1
    ))

  
