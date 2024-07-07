
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
data_raw  <-  read_xlsx(
  paste0(data_folder, "CEFF dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = if_else(cztype == "terminal", "Terminal_T2", "Enroute_T2"),
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 
  
# prepare data ----
data_prep_t2 <- data_raw %>% 
  filter(
    entity_code == mycz,
    year == if_else(year_report == 2021, 20202021,year_report)
  ) %>% 
  select(
    year,
    x4_7_total_su,
    x3_1_investment,
    x3_3_cost_authority_qes,
    x3_4_ectl_cost_eur,
    x3_5_pension_cost,
    x3_6_interest_loan,
    x3_7_change_in_law,
    x3_8_diff_det_cost_actual_cost
  ) %>%
  pivot_longer(cols = -c(year, x4_7_total_su),
               names_to = 'type',
               values_to = 'mymetric') %>% 
  mutate(
    xlabel = rep(c("New and existing investments",
                        "Competent authorities and qualified entities costs",
                        "Eurocontrol costs",
                        "Pension costs",
                        "Interest on loans",
                        "Changes in law",
                        "Total cost exempt from cost risk sharing"), 1),
         year_text = as.character(year),
         year_text = str_replace(year_text, "20202021", "2020-2021"),
  ) %>% 
  mutate(
    mymetric = mymetric/1000,
    year = as.character(year),
    year = str_replace(year, "20202021", "2020-2021")
  )
  
# t exchange rates
yearly_xrates <- get_xrates(cztype, mycz)

data_prep_xrates <- yearly_xrates %>% 
  filter(
    entity_code == mycz
  ) %>% 
  select(-entity_code) %>% 
  filter(year > 2020) %>% 
  mutate(year = as.character(year),
         year = if_else(year == '2021', '2020-2021', year)
        ) 

data_prep <- data_prep_t2 %>% 
  left_join(data_prep_xrates, by = 'year') %>% 
  mutate(mymetric = mymetric/pp_exchangerate,
         myothermetric = mymetric*1000/x4_7_total_su) %>% 
  select(xlabel, mymetric, myothermetric)
    
  
# plot table ----
mygtable(data_prep, myfont) %>% 
  cols_label(xlabel = paste0("Cost exempt from cost sharing in ", year_report),
             mymetric = "€'000",
             myothermetric = "€/SU") %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold') %>% 
  # cols_align(columns = 1, align = "left") %>%
  fmt_number(decimals = 2) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = 7
    ))



