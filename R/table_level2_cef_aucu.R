
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

# import data ----
data_calc <- aucu(cztype, mycz)
  
# prep data ----
data_prep <- data_calc %>% 
  filter(year_text == if_else(year_report == 2021, "2020-2021", as.character(year_report))) %>% 
  select(
    initial_duc,
    retro_ur,
    new_duc,
    infl_adj,
    dif_a_d_costs,
    trs_adj,
    dc_notrs,
    fin_inc,
    rev_c_mod,
    cross_fin,
    other_rev,
    loss_rev,
    total_adjustments_aucu,
    aucu
  ) %>% 
  rename(
    'Initial DUC charged' = initial_duc,
    'DUC to be charged retroactively' = retro_ur,
    DUC = new_duc,
    'Inflation adjustment' = infl_adj,
    'Cost exempt from cost-sharing' = dif_a_d_costs,
    'Traffic risk sharing adjustment' = trs_adj,
    'Traffic adj. (costs not TRS)' = dc_notrs,
    'Finantial incentives' = fin_inc,
    'Modulation of charges' = rev_c_mod,
    'Cross-financing' = cross_fin,
    'Other revenues' = other_rev,
    'Application of lower unit rate' = loss_rev,
    'Total adjustments' = total_adjustments_aucu,
    AUCU = aucu
  ) %>% 
  mutate('AUCU vs. DUC' = AUCU/DUC-1 ) %>% 
  pivot_longer( cols = everything() ,names_to = "type", values_to = "value")

# plot chart  ----
mygtable(data_prep, myfont*0.95) %>% 
  cols_label(type = paste0("Components of the AUCU in ", year_report), value = "€/SU") %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold') %>% 
  fmt_number(
    decimals = 2,
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = c(3,14, 15)
    )
  ) %>% 
  fmt_percent(rows = 15, decimals = 1, force_sign = TRUE)
  