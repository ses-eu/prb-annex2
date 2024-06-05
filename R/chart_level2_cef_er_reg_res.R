
# fix ez if script not executed from qmd file ----
if (exists("ez") == FALSE) {ez = 1}
# ez=1

# initialise list to store plots ----
myplot = list()

# loop through czs ----
for (ez in 1:no_ecz) {
  ## import data  ----
  data_raw_t1  <-  read_xlsx(
    paste0(data_folder, "CEFF dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Enroute_T1",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_raw_t2  <-  read_xlsx(
    paste0(data_folder, "CEFF dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Enroute_T2",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----
  data_filtered_t1 <- data_raw_t1 %>% 
    filter(
      #we filter on the cz_code instead of entity_code to get all entities
      charging_zone_code == ecz_list$ecz_id[ez],    
      #we keep only ANSPs
      entity_type %in% c('ANSP', 'MET', 'MUAC'),    
      #the fields we need are on a per/year basis - there are no values for 20-21 combined
      year != 20202021
    ) 
   
  #subtable for the ex post roe calc
  data_prep_t1_1 <- data_filtered_t1 %>% 
  filter(
    # for this calculation we need only actuals
    status == 'A') %>% 
     mutate(
      ex_post_roe = x3_4_total_assets * x3_8_share_of_equity_perc * x3_6_return_on_equity_perc,
    ) %>% 
    select(year,
           entity_type,
           charging_zone_code,
           entity_type_id,
           entity_name,
           entity_code,
           ex_post_roe)

  #subtable for the calc of Difference in costs: gain (+)/Loss (-) retained/borne by the ATSP
  data_prep_t1_2 <- data_filtered_t1 %>% 
    select(year,
           entity_code,
           status,
           x4_2_cost_excl_vfr
           ) %>% 
    mutate(x4_2_cost_excl_vfr = case_when(
    # we are calculating D-A costs for years<= year_report
      status == 'A' ~ (-1)*x4_2_cost_excl_vfr,
      .default = x4_2_cost_excl_vfr)
      ) %>% 
    group_by(year, entity_code) %>% 
    summarise(dif_cost_gain_loss = sum(x4_2_cost_excl_vfr)) %>% 
    mutate(dif_cost_gain_loss = case_when(
     year > year_report ~ 0,
      .default = dif_cost_gain_loss)
    )
    
  # join the t1 subtables
  data_prep_t1 <- data_prep_t1_1 %>% 
    left_join(data_prep_t1_2, by = c("year", "entity_code"))
  
  # extract data from t2
  data_prep_t2 <- data_raw_t2 %>% 
    filter(
      #we filter on the cz_code instead of entity_code to get all entities
      charging_zone_code == ecz_list$ecz_id[ez],    
      #we keep only ANSPs
      entity_type %in% c('ANSP', 'MET', 'MUAC'),    
      #we need actuals
      status == 'A',
    ) %>% 
    # the values for the combined year are 2021
    mutate(
      year = if_else(year == 20202021, 2021, year),
      trs = (x4_7_total_su / x4_6_total_su_forecast -1) * x4_1_det_cost_traffic_risk - x4_9_adjust_traffic_risk_art_27_2
      )%>% 
    select(year,
           entity_code,
           x2_5_adjust_inflation,
           x3_8_diff_det_cost_actual_cost,
           trs,
           x6_4_financial_incentive)
      
  ###############exrate and join 20 and 21, and force MET in the legend
  
  # join t1 and t2 for joint calculations
  data_prep <- data_prep_t1 %>% 
    left_join(data_prep_t2, by = c("year", "entity_code")) %>% 
    rowwise() %>% 
    mutate(
      atsp_gain_loss_cost_sharing = sum(dif_cost_gain_loss, x2_5_adjust_inflation, x3_8_diff_det_cost_actual_cost, na.rm = TRUE),
      total_net_gain_loss = sum(atsp_gain_loss_cost_sharing, trs, x6_4_financial_incentive, na.rm = TRUE),
      regulatory_result = total_net_gain_loss + ex_post_roe
    ) %>% 
    select(-entity_type, -charging_zone_code, -entity_name, -entity_code) %>% 
    mutate(xlabel = case_when(
      entity_type_id == 'ANSP1'  ~ 'Main ANSP',
      entity_type_id %like% 'MET'  ~ 'MET',
      .default = 'Other ANSP'
    )) %>% 
    group_by(year, xlabel) %>% 
    # the plot function already divides by 1000
    summarise(mymetric = sum(regulatory_result)/10^3) %>%
    ungroup() %>% 
    mutate(mymetric = case_when(
      year > year_report ~ 0,
      .default = mymetric
      )
      ) %>% 
    mutate(year_text = as.character(year)
           # , year_text = str_replace(year_text, "20202021", "2020-2021"),
    )
  
  
  ## chart parameters ----
  mychart_title <- paste0("Regulatory result at CZ level")
  myaxis_title <- "Regulatory result (€M)"
  mybarcolor <- c( '#5B9BD5', '#FFC000', '#BFBFBF')
  mytextcolor <- 'black'
  mylegend_y_position <- -0.15
  myfactor <- c("Main ANSP",
    "Other ANSP",
    "MET")
  
  ## define chart function ----
  ### function moved to utils
  
  ## plot chart  ----
  myplot[[ez]] <- mybarc_group(mywidth, myheight+30, myfont, mymargin)
  
}

# create html plotlist ----
htmltools::tagList(myplot)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document
