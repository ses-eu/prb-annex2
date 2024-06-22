# libraries ----
library(tidyr)
library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)
library(janitor)
library(webshot)
library(data.table)
library(here)
library(fs)
library(purrr)
library(plotly)
library(gt)

# functions ----
## right x characters function ----
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }  

## read range function ----
  read_range <- function(file, sheet, range){
    read_excel(
      file,
      sheet = sheet,
      range = range) %>% 
      mutate_all(~ str_replace_all(., "\r\n\r\n", "\r\n")) %>% 
      mutate_all(~ str_replace_all(., "<br><br><br><br>", "<br><br>"))   
  }

## read table range function ----
read_mytable <- function(file, sheet, table){
  wb <- loadWorkbook(paste0(data_folder, file))
  tables <- getTables(wb, sheet = sheet)
  # get the range
  table_range <- names(tables[tables == table])
  # read range
  read_range(paste0(data_folder, file), sheet, table_range) 
  }

## add columns to df if they don't exist ----
  add_cols <- function(df, cols) {
    add <- cols[!cols %in% names(df)]
    if (length(add) != 0) {
      df[add] <- NA
    }
    return(df)
  }

## get yearly exchange rates ----
  get_xrates <- function(cztype, mycz) {
    data_raw_xrates  <-  read_xlsx(
      paste0(data_folder, "CEFF dataset master.xlsx"),
      # here("data","hlsr2021_data.xlsx"),
      sheet = if_else(cztype == "terminal", "TRM_XRATE2017", "ERT_XRATE2017"),
      range = cell_limits(c(1, 1), c(NA, NA))) %>%
      as_tibble() %>% 
      clean_names()  
    
    yearly_xrates <- data_raw_xrates %>% 
      filter(
        entity_code %in% mycz == TRUE
      ) %>% 
      select(entity_code, contains('pp_exchangerate_' )) %>% 
      pivot_longer(cols = starts_with("pp_exchangerate_"),
                   names_to = "year",
                   values_to = 'pp_exchangerate') %>% 
      mutate(year = str_replace_all(year, 'pp_exchangerate_', ''),
             year = as.numeric(year),
             pp_exchangerate = if_else(pp_exchangerate == 0, NA, pp_exchangerate),
      )
    return(yearly_xrates)
  }

## aucu calculations ----
  
  aucu <- function(cztype, mycz) {
  ## import data  ----
  data_raw_t1  <-  read_xlsx(
    paste0(data_folder, "CEFF dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = if_else(cztype == "terminal", "Terminal_T1", "Enroute_T1"),
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_raw_t2  <-  read_xlsx(
    paste0(data_folder, "CEFF dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = if_else(cztype == "terminal", "Terminal_T2", "Enroute_T2"),
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_raw_t3  <-  read_xlsx(
    paste0(data_folder, "CEFF dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = if_else(cztype == "terminal", "Terminal_T3", "Enroute_T3"),
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----
  
  # filter raw tables on cz
  ## t1
  data_prep_t1 <- data_raw_t1 %>% 
    filter(
      entity_code == mycz,
      status == 'A'
    ) 
  
  ## t2
  data_prep_t2_ini <- data_raw_t2 %>% 
    filter(
      entity_code == mycz
    ) 
  
  #temp table with values for 2020 and 2021 separated that I'll need later for aucu calculations
  #first I need the 10.5 and 4.7 value separated also for the calculations 
  
  other_rev_20202021 <- data_prep_t2_ini %>% filter(year == 20202021) %>% select(x10_5_other_revenue) %>% pull()
  total_su_t2_20202021<- data_prep_t2_ini %>% filter(year == 20202021) %>% select(x4_7_total_su) %>% pull()
  
  data_temp_t2 <- data_prep_t2_ini %>%  
    filter(year == 20202021) %>% 
    mutate_all(~ if_else(is.numeric(.x),NA,.x)) %>% 
    mutate(year = 2020,
           x10_5_other_revenue = other_rev_20202021,
           x4_7_total_su= total_su_t2_20202021
    ) %>% 
    rbind(filter(data_prep_t2_ini, year == 20202021)) %>% 
    mutate(year = if_else(year == 20202021, 2021, year))
  
  data_prep_t2 <- data_prep_t2_ini %>% rbind(data_temp_t2)
  
  ## t3
  data_prep_t3 <- data_raw_t3 %>% 
    filter(
      entity_code == mycz,
      year != 'After RP' & year != 'Amounts'
    ) %>% 
    mutate(year = as.numeric(year))
  
  ## t exchange rates
  yearly_xrates <- get_xrates(cztype, mycz)
  
  data_prep_xrates <- yearly_xrates %>% 
    filter(
      entity_code == mycz
    ) %>% 
    select(-entity_code)
  
  #join all tables
  data_prep_all <- data_prep_t1 %>% 
    left_join(data_prep_t2, by = 'year', suffix = c(".t1", ".t2")) %>% 
    left_join(data_prep_t3, by = 'year', suffix = c("", ".t3")) %>% 
    left_join(data_prep_xrates, by = 'year')
  
  
  # get some parameters for 2020 and 2021. Needed later for calcs
  initial_duc_2020 <- data_prep_t2 %>% 
    filter(year == 20202021) %>% 
    select(x15_unit_rate_temp_2020) %>% pull()
  
  initial_duc_2021 <- data_prep_t2 %>% 
    filter(year == 20202021) %>% 
    select(x15_unit_rate_temp_2021) %>% pull()
  
  tsu_2020 <- data_prep_t1 %>% 
    filter(year == 2020) %>% 
    select(x5_4_total_su) %>% pull()
  
  tsu_2021 <- data_prep_t1 %>% 
    filter(year == 2021) %>% 
    select(x5_4_total_su) %>% pull()  
  
  tsu_20202021 <- data_prep_t1 %>% 
    filter(year == 20202021) %>% 
    select(x5_4_total_su) %>% pull() 
  
  # create table with forecast sus and format it so it can be used in calcs
  data_prep_forecast_su <- data_prep_all %>% 
    add_cols(., c('x15_forecast_su_temp_2022',       # add missing columns
                  'x15_forecast_su_temp_2023',
                  'x15_forecast_su_temp_2024')
    ) %>%                               
    select(contains('x15_forecast_su_temp_' )) %>% 
    pivot_longer(cols = starts_with("x15_forecast_su_temp_"),
                 names_to = "year",
                 values_to = 'x15_forecast_su_temp') %>% 
    mutate(year = str_replace_all(year, 'x15_forecast_su_temp_', ''),
           year = as.numeric(year),
           x15_forecast_su_temp = if_else(x15_forecast_su_temp == 0, NA, x15_forecast_su_temp),
    ) %>% 
    group_by(year) %>% 
    summarise(x15_forecast_su_temp = min(x15_forecast_su_temp, na.rm = TRUE)) %>% 
    mutate(x15_forecast_su_temp = if_else(x15_forecast_su_temp == Inf, NA, x15_forecast_su_temp),
    ) 
  
  # add new forecast sus to full table
  data_prep_all <- data_prep_all %>%
    left_join(data_prep_forecast_su, by = 'year')
  
  # calcs
  ## calculate all values for individual years following the indications in the CEFF computations file
  data_prep_years_split <- data_prep_all %>% 
    filter(year != 20202021) %>%
    mutate_all(~ ifelse(is.na(.), 0, .)) %>% 
    mutate(
      initial_duc = case_when(
        year == 2020 ~ (initial_duc_2020 - (total_adjustment/x15_forecast_su_temp)) * tsu_2020/(tsu_2020 + tsu_2021),
        year == 2021 ~ (initial_duc_2021 - (total_adjustment/x15_forecast_su_temp)) * tsu_2021/(tsu_2020 + tsu_2021),
        .default = if_else(x8_1_temp_unit_rate >0,
                           x8_1_temp_unit_rate - (total_adjustment/x15_forecast_su_temp),
                           x4_2_cost_excl_vfr/x5_4_total_su)
      ),
      initial_duc = initial_duc / pp_exchangerate,
      new_duc = case_when(
        year == 2020 | year == 2021 ~ x4_2_cost_excl_vfr / tsu_20202021 / pp_exchangerate,
        .default = x4_2_cost_excl_vfr / x5_4_total_su / pp_exchangerate
      ),
      retro_ur = new_duc - initial_duc,
      
      infl_adj = x2_5_adjust_inflation / x4_7_total_su / pp_exchangerate,
      dif_a_d_costs = x3_8_diff_det_cost_actual_cost / x4_7_total_su / pp_exchangerate,
      trs_adj = x4_9_adjust_traffic_risk_art_27_2 / x4_7_total_su / pp_exchangerate,
      dc_notrs = x5_1_det_cost_no_traffic_risk / x4_7_total_su / pp_exchangerate,
      fin_inc = x6_4_financial_incentive / x4_7_total_su / pp_exchangerate,
      rev_c_mod = x7_1_adj_revenue_charge_modulation / x4_7_total_su / pp_exchangerate,
      cross_fin = x9_1_cross_financing_other / x4_7_total_su / pp_exchangerate,
      other_rev = case_when(
        year == 2020 | year == 2021 ~ x10_5_other_revenue * x5_4_total_su / tsu_20202021 / x4_7_total_su / pp_exchangerate,
        .default = x10_5_other_revenue / x4_7_total_su / pp_exchangerate
      ),
      loss_rev = x11_1_loss_revenue_lower_unit_rate / x4_7_total_su / pp_exchangerate,
      
      total_adjustments_aucu = infl_adj + dif_a_d_costs + trs_adj + dc_notrs + fin_inc + rev_c_mod + cross_fin + other_rev + loss_rev,
      aucu = new_duc + total_adjustments_aucu,
      
      aucu_excluding_or = aucu - other_rev,
      
      check_adj = (x12_total_adjust - x8_2_diff_revenue_temp_unit_rate - x5_2_unit_rate_no_traffic_risk) / x4_7_total_su / pp_exchangerate,
      
      year_text = as.character(year)
      
    ) %>% 
    select(
      year_text,
      x4_7_total_su,
      x5_4_total_su,
      
      x8_1_temp_unit_rate,
      x15_forecast_su_temp,
      total_adjustment,
      initial_duc,
      retro_ur,
      new_duc,
      retro_ur,
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
      check_adj,
      aucu,
      aucu_excluding_or
    ) %>% 
    arrange(year_text)
  
  ## calculate values 2020-2021 as a sum of the individual years
  data_prep_20202021 <- data_prep_years_split %>% 
    filter(year_text == '2020' | year_text == '2021') %>%
    bind_rows(summarise(., across(where(is.numeric), sum),
                        across(where(is.character), ~'2020-2021'))) %>% 
    filter(year_text == '2020-2021') %>% 
    mutate(x4_7_total_su = x4_7_total_su/2)  # I didn't want to sum this one
  
  ## join prep tables with the relevant years
  aucu_data <- data_prep_20202021 %>% 
    rbind(filter(data_prep_years_split, year_text != '2020' & year_text != '2021'))
  
  return(aucu_data)
  }
  
## regulatory result calculations ----
  
  regulatory_result <- function(cztype, mycz) {
    ## import data  ----
    data_raw_t1  <-  read_xlsx(
      paste0(data_folder, "CEFF dataset master.xlsx"),
      # here("data","hlsr2021_data.xlsx"),
      sheet = if_else(cztype == "terminal", "Terminal_T1", "Enroute_T1"),
      range = cell_limits(c(1, 1), c(NA, NA))) %>%
      as_tibble() %>% 
      clean_names() 
    
    data_raw_t2  <-  read_xlsx(
      paste0(data_folder, "CEFF dataset master.xlsx"),
      # here("data","hlsr2021_data.xlsx"),
      sheet = if_else(cztype == "terminal", "Terminal_T2", "Enroute_T2"),
      range = cell_limits(c(1, 1), c(NA, NA))) %>%
      as_tibble() %>% 
      clean_names() 
    
    ## prepare data ----
    data_filtered_t1 <- data_raw_t1 %>% 
      filter(
        #we filter on the cz_code instead of entity_code to get all entities
        charging_zone_code == mycz,    
        #we keep only ANSPs
        entity_type %in% c('ANSP', 'MET', 'MUAC'),    
        #the fields we need are on a per/year basis - there are no values for 20-21 combined
        year != 20202021
      ) 
    
    #subtable for the ex post roe calc
    data_prep_t1_1 <- data_filtered_t1 %>% 
      select(year,
             status,
             entity_type,
             charging_zone_code,
             entity_type_id,
             entity_name,
             entity_code,
             x3_4_total_assets,
             x3_8_share_of_equity_perc,
             x3_6_return_on_equity_perc) %>% 
      mutate(
        roe = x3_4_total_assets * x3_8_share_of_equity_perc * x3_6_return_on_equity_perc,
      ) %>% 
      select(-c(x3_4_total_assets, x3_8_share_of_equity_perc, x3_6_return_on_equity_perc)) %>% 
      pivot_wider(names_from = 'status',
                  values_from = 'roe') %>% 
      rename(ex_ante_roe_nc = D,
             ex_post_roe_nc = A)
    
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
    
    #subtable for the calc actual revenues
    data_prep_t1_3 <- data_filtered_t1 %>% 
      filter(status == 'A') %>% 
      select(year,
             entity_code,
             status,
             x4_2_cost_excl_vfr
             )
    
    # join the t1 subtables
    data_prep_t1 <- data_prep_t1_1 %>% 
      left_join(data_prep_t1_2, by = c("year", "entity_code")) %>% 
      left_join(data_prep_t1_3, by = c("year", "entity_code"))
    
    # extract data from t2
    data_prep_t2 <- data_raw_t2 %>% 
      filter(
        #we filter on the cz_code instead of entity_code to get all entities
        charging_zone_code == mycz,    
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
    
    # join t1 and t2 for joint calculations
    data_prep_years_split <- data_prep_t1 %>% 
      left_join(data_prep_t2, by = c("year", "entity_code")) %>% 
      rowwise() %>% 
      mutate(
        atsp_gain_loss_cost_sharing = sum(dif_cost_gain_loss, x2_5_adjust_inflation, x3_8_diff_det_cost_actual_cost, na.rm = TRUE),
        total_net_gain_loss = sum(atsp_gain_loss_cost_sharing, trs, x6_4_financial_incentive, na.rm = TRUE),
        regulatory_result_nc = total_net_gain_loss + ex_post_roe_nc,
        actual_revenues_nc = sum(x4_2_cost_excl_vfr, total_net_gain_loss, na.rm = TRUE)
      ) %>% 
      select(-entity_type, -charging_zone_code, -entity_name, -entity_code) %>% 
      mutate(type = case_when(
        entity_type_id == 'ANSP1'  ~ 'Main ANSP',
        entity_type_id %like% 'MET'  ~ 'MET',
        .default = 'Other ANSP'
      )) %>% 
      group_by(year, type) %>% 
      # the plot function already divides by 1000
      summarise(
        atsp_gain_loss_cost_sharing_nc = sum(atsp_gain_loss_cost_sharing)/10^3,
        trs_nc = sum(trs) /10^3,
        financial_incentive_nc = sum(x6_4_financial_incentive)/10^3,
        regulatory_result_nc = sum(regulatory_result_nc)/10^3,
        ex_ante_roe_nc = sum(ex_ante_roe_nc)/10^3,
        ex_post_roe_nc = sum(ex_post_roe_nc)/10^3,
        actual_revenues_nc = sum(actual_revenues_nc)/10^3
                ) %>%
      ungroup() %>% 
      mutate(
        atsp_gain_loss_cost_sharing_nc = case_when(
          year > year_report ~ 0,
          .default = atsp_gain_loss_cost_sharing_nc),
        trs_nc = case_when(
          year > year_report ~ 0,
          .default = trs_nc),
        financial_incentive_nc = case_when(
          year > year_report ~ 0,
          .default = financial_incentive_nc),
        
        regulatory_result_nc = case_when(
          year > year_report ~ 0,
        .default = regulatory_result_nc),
        ex_ante_roe_nc = case_when(
          year > year_report ~ 0,
          .default = ex_ante_roe_nc),
        ex_post_roe_nc = case_when(
          year > year_report ~ 0,
          .default = ex_post_roe_nc),
        actual_revenues_nc = case_when(
          year > year_report ~ 0,
          .default = actual_revenues_nc)
        ) %>% 
      mutate(year_text = as.character(year)
      ) %>% 
      select(year_text, type, regulatory_result_nc, ex_ante_roe_nc, ex_post_roe_nc, actual_revenues_nc,
             atsp_gain_loss_cost_sharing_nc,
             trs_nc,
             financial_incentive_nc)
    
    ## sum 2020-2021 together
    data_prep_202021 <- data_prep_years_split %>% 
      filter(year_text == c('2020', '2021')) %>% 
      group_by(type) %>% 
      summarise(regulatory_result_nc = sum(regulatory_result_nc, na.rm = TRUE),
                ex_ante_roe_nc = sum(ex_ante_roe_nc, na.rm = TRUE),
                ex_post_roe_nc = sum(ex_post_roe_nc, na.rm = TRUE),
                actual_revenues_nc = sum(actual_revenues_nc, na.rm = TRUE),
                
                atsp_gain_loss_cost_sharing_nc = sum(atsp_gain_loss_cost_sharing_nc, na.rm = TRUE),
                trs_nc = sum(trs_nc, na.rm = TRUE),
                financial_incentive_nc = sum(financial_incentive_nc, na.rm = TRUE)
                ) %>% 
      mutate(year_text = '2020-2021') %>% 
      relocate(year_text, .before = type)
    
    data_prep_nat_curr <- data_prep_202021 %>% 
      rbind(data_prep_years_split) %>% 
      filter(year_text != '2020' & year_text != '2021') 
    
    # get exchange rates
    yearly_xrates <- get_xrates(cztype, mycz)
    
    data_prep_xrates <- yearly_xrates %>% 
      select(-entity_code) %>% 
      filter(year > 2020) %>% 
      mutate(year_text = as.character(year),
             year_text = if_else(year_text == '2021', '2020-2021', year_text)
      ) %>% select(-year)
    
    # get tsus 
    tsus <- data_raw_t1 %>% 
      filter(entity_code == ecz_list$ecz_id[ez],
             status == 'A',
             year > 2021) %>% 
      select(year, x5_4_total_su) %>% 
      mutate(year_text = as.character(year),
             year_text = if_else(year_text == '20202021', '2020-2021', year_text)
      ) %>% select(-year)
    
    regulatory_result <- data_prep_nat_curr %>% 
      left_join(data_prep_xrates, by = "year_text") %>% 
      mutate(regulatory_result = regulatory_result_nc / pp_exchangerate,
             ex_ante_roe = ex_ante_roe_nc / pp_exchangerate,
             ex_post_roe = ex_post_roe_nc / pp_exchangerate,
             actual_revenues = actual_revenues_nc / pp_exchangerate,
             
             atsp_gain_loss_cost_sharing = atsp_gain_loss_cost_sharing_nc / pp_exchangerate,
             trs = trs_nc / pp_exchangerate,
             financial_incentive = financial_incentive_nc / pp_exchangerate
             
             ) %>% 
      select(-pp_exchangerate, -regulatory_result_nc, -ex_ante_roe_nc, -ex_post_roe_nc, -actual_revenues_nc,
             -atsp_gain_loss_cost_sharing_nc, -trs_nc, -financial_incentive_nc) %>% 
      left_join(tsus, by = "year_text")
    
    return(regulatory_result)
  }
    
  
## export figure function ----
  # the export function needs webshot and PhantomJS. Install PhantomJS with 'webshot::install_phantomjs()' and then cut the folder from wherever is installed and paste it in C:\Users\[username]\dev\r\win-library\4.2\webshot\PhantomJS

  export_fig <- function (fig, fig_name, width, height) {
    fig_dir <- paste0('images/', year_report, '/', country,'/')
    invisible(export(fig, paste0(fig_dir, fig_name)))
    invisible(figure <- image_read(paste0(fig_dir,fig_name)))
    invisible(cropped <- image_crop(figure, paste0(width, "x", height)))
    invisible(image_write(cropped, paste0(fig_dir, fig_name)))
  }

  
## universal barchart  ----
mybarchart <-  function(df, mywidth, myheight, myfont, mymargin, mydecimals) {
  df %>% 
    plot_ly(
      width = mywidth,
      height = myheight,
      x = ~ xlabel,
      y = ~ mymetric,
      yaxis = "y1",
      # marker = list(color = mymarker_color),
      colors = mycolors,
      color = ~ factor(type, levels = myfactor),
      text = ~ paste0(format(round(mymetric, mydecimals),  big.mark  = ",", nsmall = mydecimals), mysuffix),
      # text = ~ mymetric,
      textangle = mytextangle,
      textposition = mytextposition, 
      insidetextanchor = myinsidetextanchor,
      textfont = list(color = mytextfont_color, size = mytextfont_size),
      cliponaxis = FALSE,
      type = "bar",
      hovertemplate = myhovertemplate,
      # hoverinfo = "none",
      showlegend = mytrace_showlegend
    ) %>% 
    config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
            # modeBarButtons = list(list("toImage")),
    ) %>% 
    layout(
      uniformtext=list(minsize = myminsize, mode='show'),
      font = list(family = myfont_family),
      title = list(text = mytitle_text,
                   x = mytitle_x, 
                   y = mytitle_y, 
                   xanchor = mytitle_xanchor, 
                   yanchor = mytitle_yanchor,
                   font = list(size = mytitle_font_size)
      ),
      bargap = mybargap,
      barmode = mybarmode,
      hovermode = myhovermode,
      hoverlabel = list(bgcolor = myhoverlabel_bgcolor),
      xaxis = list(title = myxaxis_title,
                   gridcolor = myxaxis_gridcolor,
                   showgrid = myxaxis_showgrid,
                   showline = myxaxis_showline,
                   showticklabels = myxaxis_showticklabels,
                   dtick = myxaxis_dtick,
                   tickformat = myxaxis_tickformat,
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = myxaxis_zeroline, 
                   tickfont = list(size = myxaxis_tickfont_size)
      ),
      yaxis = list(title = myyaxis_title,
                   gridcolor = myyaxis_gridcolor,
                   showgrid = myyaxis_showgrid,
                   showline = myyaxis_showline,
                   tickprefix = myyaxis_tickprefix,
                   ticksuffix = myyaxis_ticksuffix, 
                   tickformat = myyaxis_tickformat,
                   # showticklabels = TRUE,
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = myyaxis_zeroline,
                   zerolinecolor = myyaxis_zerolinecolor,
                   titlefont = list(size = myyaxis_titlefont_size), 
                   tickfont = list(size = myyaxis_tickfont_size)
      ),
      legend = list(
        traceorder= mylegend_traceorder,
        orientation = mylegend_orientation, 
        xanchor = mylegend_xanchor,
        yanchor = mylegend_yanchor,
        x = mylegend_x,  
        y = mylegend_y, 
        font = list(size = mylegend_font_size)
      ),
      margin = mymargin
    )
}

## add empty trace to force year series  ----
add_empty_trace <- function(myplot, df){
  myplot %>%   
  add_trace(
    data = df,
    x = ~ xlabel,
    y = ~ '',
    name = "Fake series to force all years in x axis",
    yaxis = "y1",
    mode = "markers",
    type = 'scatter',
    marker = list(size = mylinewidth, color = 'transparent'),
    showlegend = F,
    # hovertemplate = '',
    hoverinfo = 'none'
    )
}

## add linetrace  ----
add_line_trace <- function(myplot, df){
  myplot %>%   
    add_trace(
      data = df,
      x = ~ xlabel,
      y = ~ myothermetric,
      yaxis = myat_yaxis,
      mode = myat_mode, 
      type = 'scatter',
      name = myat_name,
      text = ~ paste0(if_else(myat_textbold == TRUE, "<b>",""),
        format(myothermetric,  big.mark  = ",", nsmall = mydecimals), mysuffix,
        if_else(myat_textbold == TRUE, "</b>","")),
      textangle = myat_textangle,
      textposition = myat_textposition,
      textfont = list(color = myat_textfont_color, size = myat_textfont_size),
      line = list(color = myat_line_color, width = myat_line_width),
      marker = list(size = myat_line_width * 3, 
                    color = myat_marker_color,
                    symbol = myat_symbol),
      showlegend = myat_showlegend
    )
}

## plot horizontal barchart  ----
myhbarc <-  function(mywidth, myheight, myfont, mymargin) {
  data_prep %>%
    plot_ly(
      width = mywidth,
      height = myheight,
      x = ~ round(mymetric, 0),
      y = ~ factor(ylabel, levels = myfactor),
      yaxis = "y1",
      marker = list(color = ~ifelse(mymetric>=0, mybarcolor_pos, mybarcolor_neg)),
      text = ~ mylabel,
      # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
      # textangle = -90,
      textposition = "auto",
      cliponaxis = FALSE,
      orientation = 'h',
      # insidetextanchor =  "middle",
      # name = mymetric,
      textfont = list(color = mytextcolor, size = myfont),
      type = "bar",
      hovertemplate = myhovertemplate,
      # hoverinfo = "none",
      showlegend = F
    ) %>%
    config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
            # modeBarButtons = list(list("toImage")),
    ) %>%
    layout(
      font = list(family = "Roboto"),
      title = list(text = mychart_title,
                   y = 0.99,
                   x = 0,
                   xanchor = 'left',
                   yanchor =  'top',
                   font = list(size = myfont * 20/15)
      ),
      bargap = 0.25,
      barmode = 'stack',
      hovermode = "y",
      hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
      yaxis = list(title = "",
                   gridcolor = 'rgb(255,255,255)',
                   showgrid = FALSE,
                   showline = FALSE,
                   showticklabels = TRUE,
                   # dtick = 1,
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = TRUE,
                   tickfont = list(size = myfont)
      ),
      xaxis = list(title = myaxis_title,
                   # automargin = T,
                   # gridcolor = 'rgb(255,255,255)',
                   showgrid = TRUE,
                   showline = FALSE,
                   # tickprefix = if_else(" ",
                   # ticksuffix = "% ",
                   fixedrange = TRUE,
                   tickformat = myxaxis_tickformat,
                   # showticklabels = TRUE,
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = TRUE,
                   zerolinecolor = 'rgb(225,225,225)',
                   titlefont = list(size = myfont), tickfont = list(size = myfont)
      ),
      showlegend = FALSE,
      margin = mymargin

    )
}

## plot horizontal barchart  ----
mygtable <-  function(df, myfont) {
  gt(df) %>% 
  tab_options(
    table.width = '100%',
    table.font.size = myfont,
    # column_labels.vlines.color = "black", # don't know why this doesn't work
    column_labels.vlines.width = '1px',
    # column_labels.border.top.color = "#EAEAEA",
    column_labels.border.top.width = '1px',
    table.border.top.width = '1px',
    table.border.bottom.width = '1px',
    data_row.padding = '3px',
    # to disable the striped bootstrap issue
    quarto.disable_processing = TRUE)  %>% 
    cols_align(
      align = "right",
      columns = c(-1)
    )
}

