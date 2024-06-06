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
  get_xrates <- function() {
    data_raw_xrates  <-  read_xlsx(
      paste0(data_folder, "CEFF dataset master.xlsx"),
      # here("data","hlsr2021_data.xlsx"),
      sheet = "ERT_XRATE2017",
      range = cell_limits(c(1, 1), c(NA, NA))) %>%
      as_tibble() %>% 
      clean_names()  
    
    yearly_xrates <- data_raw_xrates %>% 
      filter(
        entity_code %in% ecz_list$ecz_id == TRUE
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
  
  aucu <- function(ez) {
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
  
  data_raw_t3  <-  read_xlsx(
    paste0(data_folder, "CEFF dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Enroute_T3",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----
  
  # filter raw tables on ecz
  ## t1
  data_prep_t1 <- data_raw_t1 %>% 
    filter(
      entity_code == ecz_list$ecz_id[ez],
      status == 'A'
    ) 
  
  ## t2
  data_prep_t2_ini <- data_raw_t2 %>% 
    filter(
      entity_code == ecz_list$ecz_id[ez]
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
      entity_code == ecz_list$ecz_id[ez],
      year != 'After RP' & year != 'Amounts'
    ) %>% 
    mutate(year = as.numeric(year))
  
  ## t exchange rates
  yearly_xrates <- get_xrates()
  
  data_prep_xrates <- yearly_xrates %>% 
    filter(
      entity_code == ecz_list$ecz_id[ez]
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
  
  regulatory_result <- function(ez) {
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
    
    # join t1 and t2 for joint calculations
    data_prep_years_split <- data_prep_t1 %>% 
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
      ) %>% 
      select(year_text, xlabel, mymetric)
    
    ## sum 2020-2021 together
    data_prep_202021 <- data_prep_years_split %>% 
      filter(year_text == c('2020', '2021')) %>% 
      group_by(xlabel) %>% 
      summarise(mymetric = sum(mymetric, na.rm = TRUE)) %>% 
      mutate( year_text = '2020-2021') %>% 
      relocate(year_text, .before = xlabel)
    
    data_prep_nat_curr <- data_prep_202021 %>% 
      rbind(data_prep_years_split) %>% 
      filter(year_text != '2020' & year_text != '2021') 
    
    # get exchange rates
    yearly_xrates <- get_xrates()
    
    data_prep_xrates <- yearly_xrates %>% 
      filter(
        entity_code == ecz_list$ecz_id[ez]
      ) %>% 
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
      mutate(mymetric = mymetric / pp_exchangerate) %>% 
      select(-pp_exchangerate) %>% 
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


## plot bar chart with target  ----
  mybarct <-  function(mywidth, myheight, myfont, mylinewidth, mymargin) {
    data_for_chart %>% 
      plot_ly(
        width = mywidth,
        height = myheight,
        x = ~ year,
        y = ~ target,
        yaxis = "y1",
        cliponaxis = FALSE,
        name = "",
        textfont = list(color = 'transparent'),
        type = 'scatter',  mode = 'lines',
        line = list(color = 'transparent', width = 0),
        hovertemplate = paste('%{x}:<extra></extra>'),
        # hoverinfo = "none",
        showlegend = F
      ) %>% 
      add_trace(
        inherit = FALSE,
        x = ~ year,
        y = ~ actual,
        yaxis = "y1",
        marker = list(color =('#FFC000')),
        text = ~ paste0(format(actual, nsmall = mytooltip_decimals),'%'),
        # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
        textangle = mytextangle,
        textposition = "inside", 
        cliponaxis = FALSE,
        insidetextanchor =  "middle",
        name = "Actual",
        textfont = list(color = 'black', size = myfont),
        type = "bar",
        hovertemplate = paste0(mymetric, ': %{y:.', mytooltip_decimals, 'f}%<extra></extra>'),
        # hoverinfo = "none",
        showlegend = T
      ) %>%
      add_trace(
        inherit = FALSE,
        x = ~ year,
        y = ~ target,
        yaxis = "y1",
        type = 'scatter',  mode = 'lines+markers',
        line = list(color = '#FF0000', width = mylinewidth),
        marker = list(size = mylinewidth * 3, color = mymarker_color),
        name = "Target",
        opacity = 1,
        hovertemplate = paste0('Target: %{y:.', mytooltip_decimals ,'f}%<extra></extra>'),
        # hoverinfo = "none",
        showlegend = T
      ) %>%
      add_trace(
        inherit = FALSE,
        x = ~ year,
        y = ~ target + 0.025 * max(data_for_chart$target),
        yaxis = "y1",
        mode = 'text',
        text = ~ paste0('<b>', format(target, nsmall = mytooltip_decimals) ,'%', '</b>'),
        textposition = "top", cliponaxis = FALSE,
        textfont = list(color = targetcolor, size = myfont),
        # hovertemplate = paste('<extra></extra>'),
        hoverinfo = "none",
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
        hovermode = "x unified",
        hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
        xaxis = list(title = "",
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = FALSE,
                     showline = FALSE,
                     showticklabels = TRUE,
                     dtick = mydtick,
                     tickformat = mytickformat,
                     xperiod0 = min(data_for_chart$year),
                     # tickcolor = 'rgb(127,127,127)',
                     # ticks = 'outside',
                     zeroline = TRUE,
                     tickfont = list(size = myfont)
        ),
        yaxis = list(title = myaxis_title,
                     # gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     tickprefix = " ",
                     ticksuffix = "% ",
                     tickformat = ".1f",
                     # showticklabels = TRUE,
                     # tickcolor = 'rgb(127,127,127)',
                     # ticks = 'outside',
                     zeroline = TRUE,
                     zerolinecolor = 'rgb(255,255,255)',
                     titlefont = list(size = myfont), tickfont = list(size = myfont)
        ),
        # showlegend = FALSE
        legend = list(
          orientation = 'h', 
          xanchor = "center",
          x = 0.5, 
          y =-0.1,
          font = list(size = myfont)
        ),
        margin = mymargin
        
      )
  }

## plot bar chart without target  ----
  mybarc <-  function(mywidth, myheight, myfont, mymargin) {
    data_for_chart %>% 
      plot_ly(
        width = mywidth,
        height = myheight,
        x = ~ year,
        y = 0, # to force all years in x axis
        yaxis = "y1",
        cliponaxis = FALSE,
        name = "",
        textfont = list(color = 'transparent'),
        type = "bar",
        marker = list(color =('transparent')),
        # hovertemplate = '',
        hoverinfo = "none",
        showlegend = F
      ) %>%
      add_trace(
        x = ~ year,
        y = ~ actual,
        yaxis = "y1",
        marker = list(color = mybarcolor),
        text = ~ paste0(format(actual,  big.mark  = ",", nsmall = mytooltip_decimals), myticksuffix),
        # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
        textangle = mytextangle,   #auto
        textposition = mytextposition,   #"inside", 
        cliponaxis = FALSE,
        insidetextanchor = mylabelposition, # "middle",
        name = mymetric,
        textfont = list(color = mytextcolor, size = myfont),
        type = "bar",
        hovertemplate = paste0(mymetric, ': %{y:,.', mytooltip_decimals, 'f}', myticksuffix ,'<extra></extra>'),
        # hoverinfo = "none",
        showlegend = T
        
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
        hovermode = "x unified",
        hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
        xaxis = list(title = "",
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = FALSE,
                     showline = FALSE,
                     showticklabels = TRUE,
                     dtick = mydtick,  # 1,
                     # tickcolor = 'rgb(127,127,127)',
                     # ticks = 'outside',
                     zeroline = TRUE,
                     tickfont = list(size = myfont)
        ),
        yaxis = list(title = myaxis_title,
                     # gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     tickprefix = " ",
                     ticksuffix = myticksuffix,  #"% ",
                     tickformat = mytickformat_y,  #".1f",
                     # showticklabels = TRUE,
                     # tickcolor = 'rgb(127,127,127)',
                     # ticks = 'outside',
                     zeroline = TRUE,
                     zerolinecolor = 'rgb(255,255,255)',
                     titlefont = list(size = myfont), tickfont = list(size = myfont)
        ),
        # showlegend = FALSE
        legend = list(
          orientation = 'h', 
          xanchor = "center",
          x = 0.5, 
          y =-0.1,
          font = list(size = myfont)
        ),
        margin = mymargin
        
      )
  }

  ## plot capacity annual chart  ----
  mycapchart <-  function(mywidth, myheight, myfont, mylinewidth, mymargin,
                          data_prep_target,
                          data_prep_actual,
                          mytitle,
                          myrightaxis,
                          mytrafficmetric) {
    plot_ly(
      data = data_prep_actual,
      width = mywidth,
      height = myheight,
      x = ~ year,
      y = ~ delay,
      yaxis = "y1",
      color = ~ factor(type, levels = c("Capacity", "Staffing", 
                                        "Disruptions", "Weather",
                                        "Other non-ATC")
      ),   
      colors = c('#ED7D31', '#F8CBAD', '#BF8F00', '#92D050', '#A5A5A5'),
      cliponaxis = FALSE,
      type = "bar",
      # hovertemplate = paste('KEA: %{y:.2f}%<extra></extra>'),
      # hoverinfo = "none",
      showlegend = T
    ) %>% 
      add_trace(
        data = data_prep_actual,
        inherit = FALSE,
        x = ~ year,
        y = ~ average_delay,
        yaxis = "y1",
        cliponaxis = FALSE,
        name = "Total delay",
        type = 'scatter',  mode = 'lines',
        line = list(color = 'transparent', width = 0),
        text = ~ format(round(average_delay, 2), digits = 2),
        textfont = list(color = 'black', size = myfont),
        textposition = "top center",
        hovertemplate = paste('Total delay: %{y:.2f}<extra></extra>'),
        opacity = 1,
        showlegend = F
      ) %>%
      add_trace(
        data = data_prep_target,
        inherit = FALSE,
        x = ~ year,
        y = ~ target,
        cliponaxis = FALSE,
        yaxis = "y1",
        type = 'scatter',  mode = 'lines+markers',
        line = list(color = '#FF0000', width = mylinewidth),
        marker = list(size = mylinewidth * 3, color = '#FF0000'),
        name = "Target",
        text = ~ paste0 ("<b>", format(target, digits = 2), "</b>"),
        textfont = list(color = '#FF0000', size = myfont),
        textposition = "top center",
        hovertemplate = paste('Target: %{y:.2f}<extra></extra>'),
        opacity = 1,
        showlegend = T
      ) %>%
      add_trace(
        data = data_prep_actual,
        inherit = FALSE,
        x = ~ year,
        y = ~ round(movements/1000, 0),
        yaxis = "y2",
        type = 'scatter',  mode = 'lines+markers',
        line = list(color = '#FFC000', width = mylinewidth),
        marker = list(size = mylinewidth * 3, color = '#FFC000'),
        name = mytrafficmetric,
        hovertemplate = paste("IFR mvts ('000): %{y:,}<extra></extra>"),
        opacity = 1,
        showlegend = T
      ) %>%
      config( responsive = TRUE,
                      displaylogo = FALSE,
                      displayModeBar = F
                      # modeBarButtons = list(list("toImage")),
      ) %>%
      layout(
        font = list(family = "Roboto"),
        title = list(text = mytitle,
                     y = 0.99, 
                     x = 0, 
                     xanchor = 'left', 
                     yanchor =  'top',
                     font = list(size = myfont * 20/15)
        ),
        bargap = 0.25,
        barmode = 'stack',
        hovermode = "x unified",
        hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
        xaxis = list(title = "",
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = FALSE,
                     showline = FALSE,
                     showticklabels = TRUE,
                     dtick = 1,
                     zeroline = TRUE,
                     tickfont = list(size = myfont)
        ),
        yaxis = list(title = "Average minutes of delay",
                     showgrid = TRUE,
                     showline = FALSE,
                     tickformat = ".2f",
                     rangemode = "nonnegative",
                     zeroline = TRUE,
                     zerolinecolor = 'rgb(255,255,255)',
                     titlefont = list(size = myfont), tickfont = list(size = myfont)
        ),
        yaxis2 = list(title = myrightaxis,
                      overlaying = "y",
                      side = "right",
                      showgrid = FALSE,
                      showline = FALSE,
                      tickformat = ",",
                      rangemode = "nonnegative",
                      zeroline = TRUE,
                      zerolinecolor = 'rgb(255,255,255)',
                      titlefont = list(size = if_else(country == 'SES RP3', 1, myfont), 
                                       color = if_else(country == 'SES RP3', 'transparent', 'black')
                      ), 
                      tickfont = list(size = if_else(country == 'SES RP3', 1, myfont),
                                      color = if_else(country == 'SES RP3', 'transparent', 'black')
                      )
        ),
        # showlegend = FALSE
        legend = list(
          orientation = 'h', 
          xanchor = "left",
          x = mylegend_x_pos, 
          y =-0.1,
          font = list(size = myfont*0.9)
        ),
        margin = mymargin
        
      )
  }
 
  ## plot capacity monthly chart  ----
  
  mycapchart_month <-  function(mywidth, myheight, myfont, mylinewidth, mymargin) {
    plot_ly(
      data = data_prep_actual,
      width = mywidth,
      height = myheight,
      x = ~ month,
      y = ~ delay,
      yaxis = "y1",
      color = ~ factor(type, levels = c("Capacity", "Staffing", 
                                        "Disruptions", "Weather",
                                        "Other non-ATC")
      ),   
      colors = c('#ED7D31', '#F8CBAD', '#BF8F00', '#92D050', '#A5A5A5'),
      cliponaxis = FALSE,
      type = "bar",
      # hovertemplate = paste('KEA: %{y:.2f}%<extra></extra>'),
      # hoverinfo = "none",
      showlegend = T
    ) %>% 
      add_trace(
        data = data_prep_actual,
        inherit = FALSE,
        x = ~ month,
        y = ~ average_delay,
        yaxis = "y1",
        cliponaxis = FALSE,
        name = "Total delay",
        type = 'scatter',  mode = 'lines',
        line = list(color = 'transparent', width = 0),
        text = ~ format(round(average_delay, 2), digits = 2),
        textfont = list(color = 'black', size = myfont*0.9),
        textposition = "top center",
        hovertemplate = paste('Total delay: %{y:.2f}<extra></extra>'),
        opacity = 1,
        showlegend = F
      ) %>%
      config( responsive = TRUE,
                      displaylogo = FALSE,
                      displayModeBar = F
                      # modeBarButtons = list(list("toImage")),
      ) %>%
      layout(
        font = list(family = "Roboto"),
        title = list(text = mytitle,
                     y = 0.99, 
                     x = 0, 
                     xanchor = 'left', 
                     yanchor =  'top',
                     font = list(size = myfont * 20/15)
        ),
        bargap = 0.25,
        barmode = 'stack',
        hovermode = "x unified",
        hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
        xaxis = list(title = "",
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = FALSE,
                     showline = FALSE,
                     showticklabels = TRUE,
                     tickformat = "%b",
                     tick0 = min(data_prep_actual$month),
                     dtick = "M1",
                     zeroline = TRUE,
                     tickfont = list(size = myfont)
        ),
        yaxis = list(title = "Average minutes of delay",
                     showgrid = TRUE,
                     showline = FALSE,
                     tickformat = ".2f",
                     rangemode = "nonnegative",
                     zeroline = TRUE,
                     zerolinecolor = 'rgb(255,255,255)',
                     titlefont = list(size = myfont), tickfont = list(size = myfont)
        ),
        legend = list(
          orientation = 'h', 
          xanchor = "center",
          x = 0.5, 
          y =-0.1,
          font = list(size = myfont*0.9)
        ),
        margin = mymargin
        
      )
  }
  
  ## plot CEF non-stacked bar chart  ----
  mybarc_nonst <-  function(mywidth, myheight, myfont, mymargin) {
    data_prep %>% 
      plot_ly(
        width = mywidth,
        height = myheight,
        x = ~ year_text,
        y = ~ mymetric,
        yaxis = "y1",
        text = ~ paste0(format(mymetric, nsmall = 2), myticksuffix),
        textangle = mytextangle,
        textposition = mytextposition, 
        cliponaxis = FALSE,
        insidetextanchor =  mylabelposition,
        textfont = list(color = 'black', size = mytextsize),
        type = "bar",
        color = ~ factor(status, levels = myfactor),
        colors = mycolors,
        hovertemplate = paste0('%{y:.2f}', myticksuffix),
        # hoverinfo = "none",
        showlegend = T
      ) %>%
    add_trace(           ### series to force the full x series of years
      inherit = FALSE,
      data = data_prep,
      x = ~ year_text,
      y = 0,
      yaxis = "y1",
      type = 'scatter',
      mode = "markers",
      # line = list(width = 0),
      marker = list(color = 'transparent'),
      hoverinfo = "none",
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
        uniformtext=list(minsize = myfont*0.8, mode='show'),
        bargap = 0.25,
        hovermode = "x unified",
        hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
        xaxis = list(title = "",
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = FALSE,
                     showline = FALSE,
                     showticklabels = TRUE,
                     dtick = mydtick,
                     tickformat = mytickformat_x,
                     # range = myrange,
                     # tickvals = myticktext,
                     # tickcolor = 'rgb(127,127,127)',
                     # ticks = 'outside',
                     zeroline = TRUE,
                     tickfont = list(size = myfont)
        ),
        yaxis = list(title = myaxis_title,
                     # gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     ticksuffix = myticksuffix,
                     tickformat = mytickformat_y,
                     # showticklabels = TRUE,
                     # tickcolor = 'rgb(127,127,127)',
                     # ticks = 'outside',
                     zeroline = TRUE,
                     zerolinecolor = 'rgb(255,255,255)',
                     titlefont = list(size = myfont), tickfont = list(size = myfont)
        ),
        # showlegend = FALSE
        legend = list(
          orientation = 'h', 
          xanchor = "center",
          x = 0.5, 
          y = mylegend_y_position,
          font = list(size = myfont)
        ),
        margin = mymargin
      )
    
  }
  
  ## plot CEF horizontal barchart A-D  ----
  myhbarc <-  function(mywidth, myheight, myfont, mymargin) {
    data_prep %>% 
      plot_ly(
        width = mywidth,
        height = myheight,
        x = ~ round(mymetric, 0),
        y = ~ factor(ylabel, levels = myfactor),
        yaxis = "y1",
        marker = list(color = mybarcolor),
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
        hovertemplate = paste0('%{y} (A-D): %{x:+0,}<extra></extra>'),
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
                     # gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     # tickprefix = if_else(" ",
                     # ticksuffix = "% ",
                     tickformat = "+0,",
                     # showticklabels = TRUE,
                     # tickcolor = 'rgb(127,127,127)',
                     # ticks = 'outside',
                     zeroline = TRUE,
                     zerolinecolor = 'rgb(255,255,255)',
                     titlefont = list(size = myfont), tickfont = list(size = myfont)
        ),
        showlegend = FALSE,
        margin = mymargin
        
      )
  }
  
  ## plot grouped/stacked barchart  ----
  mybarc_group <-  function(mywidth, myheight, myfont, mymargin) {
    data_prep %>% 
      plot_ly(
        width = mywidth,
        height = myheight,
        y = ~ round(mymetric, mytooltip_decimals),
        x = ~ year_text,
        yaxis = "y1",
        colors = mybarcolor,
        # color = ~ xlabel,
        color = ~ factor(xlabel, levels = myfactor),
        # text = ~ mylabel,
        # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
        # textangle = -90,
        textposition = "auto", 
        cliponaxis = FALSE,
        # insidetextanchor =  "middle",
        # name = mymetric,
        textfont = list(color = mytextcolor, size = myfont),
        type = "bar",
        # hovertemplate = paste0('%{y} (A-D): %{x:+0,}<extra></extra>'),
        # hoverinfo = "none",
        showlegend = T
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
        hovermode = "x unified",
        hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
        xaxis = list(title = "",
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
        yaxis = list(title = myaxis_title,
                     # gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     # tickprefix = if_else(" ",
                     # ticksuffix = "% ",
                     tickformat = "0,",
                     # showticklabels = TRUE,
                     # tickcolor = 'rgb(127,127,127)',
                     # ticks = 'outside',
                     zeroline = TRUE,
                     zerolinecolor = 'rgb(200,200,200)',
                     titlefont = list(size = myfont), tickfont = list(size = myfont)
        ),
        legend = list(
          orientation = 'h', 
          traceorder= 'normal',
          xanchor = mylegend_x_anchor ,  # "left"
          x = mylegend_x_position,       # 0
          y = mylegend_y_position,
          font = list(size = mylegend_fontsize)
        ),
        margin = mymargin
        
      )
  }
  