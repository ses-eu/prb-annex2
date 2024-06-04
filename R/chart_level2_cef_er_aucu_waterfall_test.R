
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

  data_raw_t3  <-  read_xlsx(
    paste0(data_folder, "CEFF dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Enroute_T3",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----

  # filter raw tables on ecz
  data_prep_t1 <- data_raw_t1 %>% 
    filter(
      entity_code == ecz_list$ecz_id[ez],
      status == 'A'
    ) 
  
  data_prep_t2 <- data_raw_t2 %>% 
    filter(
      entity_code == ecz_list$ecz_id[ez]
    ) 
  
  data_prep_t3 <- data_raw_t3 %>% 
    filter(
      entity_code == ecz_list$ecz_id[ez],
      year != 'After RP' & year != 'Amounts'
    ) %>% 
    mutate(year = as.numeric(year))
  
  #join all tables
  data_prep_all <- data_prep_t1 %>% 
    left_join(data_prep_t2, by = 'year', suffix = c(".t1", ".t2")) %>% 
    left_join(data_prep_t3, by = 'year', suffix = c("", ".t3"))
    
  
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
  data_prep <- data_prep_all %>% 
    mutate(
      initial_duc = case_when(
        year == 2020 ~ (initial_duc_2020 - (total_adjustment/x15_forecast_su_temp)) * tsu_2020/(tsu_2020 + tsu_2021),
        year == 2021 ~ (initial_duc_2021 - (total_adjustment/x15_forecast_su_temp)) * tsu_2021/(tsu_2020 + tsu_2021),
        year == 20202021 ~ (initial_duc_2021 - (total_adjustment/x15_forecast_su_temp)) * tsu_2021/(tsu_2020 + tsu_2021),
        .default = if_else(x8_1_temp_unit_rate >0,
                                 x8_1_temp_unit_rate - (total_adjustment/x15_forecast_su_temp),
                                 x4_2_cost_excl_vfr/x5_4_total_su)
        ),
      
      new_duc = case_when(
        year == 2020 | year == 2021 | year == 20202021 ~ x4_2_cost_excl_vfr / tsu_20202021,
        .default = x4_2_cost_excl_vfr / x5_4_total_su
      ),
      retro_ur = new_duc - initial_duc,
      
      infl_adj = x2_5_adjust_inflation / x4_7_total_su,
      dif_a_d_costs = x3_8_diff_det_cost_actual_cost / x4_7_total_su,
      trs_adj = x4_9_adjust_traffic_risk_art_27_2 / x4_7_total_su,
      dc_notrs = x5_1_det_cost_no_traffic_risk / x4_7_total_su,
      fin_inc = x6_4_financial_incentive / x4_7_total_su,
      rev_c_mod = x7_1_adj_revenue_charge_modulation / x4_7_total_su,
      cross_fin = x9_1_cross_financing_other / x4_7_total_su,
      other_rev = x10_5_other_revenue / x4_7_total_su,
      loss_rev = x11_1_loss_revenue_lower_unit_rate / x4_7_total_su,
      
      total_adjustments_aucu = infl_adj + dif_a_d_costs + trs_adj + dc_notrs + 
        fin_inc + rev_c_mod + cross_fin + other_rev + loss_rev,
      aucu = new_duc + total_adjustments_aucu,
      check_adj = (x12_total_adjust - x8_2_diff_revenue_temp_unit_rate -
                     x5_2_unit_rate_no_traffic_risk) / x4_7_total_su
      
    ) %>% 
    
    select(
      year,
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
      aucu
    ) %>% 
    filter(year > 2021) %>% 
    mutate(
      year_text = as.character(year),
      year_text = str_replace_all(year, '20202021', '2020-2021')
    ) %>% 
    arrange(year_text)

  data_for_chart <- data_prep %>% 
    select(
      year_text,
      new_duc,
      total_adjustments_aucu,
      aucu
    ) %>% 
    pivot_longer( -year_text, names_to = 'name', values_to = 'value') %>% 
    mutate(name = factor(name, levels = c('new_duc',
                                          'total_adjustments_aucu',
                                          'aucu')))

  ## chart parameters ----
  mychart_title <- paste0("AUCU")
  myaxis_title <- "AUCU (€/SU)"
  mybarcolor <- c( '#5B9BD5', '#BFBFBF', '#9DC3E6')
  mytextcolor <- 'black'
  mylegend_y_position <- -0.2
  
  ## define chart function ----
  mybarc_group <-  function(mywidth, myheight, myfont, mymargin) {
    chart1 <- data_for_chart %>% 
      filter(year_text == '2020-2021') %>% 
      plot_ly(
        width = mywidth,
        height = myheight,
        y = ~ round(value, 2),
        x = ~ name,
        yaxis = "y1",
        type = 'waterfall',
        measure = c("relative", "relative", "total"),
        base = 0,
        decreasing = list(marker = list(color = '#BFBFBF')),
        increasing = list(marker = list(color = '#5B9BD5')),
        totals = list(marker = list(color = '#9DC3E6')),        # text = ~ mylabel,
        # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
        # textangle = -90,
        textposition = "auto", 
        cliponaxis = FALSE,
        # insidetextanchor =  "middle",
        # name = mymetric,
        textfont = list(color = mytextcolor, size = myfont),
        # type = "bar",
        # hovertemplate = paste0('%{y} (A-D): %{x:+0,}<extra></extra>'),
        # hoverinfo = "none",
        showlegend = T
      ) %>% 
      layout(
        showlegend = F,  
        waterfallgap = "0",
        xaxis = list(title = "",
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = FALSE,
                     showline = TRUE,
                     showticklabels = FALSE,
                     # dtick = 1,
                     # tickcolor = 'rgb(127,127,127)',
                     # ticks = 'outside',
                     zeroline = TRUE
        )
      )
    

    chart2 <- data_for_chart %>% 
      filter(year_text == '2022') %>% 
      plot_ly(
        width = mywidth,
        height = myheight,
        y = ~ round(value, 2),
        x = ~ name,
        yaxis = "y1",
        type = 'waterfall',
        measure = c("relative", "relative", "total"),
        base = 0,
        decreasing = (marker =list(color = '#5B9BD5')),
        increasing = (marker =list(color = '#BFBFBF')),
        totals = (marker =list(color = '#9DC3E6')),
        # text = ~ mylabel,
        # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
        # textangle = -90,
        textposition = "auto", 
        cliponaxis = FALSE,
        # insidetextanchor =  "middle",
        # name = mymetric,
        textfont = list(color = mytextcolor, size = myfont),
        # type = "bar",
        # hovertemplate = paste0('%{y} (A-D): %{x:+0,}<extra></extra>'),
        # hoverinfo = "none",
        showlegend = T
      ) %>% 
      layout(
        showlegend = F,  
        waterfallgap = "0",
        xaxis = list(title = "",
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = FALSE,
                     showline = TRUE,
                     showticklabels = FALSE,
                     # dtick = 1,
                     # tickcolor = 'rgb(127,127,127)',
                     # ticks = 'outside',
                     zeroline = TRUE
        )
      )
    
    subplot(chart1, chart2,  titleX = TRUE, shareY = T) %>% 
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
        # bargap = 0.25,
        hovermode = "x unified",
        hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
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
                     zerolinecolor = 'rgb(255,255,255)',
                     titlefont = list(size = myfont), tickfont = list(size = myfont)
        ),
        legend = list(
          orientation = 'h', 
          xanchor = "left",
          x = -0.1, 
          y = mylegend_y_position,
          font = list(size = myfont*0.9)
          ),
        margin = mymargin
        
      )
  }
  
  ## plot chart  ----
  myplot[[ez]] <- mybarc_group(mywidth, myheight+40, myfont, mymargin)
  myplot[[ez]]
}

# create html plotlist ----
htmltools::tagList(myplot)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document
