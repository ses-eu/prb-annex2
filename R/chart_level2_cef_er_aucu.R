
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
      aucu
    ) %>% 
    arrange(year_text)
  
  ## calculate values 2020-2021 as a sum of the individual years
  data_prep_20202021 <- data_prep_years_split %>% 
    filter(year_text == '2020' | year_text == '2021') %>%
    bind_rows(summarise(., across(where(is.numeric), sum),
                        across(where(is.character), ~'2020-2021'))) %>% 
    filter(year_text == '2020-2021')
  
  ## join prep tables with the relevant years
  data_prep <- data_prep_20202021 %>% 
    rbind(filter(data_prep_years_split, year_text != '2020' & year_text != '2021'))

  ## select relevant values for chart
  data_for_chart <- data_prep %>% 
    select(
      year_text,
      new_duc,
      total_adjustments_aucu,
      aucu
    ) 

  ## chart parameters ----
  mychart_title <- paste0("AUCU")
  myaxis_title <- "AUCU (€/SU)"
  mybarcolor <- c( '#5B9BD5', 'transparent', '#BFBFBF', '#9DC3E6')
  mytextcolor <- 'black'
  mylegend_y_position <- -0.28
  mymargin = list (t = 60, b = 80)
  
  ## define chart function ----
  mybarc_aucu <-  function(mywidth, myheight, myfont, mymargin) {
    mychart <- list()
    data_for_chart_filtered <- list()
    
    ### loop through years ----
    for (i in 1:nrow(data_for_chart)) {
       # i=2
      ### prepare data for chart ----
      data_for_chart_filtered[[i]] <- data_for_chart %>% 
        slice(i:i) %>% 
        pivot_longer(-c(year_text), names_to = 'type', values_to = 'metric')  %>% 
        mutate(
          mydatalabel = case_when(
            type == 'total_adjustments_aucu' ~ if_else(is.na(metric) == TRUE, 
                                                    NA, 
                                                    paste0(if_else(metric >= 0, '+', ''),
                                                           trimws(format(round(metric, 2), big.mark = ",", nsmall = 2)))
                                                    ),
            .default = if_else(is.na(metric) == TRUE, NA, format(round(metric, 2), big.mark = ",", nsmall = 2))
            ),
          new_duc = case_when(
              type == 'new_duc' ~ metric,
              .default = NA),
          total_adjustments_aucu = case_when(
              type == 'total_adjustments_aucu' ~ abs(metric),
              .default = NA),      
          aucu = case_when(
              type == 'aucu' ~ metric,
              .default = NA),
          fake_series = case_when(
              type == 'total_adjustments_aucu' ~ min(c(aucu, new_duc), na.rm = TRUE),
              .default = NA)
          ) %>% 
        relocate(fake_series, .after = new_duc) %>% 
        select(-metric) %>% 
        pivot_longer(-c(year_text, type, mydatalabel), names_to ='subtype', values_to = 'metric') %>% 
        mutate(mydatalabel = case_when(
          subtype == 'fake_series' ~ NA,
          .default = mydatalabel
            ))
    
      ### plot indivicual year charts ----
      mychart[[i]] <- data_for_chart_filtered[[i]] %>% 
      plot_ly(
        width = mywidth,
        height = myheight,
        y = ~ round(metric, 2),
        x = ~ factor(type, levels = c('new_duc', 'total_adjustments_aucu',
                                      'aucu')),
        yaxis = "y1",
        type = 'bar',
        color = ~ factor(subtype, levels = c('new_duc', 'fake_series',
                                                       'total_adjustments_aucu',
                                                       'aucu')),
        colors = mybarcolor,
        text = ~ mydatalabel,
        textangle = -90,
        textposition = "outside", 
        cliponaxis = FALSE,
        # insidetextanchor =  "middle",
        # name = mymetric,
        textfont = list(color = mytextcolor, size = myfont * 0.8),
        # hovertemplate = paste0('%{y} (A-D): %{x:+0,}<extra></extra>'),
        hoverinfo = "none",
        showlegend = FALSE
      ) %>% 
      # add_trace(
      #   # data = filter(data_for_chart_filtered[[i]], subtype != 'fake_series'),
      #   y = 0,
      #   x = 'total_adjustments_aucu',
      #   yaxis = "y1",
      #   type = 'scatter',  mode = 'markers',
      #   marker = list(size = mylinewidth, color = 'transparent'),
      #   # color = ~ factor(subtype, levels = c('new_duc', 'fake_series',
      #   #                                      'total_adjustments_aucu',
      #   #                                      'aucu')),
      #   # text = ~ mylabel,
      #   # text = ~ as.character(format(round(VALUE,0), big.mark = " ")),
      #   # textangle = -90,
      #   textposition = "none",
      #   cliponaxis = FALSE,
      #   # insidetextanchor =  "middle",
      #   # name = mymetric,
      #   textfont = list(color = 'transparent'),
      #   hovertemplate = paste0('%{x}: %{y}<extra></extra>'),
      #   # hoverinfo = "none",
      #   showlegend = F
      # ) %>%
      layout(
        showlegend = F,  
        barmode = "stack",
        bargap = '0',
        xaxis = list(title = '',
                     gridcolor = 'rgb(255,255,255)',
                     showgrid = FALSE,
                     showline = TRUE,
                     showticklabels = FALSE,
                     # dtick = 1,
                     # tickcolor = 'rgb(127,127,127)',
                     # ticks = 'outside',
                     # font = list(size = 1, color = 'transparent'),
                     zeroline = TRUE
        )
      )
      
    }

    ### group year charts ----
    subplot(mychart[[1]], mychart[[2]], mychart[[3]], mychart[[4]],
            titleX = TRUE, shareY = T) %>% 
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
        hovermode = "x",
        hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
        # uniformtext = list(minsize = myfont, mode='show'),
        yaxis = list(title = myaxis_title,
                     # gridcolor = 'rgb(255,255,255)',
                     showgrid = TRUE,
                     showline = FALSE,
                     # tickprefix = if_else(" ",
                     # ticksuffix = "% ",
                     tickformat = "0, ",
                     tickcolor='white',     # to increase space between tick and plot
                     ticklen = 7,
                     # showticklabels = TRUE,
                     # ticks = 'outside',
                     zeroline = TRUE,
                     zerolinecolor = 'rgb(255,255,255)',
                     titlefont = list(size = myfont), tickfont = list(size = myfont)
        ),
        # legend = list(
        #   orientation = 'h', 
        #   xanchor = "left",
        #   x = -0.1, 
        #   y = -0.5,
        #   font = list(size = myfont*0.9)
        #   ),
        # couldn't get the legend working so I had to resort to this
        annotations = list(
          list (
            xanchor = "center",
            x = 0.125,
            y = -0.15,
            text = '2020-2021',
            font = list(size = myfont),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          ),
          list (
            xanchor = "center",
            x = 0.375,
            y = -0.15,
            text = '2022',
            font = list(size = myfont),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          ), 
          list (
            xanchor = "center",
            x = 0.625,
            y = -0.15,
            text = '2023',
            font = list(size = myfont),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          ), 
          list (
            xanchor = "center",
            x = 0.875,
            y = -0.15,
            text = '2024',
            font = list(size = myfont),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          ), 
          list (
            xanchor = "left",
            x = 0.22,
            y = mylegend_y_position,
            text = '■',
            font = list(size = myfont * 1.2, color = '#5B9BD5'),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          ), 
          list (
            xanchor = "left",
            x = 0.26,
            y = mylegend_y_position,
            text = 'DUC',
            font = list(size = myfont),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          ),
          list (
            xanchor = "left",
            x = 0.35,
            y = mylegend_y_position,
            text = '■',
            font = list(size = myfont * 1.2, color = '#9DC3E6'),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          ), 
          list (
            xanchor = "left",
            x = 0.39,
            y = mylegend_y_position,
            text = 'AUCU',
            font = list(size = myfont),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          ),
          list (
            xanchor = "left",
            x = 0.50,
            y = mylegend_y_position,
            text = '■',
            font = list(size = myfont * 1.2, color = '#BFBFBF'),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          ), 
          list (
            xanchor = "left",
            x = 0.54,
            y = mylegend_y_position,
            text = 'Total adjustments',
            font = list(size = myfont),
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            # arrowhead = 7,
            ax = 0,
            ay = 0
          )
          ),
        
        margin = mymargin
        
      )
  }
  
  ## plot chart  ----
  myplot[[ez]] <- mybarc_aucu(mywidth, myheight+40, myfont, mymargin)
}

# create html plotlist ----
htmltools::tagList(myplot)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document



