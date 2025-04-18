# set site parameters ----
  site_dir <- here("_site")
  root_dir <- if_else(test_check == TRUE, 
                      '//ihx-vdm05/LIVE_var_www_performance$/oscar/prb-monitoring-test/',
                      '//ihx-vdm05/LIVE_var_www_performance$/oscar/prb-monitoring-prod/'
                      )
  destination_dir <- paste0(root_dir, year_report, '/')
  home_address <- if_else(test_check == TRUE, 
                          'https://www.eurocontrol.int/performance/oscar/prb-monitoring-test',
                          'https://www.sesperformance.eu'
                          )
  external_address <- if_else(test_check == TRUE,
                              str_replace(destination_dir,
                                          fixed('//ihx-vdm05/LIVE_var_www_performance$'),
                                          'https://www.eurocontrol.int/performance'),
                              str_replace(destination_dir,
                                          fixed(root_dir),
                                          paste0(home_address, '/'))
                                  )
  
  destination_dir_investments <- paste0(root_dir, "investments/", "rp3/" )
  
# set graphs parameters ----
  ## web
  mywidth = NULL
  myheight = 300
  myfont = 14
  mymargin = list (t = 40)
  mylinewidth = 3
  
  mysuffix <- ""
  mydecimals <- 2
  
  ### trace parameters
  mycolors = c('#FFC000')

  mytextangle <- 0
  mytextposition <- "outside"
  myinsidetextanchor <- NA
  mytextfont_color <- 'black'
  mytextfont_size <- myfont
  
  mytrace_showlegend <- T
  
  ### layout parameters
  myfont_family <- "Roboto"
  mybargap <- 0.25
  mybarmode <- 'group'
  myhovermode <- "x unified"
  myhoverlabel_bgcolor <- 'rgba(255,255,255,0.88)'
  myminsize <- myfont*0.8
  
  #### title
  mytitle_text <- "Chart title"
  mytitle_x <- 0.5
  mytitle_y <- 0.99
  mytitle_xanchor <- 'center'
  mytitle_yanchor <- 'top'
  mytitle_font_size <- myfont * 16/15
  
  #### xaxis
  myxaxis_title <- ''
  myxaxis_gridcolor <- 'rgb(255,255,255)'
  myxaxis_showgrid <- TRUE
  myxaxis_showline <- FALSE
  myxaxis_showticklabels <- TRUE
  myxaxis_tickformat <- "0"
  myxaxis_dtick <- 1
  myxaxis_zeroline <- TRUE
  myxaxis_tickfont_size <- myfont
  
  #### yaxis
  myyaxis_title <- "Y axis title"
  myyaxis_gridcolor <- 'rgb(240,240,240)'
  myyaxis_showgrid <- TRUE
  myyaxis_showline <- FALSE
  myyaxis_tickprefix <- ""
  myyaxis_ticksuffix <- ""
  myyaxis_tickformat <- ".1f"
  
  myyaxis_zeroline <- TRUE
  myyaxis_zerolinecolor <- 'rgb(240,240,240)'
  myyaxis_titlefont_size <- myfont
  myyaxis_tickfont_size <- myfont
  
  #### legend
  mylegend_traceorder <- 'normal'
  mylegend_orientation <- 'h'
  mylegend_xanchor <- "center"
  mylegend_yanchor <- "center"
  mylegend_x <- 0.5
  mylegend_y <- -0.1
  mylegend_font_size <- myfont
  
  #### margin
  mylocalmargin = mymargin
  
  ## pdf
  mywidth_pdf = 300 *4
  myheight_pdf = 220 *4
  myfont_pdf = 64
  mytitle_font_size_pdf <- 10 *4
  mymargin_pdf = list (t = 30)
  mylinewidth_pdf = 3
    
# set state parameters  ----

  ## main state parameters  ----
  country_lower <- country %>% str_to_lower() %>% str_replace_all(., " ","-")

  state_parameters <- params_table %>% filter(state == .env$country) 
    main_ansp <- state_parameters %>% select(main_ansp) %>% pull()
    nat_curr <- state_parameters %>% select(currency) %>% pull()
    state_type <- state_parameters %>% select(dashboard_case) %>% pull()
    pp_version <- state_parameters %>% select(pp_adoption_full) %>% pull()

  ## aua entity for capacity  ----
  saf_ansps <- saf_ansp_table %>% filter(country_name == .env$country) %>% 
      filter(year >= year_report) %>% 
      filter(year == min(year)) %>% select(ansp_name, main)
    
  no_saf_ansps <- nrow(saf_ansps)
  main_safety_ansp <- saf_ansps %>% filter(main ==1) %>% select(ansp_name) %>% pull ()
  saf_ansps <- saf_ansps %>% select(ansp_name) %>% pull()
    
  ## aua entity for capacity  ----
  aua_entities <- aua_entities_table %>% filter(state == .env$country) 
    main_ansp_aua <- aua_entities %>%  filter(year == .env$year_report) %>% select(ansp_name) %>% pull()

  ## acc list for context section  ----
  acc_list <- acc_list_table %>% filter(state == .env$country)
    acc_no <- nrow(acc_list)
    acc1 <- if_else(acc_no <1, '', acc_list$acc_full_name[1])
    acc2 <- if_else(acc_no <2, '', acc_list$acc_full_name[2])
    acc3 <- if_else(acc_no <3, '', acc_list$acc_full_name[3])
    acc4 <- if_else(acc_no <4, '', acc_list$acc_full_name[4])
    acc5 <- if_else(acc_no <5, '', acc_list$acc_full_name[5])

  ## ecz list and forecast ----
  ecz_list <- ecz_list_table %>% filter(state == .env$country) 
  no_ecz <- nrow(ecz_list)
    
    ### for spain we present only one  traffic zone
    if (country == "Spain") {
      statfor_zone <- ecz_list %>% filter(statfor_ecz_name == "Spain") %>% select(statfor_ecz_name) %>% pull()
      ecz_list <- ecz_list %>% filter (ecz_name != "Spain all")
      no_ecz <- nrow(ecz_list)
    } else {
      statfor_zone <- ecz_list %>% filter(state  == country) %>% select(statfor_ecz_name) %>% pull()
    }

  forecast <- ecz_list %>% select(forecast) %>% pull() %>%  unique()
  forecast_id <- ecz_list %>% select(forecast_id) %>% pull() %>%  unique()

  ## tcz list ----
  tcz_list <- tcz_list_table %>% filter(state == .env$country) 
  no_tcz <- nrow(tcz_list)
  
  # to avoid annoing if_else errors
  if(no_tcz == 0) {tcz_list <- tcz_list <- tcz_list_table %>% 
    filter (state == "Spain") %>% mutate_all(~NA)}

  ## context data ----
  context_data <- context_data_table %>% 
    filter(state == .env$country,
           year_report == .env$year_report
         ) 

    tsu_share <- paste0(format(round(as.numeric(context_data$tsu_share) *100,1), nsmall=1),'%')
    ert_cost_share <- paste0(format(round(as.numeric(context_data$ert_cost_share) *100,1), nsmall=1),'%')
    ert_trm_share <- context_data$ert_trm_share
    
    xrate2017 <- context_data$xrate2017
    
    no_apt_big <- context_data$no_apts_big
    no_apt_small <- context_data$no_apts_small

  other_orgs <- other_orgs_table %>% filter(state == .env$country)
    other_ansps <- other_orgs %>% filter(type == "Other ANSPs") %>% select(ansp) %>% filter(ansp != '-')
    other_met <- other_orgs %>% filter(type == "MET Providers") %>% select(ansp) %>% filter(ansp != '-')

  ### create strings for context section
  other_ansps_str <- '--'
  if(nrow(other_ansps)>0) {
    for (i in 1:nrow(other_ansps)) {
      other_ansps_str <- paste0(if_else(i == 1, '', other_ansps_str),
                                '• ',
                                other_ansps[i,],
                                '<br/>')
    }
  }

  other_met_str <- '--' 
  if(nrow(other_met)>0) {
    for (i in 1:nrow(other_met)) {
      other_met_str <- paste0(if_else(i == 1, '', other_met_str),
                              '• ',
                              other_met[i,], 
                              '<br/>')
    }
  }

# get level 2 data files (not needed for SES or NM) ----
if (country != "Network Manager" & country != "SES RP3" & country != "Home") {
  
  ## get ceff file ----
  if (country != "MUAC") {
    ceff_files <- list.files(paste0(data_folder_a2, 'ceff/'))
    ceff_file_canarias <- NA
    
    for (i in 1:length(ceff_files)) {
      if (grepl(country, ceff_files[i], fixed = TRUE) == TRUE) {
        ceff_file <- ceff_files[i]
      }
      if (country == "Spain") {
        if(grepl("Spain Continental", ceff_files[i], fixed = TRUE) == TRUE) {
        ceff_file <- ceff_files[i]}
        if(grepl("Spain Canarias", ceff_files[i], fixed = TRUE) == TRUE) {
          ceff_file_canarias <- ceff_files[i]}
        }
    }
    
    file <-  paste0(data_folder_a2, "ceff/", ceff_file)
    ceff_file <-  paste0(data_folder_a2, "ceff/", ceff_file)
    ceff_file_canarias <-  paste0(data_folder_a2, "ceff/", ceff_file_canarias)
  }
  else {
    ceff_file <- NA
    ceff_file_canarias <- NA
  }
  
  # get er cap file ----
  cap_files <- list.files(paste0(data_folder_a2, 'cap/'))
  
  for (i in 1:length(cap_files)) {
    if (grepl('RP3_monitoring_CAPACITY', cap_files[i], fixed = TRUE) == TRUE) {
      cap_file <- cap_files[i]
    }
  }
  
  cap_file <-  paste0(data_folder_a2, "cap/", cap_file)
  
  # get trm cap file ----
  for (i in 1:length(cap_files)) {
    if (grepl('RP3_monitoring_CAP_ARP', cap_files[i], fixed = TRUE) == TRUE) {
      cap_trm_file <- cap_files[i]
    }
  }
  
  cap_trm_file <-  paste0(data_folder_a2, "cap/", cap_trm_file)
  
  # get env_kea file ----
  env_files <- list.files(paste0(data_folder_a2, 'env/'))
  
  for (i in 1:length(env_files)) {
    if (grepl('RP3_monitoring_KEA_VOL2', env_files[i], fixed = TRUE) == TRUE) {
      env_kea_file <- env_files[i]
    }
  }
  
  env_kea_file <-  paste0(data_folder_a2, "env/", env_kea_file)
  
  # get env_apt file ----
  for (i in 1:length(env_files)) {
    if (grepl('RP3_monitoring_ENV_ARP_VOL2', env_files[i], fixed = TRUE) == TRUE) {
      env_apt_file <- env_files[i]
    }
  }
  
  env_apt_file <-  paste0(data_folder_a2, "env/", env_apt_file)
  
  # get env_mil file
  for (i in 1:length(env_files)) {
    if (grepl('RP3_monitoring_ENV_MIL_VOL2', env_files[i], fixed = TRUE) == TRUE) {
      env_mil_file <- env_files[i]
    }
  }
  
  env_mil_file <-  paste0(data_folder_a2, "env/", env_mil_file)
  
  # get saf file ----
  saf_files <- list.files(paste0(data_folder_a2, 'saf/'))
  
  for (i in 1:length(env_files)) {
    if (grepl('_Safety', saf_files[i], fixed = TRUE) == TRUE) {
      saf_eosm_file <- saf_files[i]
    }
  }
  
  saf_eosm_file <-  paste0(data_folder_a2, "saf/", saf_eosm_file)
} else {
  cap_file <- ''
  cap_trm_file <- ''
  ceff_file <- ''
  ceff_file_canarias <- ''
  env_kea_file <- ''
  env_apt_file <- ''
  env_mil_file <- ''
  saf_eosm_file <- ''
}

