# country <- 'Network Manager'
# source("R/parameters_new.R")

# remove .qmd files ----
  ## get file list ----
  root_files <- list.files()

  ## remove all qmd files  ----
  for (i in 1:length(root_files)) {
    if (grepl('.qmd', root_files[i], fixed = TRUE) == TRUE) {
      file.remove(root_files[i])
    }
  }

  
# replace index, quarto yaml and css files ----
  file.copy('_original_files/full_quarto.yml', '_quarto.yml', overwrite = TRUE, copy.mode = TRUE)
  
  file.copy('_original_files/full_styles.css', 'styles.css', overwrite = TRUE, copy.mode = TRUE)
  
  
  if (country == "Home") {
    file.copy('_original_files/home_index.qmd', 'index.qmd', overwrite = TRUE, copy.mode = TRUE)
    ##for the home page we add as well the other qmds here
    file.copy('_original_files/home_about.qmd', 'about.qmd', overwrite = TRUE, copy.mode = TRUE)
    file.copy('_original_files/home_disclaimer.qmd', 'disclaimer.qmd', overwrite = TRUE, copy.mode = TRUE)
    
    ## we also remove one line from the css file
    tmp_text <- readLines("_original_files/full_styles.css")
    tmp_text <- str_replace(tmp_text, fixed("  font-size: 0.9rem;"), "") 
    writeLines(tmp_text, 'styles.css')

  } else if (country == "Network Manager") {
    tmp_text <- readLines("_original_files/common_qmd_setup.qmd")
    tmp_text <- str_replace(tmp_text, "file-placeholder", "_original_files/nm_index.qmd") 
    writeLines(tmp_text, 'index.qmd')

  } else if (country == "SES RP3") {
      tmp_text <- readLines("_original_files/common_qmd_setup.qmd")
      tmp_text <- str_replace(tmp_text, "file-placeholder", "_original_files/ses_index.qmd") 
      writeLines(tmp_text, 'index.qmd')
      
  } else {
      tmp_text <- readLines("_original_files/common_qmd_setup.qmd")
      tmp_text <- str_replace(tmp_text, "file-placeholder", "_original_files/state_index.qmd") 
      writeLines(tmp_text, 'index.qmd')
      
      # generate level 2 .qmd master files (some will be removed later depending on the state case)
      level2_files <- list("capacity_er.qmd",
                           "cost-efficiency-er1-1.qmd",
                           "cost-efficiency-er1-2.qmd",
                           "cost-efficiency-er1-3.qmd",   #this one might be removed later depending on check
                           "environment_kea.qmd",
                           "environment_mil.qmd",
                           "safety.qmd")
      
      if (state_type == 1 | state_type == 3) {level2_files <- c(level2_files, c("cost-efficiency-g2g.qmd",
                                                    "cost-efficiency-tz1-1.qmd",
                                                    "cost-efficiency-tz1-2.qmd",
                                                    "cost-efficiency-tz1-3.qmd", #this one might be removed later depending on check
                                                    "environment_apt.qmd",
                                                    "capacity_trm.qmd"))
      } else if (state_type == 2) {level2_files <- c(level2_files, c("cost-efficiency-g2g.qmd",
                                                          "cost-efficiency-tz1-1.qmd",
                                                          "cost-efficiency-tz1-2.qmd",
                                                          "cost-efficiency-tz1-3.qmd",
                                                          "cost-efficiency-tz2-1.qmd",
                                                          "cost-efficiency-tz2-2.qmd",
                                                          "cost-efficiency-tz2-3.qmd",   #xxx
                                                          "environment_apt.qmd",
                                                          "capacity_trm.qmd"))
      }
      
      for (i in 1:length(level2_files)) {
        tmp_text <- readLines("_original_files/common_qmd_setup.qmd")
        tmp_text <- str_replace(tmp_text, "file-placeholder", paste0("_", level2_files[i])) 
        writeLines(tmp_text, level2_files[[i]])
      }
    }


# find all scripts that generate .qmd files ----
  ## get all R scripts ----
  rscripts <- list.files(here('R'))

  ## get only 'generate' scripts ----
  genscripts <- list()
  for (i in 1:length(rscripts)) {
    if (grepl('generate', rscripts[i], fixed = TRUE) == TRUE) {
      genscripts <- append(genscripts, rscripts[i])
    }
  }

# remove and regenerate variables ----
  file.remove('_variables.yml')
  newvariables <- paste0("doc:
    year_report: ", year_report, "
    country: '", country, "'
    country_lower: '", country_lower, "'
  "
  )
  
  cat(newvariables, file = "_variables.yml")

# modify _quarto.yml ----
  tx  <- readLines("_quarto.yml")
  ## replace string by country name as {{< var doc.country >}} gives problems in small screens
  tx <- str_replace(tx, 'country placeholder', country)
  ## set destination directory
  tx <- str_replace(tx, 'destination-dir/', external_address)
  ## set home address for other year reports
  tx <- str_replace(tx, 'home_address', home_address)
  tx <- str_replace(tx, 'country_lower', country_lower)  
  ## add check box to year report
  tx <- str_replace(tx, paste0('text: "', year_report, '"'),
                        paste0('text: "<span style= \'color: #2151bf\'>', year_report, ' &#10003</span>"'))
  
  ## Home page case ----
  if (country == "Home") {
    ### find beginning and end of blocks to remove
    for (i in 1:length(tx)) {
      if (tx[i] %like% 'sidebar:') {block_beg = i}
      if (tx[i] %like% 'block level2 end') {block_end = i}
    }
    ### this removes the unwanted lines
    tx <- tx[-c(block_beg:block_end)]

    ### write new file
    writeLines(tx, con="_quarto.yml")
    
  } else if (country == "Network Manager" | country == "SES RP3") {
    ## NM, SES case ----
    ## remove bottom page navigation
    # tx <- str_replace(tx, '  page-navigation: true', '  page-navigation: false')
    
    ### find beginning and end of level 1 state blocks
    for (i in 1:length(tx)) {
      if (tx[i] %like% 'block level1 state beginning') {block_l1_sta_beg = i}
      if (tx[i] %like% 'block level1 state end') {block_l1_sta_end = i}
    }
    ### this removes the unwanted lines
    tx <- tx[-c(block_l1_sta_beg:block_l1_sta_end)]
    
    ### find beginning and end of level 1 nm,ses blocks
    for (i in 1:length(tx)) {
      if (tx[i] %like% 'block level1 ses beginning') {block_l1_ses_beg = i}
      if (tx[i] %like% 'block level1 ses end') {block_l1_ses_end = i}
      
      if (tx[i] %like% 'block level1 nm beginning') {block_l1_nm_beg = i}
      if (tx[i] %like% 'block level1 nm end') {block_l1_nm_end = i}
    }
    ### this removes the unwanted lines
    if (country == "Network Manager") {
      tx <- tx[-c(block_l1_ses_beg:block_l1_ses_end)]
    } else {tx <- tx[-c(block_l1_nm_beg:block_l1_nm_end)]}

    ### find beginning and end of level 2 block to remove
    for (i in 1:length(tx)) {
      if (tx[i] %like% 'block level2 beginning') {block_l2_beg = i}
      if (tx[i] %like% 'block level2 end') {block_l2_end = i}
    }
    ### this removes the unwanted lines
    tx <- tx[-c(block_l2_beg:block_l2_end)]
    
    ### write new file
    writeLines(tx, con="_quarto.yml")
    
 } else {
  ## state case ----
   ### find beginning and end of level 1 blocks to remove
   for (i in 1:length(tx)) {
     if (tx[i] %like% 'block level1 ses beginning') {block_l1_ses_beg = i}
     if (tx[i] %like% 'block level1 nm end') {block_l1_nm_end = i}
   }
   ### this removes the unwanted lines
   tx <- tx[-c(block_l1_ses_beg:block_l1_nm_end)]
   
   ### no terminal zone case ----
    if (state_type == 0) {
      genscripts <- list('generate_saf_qmd.R',
                         'generate_env_kea_qmd.R',
                         'generate_env_mil_qmd.R',
                         'generate_cap_er_qmd.R',
                         'generate_ceff_er1_qmd.R',
                         'generate_ceff_er2_qmd.R',
                         'generate_ceff_er3_qmd.R')
    
      tx <- str_replace(tx, "- capacity_trm.qmd", "  # - capacity_trm.qmd")
      tx <- str_replace(tx, "- environment_apt.qmd", "  # - environment_apt.qmd")
      
      #### it's easier just to remove the lines
      for (i in 1:length(tx)) {
        if (tx[i] %like% 'cost-efficiency-tz1-1.qmd') {block_beg = i}
        if (tx[i] %like% 'cost-efficiency-g2g.qmd') {block_end = i}
      }
      ### this removes the unwanted lines
      tx <- tx[-c(block_beg:block_end)]
 
    } else if (state_type == 1) {
    
    ### with terminal zone case ----
      for (i in 1:length(tx)) {
        if (tx[i] %like% '# block tcz2') {block_beg = i}
        if (tx[i] %like% 'cost-efficiency-tz2-3.qmd') {block_end = i}
      }
      tx <- tx[-c(block_beg:block_end)]

    ### check if there are other term ATSPs ----
      sheet <- "8_TRM_ATSP"
      range <- "C13:M17" 
      trm_2_14_1  <- read_range(file, sheet, range) %>% select(c(6:11)) 
      atspcheck <- sum(is.na(trm_2_14_1) == TRUE)
  
    ### remove file, entry from menu and from list of pages to generate ----
      if (atspcheck == 24) {
        genscripts <- genscripts[genscripts != "generate_ceff6_tz_qmd.R"]
        # tx  <- readLines("_quarto.yml")
        tx <- str_replace(tx, "- cost-efficiency-tz1-3.qmd", "  # - cost-efficiency-tz1-3.qmd")

        file.remove("cost-efficiency-tz1-3.qmd")
        
      } 
    }

   writeLines(tx, con="_quarto.yml")
   
     
    ## check if there are other er ATSPs ----
    sheet <- "4_ATSP"
    range <- range <- "C13:M17" 
    ert_2_14_1  <- read_range(file, sheet, range) %>% select(c(6:11))
    atspcheck <- sum(is.na(ert_2_14_1) == TRUE)
  
    if (atspcheck == 24) {
      genscripts <- genscripts[genscripts != "generate_ceff_er3_qmd.R"]
      tx  <- readLines("_quarto.yml")
      tx <- str_replace(tx, "- cost-efficiency-er1-3.qmd", "  # - cost-efficiency-er1-3.qmd")
      writeLines(tx, con="_quarto.yml")
      
      file.remove("cost-efficiency-er1-3.qmd")
      
    } 
 }

# generate new qmd files ----
  if (country != "Network Manager" & country != "SES RP3" & country != "Home") {
    for (i in 1:length(genscripts)) {
      source(here("R", genscripts[i]))
    }
  }
  
# render site ----
  # quarto::quarto_render(as_job = FALSE)
  quarto::quarto_render(as_job = FALSE,
                        execute_params = list(country = country, 
                                              year_report = year_report,
                                              data_folder = data_folder,
                                              forecast = forecast,
                                              forecast_id = forecast_id,
                                              statfor_zone = statfor_zone,
                                              ecz_list = ecz_list,
                                              no_ecz = no_ecz,
                                              tcz_list = tcz_list,
                                              no_tcz = no_tcz,
                                              nat_curr = nat_curr,
                                              pp_version = pp_version,
                                              acc_no = acc_no,
                                              acc1 = acc1,
                                              acc2 = acc2,
                                              acc3 = acc3,
                                              acc4 = acc4,
                                              acc5 = acc5,
                                              no_apt_big = no_apt_big,
                                              no_apt_small = no_apt_small,
                                              tsu_share = tsu_share,
                                              ert_cost_share = ert_cost_share,
                                              ert_trm_share = ert_trm_share,
                                              main_ansp = main_ansp,
                                              main_ansp_aua = main_ansp_aua,
                                              other_ansps_str = other_ansps_str,
                                              other_met_str = other_met_str,
                                              
                                              # chart_layout = chart_layout, 
                                              mywidth = mywidth,
                                              myheight = myheight,
                                              myfont = myfont,
                                              mymargin = mymargin,
                                              mywidth_pdf = mywidth_pdf,
                                              myheight_pdf = myheight_pdf,
                                              myfont_pdf = myfont_pdf,
                                              mymargin_pdf = mymargin_pdf,

                                              cap_file = cap_file,
                                              ceff_file = ceff_file,
                                              env_kea_file = env_kea_file,
                                              env_apt_file = env_apt_file,
                                              env_mil_file = env_mil_file,
                                              saf_eosm_file = saf_eosm_file)
                        )

