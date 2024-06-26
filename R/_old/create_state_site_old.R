# clean environment ----
rm(list = ls())
country <- 'Austria'
year_report <- 2022
source("R/parameters.R")

for (i in 1:nrow(state_list)) {
  country <- state_list[[i, 1]]

  # parameters ----
  # country <- 'Network Manager'
  source("R/parameters.R")
  country_lower <- country %>% str_to_lower() %>% str_replace_all(., " ","-")
  
  test_check <- TRUE
  
  site_dir <- here("_site")
  root_dir <- '//ihx-vdm05/LIVE_var_www_performance$/oscar/prb-monitoring-test-private/'
  destination_dir <- paste0(root_dir, year_report, '/')
  home_address <- 'https://www.eurocontrol.int/performance/oscar/prb-monitoring-test-private'
  external_address <- str_replace(destination_dir, fixed('//ihx-vdm05/LIVE_var_www_performance$'), 'https://www.eurocontrol.int/performance')
  
# remove and copy new index and quarto yaml file ----
  file.copy('_original_files/full_quarto.yml', '_quarto.yml', overwrite = TRUE, copy.mode = TRUE)

  if (country == "Network Manager") {
      file.copy('_original_files/nm_index.qmd', 'index.qmd', overwrite = TRUE, copy.mode = TRUE)
  } else if (country == "SES RP3") {
    file.copy('_original_files/ses_index.qmd', 'index.qmd', overwrite = TRUE, copy.mode = TRUE)
    } else {
    file.copy('_original_files/state_index.qmd', 'index.qmd', overwrite = TRUE, copy.mode = TRUE)
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
                        paste0('text: "', year_report, ' &#10003"'))
  
  ## NM, SES case ----
  if (country == "Network Manager" | country == "SES RP3") {
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
    
      tx  <- gsub(pattern = "- capacity_trm.qmd", replace = "  # - capacity_trm.qmd", x = tx)
      tx  <- gsub(pattern = "- cost-efficiency-tz1-1.qmd", replace = "  # - cost-efficiency-tz1-1.qmd", x = tx)
      tx  <- gsub(pattern = "- cost-efficiency-tz1-2.qmd", replace = "  # - cost-efficiency-tz1-2.qmd", x = tx)
      tx  <- gsub(pattern = "- cost-efficiency-tz1-3.qmd", replace = "  # - cost-efficiency-tz1-3.qmd", x = tx)
      tx  <- gsub(pattern = "- cost-efficiency-tz2-1.qmd", replace = "  # - cost-efficiency-tz2-1.qmd", x = tx)
      tx  <- gsub(pattern = "- cost-efficiency-tz2-2.qmd", replace = "  # - cost-efficiency-tz2-2.qmd", x = tx)
      tx  <- gsub(pattern = "- cost-efficiency-tz2-3.qmd", replace = "  # - cost-efficiency-tz2-3.qmd", x = tx)
      tx  <- gsub(pattern = "- cost-efficiency-g2g.qmd", replace = "  # - cost-efficiency-g2g.qmd", x = tx)
      tx  <- gsub(pattern = "- environment_apt.qmd", replace = "  # - environment_apt.qmd", x = tx)
      tx  <- gsub(pattern = '- text: "----"', replace = '  # - text: "----"', x = tx)
      tx  <- gsub(pattern = '- text: "-----"', replace = '  # - text: "-----"', x = tx)
    
    } else if (state_type == 1) {
    
    ### with terminal zone case ----
      tx  <- gsub(pattern = "- cost-efficiency-tz2-1.qmd", replace = "  # - cost-efficiency-tz2-1.qmd", x = tx)
      tx  <- gsub(pattern = "- cost-efficiency-tz2-2.qmd", replace = "  # - cost-efficiency-tz2-2.qmd", x = tx)
      tx  <- gsub(pattern = "- cost-efficiency-tz2-3.qmd", replace = "  # - cost-efficiency-tz2-3.qmd", x = tx)
      tx  <- gsub(pattern = "- text: '-----'", replace = "  # - text: '-----'", x = tx)
      # writeLines(tx, con="_quarto.yml")
    
    ### check if there are other term ATSPs ----
      sheet <- "8_TRM_ATSP"
      range <- "C13:M17" 
      trm_2_14_1  <- read_range(file, sheet, range) %>% select(c(6:11)) 
      atspcheck <- sum(is.na(trm_2_14_1) == TRUE)
  
    ### remove entry from menu and from list of pages to generate ----
      if (atspcheck == 24) {
        genscripts <- genscripts[genscripts != "generate_ceff6_tz_qmd.R"]
        # tx  <- readLines("_quarto.yml")
        tx  <- gsub(pattern = "- cost-efficiency-tz1-3.qmd", 
                     replace = "  # - cost-efficiency-tz1-3.qmd", 
                     x = tx)
        # writeLines(tx, con="_quarto.yml")
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
      tx  <- gsub(pattern = "- cost-efficiency-er1-3.qmd", replace = "  # - cost-efficiency-er1-3.qmd", x = tx)
      writeLines(tx, con="_quarto.yml")
    } 
 }

# remove and regenerate .qmd files ----
  ## get file list ----
  root_files <- list.files()
  
  ## remove all qmd files except index ----
  for (i in 1:length(root_files)) {
    if ((grepl('.qmd', root_files[i], fixed = TRUE) == TRUE) & 
        (grepl('index', root_files[i], fixed = TRUE) == FALSE)) {
      file.remove(root_files[i])
    }
  }

  ## generate new qmd files ----
  if (country != "Network Manager" & country != "SES RP3") {
    for (i in 1:length(genscripts)) {
      source(here("R", genscripts[i]))
    }
  }
  
# render site ----
  quarto::quarto_render(as_job = FALSE,
                        execute_params = list(country = country, 
                                              year_report = year_report))

# copy site to test folder ----
  ## delete previous version ----
  unlink(paste0(destination_dir, country_lower), recursive = TRUE) 

  ## copy new site to folder ----
  file.copy(site_dir, destination_dir, overwrite = TRUE, recursive=TRUE, copy.mode = TRUE)

  ## rename _site with state name ----
  file.rename(paste0(destination_dir, "/_site"), paste0(destination_dir, country_lower))
  # copyDirectory(here::here("images"), paste0(destination_dir,"images"), overwrite = TRUE)
}

