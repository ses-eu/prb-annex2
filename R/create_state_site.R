# clean environment ----
  rm(list = ls()) 

# parameters ----
  source("R/parameters.R")

# remove and copy new index and quarto yaml file ----
file.copy('_original_files/full_quarto.yml', '_quarto.yml', overwrite = TRUE, copy.mode = TRUE)

if (country == "Network Manager") {
      file.copy('_original_files/nm_index.qmd', 'index.qmd', overwrite = TRUE, copy.mode = TRUE)
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
  "
  )
  
  cat(newvariables, file = "_variables.yml")

# modify _quarto.yml ----
  ## NM case ----
  if (country == "Network Manager") {
    tx  <- readLines("_quarto.yml")
    ### this removes the unwanted lines
    tx <- tx[-c(105:135)]
    ### write new file
    writeLines(tx, con="_quarto.yml")
    
 } else {
    ## no terminal zone case ----
    if (state_type == 0) {
      genscripts <- list('generate_saf_qmd.R',
                         'generate_env_kea_qmd.R',
                         'generate_env_mil_qmd.R',
                         'generate_cap_er_qmd.R',
                         'generate_ceff_er1_qmd.R',
                         'generate_ceff_er2_qmd.R',
                         'generate_ceff_er3_qmd.R')
    
      tx  <- readLines("_quarto.yml")
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
    
     writeLines(tx, con="_quarto.yml")
  
    } else if (state_type == 1) {
    
    ## with terminal zone case ----
      tx  <- readLines("_quarto.yml")
      tx  <- gsub(pattern = "- cost-efficiency-tz2-1.qmd", replace = "  # - cost-efficiency-tz2-1.qmd", x = tx)
      tx  <- gsub(pattern = "- cost-efficiency-tz2-2.qmd", replace = "  # - cost-efficiency-tz2-2.qmd", x = tx)
      tx  <- gsub(pattern = "- cost-efficiency-tz2-3.qmd", replace = "  # - cost-efficiency-tz2-3.qmd", x = tx)
      tx  <- gsub(pattern = "- text: '-----'", replace = "  # - text: '-----'", x = tx)
      writeLines(tx, con="_quarto.yml")
    
    ### check if there are other term ATSPs ----
      sheet <- "8_TRM_ATSP"
      range <- "C13:M17" 
      trm_2_14_1  <- read_range(file, sheet, range) %>% select(c(6:11)) 
      atspcheck <- sum(is.na(trm_2_14_1) == TRUE)
  
    ### remove entry from menu and from list of pages to generate ----
      if (atspcheck == 24) {
        genscripts <- genscripts[genscripts != "generate_ceff6_tz_qmd.R"]
        tx  <- readLines("_quarto.yml")
        tx2  <- gsub(pattern = "- cost-efficiency-tz1-3.qmd", 
                     replace = "  # - cost-efficiency-tz1-3.qmd", 
                     x = tx)
        writeLines(tx2, con="_quarto.yml")
      } 
    }
  
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
  if (country != "Network Manager") {
    for (i in 1:length(genscripts)) {
      source(here("R", genscripts[i]))
    }
  }
# render site ----
  quarto::quarto_render(as_job = FALSE)

# copy site to test folder ----
  site_dir <- here("_site")
  destination_dir <- paste0('//ihx-vdm05/LIVE_var_www_performance$/oscar/prb-monitoring-test/2022/')

  ## delete previous version ----
  unlink(paste0(destination_dir, country), recursive = TRUE) 
  ## copy new site to folder ----
  file.copy(site_dir, destination_dir, overwrite = TRUE, recursive=TRUE, copy.mode = TRUE)
  ## rename _site with state name ----
  file.rename(paste0(destination_dir, "/_site"), paste0(destination_dir, country))
  # copyDirectory(here::here("images"), paste0(destination_dir,"images"), overwrite = TRUE)


