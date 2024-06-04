###build site

# clean environment and set main parameters ----
  rm(list = ls())
  year_report <- 2022 # set your year report here
  out_format <- 'web' # set your output format here: 'pdf' or 'web'
  
# get functions ----
  source("R/utils.R")
  
# get data ----
  source("R/get_data.R")
  
  ## add Home to State list
  state_list <- c(state_list, "Home")

# build state pages ----
  
  ## set test check to TRUE to build only one state site, FALSE for all
  test_check <- TRUE
  
  ## build pages
  if (test_check == TRUE) {
    state_list <- 'Belgium' # set your test country here (Home for home page)
  } 

  for (i in 1:length(state_list)) {
    country <- state_list[i]
    source("R/parameters.R")
    source("R/create_pages.R")
  }
  
  ## copy site to test folder ----
  if (out_format == 'web') {
    if (country == 'Home') {
      ## Home page ----
      ### delete previous files except country folders
      files_to_be_deleted <- list.files(root_dir) 
      files_to_be_deleted <- files_to_be_deleted[files_to_be_deleted %like% c("202") == FALSE]
      
      fs::file_delete(paste0(root_dir, files_to_be_deleted)) 
      
      ### copy files to folder
      files_to_be_copied <- list.files(site_dir) 
      dirs_to_be_copied <- list.dirs(site_dir, full.names = FALSE, recursive = FALSE)
      files_to_be_copied <- files_to_be_copied[files_to_be_copied %in% dirs_to_be_copied == FALSE]
      
      fs::file_copy(paste0(site_dir,'/', files_to_be_copied),
                    paste0(root_dir, files_to_be_copied), 
                    overwrite = TRUE)
      
      for (i in 1:length(dirs_to_be_copied)) {
        fs::dir_copy(paste0(site_dir,'/', dirs_to_be_copied[i]),
                     paste0(root_dir, dirs_to_be_copied[i]), 
                     overwrite = TRUE)
      }
      
    } else {
      ## Other pages/sites ----
      ### delete previous version
      unlink(paste0(destination_dir, country_lower), recursive = TRUE) 
      
      ### copy new site to folder
      file.copy(site_dir, destination_dir, overwrite = TRUE, recursive=TRUE, copy.mode = TRUE)
      
      ### rename _site with state name
      file.rename(paste0(destination_dir, "/_site"), paste0(destination_dir, country_lower))
      # copyDirectory(here::here("images"), paste0(destination_dir,"images"), overwrite = TRUE)
    }
  }
  
  