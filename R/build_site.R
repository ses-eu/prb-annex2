###build site

# clean environment and set main parameters ----
  rm(list = ls())
  year_report <- 2020 # set your year report here
  out_format <- 'web' # set your output format here: 'pdf' or 'web'
  data_folder <- 'G:/HQ/dgof-pru/Data/SES Monitoring Dashboard/data_for_web/'
  data_folder_a2 <- paste0(data_folder, "monitoring_files/", year_report, "/")

  # set test_check to TRUE to create test pages with hyperlinks functional within the test site (defined in parameters script)
  # set test_check to FALSE to create production-ready pages with hyperlinks functional within the sesperformance.eu site
  test_check <- TRUE       
  
  ## set all_states to FALSE to build only one state site, TRUE for all
  all_states <- TRUE
  
# get functions ----
  source("R/utils.R")
  
# get data ----
  source("R/get_data.R")
  
  ## modify state list as required
  state_list_prod <- state_list
  # state_list_prod <- c(state_list, "Home")  #add home to list
  # state_list_prod <- setdiff(state_list_prod, "Belgium")  #remove state
  # states_from <- c(4:32) # 1st number is the index of 1st state from which you want to generate
  # state_list_prod <- state_list_prod[states_from]

# build pages ----
  
  ## build pages
  if (all_states == FALSE) {
    state_list_prod <- 'Austria' # set your one country/stakeholder here (Home for home page)
  } 

  for (i in 1:length(state_list)) {
    country <- state_list_prod[i]
    # country <- state_list_prod
    source("R/parameters.R")
    source("R/create_pages.R")
  
  
  ## copy site to test/prod folder ----
  if (out_format == 'web') {
    if (country == 'Home') {
      ## Home page ----
      ### delete previous files except country folders
      #### list all files/dirs in root dir
      files_to_be_deleted <- list.files(root_dir) 
      #### remove year folders from list of files to be deleted keeping only the files in root dir
      files_to_be_deleted <- files_to_be_deleted[files_to_be_deleted %like% c("202") == FALSE] 
      
      ### otherwise it deletes everything
      if (length(files_to_be_deleted) != 0) { 
        fs::file_delete(paste0(root_dir, files_to_be_deleted)) 
      }
      
      ### copy files to folder
      #### make list of files and dirs in _site dir
      files_to_be_copied <- list.files(site_dir) 
      #### make list of dirs in _site dir
      dirs_to_be_copied <- list.dirs(site_dir, full.names = FALSE, recursive = FALSE)
      #### remove dirs from list of files to be copied
      files_to_be_copied <- files_to_be_copied[files_to_be_copied %in% dirs_to_be_copied == FALSE]
      
      #### copy files to folder
      fs::file_copy(paste0(site_dir,'/', files_to_be_copied),
                    paste0(root_dir, files_to_be_copied), 
                    overwrite = TRUE)
      
      #### copy dirs to folder
      for (i in 1:length(dirs_to_be_copied)) {
        fs::dir_copy(paste0(site_dir,'/', dirs_to_be_copied[i]),
                     paste0(root_dir, dirs_to_be_copied[i]), 
                     overwrite = TRUE)
      }
      
    } else {
      ## Other pages/sites ----
      ### find list of html files
      hmtl_files <- list.files(site_dir, pattern = "\\.html$", full.names = FALSE)
      
      ### replace links to countries by links to country/section
      purrr::map(hmtl_files, replace_links)
      
      ### delete previous version
      unlink(paste0(destination_dir, country_lower), recursive = TRUE) 
      
      ### copy new site to folder
      file.copy(site_dir, destination_dir, overwrite = TRUE, recursive=TRUE, copy.mode = TRUE)
      
      ### rename _site with state name
      file.rename(paste0(destination_dir, "/_site"), paste0(destination_dir, country_lower))
      # copyDirectory(here::here("images"), paste0(destination_dir,"images"), overwrite = TRUE)
    }
  }
  
  }
  