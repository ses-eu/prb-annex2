###build site

# clean environment and set year report ----
  rm(list = ls())
  year_report <- 2022 # set your year report here

# get functions ----
  source("R/utils.R")
  
# get data ----
  source("R/get_data.R")
  
# build state pages ----
  
  ## set test check to TRUE to build only one state site, FALSE for all
  test_check <- TRUE
  
  ## build pages
  if (test_check == TRUE) {
    state_list <- 'Belgium' # set your test country here
  } 

  for (i in 1:length(state_list)) {
    country <- state_list[i]
    source("R/parameters.R")
    source("R/create_state_pages.R")
  }

  