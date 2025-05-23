for (i in 2023) {  # set your year(s) report here
###NOTES ----
# for the pdf output you need to install TinyTex in your machine
## 1. run install.packages("tinitex") in your console
## 2. This will install it in the wrong folder C:\Users\[username]\AppData\Roaming
## 3. Cut it from there and paste it in C:\Users\[username]\dev\
## 4. Add  C:\Users\[username]\dev\TinyTeX\bin\windows to your path
## 5. Install the package pdfcrop. This is necessary so the figures get properly cropped. You can do it from the command line C:\Users\[username]\dev\TinyTeX>tlmgr install pdfcrop

# and you also you need to install miktek in your machine
## 1. download the installer from https://miktex.org/download
## 2. run it from your C:\Users\[username]\dev
## 3. In the first screen select 'Always' install packages on the fly
## 4. install it in C:\Users\[username]\dev\MiKTeX
## 5. when prompted to check for updates, say yes. if missed, open the miktek console from your programmes, check for updates and update
## 6. add C:\Users\[username]\dev\MiKTeX\miktex\bin\x64\ to your path

# In case this matters I have these in my path
# C:\Users\oaolive\dev\quarto-1.6.39\bin
# C:\Users\oaolive\dev\quarto-1.6.39\bin\tools


# clean environment and set main parameters ----
  rm(list = setdiff(ls(), "i"))
  # i<- 2023
  # rm(list = ls())
  year_report <- i 
  out_format <- 'web' # set your output format here: 'pdf' or 'web'
  data_folder <- 'G:/HQ/dgof-pru/Data/SES Monitoring Dashboard/data_for_web/'
  data_folder_a2 <- paste0(data_folder, "monitoring_files/", year_report, "/")
 
  # set to true for generating the investments page - do only if last year of rp
  investments <- TRUE
    
  # set test_check to TRUE to create test pages with hyperlinks functional within the test site (defined in parameters script)
  # set test_check to FALSE to create production-ready pages with hyperlinks functional within the sesperformance.eu site
  test_check <- TRUE       
  
  ## set all_states to FALSE to build only one state site, TRUE for all
  all_states <- FALSE
  
# get functions ----
  source("R/utils.R")
  
# get context data ----
  source("R/get_context_data.R")
  
  ## modify state list as required
  state_list_prod <- state_list
  # state_list_prod <- c(state_list, "Home")  #add home to list
  # state_list_prod <- setdiff(state_list_prod, "Belgium")  #remove state
  # states_from <- c(13:32) # 1st number is the index of 1st state from which you want to generate
  # state_list_prod <- state_list_prod[states_from]
  
  if (all_states == FALSE) {
    state_list_prod <- 'France' # set your one country/stakeholder here (Home for home page)
  } 
  
# build state pages ----
  ## build pages
  for (i in 1:length(state_list_prod)) {
    country <- state_list_prod[i]
    # country <- state_list_prod
    source("R/parameters.R")
    source("R/create_pages.R")

  ## copy site to network folder ----
  if (out_format == 'web') {
    if (investments & country != "Home") {
      # ### find list of html files
      # hmtl_files <- list.files(site_dir, pattern = "\\.html$", full.names = FALSE)
      # 
      # ### replace links to countries by links to country/section
      # purrr::map(hmtl_files, replace_links)
      
      ### delete previous version
      unlink(paste0(destination_dir_investments, country_lower), recursive = TRUE) 
      
      ### copy new site to folder
      file.copy(site_dir, destination_dir_investments, overwrite = TRUE, recursive=TRUE, copy.mode = TRUE)
      
      ### rename _site with state name
      file.rename(paste0(destination_dir_investments, "/_site"), paste0(destination_dir_investments, country_lower))
 
    }
    else if (country == 'Home') {
      ## Home page ----
      ### delete previous files except country folders
      files_to_be_deleted <- list.files(root_dir) 
      files_to_be_deleted <- files_to_be_deleted[files_to_be_deleted %like% c("202") == FALSE]
      files_to_be_deleted <- files_to_be_deleted[(files_to_be_deleted == "download") == FALSE]
      
      ## otherwise it deletes everything
      if (length(files_to_be_deleted) != 0) { 
        fs::file_delete(paste0(root_dir, files_to_be_deleted)) 
      }
      
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
  } else if (out_format == 'pdf') {
    
    pdf_download_dir <- here(root_dir, "download", year_report)
    pdf_folders <- list("pdf_docs", pdf_download_dir) 

    if (!dir.exists(pdf_download_dir)) {
      dir.create(pdf_download_dir, recursive = TRUE)  
    }
    
    purrr::walk(pdf_folders, ~ file.copy(here("_book", "Performance-Review-Body.pdf"),
                                            here(.x, paste0("PRB-Annual-Monitoring-Report_",
                                                                    country,
                                                                    "_",
                                                                    year_report,
                                                                    ".pdf")),
                                            overwrite = TRUE,
                                            copy.mode = TRUE)
    )
    
  }
  }

}
