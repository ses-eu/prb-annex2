
# replace index, quarto yaml and css files ----
  ## quarto yaml
  if (out_format == 'pdf') {
    file.copy('_original_files/full_quarto_pdf.yml', '_quarto.yml', overwrite = TRUE, copy.mode = TRUE)
  } else {
    file.copy('_original_files/full_quarto.yml', '_quarto.yml', overwrite = TRUE, copy.mode = TRUE)
  }
  
  ## styles css
  file.copy('_original_files/full_styles.css', 'styles.css', overwrite = TRUE, copy.mode = TRUE)
  
# remove .qmd files ----
  ## get file list 
  root_files <- list.files()
  
  ## remove all non '_' qmd files 
  for (i in 1:length(root_files)) {
    if (grepl('.qmd', root_files[i], fixed = TRUE) == TRUE & substr(root_files[i], 1, 1) != '_') {
      file.remove(root_files[i])
    }
  }
    
# create index pages and other adjustments ----
  if (country == "Home") {
    file.copy('_original_files/home_index.qmd', 'index.qmd', overwrite = TRUE, copy.mode = TRUE)
    ###for the home page we add as well the other qmds here
    file.copy('_original_files/home_about.qmd', 'about.qmd', overwrite = TRUE, copy.mode = TRUE)
    file.copy('_original_files/home_disclaimer.qmd', 'disclaimer.qmd', overwrite = TRUE, copy.mode = TRUE)
    
    ### we also remove one line from the css file
    tmp_text <- readLines("_original_files/full_styles.css")
    tmp_text <- str_replace(tmp_text, fixed("  font-size: 0.9rem;"), "") 
    writeLines(tmp_text, 'styles.css')

  } else if (country == "Network Manager") {
    tmp_text <- readLines("_original_files/common_qmd_setup.qmd")
    tmp_text <- str_replace(tmp_text, "file-placeholder", "_original_files/nm_index.qmd") 
    writeLines(tmp_text, 'index.qmd')

  } else if (country == "SES RP3") {
      tmp_text <- readLines("_original_files/common_qmd_setup.qmd")
      if (out_format == 'pdf') {
        tmp_text <- str_replace(tmp_text, "file-placeholder", "_original_files/ses_index_pdf.qmd")
      } else {
        tmp_text <- str_replace(tmp_text, "file-placeholder", "_original_files/ses_index.qmd")
      }
      writeLines(tmp_text, 'index.qmd')
      
  } else {
      tmp_text <- readLines("_original_files/common_qmd_setup.qmd")
      tmp_text <- str_replace(tmp_text, "file-placeholder", "_original_files/state_index.qmd") 
      writeLines(tmp_text, 'index.qmd')
      
      # generate level 2 .qmd master files 
      level2_files <- c("capacity.qmd",
                           "environment.qmd",
                           "safety.qmd",
                           "cost-efficiency-g2g.qmd")

      for (i in 1:no_ecz) {
        # add level2 cef er .qmd file name to list
        level2_files <- append(level2_files, paste0("cost-efficiency-er",i,"-1.qmd"))
        
        # generate level2 _cef enroute files from generic file
        tmp_text <- readLines("_cost-efficiency-generic.qmd")
        tmp_text <- str_replace(tmp_text, "@@cz_index@@", as.character(i)) 
        tmp_text <- str_replace(tmp_text, "@@cz_type@@", "enroute") 
        tmp_text <- str_replace(tmp_text, "@@cz_type_proper@@", "En route") 
        tmp_text <- str_replace(tmp_text, "@@cz_short@@", "ecz") 
        writeLines(tmp_text, paste0('_cost-efficiency-er', i,'-1.qmd'))
        
      }
      
      for (i in 1:no_tcz) {
        # add level2 cef trm .qmd file name to list
        level2_files <- append(level2_files, paste0("cost-efficiency-tz",i,"-1.qmd"))

        # generate level2 _cef enroute files from generic file
        tmp_text <- readLines("_cost-efficiency-generic.qmd")
        tmp_text <- str_replace(tmp_text, "@@cz_index@@", as.character(i)) 
        tmp_text <- str_replace(tmp_text, "@@cz_type@@", "terminal") 
        tmp_text <- str_replace(tmp_text, "@@cz_type_proper@@", "Terminal") 
        tmp_text <- str_replace(tmp_text, "@@cz_short@@", "tcz") 
        
        writeLines(tmp_text, paste0('_cost-efficiency-tz', i,'-1.qmd'))
        
        }
      
      level2_files <- sort(level2_files)
      
      # generate actual .qmd files linked to respective _.qmd files
      for (i in 1:length(level2_files)) {
        tmp_text <- readLines("_original_files/common_qmd_setup.qmd")
        tmp_text <- str_replace(tmp_text, "file-placeholder", paste0("_", level2_files[i])) 
        writeLines(tmp_text, level2_files[i])
      }
    }

# find all scripts that generate .qmd files ----
  # ## get all R scripts ----
  # rscripts <- list.files(here('R'))
  # 
  # ## get only 'generate' scripts ----
  # genscripts <- list()
  # for (i in 1:length(rscripts)) {
  #   if (grepl('generate', rscripts[i], fixed = TRUE) == TRUE) {
  #     genscripts <- append(genscripts, rscripts[i])
  #   }
  # }

# remove and regenerate variables ----
  file.remove('_variables.yml')
  newvariables <- paste0("doc:
    year_report: ", year_report, "
    country: '", country, "'
    country_lower: '", country_lower, "'
    ecz1: '", ecz_list$ecz_name[1], "'
    ecz2: '", ecz_list$ecz_name[2], "'
    tcz1: '", tcz_list$tcz_name[1], "'
    tcz2: '", tcz_list$tcz_name[2], "'
  "
  )
  
  cat(newvariables, file = "_variables.yml")

# modify _quarto.yml ----
if (out_format == 'web') {
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
    
    if (country == "Home") {
    ## Home page case ----
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
     
    if (state_type != 0) {
    ### with terminal zone(s) ----
      
      # add text for the additional tczs
      tx_tcz_initial <- readLines("_original_files/tcz_xy.yml")
      tx_tcz <- ''
      for (i in 1:no_tcz) {
        tx_tcz <- append(tx_tcz, str_replace(tx_tcz_initial, "@@cz_index@@", as.character(i)))
        tx_tcz <- str_replace(tx_tcz, "@@cz_name@@", if_else(no_tcz == 1, "", paste0(" - ", tcz_list$tcz_name[[i]]))) 
      }
      
      # find position to insert
      for (i in 1:length(tx)) {
        if (tx[i] %like% '# include TCZ block') {block_beg = i}
      }  
      
      tx <- append(tx, tx_tcz, block_beg) 
      
      if (state_type == 3) {
        ### 2 en route cz (spain)
        tx <- str_replace(tx, "<b>En route CZ</b>", paste0("<b>En route CZ - ", ecz_list$ecz_name[[1]], "</b>")) 
        
        # add text for the additional eczs
        tx_ecz_initial <- readLines("_original_files/ecz_xy.yml")
        tx_ecz <- ''
        for (i in 2:no_ecz) {
          tx_ecz <- append(tx_ecz, str_replace(tx_ecz_initial, "@@cz_index@@", as.character(i)))
          tx_ecz <- str_replace(tx_ecz, "@@cz_name@@", paste0(" - ", ecz_list$ecz_name[[i]])) 
        }
        
        # find position to insert
        for (i in 1:length(tx)) {
          if (tx[i] %like% '# include ECZ2 block') {block_beg = i}
        }  
        
        tx <- append(tx, tx_ecz, block_beg) 
        
      }
      
      
      
      ### check if there are other term ATSPs ----
        sheet <- "8_TRM_ATSP"
        range <- "C13:M17" 
        trm_2_14_1  <- read_range(file, sheet, range) %>% select(c(6:11)) 
        atspcheck <- sum(is.na(trm_2_14_1) == TRUE)
    
      ### remove file, entry from menu and from list of pages to generate ----
        if (atspcheck == 24) {
          # genscripts <- genscripts[genscripts != "generate_ceff6_tz_qmd.R"]
          # tx  <- readLines("_quarto.yml")
          # tx <- str_replace(tx, "- cost-efficiency-tz1-3.qmd", "  # - cost-efficiency-tz1-3.qmd")
  
          # file.remove("cost-efficiency-tz1-3.qmd")
          
        } 
      }
  
     writeLines(tx, con="_quarto.yml")
     
       
      ## check if there are other er ATSPs ----
      sheet <- "4_ATSP"
      range <- range <- "C13:M17" 
      ert_2_14_1  <- read_range(file, sheet, range) %>% select(c(6:11))
      atspcheck <- sum(is.na(ert_2_14_1) == TRUE)
    
      if (atspcheck == 24) {
        # genscripts <- genscripts[genscripts != "generate_ceff_er3_qmd.R"]
        tx  <- readLines("_quarto.yml")
        # tx <- str_replace(tx, "- cost-efficiency-er1-3.qmd", "  # - cost-efficiency-er1-3.qmd")
        # writeLines(tx, con="_quarto.yml")
        
        # file.remove("cost-efficiency-er1-3.qmd")
        
      } 
   }

  # # generate new qmd files ----
  #   if (country != "Network Manager" & country != "SES RP3" & country != "Home") {
  #     for (i in 1:length(genscripts)) {
  #       source(here("R", genscripts[i]))
  #     }
  #   }
}
    
# render site ----
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
                                              acc_list = acc_list,
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
                                              # yearly_xrates = yearly_xrates,
                                              
                                              # chart_layout = chart_layout, 
                                              mywidth = mywidth,
                                              myheight = myheight,
                                              myfont = myfont,
                                              mymargin = mymargin,
                                              mylinewidth = mylinewidth,
                                              
                                              mysuffix =mysuffix,
                                              mydecimals =mydecimals,
                                              
                                              ### trace parameters
                                              mycolors = mycolors,
                                              mytextangle =mytextangle,
                                              mytextposition =mytextposition,
                                              myinsidetextanchor =myinsidetextanchor,
                                              mytextfont_color =mytextfont_color,
                                              mytextfont_size =mytextfont_size,
                                              
                                              # myhovertemplate =myhovertemplate,
                                              mytrace_showlegend =mytrace_showlegend,
                                              
                                              ### layout parameters
                                              myfont_family =myfont_family,
                                              mybargap =mybargap,
                                              mybarmode =mybarmode,
                                              myhovermode =myhovermode,
                                              myhoverlabel_bgcolor =myhoverlabel_bgcolor,
                                              myminsize =myminsize,
                                              
                                              #### title
                                              mytitle_text =mytitle_text,
                                              mytitle_x =mytitle_x,
                                              mytitle_y =mytitle_y,
                                              mytitle_xanchor =mytitle_xanchor,
                                              mytitle_yanchor =mytitle_yanchor,
                                              mytitle_font_size =mytitle_font_size,
                                              
                                              #### xaxis
                                              myxaxis_title =myxaxis_title,
                                              myxaxis_gridcolor =myxaxis_gridcolor,
                                              myxaxis_showgrid =myxaxis_showgrid,
                                              myxaxis_showline =myxaxis_showline,
                                              myxaxis_showticklabels =myxaxis_showticklabels,
                                              myxaxis_tickformat =myxaxis_tickformat,
                                              myxaxis_dtick =myxaxis_dtick,
                                              myxaxis_zeroline =myxaxis_zeroline,
                                              myxaxis_tickfont_size =myxaxis_tickfont_size,
                                              
                                              #### yaxis
                                              myyaxis_title =myyaxis_title,
                                              myyaxis_gridcolor =myyaxis_gridcolor,
                                              myyaxis_showgrid =myyaxis_showgrid,
                                              myyaxis_showline =myyaxis_showline,
                                              myyaxis_tickprefix =myyaxis_tickprefix,
                                              myyaxis_ticksuffix =myyaxis_ticksuffix,
                                              myyaxis_tickformat =myyaxis_tickformat,
                                              
                                              myyaxis_zeroline =myyaxis_zeroline,
                                              myyaxis_zerolinecolor =myyaxis_zerolinecolor,
                                              myyaxis_titlefont_size =myyaxis_titlefont_size,
                                              myyaxis_tickfont_size =myyaxis_tickfont_size,
                                              
                                              #### legend
                                              mylegend_traceorder =mylegend_traceorder,
                                              mylegend_orientation =mylegend_orientation,
                                              mylegend_xanchor =mylegend_xanchor,
                                              mylegend_yanchor =mylegend_yanchor,
                                              mylegend_x =mylegend_x,
                                              mylegend_y =mylegend_y,
                                              mylegend_font_size =mylegend_font_size,
                                              
                                              #### margin
                                              mylocalmargin = mylocalmargin,
                                              
                                              ## pdf
                                              mywidth_pdf = mywidth_pdf,
                                              myheight_pdf = myheight_pdf,
                                              myfont_pdf = myfont_pdf,
                                              mymargin_pdf = mymargin_pdf,
                                              mylinewidth_pdf = mylinewidth_pdf,

                                              cap_file = cap_file,
                                              ceff_file = ceff_file,
                                              env_kea_file = env_kea_file,
                                              env_apt_file = env_apt_file,
                                              env_mil_file = env_mil_file,
                                              saf_eosm_file = saf_eosm_file)
                        )

