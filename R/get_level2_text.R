if (!exists("country") | is.na(country)) {country = "Austria"}
if (exists("cztype") == FALSE) {cztype = "terminal"}

# safety ----
# define ranges and import data
if (country == "SES RP3") {
  saf_text <- ""
  
} else {
  sheet <- country
  
  range <- "A15:G30"
  saf_all <- read_range(saf_eosm_file, sheet, range) 
  saf_all <- saf_all |> 
    rename(a = 1) 
  
  # replace NAs by empty strings
  saf_all[is.na(saf_all)] <- ""
  
  #define titles to lookup
  saf_titles <- c("Observations",
                  "**Observations**"
  )
  
  saf_heading_positions <- which(saf_all$a %in% saf_titles)
  
  saf_text <- saf_all[(saf_heading_positions[1]+1):(saf_heading_positions[1]+1), 1]
}


# environment ----

if (country == "SES RP3") {
  #we need to define this variables to avoid errors
  env_apt_2 <- ''
  env_apt_3 <- ''
  env_apt_4 <- ''
  env_mil_1 <- ''
  env_mil_2 <- ''
  env_mil_4 <- ''
  env_mil_6 <- ''
  env_mil_8 <- ''
  
} else {
  
  # define ranges and import data
  sheet <- country
  
  if (no_tcz >0) {
    range <- "A2:P80"
    env_apt_all <- read_range(env_apt_file, sheet, range) 
    env_apt_all <- env_apt_all |> 
      rename(a = 1) 
    
    # replace NAs by empty strings
    env_apt_all[is.na(env_apt_all)] <- ""
    
    env_apt_titles <- c("1. Overview",
                        "**1. Overview**",
                        "2. Additional Taxi-Out Time", 
                        "**2. Additional Taxi-Out Time**", 
                        "3. Additional ASMA Time",
                        "**3. Additional ASMA Time**",
                        "4. Share of arrivals applying CDO",
                        "**4. Share of arrivals applying CDO**",
                        "5. Appendix",
                        "**5. Appendix**"
    )
    
    env_apt_heading_positions <- which(env_apt_all$a %in% env_apt_titles)
    
    for (i in 1:length(env_apt_heading_positions)) {
      if (i<length(env_apt_heading_positions)) {
        mytext <- apply(env_apt_all[(env_apt_heading_positions[i]+1):(env_apt_heading_positions[i+1]-1), ], 1, function(col) paste(col, collapse = "<br>"))
      } else{
        mytext <- apply(env_apt_all[(env_apt_heading_positions[i]+1):(env_apt_heading_positions[i]+1), ], 1, function(col) paste(col, collapse = "<br>"))
        
      }
      # replace repeated breaks by single  
      mytext <- gsub("(<br>)+", "<br>", mytext)
      
      #remove empty elements of vector
      mytext <- mytext[mytext != "<br>"]
      mytext <- mytext[mytext != ""]
      
      #replace double breaks by single breaks
      pattern <- "(<br>){2,}"
      mytext <- str_replace_all(mytext, pattern, "<br>")
      
      ### remove leading and trailing line breaks
      mytext <- sub("^<br>|<br>$", "", mytext)
      
      ### assign text to variable
      assign(paste0("env_apt_", i, ""), paste0(mytext,collapse = ""))
    }
    
    
    # # env apt old code based on ranges
    # range <- "A3:P4"
    # env_apt_1 <- read_range(env_apt_file, sheet, range)  
    # 
    # range <- "A5:P7"
    # env_apt_2 <- read_range(env_apt_file, sheet, range) 
    # env_apt_2 <- if_else(is.na(env_apt_2[2,1]) == FALSE, pull(env_apt_2[2,1]), pull(env_apt_2[1,8]))
    # 
    # range <- "A8:P10"
    # env_apt_3 <- read_range(env_apt_file, sheet, range) 
    # env_apt_3 <- if_else(is.na(env_apt_3[2,1]) == FALSE, pull(env_apt_3[2,1]), pull(env_apt_3[1,8]))
    # 
    # range <- "A11:P13"
    # env_apt_4 <- read_range(env_apt_file, sheet, range) 
    # env_apt_4 <- if_else(is.na(env_apt_4[2,1]) == FALSE, pull(env_apt_4[2,1]), pull(env_apt_4[1,8])) 
    # 
    # range <- "A14:P15"
    # env_apt_5 <- read_range(env_apt_file, sheet, range)   
  }
  
  # env mil
  range <- "A2:P80"
  env_mil_all <- read_range(env_mil_file, sheet, range) 
  env_mil_all <- env_mil_all |> 
    rename(a = 1) 
  
  # replace NAs by empty strings
  env_mil_all[is.na(env_mil_all)] <- ""
  
  env_mil_titles <- c("Update on Military dimension of the plan",
                      "**Update on Military dimension of the plan**",
                      "Military - related measures implemented or planned to improve environment and capacity",														
                      "**Military - related measures implemented or planned to improve environment and capacity**",											
                      "Military - related measures implemented or planned to improve capacity", 
                      "**Military - related measures implemented or planned to improve capacity**",
                      "PI#6 Effective use of reserved or segregated airspace - national level",
                      "**PI#6 Effective use of reserved or segregated airspace - national level**",
                      "Initiatives implemented or planned to improve PI#6",
                      "**Initiatives implemented or planned to improve PI#6**",
                      "**PI#7 Rate of planning via available airspace structures - national level**",
                      "PI#7 Rate of planning via available airspace structures - national level",
                      "Initiatives implemented or planned to improve PI#7",
                      "**Initiatives implemented or planned to improve PI#7**",
                      "PI#8 Rate of using available airspace structures - national level",
                      "**PI#8 Rate of using available airspace structures - national level**",
                      "Initiatives implemented or planned to improve PI#8",
                      "**Initiatives implemented or planned to improve PI#8**"
  )
  
  env_mil_heading_positions <- which(env_mil_all$a %in% env_mil_titles)
  
  for (i in 1:length(env_mil_heading_positions)) {
    if (i<length(env_mil_heading_positions)) {
      mytext <- apply(env_mil_all[(env_mil_heading_positions[i]+1):(env_mil_heading_positions[i+1]-1), ], 1, function(col) paste(col, collapse = "<br>"))
    } else{
      mytext <- apply(env_mil_all[(env_mil_heading_positions[i]+1):(env_mil_heading_positions[i]+4), ], 1, function(col) paste(col, collapse = "<br>"))
      
    }
    # replace repeated breaks by single  
    mytext <- gsub("(<br>)+", "<br>", mytext)
    
    #remove empty elements of vector
    mytext <- mytext[mytext != "<br>"]
    mytext <- mytext[mytext != ""]
    
    #replace double breaks by single breaks
    pattern <- "(<br>){2,}"
    mytext <- str_replace_all(mytext, pattern, "<br>")
    
    ### remove leading and trailing line breaks
    mytext <- sub("^<br>|<br>$", "", mytext)
    
    ### assign text to variable
    assign(paste0("env_mil_", i, ""), paste0(mytext,collapse = ""))
  }
  
  # 
  # range <- "A3:O7"
  # env_mil_3 <- read_range(env_mil_file, sheet, range) 
  # 
  # range <- "A8:O12"
  # env_mil_2 <- read_range(env_mil_file, sheet, range) 
  # 
  # range <- "A30:O34"
  # env_mil_5 <- read_range(env_mil_file, sheet, range) 
  # 
  # range <- "A52:O56"
  # env_mil_8 <- read_range(env_mil_file, sheet, range) 
  # 
  # range <- "A74:O78"
  # env_mil_11 <- read_range(env_mil_file, sheet, range) 
}
