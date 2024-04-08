
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

# import data ----
sheet <- c("8_TRM_ATSP", "8_TRM_ATSP (2)")
if (exists("tz") == FALSE) {tz = 1}

## check how many tables are needed ----
range <- "C13:C66"
trm_2_14_0 <- read_range(ceff_file, sheet[tz], range)
notables <- 5- sum(str_count(trm_2_14_0[[1]], "Other ANSP planned regulatory result"), na.rm=T)

##https://stackoverflow.com/questions/75575583/interactive-ggiraph-objects-created-in-a-loop-does-not-show-in-quarto-html-outpu
t <- list()
t1 <- list()
t2 <- list()

for (i in 1:notables) {
  ## define ranges for plot ----
  range <- paste0("C", 13 + (i - 1)*11, ":M", 17 + (i - 1)*11)
  trm_2_14_1  <- read_range(ceff_file, sheet[tz], range) 
  
  range <- paste0("C", 18 + (i - 1)*11, ":M", 22 + (i - 1)*11) 
  trm_2_14_2  <- read_range(ceff_file, sheet[tz], range)

  ## data prep 1 ----
  myrownames <- trm_2_14_1[1]
  mycolnames <- colnames(trm_2_14_1)

  trm_2_14_1_s <- trm_2_14_1 %>% select(!c(1:5)) 
  nacheck <- if_else(trm_2_14_1_s[4,1] == 'N/A' | is.na(trm_2_14_1_s[4,1]) == TRUE, 1, 0)

  if (nacheck == 1) {
    trm_2_14_1_s <- trm_2_14_1_s %>% 
      mutate_all(~ str_replace(., "N/A", "0"))  %>% 
      mutate_all(~ as.numeric(.))
  }

  trm_2_14_1_t <- transpose(trm_2_14_1_s) %>% 
    mutate_all(~ as.numeric(.)) %>% 
    mutate(across(c(1,2), ~format(round(.,0), big.mark = ",", scientific = F))) %>%
    mutate(across(c(3,4), ~paste0(format(round(.*100,1)), "%")))

  colnames(myrownames) <- "a"

  trm_2_14_1_tt <- transpose(trm_2_14_1_t) %>% as_tibble() %>% 
    mutate(myrownames,
           .before = V1) 
  
  data_for_table1 <- trm_2_14_1_tt %>% 
    mutate_all(~ str_replace(., "NA", ""))
  
  if (nacheck == 1) {
    data_for_table1[4,] <- 'N/A'
    data_for_table1[4,1] <- 'Ex-post RoE pre-tax rate (in %)'
  }

  ## data prep 2 ----
  myrownames <- trm_2_14_2[1]
  mycolnames <- colnames(trm_2_14_1)
  
  trm_2_14_2_s <- trm_2_14_2 %>% select(!c(1:5)) 
  nacheck <- if_else(trm_2_14_2_s[4,1] == 'N/A' | is.na(trm_2_14_2_s[4,1]) == TRUE, 1, 0)
  
  if (nacheck == 1) {
    trm_2_14_2_s <- trm_2_14_2_s %>% 
      mutate_all(~ str_replace(., "N/A", "0"))  %>% 
      mutate_all(~ as.numeric(.))
  }
  
  trm_2_14_2_t <- transpose(trm_2_14_2_s) %>% 
    mutate_all(~ as.numeric(.)) %>% 
    mutate(across(c(1,2), ~format(round(.,0), big.mark = ",", scientific = F))) %>%
    mutate(across(c(3,4), ~paste0(format(round(.*100,1)), "%"))) %>% 
    as_tibble() %>% 
    mutate_all(~ str_replace(., "NA%", NA_character_)) %>% 
    mutate_all(~ str_replace(., "NA", NA_character_))
  
  colnames(myrownames) <- "a"
  
  trm_2_14_2_tt <- transpose(trm_2_14_2_t)  %>% 
    mutate(myrownames,
           .before = V1) 
  
  data_for_table2 <- trm_2_14_2_tt 
  
  if (nacheck == 1) {
    for (j in 1:(1+(year_report-2020))) {
      data_for_table2[4,j+1] <- 'N/A'
    }
    if (year_report>2020) {data_for_table2[4,j+2] <- 'N/A'}
    data_for_table2[4,1] <- 'Ex-post RoE pre-tax rate (in %)'
  }

  ## plot tables ----
  t1[[i]] <- reactable(
    data_for_table1,
    bordered = TRUE,
    pagination = FALSE,
    striped = FALSE,
    compact = TRUE,
    highlight = TRUE,
    defaultColDef = colDef(style = list(
                            "font-size" = "0.75rem",
                            "white-space"= "wrap",
                            # background = "#D9D9D9", 
                            fontWeight = "bold"
                            ),
                           align = "right",
                           headerStyle = list(
                             background = "#D9D9D9",
                             fontSize = "0.75rem",
                             style=list("white-space"= "wrap")
                             )
                           
    ),
    columns = list(
      a = colDef(name = mycolnames[1], 
                                      minWidth = 43, 
                                      align = "left"
                                        
                           ), # to preserve whitespace,
      V1 = colDef(name = "2020D", 
                  minWidth = 9),
      V2 = colDef(name = "2021D", 
                  minWidth = 9),
      V3 = colDef(name = "2020-2021D", 
                  minWidth = 12),
      V4 = colDef(name = "2022D", 
                  minWidth = 9),
      V5 = colDef(name = "2023D", 
                  minWidth = 9),
      V6 = colDef(name = "2024D", 
                  minWidth = 9)
      ),
    borderless = TRUE
  )
  
  t2[[i]] <- reactable(
    data_for_table2,
    bordered = TRUE,
    pagination = FALSE,
    striped = FALSE,
    compact = TRUE,
    highlight = TRUE,
    defaultColDef = colDef( style = function(value) {
      color <- if (is.na(value)) {'#F2F2F2'} 
      list(background = color,
           fontSize = "0.72rem",
           fontWeight = "bold",
           "white-space"= "wrap")
    } ,
    align = "right",
    headerStyle = list(
      background = "#D9D9D9", 
      # color = "white", 
      fontSize = "0.75rem",
      style=list("white-space"= "wrap")
      ),
    footerStyle = list(
      fontSize = "0.1rem",
      background = "white",
      borderTop = "1px solid rgba(0, 0, 0, 0.1)",
      borderRight = "1px solid rgba(255, 255, 255, 1)",
      borderLeft = "1px solid rgba(255, 255, 255, 1)"
      )
    ),
    columns = list(
      a = colDef(name = str_replace(mycolnames[1], "planned", "actual"), 
                 minWidth = 43, 
                 align = "left",
                 footer = ""
        ), 
      V1 = colDef(name = "2020A", 
                  minWidth = 9),
      V2 = colDef(name = "2021A", 
                  minWidth = 9),
      V3 = colDef(name = "2020-2021A", 
                  minWidth = 12),
      V4 = colDef(name = "2022A", 
                  minWidth = 9),
      V5 = colDef(name = "2023A", 
                  minWidth = 9),
      V6 = colDef(name = "2024A", 
                  minWidth = 9)
    ),
    borderless = TRUE
  )
  
  t[[2*i-1]] <- t1[[i]]
  t[[2*i]] <- t2[[i]]

}
