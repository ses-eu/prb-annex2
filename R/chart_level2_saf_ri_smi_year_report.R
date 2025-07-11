if (!exists("country") | is.na(country)) {country <- "Poland"
source("R/parameters.R")
}

if (!exists("safindicator")) {safindicator <- "smi"}


# import data  ----
if (safindicator == "ri") {
  ## ri ----
  if (country != "SES RP3"){
    ## state ----
    data_raw  <-  read_xlsx(
      paste0(data_folder, "SAF EoSM.xlsx"),
      # here("data","hlsr2021_data.xlsx"),
      sheet = "RI - occurrences",
      range = cell_limits(c(1, 1), c(NA, NA))) %>%
      as_tibble() %>% 
      clean_names() %>% 
      mutate(
        main_indicator = number_of_ri,
        main_indicator_ans = number_of_ri_with_ans_contribution
      )
  } else {
    ## ses ----
    data_raw  <-  read_xlsx(
      paste0(data_folder, "SES file.xlsx"),
      # here("data","hlsr2021_data.xlsx"),
      sheet = "RI SES variation",
      range = cell_limits(c(1, 1), c(NA, NA))) %>%
      as_tibble() %>% 
      clean_names() %>% 
      mutate(
        field = str_remove_all(field, " RI")
          )
        
  }
  
} else if (safindicator == "smi") {
  ## smi ----
  if (country != "SES RP3"){
    ## state ----
      data_raw  <-  read_xlsx(
        paste0(data_folder, "SAF EoSM.xlsx"),
        sheet = "SMI - occurrences",
        range = cell_limits(c(1, 1), c(NA, NA))) %>%
        as_tibble() %>% 
        clean_names() %>% 
        mutate(
          main_indicator = number_of_smi,
          main_indicator_ans = number_of_smi_with_ans_contribution
        )
  } else {
    data_raw  <-  read_xlsx(
      paste0(data_folder, "SES file.xlsx"),
      # here("data","hlsr2021_data.xlsx"),
      sheet = "SMI SES variation",
      range = cell_limits(c(1, 1), c(NA, NA))) %>%
      as_tibble() %>% 
      clean_names() %>% 
      mutate(
        field = str_remove_all(field, " SMI")
      )    
  }
}

# process data  ----
mylabels <- c(
  paste0("Number of ", toupper(safindicator),"&nbsp;&nbsp;\nin the MS&nbsp;&nbsp;"),
  paste0("Rate of ", toupper(safindicator),"&nbsp;&nbsp;\n(per 100,000 mvts)&nbsp;&nbsp;"),
  paste0("Number of ", toupper(safindicator),"&nbsp;&nbsp;\nwith ANS contribution&nbsp;&nbsp;"),
  paste0("Rate of ", toupper(safindicator),"&nbsp;&nbsp;\nwith ANS contribution&nbsp;&nbsp;\n(per 100,000 mvts)&nbsp;&nbsp;")
)

## state ----
if (country != "SES RP3"){
  data_calc <- data_raw %>% 
    filter(state == .env$country) %>% 
    arrange(year) %>% 
    mutate(
      main_indicator = if_else(is.na(main_indicator), 0, main_indicator),
      rate_per_100_000 = if_else(is.na(rate_per_100_000), 0, rate_per_100_000),
      main_indicator_ans = if_else(is.na(main_indicator_ans), 0, main_indicator_ans),
      rate_per_100_000_with_ans_contribution = if_else(is.na(rate_per_100_000_with_ans_contribution), 0, rate_per_100_000_with_ans_contribution),
      
      variation_perc = if_else(lag(main_indicator, 1) == 0, 
                               10^6, # to identify the NAs, we'll change them back later
                               main_indicator / lag(main_indicator, 1) -1) * 100,
      rate_variation_perc = if_else(lag(rate_per_100_000, 1) == 0, 
                                    10^6,
                                    rate_per_100_000 / lag(rate_per_100_000, 1) -1)* 100,
      ans_variation_perc = if_else(lag(main_indicator_ans, 1) == 0, 
                                   10^6, 
                                   main_indicator_ans / lag(main_indicator_ans, 1) -1)* 100,
      rate_ans_variation_perc = if_else(lag(rate_per_100_000_with_ans_contribution, 1) == 0, 
                                        10^6, 
                                        rate_per_100_000_with_ans_contribution / lag(rate_per_100_000_with_ans_contribution, 1) -1)* 100,
    ) %>% 
    filter(year == year_report)
  
  data_prep1 <- data_calc %>%
    select(
      main_indicator,
      rate_per_100_000, 
      main_indicator_ans, 
      rate_per_100_000_with_ans_contribution
    )  %>% 
    pivot_longer(cols = everything(), names_to = "ylabel", values_to = "mymetric") %>% 
    mutate(
      ylabel = case_when(
        ylabel == "main_indicator" ~ mylabels[[1]],
        ylabel == "rate_per_100_000" ~ mylabels[[2]],
        ylabel == "main_indicator_ans" ~ mylabels[[3]],
        ylabel == "rate_per_100_000_with_ans_contribution" ~ mylabels[[4]]
      ),
      mylabel = as.character(round(mymetric,2 ))
    )
  
  data_prep2 <- data_calc %>%
    select(
      variation_perc,
      rate_variation_perc, 
      ans_variation_perc, 
      rate_ans_variation_perc
    ) %>% 
    pivot_longer(cols = everything(), names_to = "ylabel", values_to = "mymetric") %>% 
    mutate(
      mylabel = if_else(mymetric == 10^8, "-",
          paste0(if_else(mymetric>=0, "+", ""),as.character(round(mymetric,1 )), "%")
          ),
      mymetric = if_else(mymetric == 10^8, 0, mymetric)
    )
  
  mylocal_factor = c(
    "rate_ans_variation_perc",
    "ans_variation_perc",
    "rate_variation_perc",
    "variation_perc",
    NULL
  )
  
} else {
  ## ses ----
  data_prep1 <- data_raw %>% 
    filter(year == year_report) %>% 
    select(field, value) %>% 
    mutate(
      ylabel = case_when(
        field == "Number of in the MS" ~ mylabels[[1]],
        field == "Rate of" ~ mylabels[[2]],
        field == "Number of with ANS contribution" ~ mylabels[[3]],
        field == "Rate of with ANS contribution" ~ mylabels[[4]]
      ),
      mylabel = as.character(round(value,2 ))
    ) %>% 
    select(
      ylabel,
      mylabel,
      mymetric = value
    )
  
  data_prep2 <- data_raw %>% 
    filter(year == year_report) %>% 
    select(field, percent_variation) %>% 
    mutate(
      mymetric = percent_variation * 100,
      ylabel = field,
      mylabel = paste0(if_else(mymetric>=0, "+", ""),as.character(round(mymetric,1 )), "%")
    )
  
  mylocal_factor = c(
    "Rate of with ANS contribution",
    "Number of with ANS contribution",
    "Rate of",
    "Number of in the MS",
    NULL
  ) 
}


# plot charts  ----
if (knitr::is_latex_output()) {
  local_xaxis_tickfont_size <- myfont-3  
  local_textfont_size <- myfont-2  
} else {
  local_xaxis_tickfont_size <- myfont-1
  local_textfont_size <- myfont
}

p1 <- myhbarc2(df = data_prep1,
               suffix = "",
               local_factor = c(
                 mylabels[[4]],
                 mylabels[[3]],
                 mylabels[[2]],
                 mylabels[[1]],
                 NULL
               ),
               hovertemplate = paste0('%{x:,.1f}<extra></extra>'),         
               mybarcolor_pos = '#196AB5',
               mybarcolor_neg = 'transparent',
               
               textposition = "outside",
               textfont_size = local_textfont_size,
               
               xaxis_tickfont_size =  local_xaxis_tickfont_size,
               yaxis_tickfont_size = myfont -2,
               
               title_text = "",
               hovermode = "closest",
               margin = list(t= 40)
)

p2 <- myhbarc2(df = data_prep2,
               suffix = "%",
               local_factor = mylocal_factor,
               hovertemplate = paste0('%{x:,.1f}%<extra></extra>'),         
               hovermode = "closest",
               mybarcolor_pos = '#FFC000',
               mybarcolor_neg = '#92D050',
               
               textposition = "outside",
               textfont_size = local_textfont_size,
               
               xaxis_ticksuffix = "%",
               xaxis_tickangle = 0,
               xaxis_tickfont_size =  local_xaxis_tickfont_size,
               yaxis_showticklabels = FALSE,
               
               title_text = paste0(year_report, " vs ",year_report-1) ,
               title_x = 0.98,
               title_y = 0.83,
               title_xanchor = "center",
               title_yanchor =  "center",
               
               margin = list(t= 40, r = 50, l = 0)
)

subplot(p1, p2, nrows = 1, shareY = FALSE, titleX = TRUE, titleY = TRUE, widths = c(0.50, 0.50), margin = 0.10) %>% 
  layout(showlegend = FALSE)

