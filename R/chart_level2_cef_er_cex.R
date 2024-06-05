
# fix ez if script not executed from qmd file ----
if (exists("ez") == FALSE) {ez = 1}
# ez=1

# initialise list to store plots ----
myplot = list()

# loop through czs ----
for (ez in 1:no_ecz) {
  ## import data  ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "CEFF dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Enroute_T2",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----
  data_prep_t2 <- data_raw %>% 
    filter(
      entity_code == ecz_list$ecz_id[ez],
    ) %>% 
    ## to be used for the table, just kept until the table script is created
    # select(
    #   year,
    #   x3_1_investment,
    #   x3_3_cost_authority_qes,
    #   x3_4_ectl_cost_eur,
    #   x3_5_pension_cost,
    #   x3_6_interest_loan,
    #   x3_7_change_in_law
    # ) %>% 
    # pivot_longer(cols = -c(year),
    #              names_to = 'type', 
    #              values_to = 'mymetric') %>% 
    # mutate(xlabel = rep(c("New and existing investments", 
    #                       "Competent authorities\nand qualified entities costs",
    #                       "Eurocontrol costs",
    #                       "Pension costs",
    #                       "Interest on loans",
    #                       "Changes in law"), 4),
    #        year_text = as.character(year),
    #        year_text = str_replace(year_text, "20202021", "2020-2021"),
    # )
    select(year, x3_8_diff_det_cost_actual_cost) %>% 
    mutate(
      actual = case_when(
        year > year_report & year != 20202021 ~ NA,
        .default = round(x3_8_diff_det_cost_actual_cost/1000,0)
        ),
      year = as.character(year),
      year = str_replace(year, "20202021", "2020-2021")
    )
    
  # t exchange rates
  yearly_xrates <- get_xrates()
  
  data_prep_xrates <- yearly_xrates %>% 
    filter(
      entity_code == ecz_list$ecz_id[ez]
    ) %>% 
    select(-entity_code) %>% 
    filter(year > 2020) %>% 
    mutate(year = as.character(year),
           year = if_else(year == '2021', '2020-2021', year)
          ) 
  
  data_for_chart <- data_prep_t2 %>% 
    left_join(data_prep_xrates, by = 'year') %>% 
    mutate(actual = actual/pp_exchangerate) %>% 
    arrange(year) 
  
  
  ## chart parameters ----
  mychart_title <- paste0("Cost exempt")
  mymetric <- "Cost exempt"
  myaxis_title <- "Cost exempt from cost sharing\n(€'000)"
  mybarcolor <- c( '#8497B0')

  mytextangle <- 0
  mytextposition <- "auto"
  
  mydtick <- '1'
  mytickformat_x <- "0"
  mytextsize <- myfont
  mylabelposition <- NA
  
  myticksuffix <- ""
  mytickformat_y <- ",.0f"
  
  mytooltip_decimals <- 0

  ## define chart function ----
  ### function moved to utils
  
  ## plot chart  ----
  myplot[[ez]] <- mybarc(mywidth, myheight+20, myfont, mymargin)
  
}

# create html plotlist ----
htmltools::tagList(myplot)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document
