
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
  ############### to be adapted to reg res
  data_prep <- data_raw %>% 
    filter(
      entity_code == ecz_list$ecz_id[ez],
    ) %>% 
    select(
      year,
      x3_1_investment,
      x3_3_cost_authority_qes,
      x3_4_ectl_cost_eur,
      x3_5_pension_cost,
      x3_6_interest_loan,
      x3_7_change_in_law
    ) %>% 
    pivot_longer(cols = -c(year),
                 names_to = 'type', 
                 values_to = 'mymetric') %>% 
    mutate(xlabel = rep(c("Main ANSP",
                          "Other ANSP",
                          "MET"), 4),
           year_text = as.character(year),
           year_text = str_replace(year_text, "20202021", "2020-2021"),
    )
  
  
  ## chart parameters ----
  mychart_title <- paste0("Regulatory result - ", year_report)
  myaxis_title <- "Regulatory result (€M)"
  mybarcolor <- c( '#5B9BD5', '#FFC000', '#BFBFBF')
  mytextcolor <- 'black'
  mylegend_y_position <- -0.2
  myfactor <- c("Main ANSP",
    "Other ANSP",
    "MET")
  
  ## define chart function ----
  ### function moved to utils
  
  ## plot chart  ----
  myplot[[ez]] <- mybarc_group(mywidth, myheight+30, myfont, mymargin)
  
}

# create html plotlist ----
htmltools::tagList(myplot)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document
