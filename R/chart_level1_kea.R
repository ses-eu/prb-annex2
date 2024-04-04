
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

if (country != "Network Manager") {
  # State case ----
  mymetric <- "KEA"
  mychart_title <- paste0(mymetric, " - ", country)
  myaxis_title <- paste0(mymetric, " (%)")
  mytooltip_decimals <- 2
  targetcolor <- '#FF0000'

  ## import data  ----
  data_raw_target  <-  read_xlsx(
    paste0(data_folder, "targets.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "KEA",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_raw_actual  <-  read_xlsx(
    paste0(data_folder, "HFE_clean.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "Table_HFE",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----
  data_prep_target <- data_raw_target %>% 
    filter(
      state == .env$country
      ) %>% 
    mutate(
      # type = 'Target',
      target = round(x321_kea_target * 100, 2)
    ) %>% 
    select(
      year,
      target
    )
  
  data_prep_actual <- data_raw_actual %>% 
    filter(
      entity_name == country) %>% 
    mutate (actual = hfe_kpi) %>% 
    select(
      year,
      actual
    ) 
  
  data_for_chart <-  merge(x = data_prep_target, y = data_prep_actual, by="year", all.x = TRUE) 
  
} else {
  # NM case ----
    mymetric <- "KEP"
    mychart_title <- mymetric
    myaxis_title <- paste0(mymetric, " (%)")
    mytooltip_decimals <- 2
    targetcolor <- '#FF0000'
    
    ## import data  ----
    data_raw  <-  read_xlsx(
      paste0(data_folder, "NM_data.xlsx"),
      sheet = "Environment",
      range = cell_limits(c(1, 1), c(NA, NA))) %>%
      as_tibble() %>% 
      clean_names() 
    
    ## prepare data ----
    data_for_chart <- data_raw %>% 
      filter(year_report == .env$year_report) %>% 
      mutate(
        target = round(nm_target * 100, 2),
        actual = round(actual * 100, 2)
        ) %>% 
      select(year, target, actual)
    }


# plot chart ----
## function moved to parameters  
mybarct(NA, 300, 14)

# export to image ----
w = 1200
h = 600
export_fig(mybarct(w, h, 14 * w/900), paste0("env_", mymetric , "_main.png"), w, h)

