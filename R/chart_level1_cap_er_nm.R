
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

  mymetric <- "En-route ATFM delay savings"
  mychart_title <- paste0("Percentage of ", str_to_lower(mymetric)) %>% str_replace("atfm", "ATFM")
  myaxis_title <- paste0(mymetric, " (%)")
  mytooltip_decimals <- 1
  targetcolor <- 'transparent'
  
## import data  ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "NM_data.xlsx"),
    sheet = "Capacity_En route",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
## prepare data ----
  data_for_chart <- data_raw %>% 
    filter(year_report == .env$year_report) %>% 
    mutate(
      target = round(target * 100, 0),
      actual = round(actual * 100, 1)
    ) %>% 
    select(year, target, actual)


# plot chart ----
## function moved to parameters  
  mybarct(NA, 300, 14)

# export to image ----
w = 1200
h = 600
export_fig(mybarct(w, h, 14 * w/900), paste0("cap_er_savings_main.png"), w, h)

