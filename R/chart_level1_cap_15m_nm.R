# 
# # parameters ----
# if (exists("data_folder") == FALSE) {
#   source("R/parameters.R")
# }

mymetric <- "IFR flights with ATFM delay > 15 min"
mychart_title <- "IFR flights with ATFM delay above 15 min."
myaxis_title <- paste0(mymetric, " (%)")
mytooltip_decimals <- 1
mybarcolor <- '#ED7D31'
mytextcolor <- 'white'

# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "NM_data.xlsx"),
  sheet = "Capacity_PIs",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# prepare data ----
data_for_chart <- data_raw %>% 
  filter(
    year_report == .env$year_report) %>% 
  select(
    year,
    percent_above_15_min
  ) %>% 
  mutate(
    actual = round(percent_above_15_min * 100, 1)
      )

# plot chart ----
## moved to parameters  
mybarc(NA, 300, 14)

# export to image ----
w = 1200
h = 600
export_fig(mybarc(w, h, 14 * w/900),"cap_ifr_15_main.png", w, h)

