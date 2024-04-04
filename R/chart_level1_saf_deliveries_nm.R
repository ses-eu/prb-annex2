
# parameters ----
if (exists("data_folder") == FALSE) {
  source("R/parameters.R")
}

mymetric <- "Percentage overdeliveries"
mychart_title <- mymetric
myaxis_title <- paste0(mymetric, " (%)")
mytooltip_decimals <- 1
mybarcolor <- '#FFC000'
mytextcolor <- 'black'

# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "NM_data.xlsx"),
  sheet = "PI_overdeliveries",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# prepare data ----
data_for_chart <- data_raw %>% 
  filter(
    year_report == .env$year_report) %>% 
  select(
    year,
    percentage_overdeliveries
  ) %>% 
  mutate(
    actual = round(percentage_overdeliveries * 100, 1)
      )

# plot chart ----
## moved to parameters  
mybarc(NA, 300, 14)

# export to image ----
w = 1200
h = 600
export_fig(mybarc(w, h, 14 * w/900),"saf_deliveries_main.png", w, h)

