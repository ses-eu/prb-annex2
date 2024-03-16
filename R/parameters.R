#libraries
library(tidyr)
library(dplyr)
library(openxlsx)
library(readxl)
library(plotly)
library(stringr)
library(janitor)
library(webshot)
library(data.table)
library(magick)
library(reactable)

# parameters ----

data_folder <- 'G:/HQ/dgof-pru/Data/SES Monitoring Dashboard/PBI files/'
country <- 'Spain'
year_report <- 2022

data_folder_a2 <- 'G:/HQ/dgof-pru/Data/SES Monitoring Dashboard/Annex 2/data/'

# functions ----
## right x characters function ----
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }  

## read range function ----
  read_range <- function(file, sheet, range){
    read_excel(
      file,
      sheet = sheet,
      range = range) %>% 
      mutate_all(~ str_replace_all(., "\r\n\r\n", "\r\n")) %>% 
      mutate_all(~ str_replace_all(., "<br><br><br><br>", "<br><br>"))   
  }

## export figure function ----
  # the export function needs webshot and PhantomJS. Install PhantomJS with 'webshot::install_phantomjs()' and then cut the folder from wherever is installed and paste it in C:\Users\[username]\dev\r\win-library\4.2\webshot\PhantomJS

  export_fig <- function (fig, fig_name, width, height) {
    fig_dir <- paste0('images/', year_report, '/', country,'/')
    invisible(export(fig, paste0(fig_dir, fig_name)))
    invisible(figure <- image_read(paste0(fig_dir,fig_name)))
    invisible(cropped <- image_crop(figure, paste0(width, "x", height)))
    invisible(image_write(cropped, paste0(fig_dir, fig_name)))
  }

# get main state parameters  ----
  wb <- loadWorkbook(paste0(data_folder, "Lists.xlsx"))
  tables <- getTables(wb, sheet = "Lists")
  # get the range
  table_range <- names(tables[tables == "Table_States"])
  # get parameters
  state_parameters <- read_range(paste0(data_folder, "Lists.xlsx"), "Lists", table_range) %>% 
    filter(State == country) %>% clean_names()

  main_ansp <- state_parameters %>% select(main_ansp) %>% pull()
  nat_curr <- state_parameters %>% select(currency) %>% pull()
  state_type <- state_parameters %>% select(dashboard_case) %>% pull()

  # get ecz list and forecast
  table_range <- names(tables[tables == "Table_ECZ"])
  
  ecz_list <- read_range(paste0(data_folder, "Lists.xlsx"), "Lists", table_range) %>% 
    filter(State == country) %>% 
    left_join(read_range(paste0(data_folder, "Lists.xlsx"), 
                         "Lists", 
                         names(tables[tables == "Table_forecast"])),
                               by = "forecast_id"
              ) %>% clean_names()

      # for spain we present only one  traffic zone
  if (country == "Spain") {
    statfor_zone <- ecz_list %>% filter(statfor_ecz_name == "Spain") %>% select(statfor_ecz_name) %>% pull()
     ecz_list <- ecz_list %>% filter (ecz_name != "Spain all")
  }

  forecast <- ecz_list %>% select(forecast) %>% pull() %>%  unique()
  forecastid <- ecz_list %>% select(forecast_id) %>% pull() %>%  unique()

  # get tcz list
  table_range <- names(tables[tables == "Table_TCZ"])
  
  tcz_list <- read_range(paste0(data_folder, "Lists.xlsx"), "Lists", table_range) %>% 
    filter(State == country) %>% clean_names()
  
# get ceff file ----
ceff_files <- list.files(paste0(data_folder_a2, 'ceff/'))

for (i in 1:length(ceff_files)) {
  if (grepl(country, ceff_files[i], fixed = TRUE) == TRUE) {
    ceff_file <- ceff_files[i]
  }
}

file <-  paste0(data_folder_a2, "ceff/", ceff_file)
ceff_file <-  paste0(data_folder_a2, "ceff/", ceff_file)

# get er cap file ----
cap_files <- list.files(paste0(data_folder_a2, 'cap/'))

for (i in 1:length(cap_files)) {
  if (grepl('RP3_monitoring_CAPACITY', cap_files[i], fixed = TRUE) == TRUE) {
    cap_file <- cap_files[i]
  }
}

cap_file <-  paste0(data_folder_a2, "cap/", cap_file)

# get trm cap file ----
for (i in 1:length(cap_files)) {
  if (grepl('RP3_monitoring_CAP_ARP', cap_files[i], fixed = TRUE) == TRUE) {
    cap_trm_file <- cap_files[i]
  }
}

cap_trm_file <-  paste0(data_folder_a2, "cap/", cap_trm_file)

# get env_kea file ----
env_files <- list.files(paste0(data_folder_a2, 'env/'))

for (i in 1:length(env_files)) {
  if (grepl('RP3_monitoring_KEA_VOL2', env_files[i], fixed = TRUE) == TRUE) {
    env_kea_file <- env_files[i]
  }
}

env_kea_file <-  paste0(data_folder_a2, "env/", env_kea_file)

# get env_apt file ----
for (i in 1:length(env_files)) {
  if (grepl('RP3_monitoring_ENV_ARP_VOL2', env_files[i], fixed = TRUE) == TRUE) {
    env_apt_file <- env_files[i]
  }
}

env_apt_file <-  paste0(data_folder_a2, "env/", env_apt_file)

# get env_mil file
for (i in 1:length(env_files)) {
  if (grepl('RP3_monitoring_ENV_MIL_VOL2', env_files[i], fixed = TRUE) == TRUE) {
    env_mil_file <- env_files[i]
  }
}

env_mil_file <-  paste0(data_folder_a2, "env/", env_mil_file)

# get saf file ----
saf_files <- list.files(paste0(data_folder_a2, 'saf/'))

for (i in 1:length(env_files)) {
  if (grepl('_Safety', saf_files[i], fixed = TRUE) == TRUE) {
    saf_eosm_file <- saf_files[i]
  }
}

saf_eosm_file <-  paste0(data_folder_a2, "saf/", saf_eosm_file)
