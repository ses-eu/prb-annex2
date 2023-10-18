#libraries
library(dplyr)
library(readxl)
library(openxlsx)
library(data.table)
library(tidyr)
library(stringr)

# parameters

data_folder <- 'G:/HQ/dgof-pru/Data/SES Monitoring Dashboard/PBI files/'
country <- 'Austria'
year_report <- 2022

data_folder_a2 <- 'G:/HQ/dgof-pru/Data/SES Monitoring Dashboard/Annex 2/data/'

# read range function
read_range <- function(file, sheet, range){
  read_excel(
    file,
    sheet = sheet,
    range = range) %>% 
      mutate_all(~ str_replace_all(., "\r\n\r\n", "\r\n")) %>% 
      mutate_all(~ str_replace_all(., "<br><br><br><br>", "<br><br>"))   
}



# get ceff file
ceff_files <- list.files(paste0(data_folder_a2, 'ceff/'))

for (i in 1:length(ceff_files)) {
  if (grepl(country, ceff_files[i], fixed = TRUE) == TRUE) {
    ceff_file <- ceff_files[i]
  }
}

file <-  paste0(data_folder_a2, "ceff/", ceff_file)
ceff_file <-  paste0(data_folder_a2, "ceff/", ceff_file)

# get main ANSP and currency
wb <- loadWorkbook(paste0(data_folder, "Lists.xlsx"))
tables <- getTables(wb, sheet = "Lists")
# get the range
table_range <- names(tables[tables == "Table_States"])
# get main ANSP
state_parameters <- read_range(paste0(data_folder, "Lists.xlsx"), "Lists", table_range) %>% 
  filter(State == country)

main_ansp <- state_parameters %>% select(Main_ANSP) %>% pull()
nat_curr <- state_parameters %>% select(Currency) %>% pull()
state_type <- state_parameters %>% select(dashboard_case) %>% pull()

# get er cap file
cap_files <- list.files(paste0(data_folder_a2, 'cap/'))

for (i in 1:length(cap_files)) {
  if (grepl('RP3_monitoring_CAPACITY', cap_files[i], fixed = TRUE) == TRUE) {
    cap_file <- cap_files[i]
  }
}

cap_file <-  paste0(data_folder_a2, "cap/", cap_file)

# get trm cap file
for (i in 1:length(cap_files)) {
  if (grepl('RP3_monitoring_CAP_ARP', cap_files[i], fixed = TRUE) == TRUE) {
    cap_trm_file <- cap_files[i]
  }
}

cap_trm_file <-  paste0(data_folder_a2, "cap/", cap_trm_file)

# get env_kea file
env_files <- list.files(paste0(data_folder_a2, 'env/'))

for (i in 1:length(env_files)) {
  if (grepl('RP3_monitoring_KEA_VOL2', env_files[i], fixed = TRUE) == TRUE) {
    env_kea_file <- env_files[i]
  }
}

env_kea_file <-  paste0(data_folder_a2, "env/", env_kea_file)

# get env_apt file
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

# get saf file
saf_files <- list.files(paste0(data_folder_a2, 'saf/'))

for (i in 1:length(env_files)) {
  if (grepl('_Safety', saf_files[i], fixed = TRUE) == TRUE) {
    saf_eosm_file <- saf_files[i]
  }
}

saf_eosm_file <-  paste0(data_folder_a2, "saf/", saf_eosm_file)
