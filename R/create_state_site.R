#libraries
library(readxl)
library(openxlsx)
library(dplyr)
# library(tidyr)
library(stringr)
library(here)
library(fs)

rm(list=ls()) 

## parameters
source("R/parameters.R")

# get all R scripts
rscripts <- list.files(here('R'))

# get only 'generate' scripts
genscripts <- list()
for (i in 1:length(rscripts)) {
  if (grepl('generate', rscripts[i], fixed = TRUE) == TRUE) {
    genscripts <- append(genscripts, rscripts[i])
  }
}

# remove to be regenerated later
file.remove('_variables.yml')
newvariables <- paste0("doc:
  year_report: ", year_report, "
  country: '", country, "'
"
)

cat(newvariables, file = "_variables.yml")

# remove to be regenerated later
file.remove('_quarto.yml')

# no terminal case
if (state_type == 0) {
  genscripts <- list('generate_index_qmd.R',
                     'generate_saf_qmd.R',
                     'generate_env_kea_qmd.R',
                     'generate_env_mil_qmd.R',
                     'generate_cap_er_qmd.R',
                     'generate_ceff1_qmd.R',
                     'generate_ceff2_qmd.R',
                     'generate_ceff3_qmd.R')
  
  file.copy('R/yaml_full_version.txt', '_quarto.yml')
  tx  <- readLines("_quarto.yml")
  tx  <- gsub(pattern = "- capacity_trm.qmd", replace = "  # - capacity_trm.qmd", x = tx)
  tx  <- gsub(pattern = "- cost-efficiency-tz1-1.qmd", replace = "  # - cost-efficiency-tz1-1.qmd", x = tx)
  tx  <- gsub(pattern = "- cost-efficiency-tz1-2.qmd", replace = "  # - cost-efficiency-tz1-2.qmd", x = tx)
  tx  <- gsub(pattern = "- cost-efficiency-tz1-3.qmd", replace = "  # - cost-efficiency-tz1-3.qmd", x = tx)
  tx  <- gsub(pattern = "- cost-efficiency-tz2-1.qmd", replace = "  # - cost-efficiency-tz2-1.qmd", x = tx)
  tx  <- gsub(pattern = "- cost-efficiency-tz2-2.qmd", replace = "  # - cost-efficiency-tz2-2.qmd", x = tx)
  tx  <- gsub(pattern = "- cost-efficiency-tz2-3.qmd", replace = "  # - cost-efficiency-tz2-3.qmd", x = tx)
  tx  <- gsub(pattern = "- cost-efficiency-g2g.qmd", replace = "  # - cost-efficiency-g2g.qmd", x = tx)
  tx  <- gsub(pattern = "- environment_apt.qmd", replace = "  # - environment_apt.qmd", x = tx)
  
  writeLines(tx, con="_quarto.yml")

} else if (state_type == 1) {
  file.copy('R/yaml_full_version.txt', '_quarto.yml')
  tx  <- readLines("_quarto.yml")
  tx  <- gsub(pattern = "- cost-efficiency-tz2-1.qmd", replace = "  # - cost-efficiency-tz2-1.qmd", x = tx)
  tx  <- gsub(pattern = "- cost-efficiency-tz2-2.qmd", replace = "  # - cost-efficiency-tz2-2.qmd", x = tx)
  tx  <- gsub(pattern = "- cost-efficiency-tz2-3.qmd", replace = "  # - cost-efficiency-tz2-3.qmd", x = tx)

    # check if there are other term ATSPs
  sheet <- "8_TRM_ATSP"
  range <- "C13:M17" 
  trm_2_14_1  <- read_range(file, sheet, range) %>% select(c(6:11)) 
  atspcheck <- sum(is.na(trm_2_14_1) == TRUE)

  #remove entry from menu and from list of pages to generate
  if (atspcheck == 24) {
    genscripts <- genscripts[genscripts != "generate_ceff6_tz_qmd.R"]
    tx  <- readLines("_quarto.yml")
    tx2  <- gsub(pattern = "- cost-efficiency-tz1-3.qmd", 
                 replace = "  # - cost-efficiency-tz1-3.qmd", 
                 x = tx)
    writeLines(tx2, con="_quarto.yml")
  } 
}

# check if there are other er ATSPs
sheet <- "4_ATSP"
range <- range <- "C13:M17" 
ert_2_14_1  <- read_range(file, sheet, range) %>% select(c(6:11))
atspcheck <- sum(is.na(ert_2_14_1) == TRUE)

if (atspcheck == 24) {
  genscripts <- genscripts[genscripts != "generate_ceff3_qmd.R"]
  tx  <- readLines("_quarto.yml")
  tx2  <- gsub(pattern = "- cost-efficiency-er1-3.qmd", replace = "  # - cost-efficiency-er1-3.qmd", x = tx)
  writeLines(tx2, con="_quarto.yml")
} 



# delete and regenerate .qmd files
root_files <- list.files()
for (i in 1:length(root_files)) {
  if (grepl('.qmd', root_files[i], fixed = TRUE) == TRUE) {
    file.remove(root_files[i])
  }
}

for (i in 1:length(genscripts)) {
  source(here("R", genscripts[i]))
}


quarto::quarto_render()

stop()

# copy site to test folder
site_dir <- here("_site")
destination_dir <- paste0('//ihx-vdm05/LIVE_var_www_performance$/oscar/prb-monitoring-test/2022/')


unlink(paste0(destination_dir, country), recursive = TRUE) 
file.copy(site_dir, destination_dir, overwrite = TRUE, recursive=TRUE, copy.mode = TRUE)
file.rename(paste0(destination_dir, "/_site"), paste0(destination_dir, country))
# copyDirectory(here::here("images"), paste0(destination_dir,"images"), overwrite = TRUE)


