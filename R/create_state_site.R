#libraries
library(readxl)
library(openxlsx)
library(dplyr)
# library(tidyr)
library(stringr)
library(here)

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
}

# delete .qmd files
root_files <- list.files()
for (i in 1:length(root_files)) {
  if (grepl('.qmd', root_files[i], fixed = TRUE) == TRUE) {
    file.remove(root_files[i])
  }
}

# create yaml file depending on state type
file.remove('_quarto.yml')
if (state_type == 0) {
  file.copy('R/yaml_no_terminal.txt', '_quarto.yml')
} else if (state_type == 1) {
  file.copy('R/yaml_full_version.txt', '_quarto.yml')
}

for (i in 1:length(genscripts)) {
  source(here("R", genscripts[i]))
}




quarto::quarto_render()


