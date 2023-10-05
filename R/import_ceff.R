#libraries
library(dplyr)
library(readxl)
library(openxlsx)
library(reactable)

## parameters
source("R/parameters.R")

# get ceff file
ceff_files <- list.files(paste0(data_folder_a2, 'ceff/'))

grepl(country, ceff_files[1], fixed = TRUE)

for (i in 1:length(ceff_files)) {
  if (grepl(country, ceff_files[i], fixed = TRUE) == TRUE) {
    ceff_file <- ceff_files[i]
  }
}

# read range function
read_range <- function(file, sheet, range){
  read_excel(
    file,
    sheet = sheet,
    range = range)   
}

# define range
file <-  paste0(data_folder_a2, "ceff/", ceff_file)
sheet <- "1_ERT"

range <- "C11:M15"
ert_1_1  <- read_range(file, sheet, range)

range <- "C16:M17"
ert_1_2  <- read_range(file, sheet, range)

range <- "C20:M27"
ert_1_3  <- read_range(file, sheet, range)

