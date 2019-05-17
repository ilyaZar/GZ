# source paths for knitr document: ----------------------------------------
source("../r_code/00_helper_general.R")
source("../r_code/00_helper_duplications.R")
source("../r_code/00_helper_reg.R")
source("../r_code/00_helper_data_diagnostics.R")
# source paths for manual sourcing: ---------------------------------------
# source("data/r_code/00_helper_general.R")
# source("data/r_code/00_helper_duplications.R")
# source("data/r_code/00_helper_reg.R")
# source("data/r_code/00_helper_data_diagnostics.R")
# loading libraries: ------------------------------------------------------
library(tidyverse)
library(rlang)

local_dtpth <- "/home/chief/Dropbox/research/GZ/data/raw_data"

dtpth_reg <- paste(local_dtpth, "/data request_ilo_JLP13.dta", sep = "")
data_raw_reg <- haven::read_dta(file = dtpth_reg)

url_wiid4 <- "https://www.wider.unu.edu/sites/default/files/WIID/WIID_19Dec2018.xlsx"
dtpth_dep <- paste(local_dtpth,
                   paste("/WIID4", Sys.Date(), sep = "_"),
                   sep = "")
download.file(url = url_wiid4, destfile = dtpth_dep)
data_raw_dep <- readxl::read_excel(path = dtpth_dep)
