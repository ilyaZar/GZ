# source paths for knitr document: ----------------------------------------
# source("../../../R/helper_data_cleaning/00_helper_data_general.R")
# source("../../../R/helper_data_cleaning/00_helper_data_duplications.R")
# source("../../../R/helper_data_cleaning/00_helper_data_reg.R")
# source("../../../R/helper_data_cleaning/00_helper_data_diagnostics.R")
# source paths for manual sourcing: ---------------------------------------
source("R/helper_data_cleaning/00_helper_data_general.R")
source("R/helper_data_cleaning/00_helper_data_duplications.R")
source("R/helper_data_cleaning/00_helper_data_reg.R")
source("R/helper_data_cleaning/00_helper_data_diagnostics.R")
# loading libraries: ------------------------------------------------------
library(tidyverse)
library(magrittr)
library(haven)
library(rlang)

local_dtpth <- "/home/chief/Dropbox/research/GZ/data/raw_data"

dtpth_reg <- paste(local_dtpth, "/data request_ilo_JLP13.dta", sep = "")
data_raw_reg <- haven::read_dta(file = dtpth_reg)

# url_wiid4 <- "https://www.wider.unu.edu/sites/default/files/WIID/WIID_19Dec2018.xlsx"
# dtpth_dep <- paste(local_dtpth,
#                    paste("/WIID4", Sys.Date(), sep = "_"),
#                    sep = "")
# download.file(url = url_wiid4, destfile = dtpth_dep)

dtpth_dep <- paste(local_dtpth, "/WIID4_2019-06-12", sep = "")
data_raw_dep <- readxl::read_excel(path = dtpth_dep)
# source paths for manual sourcing: ---------------------------------------
source("analysis/2019-05-15/01_data_reg_preparation.R")
source("analysis/2019-05-15/02_data_dep_preparation.R")
source("analysis/2019-05-15/03_data_merge_reg_dep.R")
