# descriptive data analysis -----------------------------------------------
#
#
#
library(tidyverse)
library(haven)
dpth <- "/home/chief/Dropbox/research/GZ/data/raw/data request_ilo_JLP13.dta"
data_raw <- read_dta(file = dpth)
# RENAMING "IRAN, ISLAMIC REPUBLIC OF" INTO "IRAN"
# View(data_raw$country[data_raw$country == "Iran, Islamic Republic of"])
# View(data_raw$country[1628:1666])
data_raw$country[data_raw$country == "Iran, Islamic Republic of"] <- "Iran"
# View(data_raw$country[data_raw$country == "Iran, Islamic Republic of"])
# View(data_raw$country[data_raw$country == "Iran"])
# View(data_raw$country[1628:1666])
#
#
#
# I. SELECT SUBSET OF COUNTRIES USED IN JLP13
adv_econ_subset <- c("Australia", "Austria", "Belgium", "Canada", "Denmark",
                     "Finland", "France",  "Germany", "Ireland", "Israel",
                     "Italy", "Japan", "Korea", "Netherlands",  "Norway",
                     "Singapore", "Spain", "Sweden", "United Kingdom",
                     "United States")
dev_econ_subset <- c("Argentina", "Bangladesh", "Bolivia", "Brazil", "Chile",
                     "China", "Costa Rica", "Ecuador", "Egypt", "El Salvador",
                     "Ghana", "Guatemala", "Honduras", "India", "Indonesia",
                     "Iran", "Kenya", "Malaysia", "Mexico", "Pakistan",
                     "Panama", "Paraguay", "Peru", "Philippines", "Sri Lanka",
                     "Thailand", "Turkey", "Uganda", "Uruguay", "Venezuela",
                     "Zambia")
subset_countries <- sort(c(adv_econ_subset, dev_econ_subset))
#
#
#
# II. DEFINE REGIONS ACCORDING TO POVCAL CLASSIFICATION
region_povcal_europe_central_asia <- c("Turkey")
region_povcal_south_asia <- c("Bangladesh",  "India", "Pakistan",  "Sri Lanka")
region_povcal_east_asia_pacific <- c("China", "Indonesia", "Malaysia",
                                     "Philippines", "Thailand")
region_povcal_latin_america_caribbean <- c("Argentina", "Bolivia", "Brazil",
                                           "Chile", "Costa Rica", "Ecuador",
                                           "El Salvador", "Guatemala",
                                           "Honduras","Mexico", "Panama",
                                           "Paraguay", "Peru", "Uruguay",
                                           "Venezuela")
region_povcal_middle_east_north_africa <- c("Egypt", "Iran")
region_povcal_middle_sub_saharan_africa <- c("Ghana","Kenya", "Uganda",
                                             "Zambia")
regions_povcal_all <- list(region_povcal_east_asia_pacific,
                           region_povcal_europe_central_asia,
                           region_povcal_latin_america_caribbean,
                           region_povcal_middle_east_north_africa,
                           region_povcal_middle_sub_saharan_africa,
                           region_povcal_south_asia)
regional_coverage <- length(unlist(regions_povcal_all))
#
#
#
# III. COMPARE D
print(regional_coverage)
length(dev_econ_subset)  # THERE IS A TOTAL of 31 developing countries in JLP13
length(adv_econ_subset)  # THERE IS A TOTAL of 20 developed countries in JLP13
length(subset_countries) # THERE IS A TOTAL of 51 countries in JLP13

#
#
#
data_JLP13 <- filter(data_raw, country %in% subset_countries)
