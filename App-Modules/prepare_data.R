# Creates a list of the countries present in the PFU output data
countries <- unique(PSUT_Agg_Re_all_St_pfu$Country)

# Set the maximum year of analysis
max_year <- 2019

# Creates a list of years
years <- paste(1960:max_year)

# Prepare data for app
PSUT_Agg_Re_all_St_pfu_prepped <- PSUT_Agg_Re_all_St_pfu %>%
  magrittr::set_colnames(
    c(
      "Country", "Method", "Energy.type", "Gross.Net", "Stage", paste0(1960:2019)
    )
  ) %>%
  tidyr::pivot_longer(cols = `1960`:`2019`,
                      values_to = "E.dot",
                      names_to = "Year") %>%
  dplyr::mutate(Year = as.numeric(Year))


