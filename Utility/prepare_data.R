library(tidyverse)

################################################################################
# Establish country options
################################################################################

# Create two column data frame for matching codes to names
agg_regions  <- data.frame(PFU.code = c("Africa", "Asia_", "Europe", "MidEast",
                                        "NoAmr", "Oceania", "SoCeAmr", "Bunkers", "World"),
                           Country.wname = c("Africa", "Asia", "Europe", "Middle East",
                                             "North America", "Oceania", "South and Central America",
                                             "World Bunkers", "World"))
names_codes <- country_conc %>%
  dplyr::select(IEA.name, PFU.code) %>%
  dplyr::filter(PFU.code %in% countries) %>%
  dplyr::mutate(Country.wname = paste0(PFU.code, " - ", IEA.name), .before = 1) %>%
  dplyr::add_row(agg_regions) %>%
  dplyr::arrange(PFU.code)

country_options <- names_codes$PFU.code
names(country_options) <- names_codes$Country.wname

################################################################################
# Prepare aggregation map
################################################################################

# Region aggregation map
agg_map <- exemplar_lists %>%
  tidyr::unnest(Exemplars) %>%
  dplyr::filter(Exemplars %in% c("AFRI", "EURP", "MIDE", "SAMR", "ASIA",
                                 "OCEN", "NAMR", "BUNK")) %>%
  dplyr::select(-Year) %>%
  dplyr::distinct()

################################################################################
# Prepare input data
################################################################################

# Efficiency Tables
comp_alloc_tables_prepped <- comp_alloc_tables %>%
  dplyr::mutate(Machine_Eu.product = paste(comp_alloc_tables$Machine,
                                           " - ",
                                           comp_alloc_tables$Eu.product))

# Allocation Tables
comp_effic_tables_prepped <- comp_effic_tables %>%
  dplyr::mutate(Machine_Eu.product = paste(comp_effic_tables$Machine,
                                           " - ",
                                           comp_effic_tables$Eu.product))

################################################################################
# Prepare PFUAggDatabase outputs
################################################################################

EtaData <- EtaPFU %>%
  dplyr::distinct(Country, Method, Energy.type, Year, IEAMW, GrossNet, .keep_all = TRUE) %>%
  dplyr::select(Country, Method, Energy.type, Year, IEAMW, GrossNet, eta_pf, eta_fu, eta_pu) %>%
  dplyr::rename("Gross.Net" = GrossNet,
                "IEA.MW" = IEAMW,
                "Primary-Final" = eta_pf,
                "Final-Useful" = eta_fu,
                "Primary-Useful" = eta_pu) %>%
  tidyr::pivot_longer(cols = c(`Primary-Final`, `Final-Useful`, `Primary-Useful`),
                      values_to = "Eta",
                      names_to = "Stages")

AggData <- EtaPFU %>%
  dplyr::distinct(Country, Method, Energy.type, Year, IEAMW, GrossNet, .keep_all = TRUE) %>%
  dplyr::select(Country, Method, Energy.type, Year, IEAMW, GrossNet, EX.p, EX.f, EX.u) %>%
  # dplyr::distinct(Country, Method, Energy.type, Year, IEAMW, GrossNet) %>%
  dplyr::rename("Gross.Net" = GrossNet,
                "IEA.MW" = IEAMW,
                "Primary" = EX.p,
                "Final" = EX.f,
                "Useful" = EX.u) %>%
  tidyr::pivot_longer(cols = c(Primary, Final, Useful),
                      values_to = "E.dot",
                      names_to = "Stage") %>%
  dplyr::mutate(Units = "ktoe", .after = "Stage")

# Rebound space data
rebound_space_data_prepped <- AggData %>%
  dplyr::left_join(EtaData,
                   by = c("Country", "Method", "IEA.MW", "Energy.type", "Gross.Net", "Year"))


# Calculate indexed total consumption data
AggData_iyear <- AggData %>%
  dplyr::group_by(Country, Method, Energy.type, IEA.MW, Stage, Gross.Net) %>%
  dplyr::filter(Year == min(Year)) %>%
  dplyr::select(-Year) %>%
  dplyr::rename("E.dot.iyear" = E.dot) %>%
  dplyr::ungroup()

AggData_i <- AggData %>%
  dplyr::left_join(AggData_iyear, by = c("Country", "Method", "Energy.type", "IEA.MW", "Stage", "Units", "Gross.Net")) %>%
  dplyr::mutate("E.dot.i" = E.dot/E.dot.iyear) %>%
  dplyr::select(-E.dot.iyear)

# Calculate indexed total consumption data
EtaData_iyear <- EtaData %>%
  dplyr::group_by(Country, Method, Energy.type, IEA.MW, Stages, Gross.Net) %>%
  dplyr::filter(Year == min(Year)) %>%
  dplyr::select(-Year) %>%
  dplyr::rename("Eta.iyear" = Eta) %>%
  dplyr::ungroup()

EtaData_i <- EtaData %>%
  dplyr::left_join(EtaData_iyear, by = c("Country", "Method", "Energy.type", "IEA.MW", "Stages", "Gross.Net")) %>%
  dplyr::mutate("Eta.i" = Eta/Eta.iyear) %>%
  dplyr::select(-Eta.iyear)

# Retrieving country-specific index years
iyears_psut <- AggData %>%
  dplyr::group_by(Country, Method, Energy.type, Stage, IEA.MW, Units, Gross.Net) %>%
  dplyr::filter(Year == min(Year)) %>%
  dplyr::ungroup() %>%
  dplyr::select(Country, Year) %>%
  dplyr::rename(Country = Country,
                iYear_psut = Year) %>%
  # Here distinct() is called to remove duplication,
  # in the case where data is available for different years this will
  # need to be revised
  dplyr::distinct()

################################################################################
# Establish country options from PFUAggDatabase
################################################################################

names_codes_agg <- country_conc %>%
  dplyr::select(IEA.name, PFU.code) %>%
  dplyr::filter(PFU.code %in% c(unique(AggData$Country))) %>%
  dplyr::mutate(Country.wname = paste0(PFU.code, " - ", IEA.name), .before = 1) %>%
  dplyr::add_row(agg_regions) %>%
  dplyr::arrange(PFU.code)

country_options_agg <- names_codes_agg$PFU.code
names(country_options_agg) <- names_codes_agg$Country.wname

################################################################################
# Prepare economic data
################################################################################

# Prepare pwt10.0 data
# Remove rownames
rownames(pwt10_data) <- NULL

# Select relevant columns and rename columns
pwt10_data_cleaned <- pwt10_data %>%
  dplyr::select(countrycode, year, rgdpe, rgdpo, rgdpna) %>%
  dplyr::rename(Country = countrycode,
                Year = year,
                rgdpe = rgdpe,
                rgdpo = rgdpo,
                rgdpna = rgdpna) %>%
  dplyr::filter(Country %in% countries)

regions_pwt10_data_cleaned <- pwt10_data_cleaned %>%
  dplyr::left_join(agg_map, by = c("Country")) %>%
  dplyr::select(-Country) %>%
  dplyr::group_by(Year, Exemplars) %>%
  dplyr::summarise_all(sum, na.rm = TRUE) %>%
  dplyr::rename(Country = Exemplars) %>%
  dplyr::mutate(
    Country = dplyr::case_when(
      Country == "AFRI" ~ "Africa",
      Country == "ASIA" ~ "Asia_",
      Country == "EURP" ~ "Europe",
      Country == "MIDE" ~ "MidEast",
      Country == "NAMR" ~ "NoAmr",
      Country == "OCEN" ~ "Oceania",
      Country == "SAMR" ~ "SoCeAmr",
      TRUE ~ as.character(Country)
    )
  )

world_pwt10_data_cleaned <- pwt10_data_cleaned %>%
  dplyr::select(-Country) %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise_all(sum, na.rm = TRUE) %>%
  dplyr::mutate(Country = "World")

# Calculate L, the total number of hours worked in a given year
# and Ladj, the total number of hours worked adjusted by the human capital index
econ_data <- pwt10_data_cleaned %>%
  rbind(regions_pwt10_data_cleaned) %>%
  rbind(world_pwt10_data_cleaned) %>%
  # Remove rows with no GDP data
  dplyr::filter(!is.na(rgdpe))


# Retrieving country-specific index years
iyears_econ <- econ_data %>%
  dplyr::group_by(Country) %>%
  dplyr::filter(Year == min(Year)) %>%
  dplyr::ungroup() %>%
  dplyr::select(Country, Year) %>%
  dplyr::rename(Country = Country,
                iYear_econ = Year) %>%
  # Here distinct() is called to remove duplication,
  # in the case where data is available for different years this will
  # need to be revised
  dplyr::distinct()

iyears_final <- iyears_econ %>%
  dplyr::left_join(iyears_psut, by = "Country") %>%
  dplyr::mutate(
    iYear = dplyr::case_when(
      iYear_econ > iYear_psut ~ as.numeric(iYear_econ),
      iYear_psut > iYear_econ ~ as.numeric(iYear_psut),
      iYear_psut == iYear_econ ~ as.numeric(iYear_psut),
      TRUE ~ as.numeric(iYear_econ)
    )
  ) %>%
  dplyr::select(Country, iYear)


# Adding index years
econ_data_iyear <- econ_data %>%
  dplyr::left_join(iyears_final, by = "Country") %>%
  dplyr::group_by(Country) %>%
  dplyr::filter(Year == iYear) %>%
  magrittr::set_colnames(c("Country", "Year", "rgdpe.iyear", "rgdpo.iyear",
                           "rgdpna.iyear", "iYear")) %>%
  dplyr::select(-Year) %>%
  dplyr::ungroup()

# Calculate indexed GDP data, other metrics have inconsistent coverage
# so have been temporarily omitted.
econ_data_i <- econ_data %>%
  dplyr::left_join(econ_data_iyear, by = c("Country")) %>%
  # dplyr::filter(Year >= iYear) %>%
  dplyr::mutate("rgdpe.i" = ifelse(is.na(rgdpe), 1, rgdpe / rgdpe.iyear)) %>%
  dplyr::mutate("rgdpo.i" = ifelse(is.na(rgdpo), 1 , rgdpo / rgdpo.iyear)) %>%
  dplyr::mutate("rgdpna.i" = ifelse(is.na(rgdpna), 1 , rgdpna / rgdpna.iyear)) %>%
  dplyr::select(Country, Year, iYear, rgdpe.i, rgdpo.i, rgdpna.i)

econ_data_i <- econ_data_i %>%
  dplyr::group_by(Country) %>%
  dplyr::mutate(Year.Index = 1:(unique(max(Year)) - unique(min(Year)) + 1), .after = "Year") %>%
  dplyr::ungroup()

econ_data_abs <- econ_data %>%
  dplyr::select(Country, Year, rgdpe, rgdpo, rgdpna) %>%
  tidyr::pivot_longer(cols = c("rgdpe", "rgdpo", "rgdpna"), names_to = "GDP_Metric", values_to = "GDP")

econ_data_i <- econ_data_i %>%
  dplyr::select(Country, Year, rgdpe.i, rgdpo.i, rgdpna.i) %>%
  magrittr::set_colnames(c("Country", "Year", "rgdpe", "rgdpo", "rgdpna")) %>%
  tidyr::pivot_longer(cols = c("rgdpe", "rgdpo", "rgdpna"), names_to = "GDP_Metric", values_to = "GDP.i")

econ_data_gdp <- econ_data_abs %>%
  dplyr::left_join(econ_data_i, by = c("Country", "Year", "GDP_Metric"))

pfuex_econ <- AggData_i %>%
  dplyr::select(-Units) %>%
  dplyr::left_join(EtaData_i, by = c("Country", "Method", "Energy.type", "IEA.MW", "Gross.Net", "Year")) %>%
  dplyr::left_join(econ_data_gdp, by = c("Country", "Year")) %>%
  dplyr::mutate("E.dot_intensity" = E.dot / GDP) %>%
  dplyr::mutate("E.dot_intensity.i" = E.dot.i / GDP.i) %>%
  dplyr::ungroup()
