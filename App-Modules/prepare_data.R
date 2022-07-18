# Prepare data for app

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

#
#
#
PSUT_Agg_Re_all_St_pfu_prepped <- PSUT_Agg_Re_all_St_pfu %>%
  tidyr::pivot_longer(cols = c(Primary, Final, Useful),
                      values_to = "E.dot",
                      names_to = "Stage")

#
#
#
PSUT_Eta_Re_all_St_pfu_prepped <- PSUT_Eta_Re_all_St_pfu %>%
  dplyr::rename(`Primary-Final` = "eta_pf",
                `Final-Useful` = "eta_fu",
                `Primary-Useful` = "eta_pu") %>%
  tidyr::pivot_longer(cols = c(`Primary-Final`, `Final-Useful`, `Primary-Useful`),
                      values_to = "Eta",
                      names_to = "Stages")

#
#
#
comp_alloc_tables_prepped <- comp_alloc_tables %>%
  dplyr::mutate(Machine_Eu.product = paste(comp_alloc_tables$Machine,
                                           " - ",
                                           comp_alloc_tables$Eu.product))

#
#
#
comp_effic_tables_prepped <- comp_effic_tables %>%
  dplyr::mutate(Machine_Eu.product = paste(comp_effic_tables$Machine,
                                           " - ",
                                           comp_effic_tables$Eu.product))

#
#
#
rebound_space_data_prepped <- PSUT_Agg_Re_all_St_pfu_prepped %>%
  dplyr::left_join(PSUT_Eta_Re_all_St_pfu_prepped,
                   by = c("Country", "Method", "Energy.type", "Gross.Net", "Year"))

#
#
#

# Calculate indexed total consumption data
PSUT_Agg_Re_all_St_pfu_iyear <- PSUT_Agg_Re_all_St_pfu_prepped %>%
  dplyr::group_by(Country, Method, Energy.type, Stage, Gross.Net) %>%
  dplyr::filter(Year == min(Year)) %>%
  dplyr::select(-Year) %>%
  dplyr::rename("E.dot.iyear" = E.dot) %>%
  dplyr::ungroup()

PSUT_Agg_Re_all_St_pfu_i <- PSUT_Agg_Re_all_St_pfu_prepped %>%
  dplyr::left_join(PSUT_Agg_Re_all_St_pfu_iyear, by = c("Country", "Method", "Energy.type", "Stage", "Gross.Net")) %>%
  dplyr::mutate("E.dot.i" = E.dot.iyear/E.dot) %>%
  dplyr::select(-E.dot.iyear)

# Calculate indexed total consumption data
PSUT_Eta_Re_all_St_pfu_iyear <- PSUT_Eta_Re_all_St_pfu_prepped %>%
  dplyr::group_by(Country, Method, Energy.type, Stages, Gross.Net) %>%
  dplyr::filter(Year == min(Year)) %>%
  dplyr::select(-Year) %>%
  dplyr::rename("Eta.iyear" = Eta) %>%
  dplyr::ungroup()

PSUT_Eta_Re_all_St_pfu_i <- PSUT_Eta_Re_all_St_pfu_prepped %>%
  dplyr::left_join(PSUT_Eta_Re_all_St_pfu_iyear, by = c("Country", "Method", "Energy.type", "Stages", "Gross.Net")) %>%
  dplyr::mutate("Eta.i" = Eta.iyear/Eta) %>%
  dplyr::select(-Eta.iyear)

# Retrieving country-specific index years
iyears_psut <- PSUT_Agg_Re_all_St_pfu_prepped %>%
  dplyr::group_by(Country, Method, Energy.type, Stage, Gross.Net) %>%
  dplyr::filter(Year == min(Year)) %>%
  dplyr::ungroup() %>%
  dplyr::select(Country, Year) %>%
  dplyr::rename(Country = Country,
                iYear_psut = Year) %>%
  # Here distinct() is called to remove duplication,
  # in the case where data is available for different years this will
  # need to be revised
  dplyr::distinct()

# Prepare pwt10.0 data
# Remove rownames
rownames(pwt10_data) <- NULL

# Select relevant columns and rename columns
pwt10_data_cleaned <- pwt10_data %>%
  dplyr::select(isocode, year, rgdpe, rgdpo, rgdpna,
                emp, avh, hc, rnna, rkna) %>%
  dplyr::rename(Country = isocode,
                Year = year,
                rgdpe = rgdpe,
                rgdpo = rgdpo,
                rgdpna = rgdpna,
                emp = emp,
                avh = avh,
                hc = hc,
                K = rnna,
                Kserv = rkna)

# Calculate L, the total number of hours worked in a given year
# and Ladj, the total number of hours worked adjusted by the human capital index
econ_data <- pwt10_data_cleaned %>%
  dplyr::mutate("L" = (emp * avh * 1000000), .keep = "unused", .after = rgdpna) %>%
  dplyr::mutate("Ladj" = (L * hc), .after = L) %>%
  dplyr::select(-hc) %>%
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
                           "rgdpna.iyear", "L.iyear", "Ladj.iyear", "K.iyear",
                           "Kserv.iyear", "iYear")) %>%
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
  # dplyr::mutate("L.i" = ifelse(is.na(L), 1 , L / L.iyear)) %>%
  # dplyr::mutate("Ladj.i" = ifelse(is.na(Ladj), 1 , Ladj / Ladj.iyear)) %>%
  # dplyr::mutate("K.i" = ifelse(is.na(K), 1 , K / K.iyear)) %>%
  # dplyr::mutate("Kserv.i" = ifelse(is.na(Kserv), 1 , Kserv / Kserv.iyear)) %>%
  dplyr::select(Country, Year, iYear, rgdpe.i, rgdpo.i, rgdpna.i
                #, L.i, Ladj.i, K.i, Kserv.i
                )

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

pfuex_econ <- PSUT_Agg_Re_all_St_pfu_i %>%
  dplyr::left_join(PSUT_Eta_Re_all_St_pfu_i, by = c("Country", "Method", "Energy.type", "Gross.Net", "Year")) %>% # Etas data in Gross terms has already been filtered out.
  dplyr::left_join(econ_data_gdp, by = c("Country", "Year")) %>%
  # dplyr::group_by(Country, Method, Energy.type, Gross.Net, GDP_Metric) %>%
  dplyr::mutate("E.dot_intensity" = E.dot / GDP) %>%
  dplyr::mutate("E.dot_intensity.i" = E.dot.i / GDP.i) %>%
  dplyr::ungroup()
