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

