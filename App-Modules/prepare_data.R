# Prepare data for app

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

