# Establish filepaths
pfu_path <- file.path(#"Z:\\",
                      "/pfu-output-data",
                      "PipelineCaches",
                      "PFUDatabase_targets_2022-06-11",
                      "_targets")


pfuagg_path <- file.path(#"Z:\\",
                         "/pfu-output-data",
                         "PipelineCaches",
                         "PFUAggDatabase_targets_2022-06-11",
                         "_targets")

# PFUDatabase targets

countries <- targets::tar_read_raw(name = PFUDatabase::target_names$countries,
                                   store = pfu_path)

comp_alloc_tables <- targets::tar_read_raw(name = PFUDatabase::target_names$completed_allocation_tables,
                                           store = pfu_path)

comp_effic_tables <- targets::tar_read_raw(name = PFUDatabase::target_names$completed_efficiency_tables,
                                           store = pfu_path)

comp_phiu_tables <- targets::tar_read_raw(name = PFUDatabase::target_names$completed_phi_u_tables,
                                          store = pfu_path)

socio_econ_data <- targets::tar_read_raw(name = PFUDatabase::target_names$socio_econ_data,
                                         store = pfu_path)

psut_useful <- targets::tar_read_raw(name = "PSUTUsefulIEA",
                                     store = pfu_path)



# PFUAggDatabase targets

PSUT_Agg_Re_all_St_pfu <- targets::tar_read_raw(name = "PSUT_Agg_Re_all_St_pfu",
                                                store = pfuagg_path)

PSUT_Eta_Re_all_St_pfu <- targets::tar_read_raw(name = "PSUT_Eta_Re_all_St_pfu",
                                                store = pfuagg_path)



PSUT_IEA_Agg_Re_all_St_pfu <- targets::tar_read_raw(name = "PSUT_IEA_Agg_Re_all_St_pfu",
                                                    store = pfuagg_path)

PSUT_IEA_Eta_Re_all_St_pfu <- targets::tar_read_raw(name = "PSUT_IEA_Eta_Re_all_St_pfu",
                                                    store = pfuagg_path)



PSUT_MW_Agg_Re_all_St_pfu <- targets::tar_read_raw(name = "PSUT_MW_Agg_Re_all_St_pfu",
                                                   store = pfuagg_path)

PSUT_MW_Eta_Re_all_St_pfu <- targets::tar_read_raw(name = "PSUT_MW_Eta_Re_all_St_pfu",
                                                   store = pfuagg_path)










