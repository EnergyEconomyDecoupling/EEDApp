# -----
# mount_storage_data_cat <- drake::readd(target = PFUDatabase::target_names$completed_allocation_tables,
#                                        path = "/mnt/.drake",
#                                        character_only = TRUE)




# PSUT_Agg_Re_all_St_pfu
PSUT_Agg_Re_all_St_pfu <- read.csv(file = file.path(#"Z:\\",
                                                    "/pfu-output-data",
                                                    "PipelineReleases",
                                                    "PSUT_Agg_Re_all_St_pfu",
                                                    "20220610T154839Z-219ee",
                                                    "PSUT_Agg_Re_all_St_pfu.csv"))
