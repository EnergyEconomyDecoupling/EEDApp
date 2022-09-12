library(tidyverse)

################################################################################
# Establish filepaths
################################################################################

# Set filepath to credentials sql database
credentials_path <- file.path("Z:/",
                              # "/pfu-output-data",
                              "EEDAppCredentials",
                              "credentials_database.sqlite")

# # Establish filepaths to targets caches
# pfu_path <- file.path("Z:/",
#                       # "/pfu-output-data",
#                       "PipelineCaches",
#                       "PFUDatabase_targets_2022-06-11",
#                       "_targets")
#
#
# pfuagg_path <- file.path("Z:/",
#                          # "/pfu-output-data",
#                          "PipelineCaches",
#                          "PFUAggDatabase_targets_2022-06-11",
#                          "_targets")

# Establish filepaths to targets caches
pfu_path <- file.path("C:/Users/User/Desktop",
                      "pfu-output-data",
                      "PipelineCaches",
                      "pfu_pipeline_cache_20220828T174431Z",
                      "_targets")


pfuagg_path <- file.path("C:/Users/User/Desktop",
                         "pfu-output-data",
                         "PipelineCaches",
                         "pfu_agg_pipeline_cache_20220905T230423Z",
                         "_targets")

# Establish filepath to PWT 10.0 data.
pwt_path <- file.path("Z:\\",
                      #"/pfu-output-data",
                      "PWT10.0",
                      "pwt100.csv")

################################################################################
# PFUDatabase targets
################################################################################

country_conc <- targets::tar_read_raw(name = PFUDatabase::target_names$country_concordance_table,
                                      store = pfu_path)

countries <- targets::tar_read_raw(name = PFUDatabase::target_names$countries,
                                   store = pfu_path)

exemplar_lists <- targets::tar_read_raw(name = PFUDatabase::target_names$exemplar_lists,
                                        store = pfu_path)

comp_alloc_tables <- targets::tar_read_raw(name = PFUDatabase::target_names$completed_allocation_tables,
                                           store = pfu_path)

comp_effic_tables <- targets::tar_read_raw(name = PFUDatabase::target_names$completed_efficiency_tables,
                                           store = pfu_path)

comp_phiu_tables <- targets::tar_read_raw(name = PFUDatabase::target_names$completed_phi_u_tables,
                                          store = pfu_path)


################################################################################
# PFUAggDatabase targets pfu_agg_pipeline_cache_20220905T230423Z
################################################################################

# # Calculate PFU aggregations #
# PSUT_Re_all_Gr_all_Chop_all_St_pfd <- targets::tar_read_raw(name = "PSUT_Re_all_Gr_all_Chop_all_St_pfd",
#                                                             store = pfuagg_path)

# Calculate final demand sector aggregations #
SectorAggEta <- targets::tar_read_raw(name = "SectorAggEta",
                                      store = pfuagg_path)

# PFU efficiencies #
EtaPFU <- targets::tar_read_raw(name = "EtaPFU",
                                store = pfuagg_path)

################################################################################
# Load pwt10.0 data
################################################################################
pwt10_data <- read.csv(file = pwt_path)

