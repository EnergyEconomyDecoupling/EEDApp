mount_storage_data_cat1 <- drake::readd(target = PFUDatabase::target_names$completed_allocation_tables,
                                        path = "/mnt/drakecachefolder/drakecache/.drake",
                                        character_only = TRUE)

# mount_storage_data_cat2 <- drake::readd(target = PFUDatabase::target_names$completed_allocation_tables,
#                                         path = "/mnt/drakecachefolder/.drake",
#                                         character_only = TRUE)
