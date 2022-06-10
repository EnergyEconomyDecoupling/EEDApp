mount_storage_data_cat <- drake::readd(target = PFUDatabase::target_names$completed_allocation_tables,
                                       path = "/mnt/drakecachefolder/drakecache/.drake",
                                       character_only = TRUE)
