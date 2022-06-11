
# -----1
# mount_storage_data_cat <- drake::readd(target = PFUDatabase::target_names$completed_allocation_tables,
#                                         path = "/mnt/drakecachefolder/drakecache/.drake",
#                                         character_only = TRUE)

# -----2
# mount_storage_data_cat <- drake::readd(target = PFUDatabase::target_names$completed_allocation_tables,
#                                         path = "/mnt/drakecachefolder/.drake",
#                                         character_only = TRUE)

# -----3
mount_storage_data_cat <- drake::readd(target = PFUDatabase::target_names$completed_allocation_tables,
                                        path = "/mnt/.drake",
                                        character_only = TRUE)

# -----4
# mount_storage_data_cat <- drake::readd(target = PFUDatabase::target_names$completed_allocation_tables,
#                                         path = "/mnt/drakecache/.drake",
#                                         character_only = TRUE)
