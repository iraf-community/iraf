# MTIO savepos cache.

int	c_modified[SZ_CACHE]
int	c_mtdes[LEN_MTIODES,SZ_CACHE]
char	c_drive[SZ_DRIVE,SZ_CACHE]
common	/mtcacm/ c_modified, c_mtdes, c_drive
