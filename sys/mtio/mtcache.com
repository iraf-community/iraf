# MTIO savepos cache.

int	c_modified[SZ_CACHE]
int	c_mtdes[LEN_DEVPOS,SZ_CACHE]
char	c_device[SZ_DEVICE,SZ_CACHE]
char	c_iodev[SZ_IODEV,SZ_CACHE]
char	c_lkname[SZ_LKNAME,SZ_CACHE]

common	/mtcacm/ c_modified, c_mtdes, c_device, c_iodev, c_lkname
