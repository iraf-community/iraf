# IMD.COM -- Common for the IMD routines.

int	imd_magic		# set to -1 when initialized
int	imd_mode		# display access mode
char	imd_devinfo[SZ_LINE]	# device information for zopngd

common	/imdcom/ imd_magic, imd_mode, imd_devinfo
