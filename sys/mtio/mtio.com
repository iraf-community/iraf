# The MTIO Common.

int	new_mtchan				# flag newly opened channel
int	mtdev[LEN_MTIODES,MT_MAXTAPES+1]	# integer fields
char	mtnam[SZ_DEVICE,MT_MAXTAPES+1]		# array of drive names
char	mtosn[SZ_IODEV,MT_MAXTAPES+1]		# host name for device
char	mtlkn[SZ_LKNAME,MT_MAXTAPES+1]		# lock file name

common	/mtiocom/ new_mtchan, mtdev, mtnam, mtosn, mtlkn
