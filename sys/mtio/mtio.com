# The MTIO Common.

int	new_mtchan				# flag newly opened channel
int	mtdev[LEN_MTIODES,MT_MAXTAPES+1]	# integer fields
char	mtnam[SZ_DRIVE,MT_MAXTAPES+1]		# array of drive names
char	mtosn[SZ_OSDEV,MT_MAXTAPES+1]		# host name for device

common	/mtiocom/ new_mtchan, mtdev, mtnam, mtosn
