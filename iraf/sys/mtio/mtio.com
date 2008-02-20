# The MTIO Common.

int	new_mtchan				# flag newly opened channel
long	mtdev[LEN_MTIODES,MT_MAXTAPES+1]	# integer fields
pointer	mtdevcap[MT_MAXTAPES+1]			# MT_DEVCAP
char	mtnam[SZ_DEVICE,MT_MAXTAPES+1]		# array of drive names
char	mtosn[SZ_IODEV,MT_MAXTAPES+1]		# host name for device
char	mtlkn[SZ_LKNAME,MT_MAXTAPES+1]		# lock file name

common	/mtiocom/ new_mtchan, mtdev, mtdevcap, mtnam, mtosn, mtlkn
