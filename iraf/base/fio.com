# FIO Common (prototype version with single local buffer)

long	boffset[LAST_FD]	# char file offset of buffer
pointer	bufptr[LAST_FD]		# pointer to file buffer, if any
pointer	buftop[LAST_FD]		# pointer to top of file buffer
pointer	iop[LAST_FD]		# or i/o pointer
pointer	itop[LAST_FD]		# top of buffer for input
pointer	otop[LAST_FD]		# top of buffer for output
pointer	fiodes[LAST_FD]		# pointer to file descriptor
int	fflags[LAST_FD]		# bit flags
int	redir_fd[LAST_FD]	# fd of redir file if i/o redirected locally
int	zdev[LEN_DEVTBL]	# device table
int	next_dev		# next slot in device table
pointer	fp			# file pointer of file most recently accessed
char	pathname[SZ_PATHNAME]	# buffer for mapping file names

common	/fiocom/ boffset, bufptr, buftop, iop, itop, otop, fiodes, fflags,
	redir_fd, zdev, next_dev, fp, pathname
