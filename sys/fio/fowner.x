# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<finfo.h>
include	<syserr.h>

# FOWNER -- Get the name of the owner of a file.

procedure fowner (fname, owner, maxch)

char	fname[ARB]			# file name
char	owner[ARB]			# owner name string
int	maxch				# max chars in owner name string
long	file_info[LEN_FINFO]
int	finfo()

begin
	if (finfo (fname, file_info) == ERR)
	    call filerr (fname, SYS_FOWNER)
	call strcpy (FI_OWNER(file_info), owner, maxch)
end
