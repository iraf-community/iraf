# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

# FULIB -- Print an error message processed by fortran routine uliber.

procedure fulib (errcode, upkmsg, msglen)

int	errcode
char	upkmsg[ARB]		# unpacked string
int	msglen			# number of chars in string

pointer	sp, sppmsg

begin
	call smark (sp)
	call salloc (sppmsg,  SZ_LINE, TY_CHAR)

	# Construct error message string
	call sprintf (Memc[sppmsg], SZ_LINE, "ERROR %d IN %s\n")
	    call pargi (errcode)
	    call pargstr (upkmsg)

	# Call error with the constructed message
	iferr (call error (errcode, Memc[sppmsg]))
	    call erract (EA_WARN)
	
	call sfree (sp)
end
