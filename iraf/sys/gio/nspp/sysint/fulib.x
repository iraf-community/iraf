# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

# FULIB -- Print an error message processed by fortran routine uliber.

procedure fulib (errcode, upkmsg, msglen)

int	errcode
char	upkmsg[ARB]		# unpacked string
int	msglen			# number of chars in string

size_t	sz_val
pointer	sp, sppmsg

begin
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (sppmsg, sz_val, TY_CHAR)

	# Construct error message string
	call sprintf (Memc[sppmsg], SZ_LINE, "ERROR %d IN %s\n")
	    call pargi (errcode)
	    call pargstr (upkmsg)

	# Call error with the constructed message
	iferr (call error (errcode, Memc[sppmsg]))
	    call erract (EA_WARN)
	
	call sfree (sp)
end
