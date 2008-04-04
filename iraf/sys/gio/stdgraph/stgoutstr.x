# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stdgraph.h"

# STG_OUTSTR -- Format and output a control sequence containing a string
# string argument to the output device.

procedure stg_outstr (cap, strval)

char	cap[ARB]			#I graphcap capability name
char	strval[ARB]			#I string data

size_t	sz_val
pointer	sp, fmt, ctrl
include	"stdgraph.com"
int	ttygets()
errchk	ttygets

begin
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (fmt, sz_val, TY_CHAR)
	call salloc (ctrl, sz_val, TY_CHAR)

	if (ttygets (g_tty, cap, Memc[fmt], SZ_LINE) > 0) {
	    call sprintf (Memc[ctrl], SZ_LINE, Memc[fmt])
		call pargstr (strval)
	    call ttyputs (g_out, g_tty, Memc[ctrl], 1)
	}

	call sfree (sp)
end
