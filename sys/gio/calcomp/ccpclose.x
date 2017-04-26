# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"ccp.h"

# CCP_CLOSE -- Close the calcomp kernel.  Free up storage.

procedure ccp_close()

include	"ccp.com"

begin
	# Signal end of plot.
	call plot (0, 0, 999)
	# call plots (0, 0, CCP_DEVCHAN(g_cc)) #do we really want to do this?
	# (calcomp may get into funny state without, but may mess up APPEND

	# Free kernel data structures.
	call mfree (CCP_SBUF(g_cc), TY_CHAR)
	call mfree (g_cc, TY_STRUCT)

	g_cc = NULL
end
