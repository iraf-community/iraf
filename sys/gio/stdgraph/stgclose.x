# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stdgraph.h"

# STG_CLOSE -- Close the STDGRAPH kernel.  Free all storage associated with the
# stdgraph descriptor.  Note that the stdgraph kernel may be multiply opened
# (connected to two or more graphics steams, e.g., both STDGRAPH and STDIMAGE),
# hence we do not physically close down until the last stream is closed.

procedure stg_close()

include	"stdgraph.com"

begin
	g_nopen = g_nopen - 1

	if (g_nopen <= 0) {
	    call stg_deactivatews (0)
	    call flush (g_out)
	    call mfree (SG_SBUF(g_sg), TY_CHAR)
	    call mfree (g_sg, TY_STRUCT)

	    if (g_tty != NULL) {
		call ttycdes (g_tty)
		g_tty = NULL
	    }

	    if (g_term != NULL) {
		call ttycdes (g_term)
		g_term = NULL
	    }

	    if (g_pbtty != NULL) {
		call ttycdes (g_pbtty)
		g_pbtty = NULL
	    }

	    if (g_msgbuf != NULL) {
		call mfree (g_msgbuf, TY_CHAR)
		g_msgbuf = NULL
		g_msgbuflen = 0
		g_msglen = 0
	    }

	    g_nopen = 0
	}
end
