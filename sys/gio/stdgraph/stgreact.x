# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<ttset.h>
include	"stdgraph.h"

# STG_REACTIVATEWS -- Reactivate the workstation, i.e., enable graphics.

procedure stg_reactivatews (flags)

int	flags			# action flags (handled by cursor mode)

int	junk
int	ttstati(), ttyctrl(), and()
extern	stg_onerror()
include	"stdgraph.com"

begin
	if (g_active == NO) {
	    junk = ttyctrl (g_out, g_tty, "OW", 1)

	    # Post error handler to be called if we abort.
	    call onerror (stg_onerror)

	    g_active = YES
	    g_enable = YES

	    # Must disable stty ucaseout mode when in graphics mode, else
	    # plotting commands may be modified by the terminal driver.

	    g_ucaseout = ttstati (g_out, TT_UCASEOUT)
	    if (g_ucaseout == YES)
		call ttseti (g_out, TT_UCASEOUT, NO)

	    # Clear the graphics screen?
	    if (and (flags, AW_CLEAR) != 0)
		call stg_clear (0)

	    call flush (g_out)
	}
end
