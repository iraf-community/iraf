# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ttset.h>
include	<gset.h>
include	"stdgraph.h"

# STG_DEACTIVATEWS -- Deactivate the workstation, i.e., disable graphics,
# leaving the terminal in text mode.  Note that it is the CW (close
# workstation) sequence which is actually output, since the GD sequence is
# used only to write single lines of text to the status line.

procedure stg_deactivatews (flags)

int	flags			# action modifier flags

char	buf[1]
int	stg_readtty(), and()
include	"stdgraph.com"

begin
	if (g_out <= 0)
	    return

	# The g_active and g_out test permits us to be called before the
	# kernel is opened and causes redundant calls to be ignored.

	if (g_active == YES) {
	    # Pause before deactivating?
	    if (and (flags, AW_PAUSE) != 0) {
		call stg_putline (g_out, "\n[Hit return to continue]\n")
		while (stg_readtty (STDIN, buf, 1) != EOF)
		    if (buf[1] == '\r' || buf[1] == '\n')
			break
	    }

	    # Deactivate the workstation.
	    call stgctrl ("CW")

	    g_active = NO
	    g_enable = NO

	    # Reenable stty ucaseout mode if it was set when the workstation
	    # was activated.

	    if (g_ucaseout == YES)
		call ttseti (g_out, TT_UCASEOUT, YES)
	}

	# Clear the text screen?
	if (and (flags, AW_CLEAR) != 0 && g_term != NULL)
	    call ttyclear (g_out, g_term)

	call flush (g_out)
end
