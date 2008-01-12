include <fset.h>
include "igi.h"

#  CMDCAT -- Append the last command to the command buffer(s)

procedure cmdcat (igs, wrtplt)

pointer	igs		# igi parameters structure
int	wrtplt		# Write command to plot command buffer? (YES|NO)

int	in		# Input stream

int	fstati()

begin
	in = INPUT_SOURCE(igs)

	if (in != STDIN || fstati (in, F_REDIR) == YES)
	    # Not interactive
	    return

	if (wrtplt == YES)
	    # Copy the last command to the plot command buffer
	    call strcpy (LAST_COMMAND(igs), 
		PLOT_COMMAND(igs), SZ_LINE)

	if (CMD_STATE(igs) == EXPAND_MODE)
	    # Expanding a macro
	    return

	# Write to the command buffer file
	call fprintf (CMD_BUFFER(igs), "%s\n")
	    call pargstr (LAST_COMMAND(igs))
	CMD_SEQUENCE(igs) = CMD_SEQUENCE(igs) + 1
end
