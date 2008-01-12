include "igi.h"
include "commands.h"

#  IG_UNDO -- Erase the vectors drawn by the last plot command

procedure ig_undo (cmd, igs)

int	cmd
pointer	igs

begin
	if (cmd == UNDO) {
	    # Erase the last plot command
	    if (PLOT_COMMAND(igs) == EOS)
		# No command
		return

	    MG_DRAW(PLOT_PARMS(igs)) = NO
	    call ungetline (INPUT_SOURCE(igs), "\nUNSET;")
	    call ungetline (INPUT_SOURCE(igs), PLOT_COMMAND(igs))

	} else if (cmd == UNSET)
	    # Restore the draw flag
	    MG_DRAW(PLOT_PARMS(igs)) = YES
end
