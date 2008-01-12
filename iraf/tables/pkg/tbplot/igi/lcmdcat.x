include <fset.h>
include "igi.h"

#  LCMDCAT -- Append the last token to the last command buffer

procedure lcmdcat (igs, new_command)

pointer	igs		# igi parameters structure
int	new_command		# new_command previous command?

int	in

int	fstati()

begin
	in = INPUT_SOURCE(igs)
	if (in != STDIN || fstati (in, F_REDIR) == YES)
	    return

#	if (CMD_STATE(igs) == EXPAND_MODE)
#	    return

	if (new_command == YES) {
	    call strcpy (LOP_VALC(TOKEN_VALUE(igs)), 
		LAST_COMMAND(igs), SZ_LINE)
	} else {
	    if (LAST_COMMAND(igs) != EOS)
		call strcat (" ", LAST_COMMAND(igs), SZ_LINE)
	    call strcat (LOP_VALC(TOKEN_VALUE(igs)), 
		LAST_COMMAND(igs), SZ_LINE)
	}
end
