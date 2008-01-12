include <fset.h>
include "igi.h"

procedure igprompt (prompt, igs)

char	prompt[ARB]	# Prompt stringt
pointer	igs		# igi parameters structure

int	in		# Input command file descriptor

int	fstati()

begin
	in = INPUT_SOURCE(igs)

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Unread:  %d ")
		call pargi (fstati (in, F_UNREAD))
	}

	call flush  (STDERR)

	if (in != STDIN || fstati (in, F_REDIR) == YES
	    || fstati (in, F_UNREAD) != 0)
	    return

	if (CMD_STATE(igs) == COMMAND_MODE || 
	    CMD_STATE(igs) == DEFINE_MODE) {
	    call printf (prompt)
	    call flush  (STDOUT)
	}
end
