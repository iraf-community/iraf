include "igi.h"

#  IG_MODE -- End the current process state.  Pop the current mode off 
#  the state stack.

procedure ig_mode (igs)

pointer	igs		# igi parameters structure

int	spopfd()

begin
	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Mode:  %d --> ")
	    call pargi (CMD_STATE(igs))
	}

	# Kludge to make DEFINE work in cursor mode
	CMD_STATE(igs) = spopfd (STATE_STACK(igs))

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("%d ")
	    call pargi (CMD_STATE(igs))
	}
end
