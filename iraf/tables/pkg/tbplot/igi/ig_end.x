include "igi.h"

procedure ig_end (cmd, igs)

int	cmd		# Command index
pointer	igs		# igi parameters structure

int	spopfd()

begin
	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Mode:  %d;  Input:  %d --> ")
	    call pargi (CMD_STATE(igs))
	    call pargi (INPUT_SOURCE(igs))
	}

	if (CMD_STATE(igs) == COMMAND_MODE || 
	    CMD_STATE(igs) == EXPAND_MODE  || 
	    CMD_STATE(igs) == CURSOR_MODE) {
	    # Terminate the task
	    cmd = EOF
	    # Cancel the pushback buffer
	    call fcanpb (INPUT_SOURCE(igs))
	} else {
	    # Not in command mode
	    if (CMD_STATE(igs) == INPUT_MODE) {
		# Close the input file
		call close (INPUT_SOURCE(igs))
	    }

	    if (CMD_STATE(igs) == INPUT_MODE || 
		CMD_STATE(igs) == PLAYBACK_MODE) {
		# Return to the previous input stream
		INPUT_SOURCE(igs) = spopfd (INPUT_STACK(igs))
	    }

	    # End the current process state
	    CMD_STATE(igs) = spopfd (STATE_STACK(igs))
	    cmd = 0
	}

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Mode:  %d;  Input:  %d ")
	    call pargi (CMD_STATE(igs))
	    call pargi (INPUT_SOURCE(igs))
	}
end
