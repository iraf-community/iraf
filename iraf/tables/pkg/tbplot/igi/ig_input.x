include "igi.h"

procedure ig_input (igs)

pointer	igs		# igi parameters structure

int	in		# Input command source descriptor
pointer	tokvals		# Token value structure
pointer	symtabd		# Command symbol table descriptor
int	token

int	readtok(), open(), access()

begin
	in = INPUT_SOURCE(igs)
	tokvals = TOKEN_VALUE(igs)
	symtabd = SYM_TABLE(igs)

	call lcmdcat (igs, YES)

	token = readtok (in, tokvals)

	if (IS_NEWCOMMAND (token)) {
	    return
	}
	if (token != IDENTIFIER && token != STRING)
	    return

	call lcmdcat (igs, NO)
	call cmdcat  (igs, NO)

	if (access (LOP_VALC(tokvals), 0, 0) == NO) {
	    call eprintf ("File %s not found ")
		call pargstr (LOP_VALC(tokvals))
	    return
	}

	if (access (LOP_VALC(tokvals), 0, TEXT_FILE) == NO) {
	    call eprintf ("File %s is not a text file ")
		call pargstr (LOP_VALC(tokvals))
	    return
	}

	# Push the current input file descriptor on the stack
	call spshfd (in, INPUT_STACK(igs))

	# Open the command file
	in = open (LOP_VALC(tokvals), READ_ONLY, TEXT_FILE)
	INPUT_SOURCE(igs) = in

	call spshfd (CMD_STATE(igs), STATE_STACK(igs))
	CMD_STATE(igs) = INPUT_MODE

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("\tRead commands from file:  %s ")
		call pargstr (LOP_VALC(tokvals))
	}
end
