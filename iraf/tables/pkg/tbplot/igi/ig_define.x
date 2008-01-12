include "igi.h"

#  IG_DEFINE -- Define macro text.  Enter macro define mode, appending 
#  text to the macro text buffer until "end".  Enter the macro into the 
#  symbol table.

procedure ig_define (igs)

pointer	igs		# igi parameters structure

pointer	tokvals		# Token value structure
int	token
int	cmd

int	readtok(), gtokst()

begin
	tokvals = TOKEN_VALUE(igs)

	call lcmdcat (igs, YES)
	token = readtok (INPUT_SOURCE(igs), tokvals)

	call lcmdcat (igs, NO)
#	call cmdcat  (igs, NO)

	if (token == IDENTIFIER) {
	    cmd = gtokst (SYM_TABLE(igs), tokvals)
	    if (cmd > 0) {
		call eprintf ("Macro name is already a defined command:  %s ")
		    call pargstr (LOP_VALC(tokvals))
		return
	    } else if (cmd < 0) {
		call eprintf ("Will supercede a defined macro:  %s ")
		    call pargstr (LOP_VALC(tokvals))
	    }

	    call defmac (LOP_VALC(tokvals), igs)

	} else {
	    call eprintf ("Invalid macro name:  %s ")
		call pargstr (LOP_VALC(tokvals))
	}
end
