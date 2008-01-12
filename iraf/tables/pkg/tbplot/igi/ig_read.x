include "igi.h"

#  IG_READ -- Read a command file into the command buffer without
#             executing the commands 

#  8/20/91 Removed ^Ls. ZGL

procedure ig_read (igs)

pointer	igs		# igi parameters structure

int	in		# Input command stream
pointer	tokvals		# Token value structure
int	token

int	access(), readtok()

begin
	in = INPUT_SOURCE(igs)
	tokvals = TOKEN_VALUE(igs)

	call lcmdcat (igs, YES)
	token = readtok (in, tokvals)

	if (token != IDENTIFIER && token != STRING) {
	    call eprintf ("Unrecognized input command file name: %s ")
		call pargstr (LOP_VALC(tokvals))
	    return
	}

	call lcmdcat (igs, YES)
#	call cmdcat (igs, NO)

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Input command file:  %s ")
		call pargstr (LOP_VALC(tokvals))
	}

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

	call rdcmdf (LOP_VALC(tokvals), CMD_BUFFER(igs))
end


procedure rdcmdf (infile, cmdf)

char	infile[ARB]		# Input file name
int	cmdf			# Command buffer (spool) file descriptor

int	inf

int	open()

begin
#	call seek (cmdf, BOF)
	inf = open (infile, READ_ONLY, TEXT_FILE)
	call fcopyo (inf, cmdf)
	call close (inf)
end
