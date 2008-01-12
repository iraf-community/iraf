include "igi.h"
include <fset.h>

procedure ig_history (igs)

pointer	igs		# igi parameters structure

int	in		# Input command source descriptor
pointer	tokvals		# Token value structure
int	token
int	ln
pointer	sp, line, cmd, filename
int	nl
char	newl, semic
data	newl /'\n'/
data	semic /';'/

int	getline(), gettok(), stridx()
pointer	open()

begin
	in = INPUT_SOURCE(igs)
	tokvals = TOKEN_VALUE(igs)

	token = gettok (igs)

	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	if (token == CONSTANT && LOP_VALI(tokvals) != 0) {
	    # Execute the specified command sequence number

	    # Find the command buffer file name and close it
	    call salloc (filename, SZ_FNAME, TY_CHAR)
	    call fstats (CMD_BUFFER(igs), F_FILENAME, Memc[filename], SZ_FNAME)
	    call close  (CMD_BUFFER(igs))

	    # Reopen the command buffer (to reset to BOF)
	    CMD_BUFFER(igs) = open (Memc[filename], NEW_FILE, TEXT_FILE)
#	    call seek (CMD_BUFFER(igs), BOF)

	    call salloc (line, SZ_LINE, TY_CHAR)
	    Memc[line] = EOS

	    for (ln = 1; 
		getline (CMD_BUFFER(igs), Memc[line]) != EOF;
		ln = ln + 1)
		if (ln == LOP_VALI(tokvals))
		    call strcpy (Memc[line], Memc[cmd], SZ_LINE)

	    if (Memc[cmd] == EOS) {
		call sfree (sp)
		return
	    }

	    if (DEBUG_OUTPUT(igs) == YES) {
		call eprintf ("\tExecute command %d:  ")
		    call pargi (LOP_VALI(tokvals))
	    }

	} else {
	    # Execute the last command
	    call strcpy (LAST_COMMAND(igs), Memc[cmd], SZ_LINE)
	    if (DEBUG_OUTPUT(igs) == YES)
		call eprintf ("\tExecute last command:  ")
	    call ungetc (in, semic)
	}

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("%s ")
		call pargstr (Memc[cmd])
	}

	# Push a command separator
	call ungetc (in, semic)

	# Strip off the newline at end of the string
	nl = stridx (newl, Memc[cmd])
	if (nl > 0)
	    Memc[cmd+nl-1] = EOS

	# Push the command back into the input stream
	call ungetline (in, Memc[cmd])

	call printf ("%s ")
	    call pargstr (Memc[cmd])

	call sfree (sp)
end
