include "igi.h"

procedure ig_write (igs)

pointer	igs		# igi parameters structure

int	in		# Input command stream
pointer	tokvals		# Token value structure
pointer	symtabd		# Command symbol table descriptor
int	token
pointer	sp, macname, filename
bool	macro
int	sfp
pointer	sym
int	ip

int	readtok(), strmatch(), stropen(), strlen()
pointer	stfind(), strefsbuf()

begin
	in = INPUT_SOURCE(igs)
	tokvals = TOKEN_VALUE(igs)
	symtabd = SYM_TABLE(igs)

	call lcmdcat (igs, YES)

	# First argument (macro name or file name)
	token = readtok (in, tokvals)

	if (token != IDENTIFIER && token != STRING) {
	    call eprintf ("Unrecognized macro name: %s ")
		call pargstr (LOP_VALC(tokvals))
	    return
	}

	call lcmdcat (igs, NO)

	call smark (sp)
	call salloc (macname, LOP_LEN(tokvals), TY_CHAR)

	# Save the first argument
	call strcpy (LOP_VALC(tokvals), Memc[macname], LOP_LEN(tokvals))

	# Second argument (Null or file name)
	token = readtok (in, tokvals)

	call salloc (filename, LOP_LEN(tokvals), TY_CHAR)

	if (IS_NEWCOMMAND(token)) {
	    # Write the command buffer
	    # The first argument is the file name
	    call strcpy (Memc[macname], Memc[filename], LOP_LEN(tokvals))
	    call strcpy ("all", Memc[macname], LOP_LEN(tokvals))
	    macro = false
	} else if (token != IDENTIFIER && token != STRING) {
	    call eprintf ("Unrecognized output file name: %s ")
		call pargstr (LOP_VALC(tokvals))
	    call sfree (sp)
	    return
	} else {
	    # The second argument is the file name
	    call lcmdcat (igs, NO)
	    call strcpy (LOP_VALC(tokvals), Memc[filename], LOP_LEN(tokvals))
	}

#	call cmdcat (igs, NO)

	if (!macro || strmatch (Memc[macname], "{all}") != 0) {
	    # Command buffer
	    sfp = CMD_BUFFER(igs)
	    macro = false
	} else {
	    # Macro text
	    call strlwr (Memc[macname])
	    if (DEBUG_OUTPUT(igs) == YES) {
		call eprintf ("\tMacro text:  %s ")
		    call pargstr (Memc[macname])
	    }

	    # Find the macro text in the symbol table
	    symtabd = SYM_TABLE(igs)
	    sym = stfind (symtabd, Memc[macname])

	    if (sym == NULL) {
		call eprintf ("Macro %s is not defined ")
		    call pargstr (Memc[macname])
		call sfree (sp)
		return
	    }

	    ip  = strefsbuf (symtabd, SYM_STROFF(sym))
	    sfp = stropen (Memc[ip], strlen (Memc[ip]), READ_ONLY)
	    macro = true
	}

	# Copy the text to the output file
	call wrtcmd (sfp, Memc[filename], NEW_FILE)

	if (macro)
	    call strclose (sfp)

	if (DEBUG_OUTPUT(igs) == YES) {
	    if (macro) {
		call eprintf ("\tWrite macro %s to file %s ")
		    call pargstr (Memc[macname])
		    call pargstr (Memc[filename])
	    } else {
		call eprintf ("\tWrite command buffer to file %s ")
		    call pargstr (Memc[filename])
	    }
	}

	call sfree (sp)
end
