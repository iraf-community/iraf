#  IG_LIST -- List (not page) the command buffer or macro text with the 
#  line number

include "igi.h"
include <fset.h>

procedure ig_list (igs)

pointer	igs		# igi parameters structure

int	in		# Input command line
pointer	symtabd		# Command symbol table descriptor
pointer	tokvals		# Token value structure
int	token
pointer	sym
int	ln
pointer	ip
int	fd
pointer	sp, line
bool	macro
char	cmd_fname[SZ_FNAME]

int	readtok(), strlen(), stropen(), getline(), open()
pointer	stfind(), strefsbuf()

begin
	in = INPUT_SOURCE(igs)
	symtabd = SYM_TABLE(igs)
	tokvals = TOKEN_VALUE(igs)

	call lcmdcat (igs, YES)
	token = readtok (in, tokvals)

	if (token == IDENTIFIER || token == STRING) {
	    # List macro text;
	    # Find the macro in the symbol table
	    sym = stfind (symtabd, LOP_VALC(tokvals))
	    call lcmdcat (igs, YES)
	    if (sym == NULL) {
		call eprintf ("Macro %s is not defined ")
		    call pargstr (LOP_VALC(tokvals))
		return
	    }

	    # Find the macro text in the symbol table string buffer
	    ip = strefsbuf (symtabd, SYM_STROFF(sym))
	    fd = stropen (Memc[ip], strlen (Memc[ip]), READ_ONLY)

	    if (DEBUG_OUTPUT(igs) == YES) {
		call eprintf ("List macro %s ")
		    call pargstr (LOP_VALC(tokvals))
	    }

	    macro = true

	} else if (IS_NEWCOMMAND(token)) {
	    # List the command buffer
	    if (DEBUG_OUTPUT(igs) == YES) {
		call eprintf ("List the command buffer ")
	    }
	    fd = CMD_BUFFER(igs)
	    call fstats (fd, F_FILENAME, cmd_fname, SZ_FNAME)
	    call flush (fd)
	    call close (fd)
	    macro = false
	    fd = open (cmd_fname, READ_WRITE, TEXT_FILE)

	} else {
	    return
	}

	call smark  (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	call gdeactivate (GIO_GP(igs), 0)

	if (macro) {
	    call printf ("Macro %s\n")
		call pargstr (LOP_VALC(tokvals))
	} else {
	    call printf ("Command Buffer\n")
	}

	for (ln = 1;  getline (fd, Memc[line]) != EOF;  ln = ln + 1) {
	    call printf ("%4d %s")
		call pargi (ln)
		call pargstr (Memc[line])
	}

	call sfree (sp)

	if (macro) {
	    call strclose (fd)
	    call printf ("\n")
	}

	call flush (STDOUT)
	call greactivate (GIO_GP(igs), 0)

#	call cmdcat (igs, NO)
end
