include <fset.h>
include "igi.h"
include "commands.h"

procedure ig_edit (cmd, igs)

pointer	igs		# igi parameters structure
int	cmd		# Command index

int	in		# Input command source descriptor
pointer	tokvals		# Token value structure
pointer	symtabd		# Command symbol table descriptor
pointer	ifds		# Input file stack descriptor 

int	token
pointer	sym
int	sfp
pointer	sp, macname, fname, command, cmdbuf, prompt
bool	macro
int	ip

pointer	stfind(), strefsbuf()
int	open(), stropen(), spopfd(), readtok(), strlen(), access()

begin
	in = INPUT_SOURCE(igs)
	symtabd = SYM_TABLE(igs)
	tokvals = TOKEN_VALUE(igs)
	ifds = INPUT_STACK(igs)

	call lcmdcat (igs, YES)
	token = readtok (in, tokvals)

	call smark (sp)
	call salloc (prompt, SZ_LINE, TY_CHAR)

	if (IS_NEWCOMMAND(token)) {
	    # No argument;  command buffer
	    if (DEBUG_OUTPUT(igs) == YES) {
	        call eprintf ("\tCommand buffer ")
	    }

	    sfp = CMD_BUFFER(igs)
	    macro = false
	    call strcpy ("Command Buffer", Memc[prompt], SZ_LINE)

	} else {
	    # Macro text
	    call salloc (macname, SZ_LINE, TY_CHAR)
	    call strcpy (LOP_VALC(tokvals), Memc[macname], SZ_LINE)
	    if (DEBUG_OUTPUT(igs) == YES) {
		call eprintf ("\tMacro text:  %s ")
		    call pargstr (Memc[macname])
	    }
	    call sprintf (Memc[prompt], SZ_LINE, "Macro %s")
		call pargstr (Memc[macname])
	    sym = stfind (symtabd, Memc[macname])
	    call lcmdcat (igs, NO)

	    if (sym == NULL) {
		call eprintf ("Macro %s is not defined ")
		    call pargstr (Memc[macname])
		return
	    }

	    ip  = strefsbuf (symtabd, SYM_STROFF(sym))
	    sfp = stropen (Memc[ip], strlen (Memc[ip]), READ_ONLY)
	    macro = true
	}

	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (command, SZ_LINE, TY_CHAR)

	# Create the temporary command file
	call mktemp ("tmp$igi_cmd", Memc[fname], SZ_FNAME)
	call wrtcmd (sfp, Memc[fname], TEMP_FILE)

	# Build the appropriate cl command string
	if (cmd == EDITCMD) {
	    # Edit the commands
	    call gdeactivate (GIO_GP(igs), 0)
	    call sprintf (Memc[command], SZ_LINE, "edit (\"%s\")")
	    call pargstr (Memc[fname])
	    call clcmdw  (Memc[command])
	    call greactivate (GIO_GP(igs), 0)

	} else {
	    if (STDOUT_REDIR(igs) == YES)
		# List the commands
		call listout (Memc[fname])

	    else
		# Page the commands
		call gpagefile (GIO_GP(igs), Memc[fname], Memc[prompt])
	}

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("\t%s ")
		call pargstr (Memc[command])
	}

	if (cmd == EDITCMD && access (Memc[fname], 0, 0) == YES) {
	    # Copy the file back into the buffer
	    if (macro) {
		call strclose (sfp)
		call spshfd (in, ifds)
		in = open (Memc[fname], READ_WRITE, TEXT_FILE)
		INPUT_SOURCE(igs) = in
		# Reload the macro into the symbol table
		call defmac (Memc[macname], igs)
		call close (in)
		in = spopfd (ifds)
		INPUT_SOURCE(igs) = in

	    } else {
		# Rewrite the command buffer (spool) file
		call salloc (cmdbuf, SZ_FNAME, TY_CHAR)
		call fstats (sfp, F_FILENAME, Memc[cmdbuf], SZ_FNAME)
		call close (sfp)
		call delete (Memc[cmdbuf])
		sfp = open (Memc[cmdbuf], NEW_FILE, TEXT_FILE)
		call rdcmdf (Memc[fname], sfp)
		CMD_BUFFER(igs) = sfp
	    }
	}

	while (access (Memc[fname], 0, 0) == YES)
	    # Delete all versions of temporary command file
	    call delete (Memc[fname])

	call sfree (sp)
#	call cmdcat (igs, NO)
end
