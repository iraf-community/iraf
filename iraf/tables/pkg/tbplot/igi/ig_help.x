include "igi.h"
include "commands.h"

procedure ig_help (cmd, igs)

#  ig_help -- Implement the igi HELP command.  Page the igi help text
#  or some part of it.  This is done by spawning a cl help command.  Note
#  that the igi commands are written in the help as .ih sections so they
#  can be listed individually using help section= syntax.

#  There is a strange interaction between this and the cl causing STDIN
#  to become confused.  I believe this is a bug in the cl.

#  10/15/91 Modified to write help to temp file and explicitly page the
#  file to work around cl (?) bug.  ZGL

int	cmd		# Command index
pointer	igs		# igi parameters structure

pointer	tokvals		# Token value structure
int	token
pointer	sp, text
pointer tfn

string	tfnprf	"tmp$ighlp"

int	readtok()

begin
	call lcmdcat (igs, YES)
#	call gdeactivate (GIO_GP(igs), 0)
	tokvals = TOKEN_VALUE(igs)

	call smark (sp)
	call salloc (text, SZ_LINE, TY_CHAR)
	call salloc (tfn,  SZ_LINE, TY_CHAR)

	# Temporary file name for help output
	call mktemp (tfnprf, Memc[tfn], SZ_LINE)

	if (cmd == MENU) {
	    # Print the command menu
	    call sprintf (Memc[text], SZ_LINE,
		"help (\"igi\", section=\"menu\", > \"%s\")")
		call pargstr (Memc[tfn])

#	    call strcpy ("help (\"igi\", section=\"menu\")", 
#		Memc[text], SZ_LINE)

	} else if (cmd == HELP) {
	    # Specific command or section
	    token = readtok (INPUT_SOURCE(igs), tokvals)
	    if (IS_NEWCOMMAND(token)) {
		# No argument;  all help
		call sprintf (Memc[text], SZ_LINE,
		    "help (\"igi\", > \"%s\")")
		call pargstr (Memc[tfn])

#		call strcpy ("help (\"igi\")", Memc[text], SZ_LINE)

	    } else {
		# Specific help section
		call sprintf (Memc[text], SZ_LINE, 
		    "help (\"igi\", section=\"%s\", > \"%s\")")
		    call pargstr (LOP_VALC(tokvals))
		    call pargstr (Memc[tfn])

		call lcmdcat (igs, NO)
	    }

	} else if (cmd == APROPOS) {
	    # Match keyword in menu
	    token = readtok (INPUT_SOURCE(igs), tokvals)
	    if (IS_NEWCOMMAND(token)) {
		# No argument;  print the whole menu
		call sprintf (Memc[text], SZ_LINE,
		    "help (\"igi\", section=\"menu\", > \"%s\")")
		    call pargstr (Memc[tfn])

#		call strcpy ("help (\"igi\", section=\"menu\")", 
#		    Memc[text], SZ_LINE)

	    } else {
		# Specific help section
		call sprintf (Memc[text], SZ_LINE, 
		    "help (\"igi\", section=\"menu\") | match (\"%s\", > \"%s\")")
		    call pargstr (LOP_VALC(tokvals))
		    call pargstr (Memc[tfn])

		call lcmdcat (igs, NO)
	    }
	}

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("\tExecute cl command:  %s ")
		call pargstr (Memc[text])
	}

	call eprintf ("  One moment please...\n")
	call clcmdw (Memc[text])

	call gpagefile (GIO_GP(igs), Memc[tfn], EOS)
	call delete (Memc[tfn])
	call sfree (sp)

#	call greactivate (GIO_GP(igs), 0)
#	call cmdcat (igs, NO)
end
