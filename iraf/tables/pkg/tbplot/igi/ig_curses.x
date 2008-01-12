#  IG_CURSES -- Implements the CURSES command, i.e., start up cursor
#  mode to read back the cursor position.  If the argument `file' exists,
#  it is the name of a file to which are written the coordinates of
#  positions selected with the cursor.  If no argument exists, the
#  positions are written to STDOUT (the terminal) but the coordinates are
#  not saved in a file.  Typing "q" or "e" terminates.  Any other lower
#  case character will list the cursor position and redisplay the cursor. 
#
#  All igi commands are available in cursor mode using colon commands.
#  Commands expecting input coordinates ([DV]RELOCATE and [DV]DRAW) will
#  use the current cursor position regardless of any arguments input on the
#  command line;  entering these coordinates will result in a syntax error.
#
#  The CURSES command is not stored in the command buffer, but a RELOCATE
#  command with the last cursor position is stored on each cursor read or
#  ":" command.  Therefore, on PLAYBACK, no cursor interaction takes place
#  but any commands relying on the interactively specified cursor position
#  execute appropriately. 

include "igi.h"
include "commands.h"

procedure ig_curses (cmd, igs)

int	cmd		# Command index
pointer	igs		# igi parameters structure

int	token		# Token value
int	in		# Input stream
pointer	tokvals		# Token value structure
pointer	igps		# Parameters structure descriptor
pointer	sp, line, coords, curstr
real	wx, wy		# WC position
int	wcs
int	key		# Keystroke
bool	curout		# Output cursor file?
int	cfp		# Output cursor file descriptor
bool	colon		# ":" command?
char	nl
data	nl /'\n'/

int	gettok(), iggcmd(), clgcur(), open(), spopfd()

begin
	# Change the process state to Cursor Mode
	call spshfd (CMD_STATE(igs), STATE_STACK(igs))
	CMD_STATE(igs) = CURSOR_MODE

	in = INPUT_SOURCE(igs)
	igps = PLOT_PARMS(igs)

	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (coords, SZ_LINE, TY_CHAR)
	call salloc (curstr, SZ_LINE, TY_CHAR)

	token = gettok (igs)
	if (token == IDENTIFIER || token == STRING) {
	    # Write cursor text to a file
	    curout = true
	    tokvals = TOKEN_VALUE(igs)

	    # Open the output cursor file
	    cfp = open (LOP_VALC(tokvals), NEW_FILE, TEXT_FILE)
	    if (DEBUG_OUTPUT(igs) == YES) {
		call printf ("Cursor output to file:  %s ")
		    call pargstr (LOP_VALC(tokvals))
	    }

	} else if (IS_NEWCOMMAND(token)) {
	    # No cursor output file;  write to terminal
	    curout = false

	} else {
	    call eprintf ("Invalid cursor output file name:  %s ")
		call pargstr (LOP_VALC(tokvals))
	    call sfree (sp)
	    return
	}

	Memc[line] = EOS
	colon = false

	while (clgcur ("cursor", wx, wy, wcs, key, Memc[line], SZ_LINE) != EOF) {
	    # Read the cursor
	    if (key == 'e' || key == 'q')
		break

	    call sprintf (Memc[coords], SZ_LINE, "%g %g")
		call pargr (wx)
		call pargr (wy)

	    if (key == ':') {
		# Execute igi command(s) and return here
		call ungetline (in, "\nnoop;")
		call ungetline (in, Memc[line])
		colon = true

		repeat {
		    # Parse the command
		    cmd = iggcmd (igs)

		    if (cmd == DDRAW || cmd == VDRAW)
			cmd =  DRAW
		    if (cmd == DRELOCATE || cmd == VRELOCATE ||
			cmd == DMOVE ||     cmd == VMOVE)
			cmd =  RELOCATE
		    if (cmd == DRAW || cmd == RELOCATE) {
			# Use the WCS cursor coordinates 
			call ungetline (in, Memc[coords])
		    } else if (cmd != 0 && cmd != NOOP && cmd != EOF) {
			# Update the pen position
			call strcpy ("RELOCATE ", LAST_COMMAND(igs), SZ_LINE)
			call strcat (Memc[coords], LAST_COMMAND(igs), SZ_LINE)
			call cmdcat (igs, NO)
			MG_XPOS(igps) = wx
			MG_YPOS(igps) = wy
		    }

		    if (cmd != 0 && cmd != NOOP)
			# Interpret the command
			call igdcmd (cmd, igs)

		} until (cmd == NOOP || cmd == EOF)

		if (cmd == EOF)
		    # Terminate the task
		    break

	    } else {
		# Simple keystroke;  list the coordinates
		call sprintf (Memc[curstr], SZ_LINE, "%g %g %d %1s %s\n")
		    call pargr (wx)
		    call pargr (wy)
		    call pargi (wcs)
		    call pargstr (key)
		    call pargstr (Memc[line])

		# Write the cursor string to STDOUT
		call printf (Memc[curstr])

		if (curout)
		    # Write the cursor string to the cursor file
		    call fprintf (cfp, Memc[curstr])

		call strcpy ("RELOCATE ", LAST_COMMAND(igs), SZ_LINE)
		call strcat (Memc[coords], LAST_COMMAND(igs), SZ_LINE)
		call cmdcat (igs, NO)
		MG_XPOS(igps) = wx
		MG_YPOS(igps) = wy
	    }

	    Memc[line] = EOS
	}

	# Restore the process state
	CMD_STATE(igs) = spopfd (STATE_STACK(igs))

	if (curout)
	    call close (cfp)

	if (colon)
	    # Kludge to get a prompt after ":" commands
	    call ungetc (in, nl)

	call sfree (sp)
end
