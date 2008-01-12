include <fset.h>
include <tbset.h>
include <gio.h>
include "igi.h"
include	"commands.h"

#  8/20/91 Removed ^Ls. ZGL
#  3/3/92 Added NUMBER command.  ZGL
## 15 June 1992  Added initcmd parameter to execute initial command(s).  ZGL
## 24 June 1992  Add cases for ZSECTION, PIXMAP and FITPIX.  ZGL
## 7/17/92  Add FILLPAT.  ZGL
## 7/21/92  Add BARGRAPH, alias for HISTOGRAM.  ZGL
## 7/24/92  Add SAOCMAP.  ZGL
## 3/15/93  Add version.h include file.
## 7/2/93   Add WCSLAB.  ZGL
## 7/13/93  Add VERSION command, remove igopen() and igclose() to
##	    separate files.
## 5/21/97  Add PSFONT command.  WJH
##

procedure t_igi ()

pointer	igs		# igi parameters structure
int	cmd		# Command index into symbol table

int	i		# Generic.

pointer	sp, icmd

int	envgeti(), iggcmd()
pointer	igcopen()

begin 

#	call eprintf("IGI: Starting Revised IGI Now...\n")
#	call memlev(3)

	# Set size of the pushback buffer
	iferr (i = envgeti ("igi_buflen"))
	    i = 0
	call fseti (STDIN, F_PBBSIZE, i)
	
	igs = igcopen ()


	call smark (sp)
	call salloc (icmd, SZ_LINE, TY_CHAR)

	#  Find initial command string from cl parameter
	call clgstr ("initcmd", Memc[icmd], SZ_LINE)

	if (Memc[icmd] != EOS) {
	    #  Make sure there's a newline at the end
	    call strcat ("\n", Memc[icmd], SZ_LINE)

	    #  Push back initial command string
	    call ungetline (INPUT_SOURCE(igs), Memc[icmd])
	}

	call sfree (sp)

	repeat {
	    PRINT_ERROR(igs) = YES
	    cmd = iggcmd (igs)
	    if (cmd != 0)
		call igdcmd (cmd, igs)
	} until (cmd == EOF)

#	call eprintf("IGI: Finished...\n")
	call igcclose (igs)
end


pointer procedure igcopen ()

#  IGCOPEN -- Initialize igi command interface.

## 2 July 1993  ZGL

pointer	igs		# igi structure

pointer	symtabd		# Command symbol table descriptor
pointer	tokvals		# Token value structure descriptor
pointer	ifds		# Input file stack structure descriptor 
int	in		# Input command file descriptor

pointer	csds
pointer	sp, filename
pointer	device
pointer	metacode
int	stream
int	mode
pointer	gp		# Graphics descriptor
pointer tty
pointer	kfname
int	ntty

string	IGI_SITE	"STScI igi Version "
string	IGI_WELCOME	" -- type HELP for info, END to stop "

int	open(), btoi(), strmatch(), fstati()
pointer gopen(), igopen(), stopen()
pointer ttygdes()
int	ttygets()
bool	clgetb()

begin
        call salloc (kfname,  SZ_FNAME, TY_CHAR)

	call smark (sp)

	# Set up graphics stream
	call salloc (device, SZ_LINE, TY_CHAR)
	call clgstr ("device", Memc[device], SZ_LINE)
	
	if (strmatch (Memc[device], "{file}") != 0 || Memc[device] == EOS) {
	    # Write graphics to metacode file
	    call salloc (metacode, SZ_FNAME, TY_CHAR)
	    call clgstr ("metacode", Memc[metacode], SZ_FNAME)
	    stream = open (Memc[metacode], NEW_FILE, BINARY_FILE)
	    call strcpy ("stdvdm", Memc[device], SZ_LINE)
	} else {
	    stream = STDGRAPH
        }

	if (clgetb ("append"))
	    mode = APPEND
	else {
	    mode = NEW_FILE
	}

	tty = ttygdes (Memc[device])
	ntty =  ttygets(tty,"kf", Memc[kfname], SZ_FNAME)
		
	# Open graphics
	gp = gopen (Memc[device], mode, stream)
        #gp = gopen ("psi_land",NEW_FILE, STDGRAPH)


	igs = igopen (gp, mode)


	CMD_STATE(igs) = COMMAND_MODE

	# Input stream
	in = STDIN
	INPUT_SOURCE(igs) = in

	# Initialize the command buffers
	call salloc (filename, SZ_FNAME, TY_CHAR)
	call mktemp ("tmp$igi_plt_cmd", Memc[filename], SZ_FNAME)
	CMD_BUFFER(igs) = open (Memc[filename], NEW_FILE, TEXT_FILE)
	call mktemp ("tmp$igi_all_cmd", Memc[filename], SZ_FNAME)
	ALL_COMMANDS(igs) = open (Memc[filename], NEW_FILE, TEXT_FILE)
	call malloc (PLOT_CMD_PNT(igs), SZ_LINE, TY_CHAR)
	PLOT_COMMAND(igs) = EOS
	call malloc (LAST_CMD_PNT(igs), SZ_LINE, TY_CHAR)
	LAST_COMMAND(igs) = EOS
	CMD_SEQUENCE(igs) = 0
	WRITE_CMD(igs)    = NO

	# Open the macro symbol table
	symtabd = stopen ("IGI Macros", LEN_INDEX, LEN_STAB, SZ_SBUF)
	SYM_TABLE(igs) = symtabd

	# Allocate the token value structure
	call malloc (tokvals, LEN_LOPS, TY_STRUCT)
	LOP_VALP(tokvals) = NULL
	LOP_LEN(tokvals)  = 0
	TOKEN_VALUE(igs)  = tokvals

	# Allocate and initialize the input file descriptor stack structure
	call malloc (ifds, LEN_STKS, TY_STRUCT)
	INPUT_STACK(igs) = ifds
	STK_STACK(ifds)  = NULL
	STK_INDEX(ifds)  = 0
	STK_DEPTH(ifds)  = 0

	# Allocate the command state stack structure
	call malloc (csds, LEN_STKS, TY_STRUCT)
	STATE_STACK(igs) = csds 
	STK_STACK(csds)  = NULL
	STK_INDEX(csds)  = 0
	STK_DEPTH(csds)  = 0

	# Debug?
	DEBUG_OUTPUT(igs) = btoi (clgetb ("debug"))

	# STDOUT redirected?
	STDOUT_REDIR(igs) = fstati (STDOUT, F_REDIR)

	call sfree (sp)

	if (fstati (in, F_REDIR) == YES)
	    return (igs)

	call printf (IGI_SITE)
	call clgstr ("Version", Memc[device], SZ_LINE)
	call printf (Memc[device])
	call printf (IGI_WELCOME)

	return (igs)
end


procedure igcclose (igs)

pointer	igs		# igi parameters structure

pointer	symtabd		# Command symbol table descriptor
pointer	tokvals		# Token value structure
pointer	ifds		# Input file stack descriptor 
pointer	igps		# Parameters structure
int	stream		# Graphics stream

pointer	sp, filename

begin
	tokvals = TOKEN_VALUE(igs)
	symtabd = SYM_TABLE(igs)

	ifds = INPUT_STACK(igs)
	igps = PLOT_PARMS(igs)

	# Graphics
	stream = GP_FD(GIO_GP(igs))
	call gclose (GIO_GP(igs))
	if (stream != STDGRAPH)
	    # Close metacode file
	    call close (stream)

	# Macro symbol table
	call stclose (symtabd)
	if (LOP_LEN(tokvals) > 0)
	    call mfree (LOP_VALP(tokvals), TY_CHAR)

	# Token values
	call mfree (tokvals, TY_STRUCT)

	# Input stream stack
	if (STK_DEPTH(ifds) > 0)
	    call mfree (STK_STACK(ifds), TY_INT)
	call mfree (ifds, TY_STRUCT)

	# Command state stack
	if (STK_DEPTH(STATE_STACK(igs)) > 0)
	    call mfree (STK_STACK(STATE_STACK(igs)), TY_INT)
	call mfree (STATE_STACK(igs), TY_STRUCT)

	# Last command
	call mfree (LAST_CMD_PNT(igs), TY_CHAR)

	# Plot command
	call mfree (PLOT_CMD_PNT(igs), TY_CHAR)
	
	call smark (sp)
	call salloc (filename, SZ_FNAME, TY_CHAR)

	# Command buffer files
	call fstats (CMD_BUFFER(igs), F_FILENAME, Memc[filename], SZ_FNAME)
	call close  (CMD_BUFFER(igs))
	call delete (Memc[filename])

	call fstats (ALL_COMMANDS(igs), F_FILENAME, Memc[filename], SZ_FNAME)
	call close  (ALL_COMMANDS(igs))
	call delete (Memc[filename])

	call sfree (sp)

#	# Temporary output spool file
#	call delete (SPOOL_OUTPUT(igs))

	call igclose (igs)
end


#  IGGCMD -- Parse the input to interpret igi commands

int procedure iggcmd (igs)

pointer	igs		# igi parameters structure

int	in
pointer	symtabd		# Command symbol table descriptor
pointer	tokvals		# Token value structure
int	token
int	command

int	gettok(), gtokst(), fstati()

begin
	in = INPUT_SOURCE(igs)
	tokvals = TOKEN_VALUE(igs)
	symtabd = SYM_TABLE(igs)

	call igprompt ("igi> ", igs)

	# Get command name
	repeat {
	    token = gettok (igs)
	    switch (token) {

	    case EOF, IDENTIFIER:
		break

	    case '!':
		# Escape to the cl
		command = ESCAPE
		break

	    case '?':
		# Print help menu
		command = MENU
		break

	    case '^':
		# Execute a previous command
		command = PREVIOUS
		break

	    case '\n':
		# Prompt on newline
		call igprompt ("igi> ", igs)

	    case ';', ' ', ',', '&', '#':
		# Ignore command and argument separators, etc.
		;

	    default:
		call eprintf ("Syntax error ")

	    }

	} until (token == EOF)

	if (token == IDENTIFIER) {
	    command = gtokst (symtabd, tokvals)
	    if (DEBUG_OUTPUT(igs) == YES) {
		call eprintf ("%d %s ")
		    call pargi (command)
		    call pargstr (LOP_VALC(tokvals))
	    }
	}

	if (command == END || command == BYE)
	    command = EOF	

	else if (token == EOF)
	    if (in == STDIN && fstati (in, F_REDIR) == NO)
		# User must enter END or BYE to exit in command mode
		# (Kludge to work around DEL returning EOF)
		command = 0

	    else
		command = EOF	

	else if (command == 0) {
	    call eprintf ("Unknown or ambiguous command:  %s ")
		call pargstr (LOP_VALC(tokvals))
	}

	return (command)
end


#  IGDCMD -- Execute the igi command

procedure igdcmd (cmd, igs)

int	cmd
pointer	igs		# igi parameters structure

char	version[SZ_LINE]

begin
	switch (cmd) {

	case ANGLE:
	    # Specify text and marker rotation
	    call ig_angle (igs)

	case APROPOS, HELP, MENU:
	    # List help text
	    call ig_help (cmd, igs)

	case AXIS:
	    # Draw an arbitrary axis
	    call ig_axis (igs)

	case BARGRAPH, HISTOGRAM:
	    # Plot histogram-style curve (bar graph)
	    call ig_histogram (igs)

	case BOX:
	    # Draw axes and labels
	    call ig_box (igs)

	case COLOR:
	    # Set color index
	    call ig_color (igs)

	case CONNECT:
	    # Draw the curve
	    call ig_connect (igs)

	case CURSES:
	    # Interactive cursor
	    call ig_curses (cmd, igs)

	case DATA:
	    # Specify input data file
	    call ig_data (igs)

	case DEFINE:
	    # Define macro text
	    call ig_define (igs)

	case DELETE:

	case DLIST:
	    # List data buffers
	    call ig_dlist (igs)

	case DOT:
	    # Draw a single point marker
	    call ig_dot (igs)

	case EDITCMD, PAGECMD:
	    # Command buffer
	    call ig_edit (cmd, igs)

	case ELLIPSE:
	    # Draw an ellipse
	    call ig_ellipse (igs)

	case END, BYE, EOF:
	    # End current state (terminate)
	    call ig_end (cmd, igs)

	case ERASE:
	    # Erase the screen
	    call ig_erase (igs)

	case ERRORBAR:
	    # Draw error bars
	    call ig_errorbar (igs)

	case ESCAPE:
	    # Escape a cl command
	    call ig_escape (igs)

	case ETYPE:
	    # Change the error bar style
	    call ig_etype (igs)

	case EEVALUATE, LEVALUATE, PEVALUATE, SEVALUATE, 
	     XEVALUATE, YEVALUATE, ZEVALUATE:
	    # Modify plot buffer
	    call ig_evaluate (cmd, igs)

	case EXPAND:
	    # Change character and point size
	    call ig_expand (igs)

	case FITPIX, LOCATION, PHYSICAL, VPAGE:
	    # Set viewport (NDC) scale
	    call ig_scale (cmd, igs)

	case FILLPAT:
	    # Set the fill pattern
	    call ig_fillpat (igs)

	case FMTICK:
	    # Change the tick label format
	    call ig_fmtick (igs)

	case FONTSET:
	    # Select igi or gio fonts
	    call ig_fontset (igs)

	case GRID:
	    # Draw a grid connecting major ticks
	    call ig_grid (igs)

	case ID:
	    call ig_id (igs)

	case IMGWCS:
	    call ig_imgwcs (igs)

	case INPUT:
	    # Input command buffer
	    call ig_input (igs)

	case JUSTIFY:
	    # Set text justification
	    call ig_justify (igs)

	case LABEL:
	    # Draw text
	    call ig_label (igs)

	case LIMITS:
	    # Set WC scale
	    call ig_limits (igs)

	case LINES:
	    # Specify a range of input data lines to read
	    call ig_lines (igs)

	case LIST:
	    # List command buffer or macro text
	    call ig_list (igs)

	case LOGO:
	    # Draw the STScI logo
	    call ig_logo (igs)

	case LTYPE:
	    # Specify line type (style)
	    call ig_ltype (igs)

	case LWEIGHT:
	    # Specify line weight (width)
	    call ig_lweight (igs)

	case MACROS:
	    # List defined macro names
	    call ig_mlist (igs)

	case MARGIN:
	    # Reset the plot scale to make border between data and axes
	    call ig_margin (igs)

	case MINMAX:
	    # List the extremes of the data vectors
	    call ig_minmax (igs)

	case MODE:
	    # End the current process state
	    call ig_mode (igs)

	case MOVE,  DRAW,  RELOCATE, 
	     DMOVE, DDRAW, DRELOCATE, 
	     PMOVE, PDRAW, PRELOCATE, 
	     VMOVE, VDRAW, VRELOCATE:
	    # Move (pen up or down)
	    call ig_move (cmd, igs)

	case NOOP:
	    # Dummy

	case NOTATION:
	    # Axis label notation
	    call ig_notation (igs)

	case NUMBER:
	    # Draw element number at each point
	    call ig_number(igs)

	case PIXMAP:
	    call ig_pixmap (igs)

	case PLAYBACK:
	    # Execute the command buffer
	    call ig_playback (igs)

	case POINTS:
	    # Draw data points
	    call ig_points (igs)

	case POLYGON:
	    # Draw a filled polygon.
	    call ig_polygon (igs)

	case PREVIOUS:
	    call ig_history (igs)

	case PSFONT:
	    # Select igi or gio fonts
	    call ig_psfont (igs)

	case PTYPE:
	    # Specify the point marker type
	    call ig_ptype (igs)

	case PUTLABEL:
	    # Draw text
	    call ig_putlabel (igs)

	case READ:
	    # Read a file into the command buffer
	    call ig_read (igs)

	case RESET:
	    # Set plot parameters to defaults
	    call ig_reset (igs)

	case SAOCMAP:
	    # Use SAOimage color map on cell array output
	    call ig_saocmap (igs)

	case SHOW:
	    # Display plot parameters
	    call ig_show (igs)

	case STEP:
	    # Plot step curve (pseudo-histogram)
	    call ig_step (igs)

	case TICKSIZE:
	    # Tick spacing
	    call ig_ticksize (igs)

	case ULLIM:
	    # Draw upper or lower limits
	    call ig_ullim (igs)

	case UNDO, UNSET:
	    # Erase the last command
	    call ig_undo (cmd, igs)

	case WINDOW:
	    # Define multipane window
	    call ig_window (igs)

	case WRITE:
	    # Write command buffer or macro text to a file
	    call ig_write (igs)

	case XCOLUMN, YCOLUMN, ECOLUMN, 
	     PCOLUMN, LCOLUMN, SCOLUMN:
	    # Read a column of data
	    call ig_column (cmd, igs)

	case XFLIP, YFLIP:
	    # Flip an axis
	    call ig_flip (cmd, igs)

	case XLABEL, YLABEL, TITLE:
	    # Specify the axis label
	    call ig_xylabel (cmd, igs)

	case XLOGARITHM, YLOGARITHM:
	    # Take the log of the input data
	    call ig_logarithm (cmd, igs)

	case XSIXTY, YSIXTY:
	    # Sexagesimal axis labels
	    call ig_sixty (cmd, igs)

	case XSECTION, YSECTION, ESECTION,
	     PSECTION, LSECTION, SSECTION:
	    # Read an image section into a 1-D buffer
	    call ig_section (cmd, igs)

	case VERSION:
	    # Write the igi version and date
	    call clgstr ("Version", version, SZ_LINE)
	    call printf ("Version %s\n")
		call pargstr (version)

	case WCSLAB:
	    # Draw WCS axis labels
	    call ig_wcslab (igs)

	case ZRANGE:
	    # Specify the mapping between pixel and display values
	    call ig_zrange (igs)

	case ZSECTION:
	    # Read an image section into the pixmap buffer
	    call ig_zsection (cmd, igs)
	}
end
