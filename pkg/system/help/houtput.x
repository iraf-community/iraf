# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ttyset.h>
include	<chars.h>
include	<fset.h>
include	"help.h"

define	HELP	"[q=quit,d=downhalf,f|sp=downfull,j|cr=downline,N=next]"
define	QUIT		'q'
define	FWD_SCREEN	'f'
define	SCROLL_DOWN	'd'
define	NEXT_LINE	'j'
define	NEXT_FILE	'N'
define	NEXT_FILE_ALT	'n'
define	REDRAW		'\014'


# HOUTPUT -- The help line output procedure.  Count output lines and paginate
# output if enabled.  If manpage format is desired pass output lines to manpage
# filter.

procedure houtput (ctrl, lbuf)

pointer	ctrl
char	lbuf[ARB]
bool	formfeed, query_enabled
int	map_cc, maxlines, nlines, ip, cmd
int	envgeti(), ho_getcmd()
data	map_cc /YES/

begin
	if (H_RAWOUT(ctrl) == YES) {
	    call putline (H_OUT(ctrl), lbuf)
	    return
	} else if (H_MANPAGE(ctrl) == YES) {
	    call man_output (H_OUT(ctrl), lbuf, H_NLPP(ctrl),
		H_LMARGIN(ctrl), H_RMARGIN(ctrl))
	    return
	} else if (H_EOF(ctrl) == YES)
	    return

	# Check for end of page (either full or formfeed in line)
	# before processing the output line, because we want to
	# pause to let the user read the screen before paging.

	ip = 1
	formfeed = (lbuf[1] == '\f')
	if (formfeed)
	    ip = 2

	iferr (maxlines = envgeti ("ttynlines") - 1)
	    maxlines = 24

	# Help blocks and files are preceded by a form feed and a header.
	# The "more" query is issued between blocks and files to give the
	# user a chance to read the previous page before the screen is cleared.
	# The query is disabled for the first block or file by initialization
	# of nlines to -1 in the main help routine.

	query_enabled = (H_NLINES(ctrl) > 0)
	nlines = max (0, H_NLINES(ctrl))

	if (H_PAGINATE(ctrl) == YES) {
	    if (formfeed || nlines >= maxlines) {
		if (query_enabled) {
		    # Pause to give the user a chance to read the output, and
		    # to indicate whether they wish to continue.

		    repeat {
			cmd = ho_getcmd (H_TTY(ctrl))

			switch (cmd) {
			case FWD_SCREEN, BLANK, REDRAW:
			    nlines = 0
			case SCROLL_DOWN:
			    nlines = (maxlines + 1) / 2
			case NEXT_LINE, CR, LF:
			    nlines = maxlines - 1
			case NEXT_FILE, NEXT_FILE_ALT:
			    H_EOF(ctrl) = YES
			    nlines = 0
			case QUIT:
			    H_QUIT(ctrl) = YES
			    H_EOF(ctrl) = YES
			default:
			    call eprintf ("\07")
			    call flush (STDERR)
			    cmd = ERR
			}
		    } until (cmd > 0)
		}

		if (formfeed && H_QUIT(ctrl) == NO) {
		    call ttyclear (H_OUT(ctrl), H_TTY(ctrl))
		    nlines = 0
		}
	    }
	}

	# Do not output the line if the user just said to quit.
	if (H_EOF(ctrl) == NO)
	    if (lbuf[ip] != EOS) {
		call ttyputline (H_OUT(ctrl), H_TTY(ctrl), lbuf[ip], map_cc)
		nlines = nlines + 1
	    }

	H_NLINES(ctrl) = nlines
end


# HO_GETCMD -- Query the user for a single character command keystroke.

int procedure ho_getcmd (tty)

pointer	tty			# tty descriptor

int	key
char	strval[1]
int	clgkey()

begin
	# Ensure synchronization with the standard output.
	call flush (STDOUT)

	# Print query in standout mode.
	call ttyso (STDERR, tty, YES)
	call eprintf (HELP)
	call ttyso (STDERR, tty, NO)
	call flush (STDERR)

	call fseti (STDIN, F_SETREDRAW, REDRAW)

	# Read the command keystroke in raw mode.
	if (clgkey ("cl.ukey", key, strval, 1) == EOF)
	    key = QUIT
	else if (key == INTCHAR)
	    key = QUIT

	call fseti (STDIN, F_SETREDRAW, 0)

	# Erase the prompt and return.
	call eprintf ("\r")
	call ttyclearln (STDERR, tty)
	call flush (STDERR)

	return (key)
end
