# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ttyset.h>
include	<ctype.h>
include	<mach.h>
include	<fset.h>
include	<gset.h>
include	<gki.h>
include	<gio.h>
include	"gtr.h"
include	"grc.h"

define	MAX_KWLEN	10

# Assign opcodes to the recognized keywords.

define	KW_AXES		1
define	KW_CASE		2
define	KW_CLEAR	3
define	KW_CURSOR	4
define	KW_GFLUSH	5
define	KW_HELP		6
define	KW_INIT		7
define	KW_MARKCUR	8
define	KW_OFF		9
define	KW_ON		10
define	KW_PAGE		11
define	KW_READ		12
define	KW_SHOW		13
define	KW_SNAP		14
define	KW_TXQUALITY	15
define	KW_TXSET	16
define	KW_VIEWPORT	17
define	KW_WRITE	18
define	KW_XRES		19
define	KW_YRES		20
define	KW_ZERO		21


# GRC_COMMAND -- Process a ":." cursor mode option string.  The RC structure
# contains the current values of the cursor mode options.  Some option strings
# are commands that do something, others set options, and still others show
# the status of the program.

int procedure grc_command (rc, stream, sx, sy, raster, rx, ry, opstr)

pointer	rc			#I rcursor descriptor
int	stream			#I graphics stream
real	sx, sy			#I screen coords of cursor
int	raster			#I raster number
real	rx, ry			#I raster coords of cursor
char	opstr[ARB]		#I options string excluding the leading ":.".

pointer	tr, p_tr, sp, fname, lbuf, tty
bool	clobber, fullframe, auto_gflush
int	ip, op, ch, opcode, cursor
int	save1, save2, i, xres, yres, quality
char	kwname[MAX_KWLEN]

pointer	gtr_init(), grc_open(), ttyodes()
int	strdic(), grc_boolval(), ttygeti(), ttystati()
real	grc_realval()
string	keywords "|axes|case|clear|cursor|gflush|help|init|markcur|off|on|page|\
read|show|snap|txquality|txset|viewport|write|xres|yres|zero|"
errchk	gtr_redraw, gki_flush, gtr_init
define	exit_ 91

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	# The terminal is left in graphics mode when the user types return to
	# enter the command.  Echo the user command to the terminal without
	# the newline to leave the terminal in status line mode, so that any
	# output directly to the terminal from the lower level code in the CL
	# goes into the status line.

	call strcpy (":.", Memc[lbuf], SZ_LINE)
	op = lbuf + 2
	for (ip=1;  opstr[ip] != EOS && opstr[ip] != '\n';  ip=ip+1) {
	    Memc[op] = opstr[ip]
	    op = op + 1
	}
	Memc[op] = EOS
	call stg_putline (STDERR, Memc[lbuf])

	tr = gtr_init (stream)
	ip = 1

	while (ip == 1 || opstr[ip] != EOS) {
	    while (IS_WHITE(opstr[ip]))
		ip = ip + 1

	    # If EOS and not first command, all done.  If first command do
	    # not quit, rather assume ":.help" (see below).

	    if (ip > 1 && opstr[ip] == EOS)
		break

	    # Extract the keyword into the KWNAME buffer.  Leave the input
	    # pointer positioned to the first char following the keyword.

	    for (op=1;  opstr[ip] != EOS;  ip=ip+1) {
		ch = opstr[ip]
		if (IS_ALNUM(ch)) {
		    kwname[op] = ch
		    op = op + 1
		} else
		    break
	    }
	    kwname[op] = EOS

	    # Look up the keyword in the dictionary.  If not found ring the bell
	    # but do not return EOF (do not quit cursor mode).

	    if (op == 1)
		opcode = KW_HELP
	    else {
		opcode = strdic (kwname, kwname, MAX_KWLEN, keywords)
		if (opcode <= 0) {
		    call fprintf (STDERR, "\7")
		    goto exit_
		}
	    }

	    # Process the command.

	    switch (opcode) {
	    case KW_AXES:
		# Set flag to draw axes of viewport when screen readrawn.
		RC_AXES(rc) = grc_boolval (opstr, ip)

	    case KW_CASE:
		# Enable/disable case sensitivity.
		RC_CASE(rc) = grc_boolval (opstr, ip)

	    case KW_CLEAR:
		# Clear the alpha screen.
		iferr (tty = ttyodes ("terminal"))
		    call grc_warn (STDERR)
		else {
		    do i = 1, ttystati (tty, TTY_NLINES) {
			call ttygoto (STDOUT, tty, 1, i)
			call ttyclearln (STDOUT, tty)
		    }
		    call flush (STDOUT)
		    call ttycdes (tty)
		}

	    case KW_CURSOR:
		# Select the cursor to be referenced in all subsequent reads
		# and writes.

		ip = ip + 1
		cursor = max (0, nint (grc_realval (opstr, ip)))
		call stg_lockcursor (cursor)

	    case KW_GFLUSH:
		# Flush any buffered graphics output (dispose of spooled
		# plotter output).

		call stg_putline (STDERR, " - ")
		call gtr_gflush (STDPLOT)

	    case KW_HELP:
		# Print help text for cursor mode.
		call gtr_page (STDERR, stream)
		iferr (call pagefile (KEYSFILE, "cursor mode help"))
		    call grc_warn (STDERR)
		ip = ip + 1

	    case KW_INIT:
		# Disconnect all kernels and free memory.  Exits cursor mode
		# with an EOF.

		call stg_putline (STDERR, " - ")
		call gtr_reset (OK)
		call sfree (sp)
		return (EOF)

	    case KW_MARKCUR:
		# Enable marking of the cursor position when the cursor is read.
		RC_MARKCUR(rc) = grc_boolval (opstr, ip)

	    case KW_OFF:
		# Disable the listed keys.
		call grc_keys (rc, opstr, ip, 0)

	    case KW_ON:
		# Enable or set the listed keys.
		call grc_keys (rc, opstr, ip, 1)

	    case KW_PAGE:
		# Enable screen clear when ?, show, etc. print text.
		TR_PAGE(tr) = grc_boolval (opstr, ip)

	    case KW_READ:
		# Fill the frame buffer from a metacode spool file.

		call grc_word (opstr, ip, Memc[fname], SZ_FNAME)
		call grc_read (tr, stream, Memc[fname])

	    case KW_SHOW:
		# Show status of RCURSOR and GIOTR.

		call gtr_page (STDERR, stream)
		call fprintf (STDERR, "Cursor Mode Parameters:\n\n")
		call grc_status (STDERR, rc)

		call fprintf (STDERR, "\n\nGraphics Kernel Status:\n\n")
		call gtr_status (STDERR)

	    case KW_SNAP:
		# Write a snapshot of the screen to a plotter.  Open a subkernel
		# on STDPLOT, redraw the screen into the STDPLOT fio buffer,
		# flush the buffered metacode to the kernel, then restore
		# everything.  NOTE: should restore things automatically if an
		# interrupt occurs.

		call stg_putline (STDERR, " - ")
		call grc_word (opstr, ip, Memc[fname], SZ_FNAME)
		iferr (p_tr = grc_open (Memc[fname], NEW_FILE, STDPLOT, rc)) {
		    call grc_warn (STDERR)
		    goto exit_
		}

		call gki_redir (stream, STDPLOT, save1, save2)
		call fseti (STDPLOT, F_CANCEL, OK)

		iferr {
		    call gtr_redraw (stream)
		    call gki_flush (STDPLOT)
		} then
		    call grc_warn (STDERR)

		call gki_redir (stream, 0, save1, save2)

		auto_gflush = (ttygeti (TR_TTY(p_tr), "MF") <= 1)
		call grc_close (STDPLOT, rc)

		if (auto_gflush)
		    call gtr_gflush (STDPLOT)

		call stg_putline (STDERR, " done")

	    case KW_VIEWPORT:
		# Set the viewport in world coordinates.
		call grc_viewport (tr, stream,
		    sx, sy, raster, rx, ry, opstr, ip)

	    case KW_WRITE:
		# Save the contents of the frame buffer in a file.
		# "w!" clobbers any existing file and "w+" writes the
		# full frame.  By default the frame is appended to the
		# output file.

		if (opstr[ip] == '!') {
		    clobber = true
		    ip = ip + 1
		} else
		    clobber = false

		if (opstr[ip] == '+') {
		    fullframe = true
		    ip = ip + 1
		} else
		    fullframe = false

		# Extract the filename.
		call grc_word (opstr, ip, Memc[fname], SZ_FNAME)

		# Write to the spoolfile.
		call grc_write (tr, stream, Memc[fname], clobber, fullframe)

	    case KW_XRES:
		# Set the stdgraph X resolution.
		xres = nint (grc_realval (opstr, ip))
		yres = 0
		call stg_resolution (xres, yres)

	    case KW_YRES:
		# Set the stdgraph Y resolution.
		xres = 0
		yres = nint (grc_realval (opstr, ip))
		call stg_resolution (xres, yres)

	    case KW_TXQUALITY:
		# Set character generator quality.

		while (IS_WHITE(opstr[ip]))
		    ip = ip + 1

		switch (opstr[ip]) {
		case 'l':
		    quality = GT_LOW
		case 'm':
		    quality = GT_MEDIUM
		case 'h':
		    quality = GT_HIGH
		default:
		    quality = 0
		}
		call stg_txquality (quality)

	    case KW_TXSET:
		# Set the text drawing attributes.
		call gtxset (TR_TXAP(tr), opstr, ip)

	    case KW_ZERO:
		# Reset and redraw.
		call gtr_ptran (stream, 0., 1., 0., 1.)
		call gtr_writecursor (stream, .5, .5)
		call gtr_redraw (stream)
	    }

	    # Advance to the next statement or the end of string.  Any unused
	    # characters in the statement just processed are discarded.

	    while (opstr[ip] != ';' && opstr[ip] != EOS)
		ip = ip + 1
	    while (opstr[ip] == ';' || opstr[ip] == '.')
		ip = ip + 1
	}
exit_
	# Restore the terminal to graphics mode if gtr_page was not called to
	# deactivate the ws. (this leaves the waitpage flag set).

	if (TR_WAITPAGE(tr) == NO)
	    call stg_putline (STDERR, "\n")

	# Leave the graphics descriptor set up as we found it.
	tr = gtr_init (stream)

	call flush (STDERR)
	call sfree (sp)
	return (OK)
end


# GRC_WORD -- Extract the next whitespace delimited word from the command line.

procedure grc_word (opstr, ip, outstr, maxch)

char	opstr[ARB]		# input string
int	ip			# pointer into input string
char	outstr[ARB]		# output string
int	maxch			# max chars out
int	op

begin
	while (IS_WHITE (opstr[ip]))
	    ip = ip + 1

	op = 1
	while (!IS_WHITE (opstr[ip]) && opstr[ip] != EOS) {
	    outstr[op] = opstr[ip]
	    op = op + 1
	    ip = ip + 1
	}

	outstr[op] = EOS
end


# GRC_BOOL -- Get the boolean value of a parameter.  Upon entry, the input
# pointer is positioned to the first character following the parameter name.

int procedure grc_boolval (opstr, ip)

char	opstr[ARB]			# command string
int	ip				# input pointer
int	value
int	btoi()

begin
	while (IS_WHITE (opstr[ip]))
	    ip = ip + 1

	if (opstr[ip] == '=') {
	    ip = ip + 1
	    while (IS_WHITE (opstr[ip]))
		ip = ip + 1
	    value = btoi (opstr[ip] != 'n' && opstr[ip] != 'N')
	    while (IS_ALPHA (opstr[ip]))
		ip = ip + 1
	} else
	    value = btoi (opstr[ip] != '-')

	return (value)
end


# GRC_REALVAL -- Get the real value of a parameter.  Upon entry, the input
# pointer is positioned to the first character following the parameter name.
# Zero is returned if no value is given.

real procedure grc_realval (opstr, ip)

char	opstr[ARB]			# command string
int	ip				# input pointer
real	value
int	ctor()

begin
	while (IS_WHITE (opstr[ip]))
	    ip = ip + 1
	if (opstr[ip] == '=')
	    ip = ip + 1
	while (IS_WHITE (opstr[ip]))
	    ip = ip + 1

	if (ctor (opstr, ip, value) <= 0)
	    value = 0

	return (value)
end


# GRC_KEYS -- Enable the listed keys or ranges of keys.  The operation is
# additive, i.e., only the named keys are affected.

procedure grc_keys (rc, opstr, ip, onoff)

pointer	rc			# rcursor descriptor
char	opstr[ARB]		# command string
int	ip			# next char in opstr
int	onoff			# set keys on (1) or off (0)

int	new_value
int	ch, ch1, ch2, ip_start, i
string	keys KEYSTROKES

begin
	while (IS_WHITE (opstr[ip]))
	    ip = ip + 1

	ip_start = ip
	for (ch=opstr[ip];  ch != EOS;  ch=opstr[ip]) {
	    if (ch == ';' || ch == '\n' || IS_WHITE(ch))
		break

	    ch1 = ch
	    if (opstr[ip+1] == '-' && opstr[ip+2] != EOS) {
		# Enable a range of keys.
		ip  = ip + 2
		ch2 = opstr[ip]
	    } else if (opstr[ip+1] == '=' && opstr[ip+2] != EOS) {
		# Assign the value of a key.
		ip = ip + 3
		RC_KEYS(rc,ch) = opstr[ip]
		next
	    } else
		ch2 = ch

	    for (ch=ch1;  ch <= ch2;  ch=ch+1) {
		if (onoff == 0)
		    new_value = 0
		else
		    new_value = ch
		RC_KEYS(rc,ch) = new_value
	    }

	    ip = ip + 1
	}

	# If no keys were listed, set all cursor mode keys.
	if (ip == ip_start)
	    for (i=1;  keys[i] != EOS;  i=i+1) {
		ch = keys[i]
		if (onoff == 0)
		    new_value = 0
		else
		    new_value = ch
		RC_KEYS(rc,ch) = new_value
	    }
		
	# The ":" key cannot be mapped or disabled.
	RC_KEYS(rc,':') = ':'
end


# GRC_VIEWPORT -- Set the viewport in world coordinates.  Use the current
# cursor position to determine the WCS, then convert the world coordinates
# of the viewport given by the user into NDC coordinates and set the work-
# station transformation.

procedure grc_viewport (tr, stream, sx, sy, raster, rx, ry, opstr, ip)

pointer	tr			#I giotr descriptor
int	stream			#I graphics stream
real	sx, sy			#I screen coordinates of cursor
int	raster			#I raster number
real	rx, ry			#I raster coordinates of cursor
char	opstr[ARB]		#I command string
int	ip			#I input pointer

pointer	w
int	i, wcs
real	wx, wy, value
real	vn[4], vw[4], ct[LEN_CT]
int	ctor()

begin
	# Select a WCS.  We are not otherwise interested in the cursor value.
	call grc_scrtowcs (stream, sx, sy, raster, rx, ry, wx, wy, wcs)
	w = TR_WCSPTR(tr,wcs)
	call grc_settran (w, ct)

	# Start with the current viewport.
	call gtr_gtran (stream, vn[1], vn[2], vn[3], vn[4])

	# Transform to world coordinates.
	call grc_ndctowcs (ct, vn[1], vn[3], vw[1], vw[3])
	call grc_ndctowcs (ct, vn[2], vn[4], vw[2], vw[4])

	# Get the new viewport (world) coordinates.
	do i = 1, 4
	    if (ctor (opstr, ip, value) <= 0)
		break
	    else
		vw[i] = value

	# Transform to NDC coordinates.
	call grc_wcstondc (ct, vw[1], vw[3], vn[1], vn[3])
	call grc_wcstondc (ct, vw[2], vw[4], vn[2], vn[4])

	# Set the new workstation transformation.
	call gtr_ptran (stream, vn[1], vn[2], vn[3], vn[4])

	# Redraw the screen.
	call gtr_redraw (stream)
end
