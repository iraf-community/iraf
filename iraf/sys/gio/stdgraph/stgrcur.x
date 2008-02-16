# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<error.h>
include	<chars.h>
include	<ttset.h>
include	<gset.h>
include	<fset.h>
include	<gki.h>
include	"stdgraph.h"

define	MAX_LENCUR	17
define	MAX_KEYLINES	30
define	KEYSIZE		1
define	QUIT		'q'
define	GO		'g'

# STG_READCURSOR -- Physically read the cursor, returning the cursor position
# in GKI coordinates and the keystroke value as output arguments.  The cursor
# value string is read in raw mode with interrupts disabled.  Receipt of the
# EOF character or CR causes EOF to be returned as the key value.
#
# The cursor is described by two parameters, a pattern string (CD) and a string
# length parameter (CN).
#
#	CD	A pattern specifying either the delimiter sequence if
#		len_curval > 0, or the entire cursor value string if
#		len_curval <= 0.
#
#	CN	If no pattern is given, CN is the number of characters to be
#		read and automatic error detection is not possible.  If a
#		pattern is given, a negative CN specifies the minimum number
# 		of characters in the cursor value string, with the pattern
#		being used to match the characters in the actual cursor value
#		string.  A positive CN specifies a fixed length cursor value
#		string, which which case the pattern is used only to determine
#		when a valid cursor value string has been received.
#
# The cursor read algorithm tries to ignore unsolicited input and recover from
# bad cursor reads, or loss of hardware cursor mode during a cursor read, e.g.,
# if the screen is accidentally cleared or the terminal otherwise initialized.
#
# The physical cursor read sequence is implemented by the stg_rdcursor routine.
# The purpose of the higher level routine is to support STTY playback mode.
# In playback mode, terminal input is taken from a logfile rather than from
# the physical terminal; this is used to prepare automatic scripts to test
# software and for demos.  If playback mode is enabled and `verify' is enabled,
# the logged cursor position will be read, a WC will be issued to move the
# cursor to that position, and then the physical cursor will be read and the
# return value discard, returning the logged position to the calling program.
# In playback mode with `verify' disabled, we need only disable the RC
# instruction and read the logged cursor position; the physical cursor is
# never turned on.

procedure stg_readcursor (cursor, cn, key, sx, sy, raster, rx, ry)

int	cursor		#I cursor to be read
int	cn		#O cursor which was read
int	key		#O keystroke which terminated cursor read
int	sx, sy		#O screen coordinates of cursor in GKI units
int	raster		#O raster number
int	rx, ry		#O raster coordinates of cursor in GKI units

short	texts[4]
char	textc[4], ch
pointer	sp, pbdevice, tx, o_tx
int	delay, nchars, mx, my, i, j, k
bool	playback_mode, pbverify_mode

bool	strne()
pointer	ttygdes()
int	ttstati(), ttstats(), ctocc()
errchk	ttygdes, syserr
include	"stdgraph.com"
define	samedev_ 91

begin
	playback_mode = (ttstati (STDIN, TT_PLAYBACK) == YES)

	if (!playback_mode) {
	    call stg_rdcursor (g_tty, cursor, YES,
		cn, key, sx, sy, raster, rx, ry)
	    return
	}

	call smark (sp)
	call salloc (pbdevice, SZ_GDEVICE, TY_CHAR)
	call salloc (o_tx, LEN_TX, TY_STRUCT)

	# The playback script may have been generated on a different graphics
	# terminal than the one we are playing it back on.  Open the graphcap
	# descriptor for the device used when the script was recorded.  This
	# must be used when decoding cursor input from the logfile.  If device
	# name not recorded in logfile, try to make do with the descriptor for
	# the current stdgraph device.

	if (ttstats (STDIN, TT_GDEVICE, Memc[pbdevice], SZ_GDEVICE) <= 0) {
	    # Device name not recorded in logfile.
	    call syserr (SYS_STTYNOGDEV)

	} else if (g_pbtty == NULL || strne (g_pbdevice, Memc[pbdevice])) {
	    # Device name was recorded; try to load graphcap for it if not
	    # already loaded.

	    if (g_pbtty != NULL)
		call ttycdes (g_pbtty)
	    iferr (g_pbtty = ttygdes (Memc[pbdevice])) {
		g_pbtty = NULL
		call erract (EA_ERROR)
	    }

	    call strcpy (Memc[pbdevice], g_pbdevice, SZ_GDEVICE)
	}

	# Set the playback delay to 0 msec while reading the cursor, else
	# the multicharacter cursor read will take forever.  We issue the
	# delay below, ourselves, one per cursor read.

	delay = ttstati (STDIN, TT_PBDELAY)
	call ttseti (STDIN, TT_PBDELAY, 0)

	# Read the logged cursor position with RC disabled.
	call stg_rdcursor (g_pbtty, cursor, NO,
	    cn, key, sx, sy, raster, rx, ry)

	# Determine if verify mode is set for this cursor read.  This must
	# be done after the call to stg_rdcursor to permit processing of
	# any \{ .. \} in the logfile.

	pbverify_mode = (ttstati (STDIN, TT_PBVERIFY) == YES)

	# Set passthru mode to read/write the device directly.
	call ttseti (STDIN, TT_PASSTHRU, YES)

	# Encode the logged keystroke as a character string.
	if (key == EOF) {
	    call strcpy ("EOF", textc, 4)
	    nchars = 3
	} else if (key <= ' ') {
	    ch = key
	    nchars = ctocc (ch, textc, 4)
	} else {
	    nchars = 1
	    textc[1] = key
	}

	# Pack the string in a short array for the GKI operator.
	call achtcs (textc, texts, nchars)

	# Set the text drawing attributes.
	tx = SG_TXAP(g_sg)
	call amovi (Memi[tx], Memi[o_tx], LEN_TX)
	TX_SIZE(tx) = KEYSIZE
	TX_HJUSTIFY(tx) = GT_LEFT
	TX_VJUSTIFY(tx) = GT_BOTTOM

	# Echo the key character in graphics mode on the top line of the screen,
	# duplicating the text drawn at the cursor position.

	mx = nint ((g_keycol + 0.5) * SG_CHARWIDTH(g_sg,1))
	my = GKI_MAXNDC - nint ((g_keyline + 0.2) * SG_CHARHEIGHT(g_sg,KEYSIZE))

	call stg_text (mx, my, texts, nchars)
	g_keyline = g_keyline + 1
	if (g_keyline > MAX_KEYLINES) {
	    g_keycol = g_keycol + 1
	    g_keyline = 1
	}

	# Echo the logged keystroke at the position of the cursor.  This may
	# not always be readable, but at least it marks the cursor position.

	call stg_text (sx, sy, texts, nchars)

	if (pbverify_mode) {
	    # Issue a WC to set the cursor position, and perform a normal
	    # cursor read in passthru mode, discarding the return value.

	    call stg_setcursor (sx, sy, cursor)
	    call stg_rdcursor (g_tty, cursor, YES, i, i, j, k, i, j, k)

	    # User wants to terminate playback mode?
	    if (k == QUIT || k == INTCHAR) {
		call ttseti (STDIN, TT_PLAYBACK, NO)
		call stg_ctrl ("GD")
		call putline (STDOUT, "[playback mode terminated]")
		call stg_ctrl ("GE")
		call flush (STDOUT)
		call zwmsec (500)
		if (k == INTCHAR)
		    key = EOF
	    } else if (k == GO)
		call ttseti (STDIN, TT_PBVERIFY, NO)
	} else
	    call zwmsec (delay)

	# Restore everything modified earlier.
	call ttseti (STDIN, TT_PASSTHRU, NO)
	call ttseti (STDIN, TT_PBDELAY, delay)
	call amovi (Memi[o_tx], Memi[tx], LEN_TX)

	call sfree (sp)
end


# STG_RDCURSOR -- Physically read the cursor; an internal routine called only
# by the stg_readcursor procedure.  A lower level routine is needed since
# two cursor reads may be required in STTY playback mode, one to read the
# logged cursor position and another to read the physical cursor to synch
# with the user.  This is the real cursor read routine; the only concession
# to playback mode is the `output_rc' switch, to disable output of the RC
# instruction to the terminal, so that the routine does only input from the
# logical device.

procedure stg_rdcursor (tty, cursor, output_rc, cn, key, sx,sy, raster, rx,ry)

pointer	tty		#I graphcap descriptor
int	cursor		#I cursor to be read
int	output_rc	#I flag to output the RC instruction
int	cn		#O cursor which was read
int	key		#O keystroke which terminated cursor read
int	sx, sy		#O cursor screen position in GKI coords
int	raster		#O raster number
int	rx, ry		#O cursor raster position in GKI coords

pointer	decodecur, delimcur, pattern, patbuf, sp, otop
int	len_pattern, len_curval, sv_iomode, nchars, ip, op, i1, i2, ch

bool	ttygetb()
int	getci(), stg_encode()
int	ttygets(), ttygeti(), gstrcpy(), gpatmatch(), patmake(), fstati()
include	"stdgraph.com"
define	quit_ 91

begin
	call smark (sp)
	call salloc (pattern, SZ_LINE, TY_CHAR)
	call salloc (patbuf, SZ_LINE, TY_CHAR)
	call salloc (decodecur, SZ_LINE, TY_CHAR)
	call salloc (delimcur, SZ_FNAME, TY_CHAR)

	key = EOF

	# Make sure there is a cursor before going any further.
	if (!ttygetb (g_tty, "RC"))
	    goto quit_

	len_curval = ttygeti (tty, "CN")
	if (ttygets (tty, "SC", Memc[decodecur], SZ_LINE) <= 0)
	    goto quit_

	len_pattern = 0
	if (ttygets (tty, "CD", Memc[delimcur], SZ_FNAME) > 0)
	    len_pattern = gstrcpy (Memc[delimcur], Memc[pattern], SZ_LINE)

	# Either len_curval or pattern must be given, preferably both.
	if (len_curval == 0 && len_pattern == 0)
	    goto quit_

	# Encode the cursor value pattern, which may be either a pattern
	# matching the entire cursor value, or just the delimiter.  The value
	# of len_curval may be negative if a pattern is given, but must be
	# positive otherwise.  If the pattern is a delimiter string, append
	# the $ metacharacter to match only at the end of the string.

	if (len_pattern > 0) {
	    if (len_curval > 0) {
		Memc[pattern+len_pattern] = '$'
		len_pattern = len_pattern + 1
		Memc[pattern+len_pattern] = EOS
	    }
	    if (patmake (Memc[pattern], Memc[patbuf], SZ_LINE) == ERR)
		goto quit_
	} else if (len_curval < 0)
	    len_curval = -len_curval

	# Set raw mode on the input file (the graphics terminal).
	call flush (STDOUT); call flush (STDERR)
	sv_iomode = fstati (g_in, F_IOMODE)
	if (sv_iomode != IO_RAW)
	    call fseti (g_in, F_IOMODE, IO_RAW)

	repeat {
	    # Initiate a cursor read.
	    if (output_rc == YES) {
		call stg_ctrl1 ("RC", cursor)
		call flush (g_out)
	    }

	    # Read the cursor value string.  If a pattern is given accumulate
	    # at least abs(len_curval) characters and stop when the pattern
	    # is matched, returning the last len_curval characters if
	    # len_curval > 0, else the matched substring.  If no pattern is
	    # given simply accumulate len_curval characters.  The number of
	    # characters we will accumulate in one iteration is limited to
	    # MAX_LENCUR to permit retransmission of the RC control sequence
	    # in the event that hardware cursor mode is accidentally cleared.

	    for (op=1;  op <= MAX_LENCUR;  op=op+1) {
		g_mem[op] = getci (g_in, key)
		g_mem[op+1] = EOS

		if (key == EOF) {
		    # Turn off raw input mode and return EOF.
		    key = EOF
		    if (sv_iomode != IO_RAW)
			call fseti (g_in, F_IOMODE, sv_iomode)
		    goto quit_

		} else if (len_pattern > 0) {
		    # A pattern string was given.  Once the minimum number of
		    # chars have been accumulated, try to match the pattern,
		    # which may match either the cursor string delimiter (in
		    # the case of a fixed length cursor value), or the entire
		    # cursor string (which may then be variable length).

		    if (op < abs(len_curval))
			next
		    else if (gpatmatch (g_mem[1], Memc[patbuf], i1,i2) > 0) {
			if (len_curval > 0)
			    ip = op - len_curval + 1 	# fixed length cur
			else
			    ip = i1			# variable length cur
			break
		    }

		} else if (op >= len_curval) {
		    # No pattern was given.  Terminate the cursor read once
		    # the len_curval characters have been accumulated.

		    ip = 1
		    break
		}
	    }

	    # We have received too many characters, indicating that cursor
	    # mode was lost and the user has been pounding on the keyboard
	    # trying to get the cursor back.  Discard the chars, restart
	    # the cursor and try again.

	    if (op > MAX_LENCUR)
		op = -1

	} until (op >= abs(len_curval) || len_curval == 0)

	# Decode the cursor value string and return the position and key
	# as output values.  Return the cursor position in GKI coordinates.
	# If extra characters were typed, e.g., before the cursor was turned
	# on, and the cursor has a delimiter string, the extra characters will
	# have been read into low memory and we should be able to ignore them
	# and still get a valid read.

	g_reg[E_IOP] = ip
	call aclri (g_reg, 7)
	if (stg_encode (Memc[decodecur], g_mem, g_reg) != OK)
	    call syserr (SYS_GGCUR)

	# Multiple cursors are not implemented yet so just echo input.
	cn  = cursor

	# Standard cursor value.
	sx  = nint ((g_reg[1] - g_x1) / g_dx)
	sy  = nint ((g_reg[2] - g_y1) / g_dy)
	key = g_reg[3]

	# Only some devices return the following fields.  Note that FX,FY
	# are returned by stg_encode in GKI coordinates.

	nchars = g_reg[4]
	raster = g_reg[5]
	if (raster == 0) {
	    rx = sx
	    ry = sy
	} else {
	    rx = g_reg[6]
	    ry = g_reg[7]
	}

	# If the NCHARS field is nonzero then a data block of length nchars
	# follows the cursor value struct returned by the terminal.  Read this
	# into the g_msgbuf message buffer.  The client makes a subsequent
	# call to stg_readtty to access this data, otherwise it is discarded
	# in the next cursor read.

	if (nchars > 0) {
	    if (nchars > g_msgbuflen) {
		g_msgbuflen = (nchars + SZ_MSGBUF - 1) / SZ_MSGBUF * SZ_MSGBUF
		call realloc (g_msgbuf, g_msgbuflen, TY_CHAR)
	    }

	    # We should encode this data transfer in a way that permits
	    # detection and recovery from lost data.  For the moment, the
	    # following assumes that nchars of data will actually be received.

	    op = g_msgbuf
	    otop = g_msgbuf + nchars
	    while (op < otop && getci (g_in, ch) != EOF) {
		Memc[op] = ch
	        op = op + 1
	    }
	    g_msglen = op - g_msgbuf
	    Memc[op] = EOS

	} else
	    g_msglen = 0

	# Turn off raw input mode.
	if (sv_iomode != IO_RAW)
	    call fseti (g_in, F_IOMODE, sv_iomode)

	# Return EOF if any EOF character (e.g., <ctrl/z> or <ctrl/d>) or the
	# interrupt character is typed.

	if (key == EOFCHAR || key == INTCHAR || key == '\004' || key == '\032')
	    key = EOF
quit_
	# Terminate the cursor read.
	if (output_rc == YES) {
	    call stg_ctrl1 ("RE", cursor)
	    call flush (g_out)
	}

	call sfree (sp)
end
