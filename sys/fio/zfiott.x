# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<ttset.h>
include	<ctype.h>
include	<chars.h>
include	<fio.h>

# ZFIOTT -- Logical device driver for terminals.  This VOS level driver
# implements various software terminal options as a transformation on the
# data stream to the hardware terminal driver "os$zfioty.c".  In particular,
# the TT driver can transform input from a monocase terminal into the mixed
# case input required by IRAF, allowing IRAF to be used with old monocase
# terminals, and allowing the user to lock a dual case terminal into upper
# case if desired.  The driver can also log i/o to a file, log the input
# or output streams to separate files, or take input from an input logfile
# (`playback' the command input in a file).

define	HELP "\
\n\r[space or return to continue, g to turn verify off, q to quit]"

define	CONTINUE	' '		# execute command from logfile
define	CONTINUE_ALT	'\r'		# alternative char to continue
define	QUIT		'q'		# terminate playback mode
define	GO		'g'		# continue with verify disabled
define	CTRLCHAR	'^'		# used for shift escape functions
define	SHIFTLOCK	'+'		# ^+
define	SHIFTOFF	'-'		# ^-
define	BEGINCOM	'{'		# \{ comment \}
define	ENDCOM		'}'		# \{ comment \}

define	RMARGIN		75		# for spoolfile output
define	DONTCARE	2		# something other than YES or NO
define	PBDELAY		500		# default delay in playback mode (msec)
define	SZ_LOGLINE	4096		# max chars in a logfile record
define	SZ_DEVNAME	20		# max size termcap/graphcap device name

define	IOFILE		"home$ttyio.log"
define	INFILE		"home$ttyin.log"
define	OUTFILE		"home$ttyout.log"
define	PBFILE		"home$ttyin.log"


# ZGETTT -- Get a line of text from a terminal.  Map the input to lower case
# if indicated, and not in raw mode.

procedure zgettt (fd, buf, maxch, status)

int	fd			# input file
char	buf[ARB]		# output buffer
int	maxch			# max chars out
int	status			# actual chars out

pointer	sp, logbuf
int	nchars, ch, i
int	ztt_lowercase(), ztt_query(), gstrcpy(), and()
include	"zfiott.com"
define	nextline_ 91
define	again_ 92

begin
	# Set raw mode if reading a single character.
	if (maxch == 1)
	    tty_rawmode = true

	if (tty_playback && !tty_passthru) {
	    # Read from the command input spoolfile.

	    if (tty_inbuf[tty_ip] == EOS) {
		call smark (sp)
		call salloc (logbuf, SZ_LOGLINE, TY_CHAR)
nextline_
		call ztt_getlog (tty_pbinchan, Memc[logbuf], SZ_LOGLINE, nchars)

		if (nchars == 1 && Memc[logbuf] == EOFCHAR) {
		    call ztt_ttyput ("[EOF]\n")
		    status = 0

		} else if (nchars > 0) {
		    # Process any \{ ... \} sequences in the line from the
		    # logfile, leave 'status' chars of data text in tty_inbuf.

		    if (ztt_query (Memc[logbuf], nchars,
			tty_inbuf, SZ_LINE, status) == QUIT) {

			# User commands us to quit.
			tty_inbuf[1] = EOS
			tty_ip = 1
			status = 0

		    } else {
			# Copy data text to tty_inbuf.
			tty_inbuf[status+1] = EOS
			status = gstrcpy (tty_inbuf, buf, maxch)
			tty_ip = status + 1

			# If there was no data on the line but we get here,
			# then the line must have been all control directive,
			# so go fetch another line from the logfile.

			if (status == 0)
			    goto nextline_
		    }
		} else
		    status = nchars

		call sfree (sp)

	    } else {
		status = gstrcpy (tty_inbuf[tty_ip], buf, maxch)
		tty_ip = tty_ip + status
		if (!tty_verify || tty_rawmode)
		    call zwmsec (tty_delay)
	    }

	    # Terminate playback if there is the read returns zero chars,
	    # unless this was due to a programmed EOF in the data stream.

	    if (status <= 0 && Memc[logbuf] != EOFCHAR) {
		call ztt_playback (NO)
		call ztt_ttyput ("[playback mode terminated]\n")
		buf[1] = '\n'
		status = 1
	    }

	} else {
	    # Read from the terminal.
again_	    call zgetty (fd, buf, maxch, status)

	    if (status > 0) {
		# Some terminals set the parity bit, which may not be masked
		# by the OS terminal driver in raw mode.  Make sure that the
		# parity bits are cleared.

		if (tty_rawmode)
		    do i = 1, status {
			ch = buf[i]
			buf[i] = and (ch, 177B)
		    }

		# Filter the input if a filter has been posted and the filter
		# key is seen as the first character of the input data block.
		# The filter edits the input buffer and returns the number of
		# input characters left in the buffer after applying the filter.

		if (tty_filter != 0)
		    if (buf[1] == tty_filter_key) {
			call zcall4 (tty_filter, fd, buf, maxch, status)
			if (status == 0)
			    goto again_
		    }
	    }
	}

	# Log the input string if input logging is in effect.
	if (tty_login && !tty_passthru) {
	    if (status <= 0)
		call ztt_putlog (tty_inlogchan, "\032", 1)
	    else
		call ztt_putlog (tty_inlogchan, buf, status)
	}

	# If UCASE mode in set and not in raw mode, map the input string to
	# lower case.

	if ((tty_ucasein || tty_ucaseout) && status > 0)
	    if (!tty_rawmode && tty_ucasein)
		status = ztt_lowercase (buf, buf, status)
end


# ZPUTTT -- Put "nchars" characters into the text file "fd".  Map the output
# to upper case if so indicated.  Watch for the RAWOFF control string, used
# to turn raw mode off.

procedure zputtt (fd, buf, nchars, status)

int	fd			# file to be written to
char	buf[ARB]		# data to be output
int	nchars			# nchars to write to file
int	status			# return status

int	ch
pointer	sp, obuf
bool	ctrlstr
int	strncmp()
include	"zfiott.com"
define	noucase_ 91

begin
	# Do not map the raw-mode-off control sequence to upper case.
	ctrlstr = false
	if (tty_rawmode)
	    if (nchars == LEN_RAWCMD && buf[1] == ESC)
		if (strncmp (buf, RAWOFF, LEN_RAWCMD) == 0) {
		    ctrlstr = true
		    tty_rawmode = false
		} else if (strncmp (buf, RAWON, LEN_RAWCMD) == 0) {
		    ctrlstr = true
		    tty_rawmode = true
		}

	if (tty_ucaseout) {
	    # If not control string and raw mode is not in effect, map the
	    # string to upper case and output it.  Do not map escape or control
	    # sequences, i.e., any string which begins with a control character.

	    if (!ctrlstr && !tty_rawmode) {
		ch = buf[1]
		if (ch < BLANK)
		    if (ch != HT && ch != LF && ch != FF && ch != CR)
			goto noucase_

		call smark (sp)
		call salloc (obuf, SZ_LINE, TY_CHAR)

		call ztt_uppercase (buf, Memc[obuf], nchars)
		call zputty (fd, Memc[obuf], nchars, status)

		if (tty_logout && !tty_passthru)
		    call ztt_putlog (tty_outlogchan, Memc[obuf], nchars)

		call sfree (sp)
		return
	    }
	}
noucase_
	call zputty (fd, buf, nchars, status)
	if (tty_logout && !tty_passthru)
	    call ztt_putlog (tty_outlogchan, buf, nchars)
end


# ZTT_LOGIO -- Enable or disable logging of terminal i/o in a file.  Logging
# is used for debug purposes but may also be used to keep a complete record
# of a terminal session.

procedure ztt_logio (inflag, outflag)

int	inflag			# log input stream (YES|NO|DONTCARE)
int	outflag			# log output stream (YES|NO|DONTCARE)

int	status
pointer	sp, osfn, fname
string	openerr "cannot open file "
include	"zfiott.com"

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (osfn, SZ_PATHNAME, TY_CHAR)

	# Enable/disable logging of the input stream.
	if (inflag == YES) {
	    if (tty_login) {
		call zclstx (tty_inlogchan, status)
		tty_inlogchan = NULL
		tty_login = false
	    }

	    if (tty_logio)
		call strcpy (tty_iofile, Memc[fname], SZ_PATHNAME)
	    else
		call strcpy (tty_infile, Memc[fname], SZ_PATHNAME)

	    ifnoerr (call fmapfn (Memc[fname], Memc[osfn], SZ_PATHNAME)) {
		call zopntx (Memc[osfn], APPEND, tty_inlogchan)
		tty_login = (tty_inlogchan != ERR)
	    }

	    if (tty_login)
		call ztt_logdev (tty_inlogchan)
	    else {
		call ztt_ttyput (openerr)
		call ztt_ttyput (Memc[fname])
		call ztt_ttyput ("\n")
	    }

	} else if (inflag == NO && tty_login) {
	    call zclstx (tty_inlogchan, status)
	    tty_inlogchan = NULL
	    tty_login = false
	    if (tty_logio) {
		tty_logout = false
		tty_logio = false
	    }
	}

	# If LOGIO mode is in effect, set the output logfile to the same
	# as the input logfile, otherwise open the output logfile.

	if (tty_logio && tty_login) {
	    tty_logout = true
	    tty_outlogchan = tty_inlogchan

	} else if (outflag == YES) {
	    if (tty_logout) {
		call zclstx (tty_outlogchan, status)
		tty_outlogchan = NULL
		tty_logout = false
	    }

	    ifnoerr (call fmapfn (tty_outfile, Memc[osfn], SZ_PATHNAME)) {
		call zopntx (Memc[osfn], APPEND, tty_outlogchan)
		tty_logout = (tty_outlogchan != ERR)
	    }

	    if (tty_logout)
		call ztt_logdev (tty_outlogchan)
	    else {
		call ztt_ttyput (openerr)
		call ztt_ttyput (tty_outfile)
		call ztt_ttyput ("\n")
	    }

	} else if (outflag == NO && tty_logout) {
	    call zclstx (tty_outlogchan, status)
	    tty_outlogchan = NULL
	    tty_logout = false
	}
	 
	call sfree (sp)
end


# ZTT_PLAYBACK -- Enable or disable playback mode.  When playback mode is
# in effect command input is redirected to a tty logfile rather than to the
# terminal.  Successive commands are read from the logfile and echoed on
# the terminal.  If `verify' mode playback is enabled the user must then
# tap the space bar or CR to continue, at which time the line of text is
# returned to the calling program.  Playback mode terminates when EOF is
# seen on the input file, or when the user types `q' in response to the
# verify query.

procedure ztt_playback (flag)

int	flag			# YES to enable playback, NO to disable

int	status
pointer	sp, osfn
extern	ztt_pboff()
string	openerr "cannot open file "
include	"zfiott.com"

begin
	call smark (sp)
	call salloc (osfn, SZ_PATHNAME, TY_CHAR)

	if (flag == YES) {
	    # If we try to turn on playback mode while in login mode, log
	    # the command but do not interrupt login mode or try to reopen
	    # the ttyin.log file.  The logged `stty playback' command will
	    # cause a (possibly infinite) loop when the logfile is later
	    # played back.

	    if (tty_login) {
		call ztt_ttyput ("[command logged but not executed]\n")
		call sfree (sp)
		return
	    }

	    # Clear playback mode if already in effect.
	    if (tty_playback) {
		call zclstx (tty_pbinchan, status)
		tty_pbinchan = NULL
		tty_playback = false
	    }

	    # Open login file.
	    ifnoerr (call fmapfn (tty_pbfile, Memc[osfn], SZ_PATHNAME)) {
		call zopntx (Memc[osfn], READ_ONLY, tty_pbinchan)
		tty_playback = (tty_pbinchan != ERR)
	    }

	    # Setup to clear playback mode if error occurs during playback.
	    if (tty_playback) {
		call onerror (ztt_pboff)
		tty_tdevice[1] = EOS
		tty_gdevice[1] = EOS
	    } else {
		call ztt_ttyput (openerr)
		call ztt_ttyput (tty_pbfile)
		call ztt_ttyput ("\n")
	    }

	} else if (flag == NO && tty_playback) {
	    # Clear playback mode.

	    call zclstx (tty_pbinchan, status)
	    tty_pbinchan = NULL
	    tty_playback = false
	}

	call sfree (sp)
end


# ZTT_PBOFF -- Called during error recovery to disable playback mode.

procedure ztt_pboff (errcode)

int	errcode			# error status

int	status
include	"zfiott.com"

begin
	if (errcode != OK && tty_playback) {
	    call zclstx (tty_pbinchan, status)
	    tty_pbinchan = NULL
	    tty_playback = false
	    tty_rawmode  = false
	    tty_passthru = false
	}
end


# ZTT_LOGDEV -- Record the names of the terminal and stdgraph devices in a
# logfile.  The format ("\X=devname\n") MUST agree with that in ztt_getlog.
# Also timestamp the logfile.

procedure ztt_logdev (chan)

int	chan			# output file

int	status
pointer	sp, obuf, devname
int	envfind(), strlen()

begin
	call smark (sp)
	call salloc (obuf, SZ_LINE, TY_CHAR)
	call salloc (devname, SZ_FNAME, TY_CHAR)

	# Timestamp the new entry in the logfile.
	call strcpy ("\O=", Memc[obuf], SZ_LINE)
	call sysid (Memc[obuf+3], SZ_LINE-3)
	call strcat ("\n", Memc[obuf], SZ_LINE)
	call zputtx (chan, Memc[obuf], strlen(Memc[obuf]), status)

	if (envfind ("terminal", Memc[devname], SZ_FNAME) > 0) {
	    call strcpy ("\T=", Memc[obuf], SZ_LINE)
	    call strcat (Memc[devname], Memc[obuf], SZ_LINE)
	    call strcat ("\n", Memc[obuf], SZ_LINE)
	    call zputtx (chan, Memc[obuf], strlen(Memc[obuf]), status)
	}
	if (envfind ("stdgraph", Memc[devname], SZ_FNAME) > 0) {
	    call strcpy ("\G=", Memc[obuf], SZ_LINE)
	    call strcat (Memc[devname], Memc[obuf], SZ_LINE)
	    call strcat ("\n", Memc[obuf], SZ_LINE)
	    call zputtx (chan, Memc[obuf], strlen(Memc[obuf]), status)
	}

	call sfree (sp)
end


# ZTT_PUTLOG -- Put a message to the logfile.  All characters in the data
# string are rendered into printable form.  Long lines are broken and the
# output is followed by a newline.

procedure ztt_putlog (chan, dstr, nchars)

int	chan			# kernel i/o channel
char	dstr[ARB]		# data string
int	nchars			# length of data string (0 if EOS delimited)

char	cch
pointer	sp, obuf, op
int	status, ip, ch, n
int	strlen(), ctocc()
define	output {Memc[op]=($1);op=op+1}
include	"zfiott.com"

begin
	# It is harmless to call us if logging is disabled.
	if (!tty_login && !tty_logout)
	    return

	call smark (sp)
	call salloc (obuf, SZ_LINE, TY_CHAR)

	n = nchars
	if (n <= 0)
	    n = strlen (dstr)

	# Output the data string, rendering all characters into printable form.
	# Break long lines.  The characters \ and ^ must be escaped since they
	# are logfile metacharacters.  Data spaces are output as \s since
	# whitespace in the logfile is ignored.

	op = obuf
	do ip = 1, n {
	    ch = dstr[ip]
	    if (ch == ' ') {
		output ('\\')
		output ('s')
	    } else if (ch == '^' || ch == '\\') {
		output ('\\')
		output (ch)
	    } else if (IS_PRINT (ch)) {
		output (ch)
	    } else if (ch == NUL) {
		output ('^')
		output ('@')
	    } else {
		cch = ch
		op = op + ctocc (cch, Memc[op], 5)
	    }

	    if (op - obuf >= RMARGIN && ip+1 < n) {
		output ('\\')
		output ('\n')
		call zputtx (chan, Memc[obuf], op-obuf, status)
		for (op=obuf;  op < obuf+4;  op=op+1)
		    Memc[op] = ' '
	    }
	}

	# Terminate and output the line.
	if (op > obuf) {
	    output ('\n')
	    call zputtx (chan, Memc[obuf], op-obuf, status)
	    call zflstx (chan, status)
	}

	call sfree (sp)
end


# ZTT_GETLOG -- Read text from a logfile written by ztt_putlog.  All control
# codes and spaces are rendered into escape sequences; newline marks the end
# of each record and an escaped newline followed by leading whitespace on a
# line indicates continuation.  Blank lines in the logfile equate to null
# length records and are ignored.

procedure ztt_getlog (chan, obuf, maxch, nchars)

int	chan			# kernel input channel (text file)
char	obuf[maxch]		# output buffer
int	maxch			# max chars to return
int	nchars			# nchars returned or EOF

bool	incom
int	lastch, ch, op, o
char	cch, cc[4], devname[SZ_DEVNAME]
int	ztt_getchar(), cctoc()
include	"zfiott.com"

begin
	# Process characters and escape sequence encoded characters from
	# the logfile until either maxch character have been output or an
	# unescaped newline is seen.  Ignore empty lines.  Text enclosed
	# in \{ ... \} (comment text) is returned without change.

	incom = false
	ch = NULL

	for (op=1;  op <= 1 && ch != EOF;  ) {
	    while (op <= maxch) {
		# Get the next character (not efficient, but doesn't matter
		# since this is only called in `stty playback' mode).

		if (ztt_getchar (chan, ch) == EOF) {
		    break
		} else if (IS_WHITE (ch) && !incom) {
		    next
		} else if (ch == '\n' && !incom) {
		    break

		} else if (ch == '^') {
		    # Map a control code, e.g., ^[.
		    if (ztt_getchar (chan, ch) == EOF)
			break
		    ch = mod (ch, 40B)

		} else if (ch == '\\') {
		    # Map an escape sequence, e.g., \n, \r, \^, \040, etc.
		    if (ztt_getchar (chan, ch) == EOF)
			break

		    switch (ch) {
		    case '\n':
			next
		    case 's':
			ch = ' '
			# output ch, below

		    case BEGINCOM, ENDCOM:
			# Copy a \{ ... \} logfile comment (to be echoed but
			# not returned as data).

			obuf[op] = '\\'
			op = op + 1
			obuf[op] = ch
			op = op + 1

			incom = (ch == BEGINCOM)
			next

		    case 'T', 'G', 'O', '#':
			# Recall a terminal or stdgraph device name from the
			# logfile (device used when logfile was written).
			# The format must be "\X=devname\n".  \O is the
			# timestamp string, which we simply read and discard.
			# \# is for logfile comments.

			lastch = ch
			o = 1
			repeat {
			    if (ztt_getchar (chan, ch) == EOF)
				break
			    else if (ch == '\n')
				break
			    else {
				devname[o] = ch
				o = min (SZ_DEVNAME, o + 1)
			    }
			}
			devname[o] = EOS

			if (lastch == 'T')
			    call strcpy (devname[2], tty_tdevice, SZ_DEVNAME)
			else if (lastch == 'G')
			    call strcpy (devname[2], tty_gdevice, SZ_DEVNAME)
			next

		    case '^', '\\':
			# output ch, below

		    default:
			cc[1] = '\\'
			cc[2] = ch
			if (IS_DIGIT (ch)) {
			    for (o=3;  o <= 4;  o=o+1)
				if (ztt_getchar (chan, ch) == EOF)
				    break
				else
				    cc[o] = ch
			    cc[o] = EOS
			} else
			    cc[3] = EOS
			o = 1;  o = cctoc (cc, o, cch)
			ch = cch
			# output ch, below
		    }
		}

		obuf[op] = ch
		op = op + 1
	    }
	}

	nchars = op - 1
end


# ZTT_QUERY -- Called in playback mode to echo a line of logfile input text to
# the terminal and wait for the user to tap the CONTINUE key to continue.
# If the response is QUIT playback mode is terminated and input is restored
# to the terminal.  If the response is GO verify mode is disabled for the
# remainder of the playback session.  It would be easy for us to allow the
# user to edit the command line rather than just accept it, but this could too
# easily cause loss of sync with the input logfile, hence is not allowed.
# Echoing and verify are disabled if raw mode is in effect.

int procedure ztt_query (logtext, nchars, dtext, maxch, sz_dtext)

char	logtext[ARB]		# line of text from logfile
int	nchars			# nchars in logfile text
char	dtext[maxch]		# line of text to be returned from zgettt
int	maxch			# max chars returned
int	sz_dtext		# actual chars returned

char	text[1]
pointer	sp, etext, ep
bool	learn, incom, verify, format_control
int	status, delay, ip_save, ip, op, ch, n

int	ctoi()
include	"zfiott.com"
define	done_ 91
define	deposit_ 92

begin
	call smark (sp)
	call salloc (etext, SZ_LINE, TY_CHAR)

	# The logfile line may contain embedded sequences of text which are
	# to be echoed to the terminal, but which are not to be returned as
	# data to the calling program.  This comment or explanatory text is
	# enclosed in braces as "\{ ... \}".  Control over the verify/delay
	# parameters may be specified for the command block by modifying
	# the opening sequence, i.e., "\{%V+ ..." sets verify mode for the
	# block, "\{%V-" disables verify mode, and "\{%NNNN sets the delay
	# to NNNN msec.  A leading !, e.g., "%!V+" causes the change to be
	# "learned", i.e., the control parameter is permanently changed.

	verify = (tty_verify && !tty_rawmode)
	delay  = tty_delay
	incom  = false
	ep     = etext
	op     = 1

	# Process the logfile text into the text to be echoed and the data
	# (obuf) text to be returned to the calling program.

	format_control = false
	for (ip=1;  ip <= nchars;  ) {
	    if (logtext[ip] == '\\') {
		if (ip < nchars && logtext[ip+1] == BEGINCOM) {
		    # Begin comment section.
		    ip = ip + 2
		    incom = true

		    # Check for the verify/delay overrides.
		    while (logtext[ip] == '%') {
			ip_save = ip
			ip = ip + 1

			# If !V+ or !delay, learn new value.
			learn = (logtext[ip] == '!')
			if (learn)
			    ip = ip + 1

			if (logtext[ip] == 'V') {
			    ip = ip + 1
			    if (logtext[ip] == '+') {
				verify = true
				if (learn)
				    tty_verify = true
				ip = ip + 1
			    } else if (logtext[ip] == '-') {
				verify = false
				if (learn)
				    tty_verify = false
				ip = ip + 1
			    } else
				ip = ip_save
			} else if (IS_DIGIT (logtext[ip])) {
			    if (ctoi (logtext, ip, delay) <= 0) {
				delay = tty_delay
				ip = ip_save
			    } else if (learn)
				tty_delay = delay
			}

			if (ip > ip_save)
			    format_control = true
			else
			    break
		    }

		} else if (incom && ip < nchars && logtext[ip+1] == ENDCOM) {
		    # End comment section.
		    ip = ip + 2
		    incom = false
		} else
		    goto deposit_

	    } else {
deposit_	# Do not include the trailing data-newline in the echo text.
		if (incom || (!tty_rawmode && logtext[ip] != '\n'))  {
		    Memc[ep] = logtext[ip]
		    ep = ep + 1
		}
		if (logtext[ip] == '\n' && ip < nchars) {
		    if (ep > etext) {
			n = ep - etext
			call zputty (tty_koutchan, Memc[etext], ep-etext, n)
			call zflsty (tty_koutchan, status)
		    }
		    ep = etext
		}
		if (!incom) {
		    op = min (maxch, op)
		    dtext[op] = logtext[ip]
		    op = op + 1
		}
		ip = ip + 1
	    }
	}

	# Don't need to add EOS for counted kernel i/o strings.
	sz_dtext = op - 1

	# Output any remaining echo text.
	if (ep > etext) {
	    n = ep - etext
	    call zputty (tty_koutchan, Memc[etext], n, status)
	    call zflsty (tty_koutchan, status)
	    ep = etext
	}

	# Do not verify or delay for blank lines with no format control.
	if (!format_control && sz_dtext == 1 && dtext[1] == '\n') {
	    ch = NULL
	    goto done_
	}

	# If verify is disabled, return after the specified delay.
	if (!verify) {
	    call zwmsec (delay)
	    ch = NULL
	    goto done_
	}

	# If verify is enabled, wait for user response.  Note that the 1
	# char read leaves the terminal in raw mode.

	repeat {
	    call zgetty (tty_kinchan, text, 1, status)
	    if (status > 0)
		ch = text[1]
	    else 
		ch = EOF

	    if (ch == EOF || ch == INTCHAR || ch == QUIT) {
		call ztt_playback (NO)
		ch = QUIT
		break
	    } else if (ch == GO) {
		tty_verify = false
		break
	    } else if (ch == CONTINUE || ch == CONTINUE_ALT) {
		break
	    } else {
		# Ignore other characters.
		call ztt_ttyput (HELP)
	    }
	} 

	# Restore terminal to line mode, if raw mode was not already in
	# effect before our query.

	if (!tty_rawmode)
	    call ztt_ttyput (RAWOFF)

done_
	if (dtext[sz_dtext] == '\n')
	    call ztt_ttyput ("\n")

	call sfree (sp)
	return (ch)
end


# ZTT_GETCHAR -- Get a character from a channel.

int procedure ztt_getchar (chan, ch)

int	chan			# input channel
int	ch			# receives character

char	text[1]
int	status

begin
	call zgettx (chan, text, 1, status)
	if (status <= 0) {
	    ch = EOF
	    return (EOF)
	} else {
	    ch = text[1]
	    return (ch)
	}
end


# ZTT_LOWERCASE -- Map a character string input in upper case to lower case.
# Control sequences may be embedded in the sequence to artifically generate
# upper case characters.
#
#	^	shift up next character
#	^+	shift lock (stay in upper case)
#	^-	clear shift lock
#	^^	a single ^
#
# The case shift control sequences are shown above.  These are not recognized
# when the terminal is in raw mode.

int procedure ztt_lowercase (in, out, nchars)

char	in[ARB]			# input string
char	out[ARB]		# output string
int	nchars			# input string length

int	ch
int	ip, op
include	"zfiott.com"

begin
	op = 1
	for (ip=1;  ip <= nchars;  ip=ip+1) {
	    ch = in[ip]

	    if (ch == CTRLCHAR) {
		ch = in[ip+1]
		ip = ip + 1

		switch (ch) {
		case CTRLCHAR:
		    out[op] = ch
		    op = op + 1
		case SHIFTLOCK:
		    tty_shiftlock = true
		case SHIFTOFF:
		    tty_shiftlock = false
		default:
		    if (IS_LOWER (ch))
			ch = TO_UPPER (ch)
		    out[op] = ch
		    op = op + 1
		}
	    } else if (tty_shiftlock) {
		if (IS_LOWER (ch))
		    ch = TO_UPPER (ch)
		out[op] = ch
		op = op + 1
	    } else {
		if (IS_UPPER (ch))
		    ch = TO_LOWER (ch)
		out[op] = ch
		op = op + 1
	    }
	}

	return (op - 1)
end


# ZTT_UPPERCASE -- Convert a string to upper case.

procedure ztt_uppercase (in, out, nchars)

char	in[ARB]			# input string
char	out[ARB]		# output string
int	nchars			# string length

int	ch, i

begin
	do i = 1, nchars {
	    ch = in[i]
	    if (IS_LOWER (ch))
		ch = TO_UPPER (ch)
	    out[i] = ch
	}
end


# ZTT_TTYPUT -- Write directly to the user terminal.

procedure ztt_ttyput (message)

char	message[ARB]		# message string

int	status
int	stridxs(), strlen()
include	"zfiott.com"

begin
	call zputty (tty_koutchan, message, strlen(message), status)
	if (stridxs ("\n", message) > 0)
	    call zflsty (tty_koutchan, status)
end


# ZSETTT -- Set TT terminal driver options.  Must be called before any i/o is
# done via the TT driver, e.g., by fio$finit.

procedure zsettt (chan, param, value)

int	chan			# kernel i/o channel (not used)
int	param			# parameter to be set
int	value			# new value

bool	itob()
bool	first_time
data	first_time /true/
include	"zfiott.com"

begin
	switch (param) {
	case TT_INITIALIZE:
	    if (!first_time) {
		# Close any open log files.
		call ztt_playback (NO)
		call ztt_logio (NO, NO)
	    }

	    tty_inlogchan  = NULL
	    tty_outlogchan = NULL
	    tty_pbinchan   = NULL
	    tty_ucasein    = false
	    tty_ucaseout   = false
	    tty_shiftlock  = false
	    tty_rawmode    = false
	    tty_logio      = false
	    tty_login      = false
	    tty_logout     = false
	    tty_playback   = false
	    tty_verify     = false
	    tty_passthru   = false
	    tty_delay      = PBDELAY
	    tty_filter     = NULL
	    tty_filter_key = 0

	    call strcpy (IOFILE,  tty_iofile,  SZ_FNAME)
	    call strcpy (INFILE,  tty_infile,  SZ_FNAME)
	    call strcpy (OUTFILE, tty_outfile, SZ_FNAME)
	    call strcpy (PBFILE,  tty_pbfile,  SZ_FNAME)

	    tty_tdevice[1] = EOS
	    tty_gdevice[1] = EOS
	    tty_inbuf[1] = EOS
	    tty_ip = 1

	    first_time = false

	case TT_KINCHAN:
	    tty_kinchan = value
	case TT_KOUTCHAN:
	    tty_koutchan = value
	case TT_LOGINCHAN:
	    tty_inlogchan = value
	case TT_LOGOUTCHAN:
	    tty_outlogchan = value
	case TT_PBINCHAN:
	    tty_pbinchan = value
	case TT_SHIFTLOCK:
	    tty_shiftlock = itob (value)
	case TT_RAWMODE:
	    tty_rawmode = itob (value)

	case TT_UCASEIN:
	    tty_ucasein = itob (value)
	case TT_UCASEOUT:
	    tty_ucaseout = itob (value)

	case TT_LOGIO:
	    tty_logio = true
	    call ztt_logio (value, value)
	case TT_LOGIN:
	    tty_logio = false
	    call ztt_logio (value, DONTCARE)
	case TT_LOGOUT:
	    tty_logio = false
	    call ztt_logio (DONTCARE, value)
	case TT_PASSTHRU:
	    tty_passthru = itob (value)

	case TT_PLAYBACK:
	    call ztt_playback (value)
	case TT_PBVERIFY:
	    tty_verify = itob (value)
	case TT_PBDELAY:
	    tty_delay = value

	case TT_FILTER:
	    tty_filter = value
	case TT_FILTERKEY:
	    tty_filter_key = value

	default:
	    # (ignore)
	}
end


# ZSTTTT -- Stat TT terminal driver options.  Check for the special TT params,
# else pass the request to the hardware driver.

procedure zstttt (fd, param, lvalue)

int	fd			# file number (not used)
int	param			# parameter to be set
long	lvalue			# new value

int	btoi()
include	"zfiott.com"

begin
	switch (param) {
	case TT_KINCHAN:
	    lvalue = tty_kinchan
	case TT_KOUTCHAN:
	    lvalue = tty_koutchan
	case TT_LOGINCHAN:
	    lvalue = tty_inlogchan
	case TT_LOGOUTCHAN:
	    lvalue = tty_outlogchan
	case TT_PBINCHAN:
	    lvalue = tty_pbinchan
	case TT_UCASEIN:
	    lvalue = btoi (tty_ucasein)
	case TT_UCASEOUT:
	    lvalue = btoi (tty_ucaseout)
	case TT_SHIFTLOCK:
	    lvalue = btoi (tty_shiftlock)
	case TT_RAWMODE:
	    lvalue = btoi (tty_rawmode)
	case TT_LOGIO:
	    lvalue = btoi (tty_logio)
	case TT_LOGIN:
	    lvalue = btoi (tty_login)
	case TT_LOGOUT:
	    lvalue = btoi (tty_logout)
	case TT_PASSTHRU:
	    lvalue = btoi (tty_passthru)
	case TT_PLAYBACK:
	    lvalue = btoi (tty_playback)
	case TT_PBVERIFY:
	    lvalue = btoi (tty_verify)
	case TT_PBDELAY:
	    lvalue = tty_delay
	case TT_FILTER:
	    lvalue = tty_filter
	case TT_FILTERKEY:
	    lvalue = tty_filter_key
	default:
	    call zsttty (fd, param, lvalue)
	}
end


# ZSESTT -- Set a TT terminal driver string valued option.

procedure zsestt (fd, param, svalue)

int	fd			# file number (not used)
int	param			# parameter to be set
char	svalue[ARB]		# new value

include	"zfiott.com"

begin
	switch (param) {
	case TT_IOFILE:
	    call strcpy (svalue, tty_iofile, SZ_FNAME)
	case TT_INFILE:
	    call strcpy (svalue, tty_infile, SZ_FNAME)
	case TT_OUTFILE:
	    call strcpy (svalue, tty_outfile, SZ_FNAME)
	case TT_PBFILE:
	    call strcpy (svalue, tty_pbfile, SZ_FNAME)
	case TT_TDEVICE:
	    call strcpy (svalue, tty_tdevice, SZ_DEVNAME)
	case TT_GDEVICE:
	    call strcpy (svalue, tty_gdevice, SZ_DEVNAME)
	default:
	    # (ignore)
	}
end


# ZSTSTT -- Stat TT terminal driver string valued option.

procedure zststt (fd, param, outstr, maxch, nchars)

int	fd			# file number (not used)
int	param			# parameter to be set
char	outstr[maxch]		# string value
int	maxch			# max chars out
int	nchars			# len (outstr)

int	gstrcpy()
include	"zfiott.com"

begin
	switch (param) {
	case TT_IOFILE:
	    nchars = gstrcpy (tty_iofile, outstr, maxch)
	case TT_INFILE:
	    nchars = gstrcpy (tty_infile, outstr, maxch)
	case TT_OUTFILE:
	    nchars = gstrcpy (tty_outfile, outstr, maxch)
	case TT_PBFILE:
	    nchars = gstrcpy (tty_pbfile, outstr, maxch)
	case TT_TDEVICE:
	    nchars = gstrcpy (tty_tdevice, outstr, maxch)
	case TT_GDEVICE:
	    nchars = gstrcpy (tty_gdevice, outstr, maxch)
	default:
	    nchars = 0
	}
end


# The following functions are straight pass throughs to the hardware
# driver for this device.
# --------------------------

# ZOPNTT -- Open a terminal.

procedure zopntt (osfn, mode, chan)

char	osfn[ARB]		# UNIX filename
int	mode			# file access mode
int	chan			# UNIX channel of file (output)

begin
	call zopnty (osfn, mode, chan)
end


# ZCLSTT -- Close a terminal.

procedure zclstt (fd, status)

int	fd			# channel
int	status			# return status

begin
	call zclsty (fd, status)
end


# ZFLSTT -- Flush any buffered terminal output.

procedure zflstt (fd, status)

int	fd			# channel
int	status			# return status

begin
	call zflsty (fd, status)
end


# ZSEKTT -- Seek on a text file to the character offset given by a prior
# call to ZNOTTT.  This offset should always refer to the beginning of a line.
# (not used for terminals).

procedure zsektt (fd, offset, status)

int	fd			# channel
long	offset			# new offset
int	status			# return status

begin
	call zsekty (fd, offset, status)
end


# ZNOTTT -- Return the seek offset of the beginning of the current line
# of text (not used for terminals).

procedure znottt (fd, offset)

int	fd			# channel
long	offset			# file offset

begin
	call znotty (fd, offset)
end
