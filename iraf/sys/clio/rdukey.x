# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<ctype.h>
include	<ttset.h>
include	<fset.h>

# RD_UKEY -- Read a user keystroke from the terminal.  The "ukey" object is
# either a single key, or the : key plus associated string value.
# The value of a ukey parameter is returned as a string (as for rcursor)
# and is normally fetched by an applications program with CLGKEY.  The
# format of the command string is
#
#	ch strval
#
# where the `strval' is present only if CH=:, i.e., the command is a colon
# escape.  Control keys are encoded as octal codes of the form \NNN.

int procedure rdukey (keystr, maxch)

char	keystr[ARB]	 	# receives keystroke command string
int	maxch			# max chars out

int	junk, ch
int	delay, key, tty
pointer	sp, buf, ip, op
bool	rawmode_set, ucasein_set
bool	playback_set, pbverify_set

pointer	ttyodes()
int	fstati(), ttstati(), envgets(), getci()
define	again_ 91
define	done_  92
errchk	ttyodes, syserrs

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	call flush (STDERR)
	call flush (STDOUT)

	# Note whether playback mode is in effect, and set raw mode if it
	# is not already set.

	rawmode_set  = (fstati  (STDIN, F_RAW) == YES)
	playback_set = (ttstati (STDIN, TT_PLAYBACK) == YES)
	pbverify_set = (ttstati (STDIN, TT_PBVERIFY) == YES)
	ucasein_set  = (ttstati (STDIN, TT_UCASEIN) == YES)

	if (!rawmode_set)
	    call fseti (STDIN, F_RAW, YES)
	if (playback_set) {
	    delay = ttstati (STDIN, TT_PBDELAY)
	    call ttseti (STDIN, TT_PBDELAY, 0)
	}

	# Get keystroke.
	tty = NULL
again_
	if (getci (STDIN, key) == EOF)
	    goto done_

	if (tty == NULL && (key == ':' || playback_set)) {
	    junk = envgets ("terminal", Memc[buf], SZ_LINE)
	    tty = ttyodes (Memc[buf])
	    if (tty == ERR) {
		if (!rawmode_set)
		    call fseti (STDIN, F_RAW, NO)
		call syserrs (SYS_TTYDEVNF, Memc[buf])
	    }
	}

	# If colon escape, clear the current line and read the string value.
	# The read is performed in raw mode to avoid a line feed and scroll
	# when the CR is typed.
	
	if (key == ':') {
	    call ttyclearln (STDOUT, tty)
	    call ttyputline (STDOUT, tty, "\r:", NO)
	    call flush (STDOUT)

	    for (op=buf;  getci (STDIN, ch) != EOF;  ) {
		if (ch == '\177' || ch == '\010') {
		    if (op > buf) {
			op = op - 1
			Memc[op] = EOS
			call ttyclearln (STDOUT, tty)
			call ttyputline (STDOUT, tty, "\r:", NO)
			call ttyputline (STDOUT, tty, Memc[buf], NO)
			call flush (STDOUT)
		    }
		} else if (ch == '\003') {
		    call ttyclearln (STDOUT, tty)
		    goto again_
		} else if (ch == '\n' || ch == '\r' || (op - buf) >= SZ_LINE) {
		    break
		} else {
		    call putci (STDOUT, ch)
		    call flush (STDOUT)
		    if (ucasein_set && IS_UPPER(ch))
			Memc[op] = TO_LOWER(ch)
		    else
			Memc[op] = ch
		    op = op + 1
		}
	    }

	    Memc[op] = '\n';  op=op+1
	    Memc[op] = EOS

	    call flush (STDOUT)

	} else {
	    Memc[buf] = EOS
	    if (ucasein_set && IS_UPPER(key))
		key = TO_LOWER(key)
	}

done_
	# When we get here the key character has been set and the string
	# value, if any, is in buf.  If in playback mode with verify
	# enabled, wait for the user to type a key before continuing.
	
	if (playback_set) {
	    call ttseti (STDIN, TT_PASSTHRU, YES)

	    if (key != ':') {
		if (!pbverify_set)
		    call zwmsec (delay)
		call ttyso (STDOUT, tty, YES)
		if (key > ' ')
		    call printf (" [key=%c]")
		else
		    call printf (" [key=\\%o]")
		call pargi (key)
		call ttyso (STDOUT, tty, NO)
		call flush (STDOUT)
	    }

	    if (pbverify_set) {
		# Read directly from user terminal in passthru mode.
		while (getci (STDIN, ch) != EOF)
		    if (ch == ' ') {
			break
		    } else if (ch == 'q' || ch == '\003') {
			call putline (STDOUT, "\r[playback mode terminated]")
			call flush (STDOUT)
			call zwmsec (500)
			call ttseti (STDIN, TT_PLAYBACK, NO)
			break
		    } else if (ch == 'g') {
			call ttseti (STDIN, TT_PBVERIFY, NO)
			break
		    } else {
			call ttyclearln (STDOUT, tty)
			call ttyso (STDOUT, tty, YES)
			call putline (STDOUT,
			    "\r[space=continue,q=quit,g=noverify]")
			call ttyso (STDOUT, tty, NO)
			call flush (STDOUT)
		    }
	    } else
		call zwmsec (delay)

	    call ttseti (STDIN, TT_PASSTHRU, NO)
	    call ttseti (STDIN, TT_PBDELAY, delay)
	}

	if (tty != NULL) {
	    call ttyclearln (STDOUT, tty)
	    call ttycdes (tty)
	}

	if (!rawmode_set)
	    call fseti (STDIN, F_RAW, NO)

	if (key == EOF || key == '\032' || key == '\004') {
	    call strcpy ("EOF\n", keystr, maxch)
	    call sfree (sp)
	    return (EOF)

	} else {
	    op = 1
	    if (key > ' ') {
		keystr[op] = key;  op=op+1
	    } else if (maxch >= 4) {
		keystr[op] = '\\';  op=op+1
		keystr[op] = '0';  op=op+1
		keystr[op] = key / 8 + '0';  op=op+1
		keystr[op] = mod(key,8) + '0';  op=op+1
	    }

	    if (Memc[buf] != EOS && maxch > 1) {
		keystr[op] = ' ';  op=op+1
		for (ip=buf;  op < maxch && Memc[ip] != EOS;  ip=ip+1) {
		    keystr[op] = Memc[ip]
		    op = op + 1
		}
	    }

	    # The return string value must be newline delimited.
	    keystr[op] = '\n';  op=op+1
	    keystr[op] = EOS

	    call sfree (sp)
	    return (op - 1)
	}
end
