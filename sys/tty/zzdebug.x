# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<ttyset.h>
include	"tty.h"

# Debug TTY package.

task	find=t_find, cap=t_cap, init=t_init

define	SZ_CAPSTR	30
define	FAKE_PADCHAR	177B


# FIND -- Find an entry in the termcap database, and print out the caplist
# string.

procedure t_find()

char	ttyname[SZ_FNAME]
pointer	tty, ttygdes()

begin
	call clgstr ("ttyname", ttyname, SZ_FNAME)
	tty = ttygdes (ttyname)

	call printf ("Termcap entry for '%s', %d capabilities\n")
	    call pargstr (ttyname)
	    call pargi (T_NCAPS(tty))
	call printf ("    pc=%oB, bsok=%b, htok=%b, %d by %d, %d baud\n")
	    call pargi (T_PADCHAR(tty))
	    call pargi (T_BSOK(tty))
	    call pargi (T_HTOK(tty))
	    call pargi (T_NCOLS(tty))
	    call pargi (T_NLINES(tty))
	    call pargi (T_BAUD(tty))

	call putline (STDOUT, T_CAPLIST(tty))
	call putci (STDOUT, '\n')
	call ttycdes (tty)
end


# CAP -- Open descriptor, then sit in a get/put capability loop.  Capabilities
# are put to the stdout in a readable form.

procedure t_cap()

char	ttyname[SZ_FNAME], capstr[SZ_CAPSTR]
char	ctrlstr[SZ_CTRLSTR], tempstr[SZ_CTRLSTR], obuf[SZ_LINE]
int	fd, args[MAX_COORDS], nargs, nchars

int	stropen(), ttygets(), ttysubi(), clglstr(), strlen(), nscan()
pointer	tty, ttygdes()

begin
	call clgstr ("ttyname", ttyname, SZ_FNAME)
	tty = ttygdes (ttyname)
	call ttyseti (tty, TTY_PADCHAR, FAKE_PADCHAR)

	call printf ("Termcap entry for '%s', %d capabilities\n")
	    call pargstr (ttyname)
	    call pargi (T_NCAPS(tty))
	call printf ("    pc=%oB, bsok=%b, htok=%b, %d by %d, %d baud\n")
	    call pargi (T_PADCHAR(tty))
	    call pargi (T_BSOK(tty))
	    call pargi (T_HTOK(tty))
	    call pargi (T_NCOLS(tty))
	    call pargi (T_NLINES(tty))
	    call pargi (T_BAUD(tty))

	while (clglstr ("cap", capstr, SZ_CAPSTR) != EOF)
	    if (ttygets (tty, capstr, ctrlstr, SZ_CTRLSTR) == 0) {
		call printf ("capability '%s' not found\n")
		    call pargstr (capstr)
	    } else {
		fd = stropen (obuf, SZ_LINE, NEW_FILE)

		# Expand args? (as in "cap: cm 11 4")
		if (strlen (capstr) > 2) {
		    call sscan (capstr[3])
		    for (nargs=0;  nscan() == nargs;  nargs=nargs+1)
			call gargi (args[nargs+1])
		    nargs = nargs - 1
		    call strcpy (ctrlstr, tempstr, SZ_CTRLSTR)
		    nchars = ttysubi (tempstr, ctrlstr,SZ_CTRLSTR, args,nargs)
		} else
		    nargs = 0

		call ttyputs (fd, tty, ctrlstr, 1)
		if (nargs > 0 && args[1] != 0 || args[2] != 0) {
		    call fprintf (fd, " residual x=%d, y=%d")
			call pargi (args[1])
			call pargi (args[2])
		}
		call close (fd)
		call dump_chars (STDOUT, obuf)
	    }
	call putci (STDOUT, '\n')
		
	call ttycdes (tty)
end


# INIT -- Output initialization string in human readable form on the standard
# output.

define	SZ_OBUF		1024


procedure t_init()

char	ttyname[SZ_FNAME]
char	obuf[SZ_OBUF]
int	fd
int	stropen()
pointer	tty, ttygdes()

begin
	call clgstr ("ttyname", ttyname, SZ_FNAME)
	tty = ttygdes (ttyname)
	call ttyseti (tty, TTY_PADCHAR, FAKE_PADCHAR)

	# Print header identifying basic terminal capabilities.
	call printf ("Termcap entry for '%s', %d capabilities\n")
	    call pargstr (ttyname)
	    call pargi (T_NCAPS(tty))
	call printf ("    pc=%oB, bsok=%b, htok=%b, %d by %d, %d baud\n")
	    call pargi (T_PADCHAR(tty))
	    call pargi (T_BSOK(tty))
	    call pargi (T_HTOK(tty))
	    call pargi (T_NCOLS(tty))
	    call pargi (T_NLINES(tty))
	    call pargi (T_BAUD(tty))

	# Dump initialization string into buffer, print buffer in readable
	# form on STDOUT.

	fd = stropen (obuf, SZ_OBUF, NEW_FILE)
	call ttyinit (fd, tty)
	call close (fd)
	call dump_chars (STDOUT, obuf)
	call putci (STDOUT, '\n')
		
	call ttycdes (tty)
end


# DUMP_CHARS -- Print out a sequence of normal and control chars in a nice
# readable form.

procedure dump_chars (fd, str)

int	fd
char	str[ARB]
char	ch
int	ip, iptop
int	stridx()
errchk	putci, putline

begin
	for (ip=1;  str[ip] != EOS;  ) {
	    if (ip > 1)
		call putci (fd, '\n')
	    call putline (fd, "    ")
	    for (iptop=ip+50;  ip < iptop && str[ip] != EOS;  ip=ip+1) {
		ch = str[ip]
		if (IS_CNTRL(ch)) {
		    if (stridx (ch, "\b\f\t\r\n") > 0)
			call putcc (fd, ch)
		    else {
			call putci (fd, '^')
			call putci (fd, ch + 'A' - 1)
		    }
		} else if (ch == FAKE_PADCHAR) {
		    call putci (fd, '.')
		} else
		    call putc  (fd, ch)
	    }
	}

	if (ip < iptop)
	    call putci (fd, '\n')
end
