# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<syserr.h>
include	<ctype.h>
include	<ttyset.h>
include	<ttset.h>

.help sttyco
.nf ---------------------------------------------------------------------------
STTYCO -- Set/stat VOS terminal driver options via command string.  This is a
high level front-end to the ttset/ttstat procedures, used to set or query the
individual terminal driver parameters.  Since STTYCO is driven by a command
string it may be called either as a task or as a subroutine.  When called as
a task, e.g, as the STTY task in the CL, the argument list is simply
concatenated into a long string and passed to STTYCO for processing.

The argument list consists of zero or more argument strings, as follows:

	reset			Reset default terminal settings
	init			Send initialization sequence to the terminal
	show			Show terminal settings
	all			Show all parameters, even if not in use
	<nullarglist>		Show terminal settings
	<unknown>	 	Assumed to be the termcap name of a terminal.
				  Set envvars `terminal', `ttyncols', and
				  `ttynlines' for the named terminal.
	baud=N			Set envvar `ttybaud=N'.
	ncols=N			Set envvar `ttyncols=N'.
	nlines=N		Set envvar `ttynlines=N'.
	resize			Reset the screen size parameters.

	clear			Disable named driver functions.

	ucasein 		Map input to lower case.
	ucaseout 		Map output to upper case.

	logio [=logiofile] 	Log all i/o to the terminal in a file.
	login [=loginfile] 	Log input from terminal in a file.
	logout [=logoutfile] 	Log output to terminal in a file.

	playback [=pbfile]	Read terminal input from a logfile.
	verify			Pause at newline when in playback mode.
	delay=N			Msec delay in playback mode, verify disabled.

Simply naming a parameter like ucasein, logio, etc., causes that function to
be enabled, or disabled if preceeded by the keyword `clear'.  Any of the
sequences +, -, =yes, =no may also be appended to turn the function on or off.
The logging functions may also take a filename argument, e.g., logio=file
enables i/o logging into the named file.  The default filenames are as follows:

	logio		home$ttyio.log
	login		home$ttin.log
	logout		home$ttout.log
	playback	home$ttin.log

If verify is disabled a delay will precede each record returned from the
input file; the default delay is quite short.  Pause mode is terminated by
typing a space or carriage return to continue execution, or `q' to terminate
playback mode.
.endhelp ----------------------------------------------------------------------

define	STTY_KEYWORDS "|reset|init|all|show|baud|ncols|nlines|resize|clear|\
	|ucasein|ucaseout|logio|login|logout|playback|verify|delay|"

define	RESET		1	# reset default terminal settings
define	INIT		2	# send initialization sequence to the terminal
define	ALL		3	# show all parameters
define	SHOW		4	# show parameters
define	BAUD		5	# set envvar `ttybaud'
define	NCOLS		6	# set envvar `ttyncols'
define	NLINES		7	# set envvar `ttynlines'
define	RESIZE		8	# reset the screen size parameters
define	CLEAR		9	# set default action to NO rather than YES
#	newline		10
define	UCASEIN		11	# map input to lower case
define	UCASEOUT	12	# map output to upper case
define	LOGIO		13	# log all i/o in a file
define	LOGIN		14	# log input in a file
define	LOGOUT		15	# log output in a file
define	PLAYBACK	16	# take input from a file
define	VERIFY		17	# wait for user to type a key after each record
define	DELAY		18	# msec delay after each record in playback mode


# STTYCO -- Main entry point.  Input consists of an argument list of arbitrary
# length.  Output is to the given file descriptor.

procedure sttyco (args, ttin, ttout, outfd)

char	args[ARB]		# argument list
int	ttin, ttout		# tty file descriptors
int	outfd			# write task output here

pointer	sp, keyw, value, tty
int	startcol, ival, nargs, yesno, defact, show, all, ip
int	stty_getarg(), strdic(), ctoi()
pointer	ttyodes()

string	keywords STTY_KEYWORDS
errchk	ttseti, ttsets, stty_ttyinit
define	argerr_ 91

begin
	call smark (sp)
	call salloc (keyw, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_FNAME, TY_CHAR)

	defact = YES
	show = NO
	all = NO
	ip = 1

	# Process successive keyword=value arguments.

	for (nargs=0;  stty_getarg (args, ip, Memc[keyw], SZ_FNAME, Memc[value],
	    SZ_FNAME, defact, yesno) != EOF;  nargs = nargs + 1) {

	    switch (strdic (Memc[keyw], Memc[keyw], SZ_FNAME, keywords)) {
	    case RESET:
		call ttseti (ttin, TT_INITIALIZE, yesno)
	    case INIT:
		call stty_ttyinit (ttin, ttout, "terminal")

	    case ALL:
		all = yesno
		show = YES
	    case SHOW:
		show = yesno
	    case BAUD:
		if (IS_DIGIT (Memc[value]))
		    call stty_envreset ("ttybaud", Memc[value])
		else
		    goto argerr_
	    case NCOLS:
		if (IS_DIGIT (Memc[value]))
		    call stty_envreset ("ttyncols", Memc[value])
		else
		    goto argerr_
	    case NLINES:
		if (IS_DIGIT (Memc[value]))
		    call stty_envreset ("ttynlines", Memc[value])
		else
		    goto argerr_

	    case RESIZE:
		tty = ttyodes ("terminal")
		call stty_setsize (ttin, ttout, tty)
		call ttycdes (tty)

	    case CLEAR:
		defact = NO

	    case UCASEIN:
		call ttseti (ttin, TT_UCASEIN, yesno)
	    case UCASEOUT:
		call ttseti (ttin, TT_UCASEOUT, yesno)

	    case LOGIO:
		if (yesno == YES && Memc[value] != EOS)
		    call ttsets (ttin, TT_IOFILE, Memc[value])
		call ttseti (ttin, TT_LOGIO, yesno)
	    case LOGIN:
		if (yesno == YES && Memc[value] != EOS)
		    call ttsets (ttin, TT_INFILE, Memc[value])
		call ttseti (ttin, TT_LOGIN, yesno)
	    case LOGOUT:
		if (yesno == YES && Memc[value] != EOS)
		    call ttsets (ttin, TT_OUTFILE, Memc[value])
		call ttseti (ttin, TT_LOGOUT, yesno)
	    case PLAYBACK:
		if (yesno == YES && Memc[value] != EOS)
		    call ttsets (ttin, TT_PBFILE, Memc[value])
		call ttseti (ttin, TT_PLAYBACK, yesno)
		    
	    case VERIFY:
		call ttseti (ttin, TT_PBVERIFY, yesno)
	    case DELAY:
		startcol = 1
		if (ctoi (Memc[value], startcol, ival) > 0)
		    call ttseti (ttin, TT_PBDELAY, ival)
		else
		    goto argerr_

	    default:
		# If not keyword, must be a terminal name.
		iferr (call stty_newterm (ttin, ttout, Memc[keyw]))
		    call erract (EA_WARN)
	    }
	}

	# If the argument list was null or the SHOW flag was set, show
	# the current terminal settings.

	if (nargs == 0 || show == YES)
	    call stty_showterm (ttin, ttout, outfd, all)

	call sfree (sp)
	return

argerr_
	call syserrs (SYS_STTYNUMARG, Memc[keyw])
	call sfree (sp)
end


# STTY_NEWTERM -- Configure the environment for a new type of terminal.

procedure stty_newterm (ttin, ttout, terminal)

int	ttin, ttout			# tty file descriptors
char	terminal[ARB]			# termcap name of new terminal

pointer	sp, vp, tty
pointer	ttyodes()

bool	ttygetb()
int	ttygets()
errchk	ttyodes, stty_envreset, ttygsize

begin
	call smark (sp)
	call salloc (vp, SZ_FNAME, TY_CHAR)

	tty = ttyodes (terminal)

	# Set the terminal parameters.
	call stty_envreset ("terminal", terminal)
	call stty_setsize (ttin, ttout, tty)

	# Set the stdgraph device name for this terminal, if given.
	if (ttygetb (tty, "gd")) {
	    if (ttygets (tty, "gd", Memc[vp], SZ_FNAME) <= 0)
		call strcpy (terminal, Memc[vp], SZ_FNAME)
	} else
	    call strcpy ("none", Memc[vp], SZ_FNAME)
	call stty_envreset ("stdgraph", Memc[vp])

	call ttycdes (tty)
	call sfree (sp)
end


# STTY_SETSIZE -- Determine the terminal screen size in characters, and set
# up the environment appropriately.

procedure stty_setsize (ttin, ttout, tty)

int	ttin, ttout			# tty file descriptors
pointer	tty

char	num[4]
int	ncols, nlines, n
int	itoc()

begin
	call ttygsize (ttin, ttout, tty, ncols, nlines)

	n = itoc (ncols, num, 4)
	call stty_envreset ("ttyncols", num)
	call ttyseti (tty, TTY_NCOLS, ncols)

	n = itoc (nlines, num, 4)
	call stty_envreset ("ttynlines", num)
	call ttyseti (tty, TTY_NLINES, nlines)
end


# STTY_TTYINIT -- Output the initialization string and the contents of
# the initialization file to the terminal, if either is specified in the
# termcap entry for the device.

procedure stty_ttyinit (ttin, ttout, terminal)

int	ttin, ttout			# tty file descriptors
char	terminal[ARB]			# termcap name of new terminal

pointer	tty
pointer	ttyodes()

begin
	tty = ttyodes (terminal)
	call ttyinit (ttout, tty)
	call flush (ttout)
	call ttycdes (tty)
end


# STTY_ENVRESET -- Set the value of an environment variable in the current
# process and in all connected subprocesses.

procedure stty_envreset (envvar, value)

char	envvar[ARB]		# environment variable to be set
char	value[ARB]		# new value

errchk	envreset

begin
	call envreset (envvar, value)
	call prenvset (0, envvar, value)
end


# STTY_SHOWTERM -- Show the current terminal driver status.

procedure stty_showterm (ttin, ttout, fd, all)

int	ttin, ttout		# tty file descriptors
int	fd			# where the output goes
int	all			# show all params, even if not in use

int	junk
pointer	sp, val
bool	ucasein, ucaseout, shift, logio, login, logout, playback, showall
int	ttstati(), ttstats(), envfind()

string	unknown "[unknown]"
string	on "on"
string	off "off"

begin
	call smark (sp)
	call salloc (val, SZ_FNAME, TY_CHAR)

	ucasein  = (ttstati (ttin, TT_UCASEIN) == YES)
	ucaseout = (ttstati (ttin, TT_UCASEOUT) == YES)
	shift	 = (ttstati (ttin, TT_SHIFTLOCK) == YES)
	logio	 = (ttstati (ttin, TT_LOGIO) == YES)
	login	 = (ttstati (ttin, TT_LOGIN) == YES)
	logout	 = (ttstati (ttin, TT_LOGOUT) == YES)
	playback = (ttstati (ttin, TT_PLAYBACK) == YES)
	showall  = (all == YES)

	# Show tty environment variables.

	if (envfind ("terminal", Memc[val], SZ_FNAME) <= 0)
	    call strcpy ("?", Memc[val], SZ_FNAME)
	call fprintf (fd, "%s ")
	    call pargstr (Memc[val])

	if (envfind ("ttyncols", Memc[val], SZ_FNAME) <= 0)
	    call strcpy ("?", Memc[val], SZ_FNAME)
	call fprintf (fd, "ncols=%s ")
	    call pargstr (Memc[val])

	if (envfind ("ttynlines", Memc[val], SZ_FNAME) <= 0)
	    call strcpy ("?", Memc[val], SZ_FNAME)
	call fprintf (fd, "nlines=%s ")
	    call pargstr (Memc[val])

	if (showall || ucasein || ucaseout) {
	    if (envfind ("ttybaud", Memc[val], SZ_FNAME) <= 0)
		call strcpy ("?", Memc[val], SZ_FNAME)
	    call fprintf (fd, "baudrate=%s ")
		call pargstr (Memc[val])

	    call fprintf (fd, "ucasein=%b ")
		call pargb (ucasein)
	    call fprintf (fd, "ucaseout=%b ")
		call pargb (ucaseout)
	    call fprintf (fd, "shift=%b\n")
		call pargb (shift)
	} else
	    call fprintf (fd, "\n")

	# Show internal driver state variables in `show all' mode.

	if (showall) {
	    call fprintf (fd,
		"kichan=%d kochan=%d lichan=%d lochan=%d pbchan=%d ")
		call pargi (ttstati (ttin, TT_KINCHAN))
		call pargi (ttstati (ttin, TT_KOUTCHAN))
		call pargi (ttstati (ttin, TT_LOGINCHAN))
		call pargi (ttstati (ttin, TT_LOGOUTCHAN))
		call pargi (ttstati (ttin, TT_PBINCHAN))

	    call fprintf (fd, "raw=%b passthru=%b\n")
		call pargb (ttstati (ttin, TT_RAWMODE) == YES)
		call pargb (ttstati (ttin, TT_PASSTHRU) == YES)
	}

	# Show the status of the logging options.

	if (logio || showall) {
	    junk = ttstats (ttin, TT_IOFILE, Memc[val], SZ_FNAME)
	    call fprintf (fd, "logio=%s [%s]\n")
		call pargstr (Memc[val])
		if (logio)
		    call pargstr (on)
		else
		    call pargstr (off)
	}
	
	if (!logio || showall) {
	    if (login || showall) {
		junk = ttstats (ttin, TT_INFILE, Memc[val], SZ_FNAME)
		call fprintf (fd, "login=%s [%s]\n")
		    call pargstr (Memc[val])
		    if (login)
			call pargstr (on)
		    else
			call pargstr (off)
	    }
	    if (logout || showall) {
		junk = ttstats (ttin, TT_OUTFILE, Memc[val], SZ_FNAME)
		call fprintf (fd, "logout=%s [%s]\n")
		    call pargstr (Memc[val])
		    if (logout)
			call pargstr (on)
		    else
			call pargstr (off)
	    }
	}

	if (playback || showall) {
	    junk = ttstats (ttin, TT_PBFILE, Memc[val], SZ_FNAME)
	    call fprintf (fd, "playback=%s [%s] ")
		call pargstr (Memc[val])
		if (playback)
		    call pargstr (on)
		else
		    call pargstr (off)

	    call fprintf (fd, "verify=%b ")
		call pargb (ttstati (ttin, TT_PBVERIFY) == YES)
	    call fprintf (fd, "delay=%d (msec)")
		call pargi (ttstati (ttin, TT_PBDELAY))
	    call fprintf (fd, "\n")
	}

	if (playback) {
	    call fprintf (fd, "script recorded with terminal=%s, stdgraph=%s\n")
		if (ttstats (ttin, TT_TDEVICE, Memc[val], SZ_FNAME) <= 0)
		    call pargstr (unknown)
		else
		    call pargstr (Memc[val])
		if (ttstats (ttin, TT_GDEVICE, Memc[val], SZ_FNAME) <= 0)
		    call pargstr (unknown)
		else
		    call pargstr (Memc[val])
	}

	call sfree (sp)
end


# STTY_GETARG -- Get the next argument from an argument list.  Arguments are of
# the form keyw, keyw+, keyw-, keyw=(yes|y), keyw=(no|n), or keyw=value.
# All forms are reduced to a flag YESNO and a value string VALUE.  The forms
# keyw=yes, key=no, etc., cause YESNO to be set but not VALUE.  If only the
# keyword name is given YESNO is set to YES and VALUE to the null string.
# The number of characters in the keyword=value argument string is returned
# as the function value, or EOF when the argument list is exhausted.

int procedure stty_getarg (args, ip, keyw, maxkc, value, maxvc, defact, yesno)

char	args[ARB]		# argument string
int	ip			# index into argument string [RW]
char	keyw[ARB]		# receives keyword name
int	maxkc			# max chars in keyw string	
char	value[ARB]		# receives value string or EOS
int	maxvc			# max chars in value string
int	defact			# default action (yes/no) if only keyword given
int	yesno			# boolean value of parameter

int	op
int	ip_save
bool	streq()

begin
	while (IS_WHITE (args[ip]))
	    ip = ip + 1
	
	ip_save = ip

	# Get keyword name.
	for (op=1;  IS_ALNUM (args[ip]);  ip=ip+1) {
	    keyw[op] = args[ip]
	    op = min (maxkc, op + 1)
	}
	keyw[op] = EOS

	while (IS_WHITE (args[ip]))
	    ip = ip + 1

	value[1] = EOS
	yesno = defact

	if (args[ip] == '=') {
	    # Extract value string.
	    op = 1
	    for (ip=ip+1;  args[ip] > ' ';  ip=ip+1) {
		value[op] = args[ip]
		op = min (maxvc, op + 1)
	    }
	    value[op] = EOS

	    # Check for keyw=[yes|no].
	    if (streq (value, "yes") || streq (value, "y")) {
		yesno = YES
		value[1] = EOS
	    } else if (streq (value, "no") || streq (value, "n")) {
		yesno = NO
		value[1] = EOS
	    }
	} else if (args[ip] == '+') {
	    yesno = YES
	    ip = ip + 1
	} else if (args[ip] == '-') {
	    yesno = NO
	    ip = ip + 1
	}

	if (ip <= ip_save)
	    return (EOF)
	else
	    return (ip - ip_save)
end
