# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>
include	<chars.h>

# Debug the ENVIRON environment list package.  The following definitions are
# from the header of "environ.x" and are used by envdebug to examine the
# environment list data structures; these should be compared to the defs in
# environ.x to make sure they agree.  Use of a header file is not warranted
# since we really do not want the environ.x data structures known outside
# the package.

task	get	= t_get,
	put	= t_put,
	list	= t_list,
	mark	= t_mark,
	free	= t_free,
	debug	= t_debug,
	spawn	= t_spawn,
	edit	= t_edit,
	tty	= t_tty,
	urlget	= t_urlget


# Strings may optionally be quoted in SET stmts with either ' or ".
define	IS_QUOTE	($1 == '\'' || $1 == '"')

# Size limiting definitions.

define	NTHREADS	100		# number of hash threads
define	HASH_FACTOR	1637		# divisor for hash function
define	NHASHCHARS	6		# no. chars used for hashing
define	LEN_ENVBUF	1500		# storage for environment list
define	INC_ENVBUF	500		# increment if overflow occurs
define	MAX_SZKEY	32		# max chars in a key
define	MAX_SZVALUE	80		# max chars in value string
define	MAX_LENLISTELEM	(3+(MAX_SZKEY+1+MAX_SZVALUE+1+SZ_SHORT-1)/SZ_SHORT)
define 	SZ_BUF		8192			# http response buffer

# List element structure, stored in ENVBUF, which is allocated as an array of
# type SHORT integer.  Each list element is aligned on a short integer boundary
# within the array.  E_NEXT points to the next element in a thread, whereas
# E_LASTELEM points to the last element in the envbuf (which is a stack).

define	E_NEXT		Mems[$1]	# next element in thread (list)
define	E_LASTELEM	Mems[$1+1]	# next element in envbuf
define	E_REDEF		Mems[$1+2]	# set if element is redefined
define	E_SETP		P2C($1+3)	# char pointer to name field
define	E_SET		Memc[E_SETP($1)]	# "name=value" string
define	E_SETOFFSET	3


# GET -- Lookup the definition of an environment variable.

procedure t_get()

char	name[SZ_FNAME]
char	value[SZ_LINE]
int	envgets()

begin
	call clgstr ("name", name, SZ_FNAME)
	if (envgets (name, value, SZ_LINE) <= 0) {
	    call printf ("%s not found\n")
		call pargstr (name)
	} else {
	    call printf ("%s = %s\n")
		call pargstr (name)
		call pargstr (value)
	}
end


# PUT -- Enter a new environment variable or list of variables into the
# environment list.  Enter "stmt: set name=value" to enter a single variable,
# or "stmt: set @filename" to process set statements from a file.

procedure t_put()

char	stmt[SZ_LINE]
int	envscan()

begin
	call clgstr ("statement", stmt, SZ_LINE)
	call printf ("%d set statements processed\n")
	    call pargi (envscan (stmt))
end


# LIST -- Print the environment list.

procedure t_list()

bool	clgetb()
int	btoi()

begin
	call envlist (STDOUT, "    ", btoi (clgetb ("show_redefs")))
end


# MARK -- Mark the end of the environment list for later restoration by
# the FREE task.

procedure t_mark()

int	top
common	/xxx/ top

begin
	call envmark (top)
	call printf ("top = %d\n")
	    call pargi (top)
end


# FREE -- Free the environment list back to the last position marked.

procedure t_free()

int	top
int	envfree()
common	/xxx/ top

begin
	call printf ("free uncovers %d redefs\n")
	    call pargi (envfree (top, 0))
end


# DEBUG -- Print the internal data structures (the hash table) of the
# environment list package.

procedure t_debug()

begin
	call envdebug (STDOUT)
end


# ENVDEBUG -- Print the contents of the environment list data structures for
# debugging the code.

procedure envdebug (fd)

int	fd			# output file
int	i, t, head
pointer	el, ep
include	"environ.com"

begin
	call fprintf (fd, "envbuf at %d, len %d, last=%d, top=%d, %d%% full\n")
	    call pargi (envbuf)
	    call pargi (len_envbuf)
	    call pargi (last)
	    call pargi (top)
	    call pargr (real(top) / real(len_envbuf) * 100.0)

	for (t=1;  t <= NTHREADS;  t=t+1) {
	    call fprintf (fd, "%6d"); call pargi (t)
	    head = threads[t]
	    if (head != NULL)
		for (i=head;  i != NULL;  i=E_NEXT(el)) {
		    el = envbuf + i
		    call putci (fd, ' ')
		    for (ep=E_SETP(el);  Memc[ep] != '=';  ep=ep+1)
			call putc (fd, Memc[ep])
		}
	    call putci (fd, '\n')
	}
end


# SPAWN -- Spawn a connected subprocess.  Used to test process control and
# interprocess communication.

procedure t_spawn()

char	process[SZ_FNAME]
char	lbuf[SZ_LINE]
int	in, out, pid
int	prgetline(), propen(), prclose(), strmatch()
define	done_ 91

begin
	call clgstr ("process", process, SZ_FNAME)
	pid = propen (process, in, out)

	call putline (STDERR, "-> ")
	call flush (STDERR)

	while (prgetline (STDIN, lbuf) != EOF) {
	    if (strmatch (lbuf, "^bye") > 0)
		break
	    else {
		call putline (out, lbuf)
		call flush (out)
	    }

	    while (prgetline (in, lbuf) != EOF) {
		call putline (STDERR, lbuf)

		if (strmatch (lbuf, "^bye") > 0)
		    break
		else {
		    call putline (STDERR, ">> ")
		    call flush (STDERR)
		    if (prgetline (STDIN, lbuf) == EOF)
			goto done_
		    call putline (out, lbuf)
		}

		call flush (STDERR)
		call flush (out)
	    }

	    call putline (STDERR, "------------\n")
	    call putline (STDERR, "-> ")
	    call flush (STDERR)
	}

done_
	call putline (STDERR, "\n")
	call eprintf ("termination code %d\n")
	    call pargi (prclose (pid))
end


# EDIT -- Test raw mode to a terminal.

procedure t_edit()

char	lbuf[SZ_LINE], temp[SZ_LINE], ch
int	i, stdline

char	getchar()
int	envgets(), ttygeti()
pointer	tty, ttyodes()
define	accum_ 91
define	done_ 92

begin
	# Set terminal to raw mode.
	call fseti (STDIN, F_RAW, YES)

	# Open termcap for terminal.
	if (envgets ("terminal", lbuf, SZ_LINE) <= 0)
	    call strcpy ("vt100", lbuf, SZ_LINE)
	tty = ttyodes (lbuf)
	stdline = ttygeti (tty, "li")

	# Edit loop.  The variable I is the character position within the
	# line.  Start out in insert mode, with line displayed at bottom
	# of terminal screen.

	lbuf[1] = EOS
	i = 1
	call ttygoto (STDOUT, tty, 1, stdline)
	call flush (STDOUT)
	goto accum_

	while (getchar (ch) != EOF) {
	    switch (ch) {

	    case 'h':
		# Move left one column.
		if (i <= 1)
		    call putci (STDOUT, BEL)
		else {
		    call putci (STDOUT, BS)
		    i = i - 1
		}

	    case 'l':
		# Move right one column.
		if (lbuf[i+1] == EOS)
		    call putci (STDOUT, BEL)
		else {
		    call putc  (STDOUT, lbuf[i])
		    i = i + 1
		}

	    case 'x':
		# Delete a character.
		call strcpy (lbuf[i+1], lbuf[i], SZ_LINE-i+1)
		call putline (STDOUT, lbuf[i])
		call putci (STDOUT, BLANK)
		call ttygoto (STDOUT, tty, i, STDLINE)

		if (i > 1 && lbuf[i] == EOS) {
		    call putci (STDOUT, BS)
		    i = i - 1
		}

	    case 'i':
		# Insert a character.
accum_
		while (getchar (ch) != ESC) {
		    call putc (STDOUT, ch)
		    if (ch == '\r')
			goto done_

		    # Insert char in line buffer.
		    call strcpy (lbuf[i], temp, SZ_LINE)
		    lbuf[i] = ch
		    i = i + 1
		    call strcpy (temp, lbuf[i], SZ_LINE-i+1)

		    # Redraw right portion of line.
		    call putline (STDOUT, lbuf[i])
		    call ttygoto (STDOUT, tty, i, STDLINE)
		    call flush (STDOUT)
		}

		if (i > 1) {
		    call putci (STDOUT, BS)
		    i = i - 1
		}

	    case '\f':
		# Redraw line.
		call printf ("\r%s")
		    call pargstr (lbuf)
		call ttygoto (STDOUT, tty, i, STDLINE)

	    case '\r':
		break

	    default:
		call putci (STDOUT, BEL)
	    }

	    call flush (STDOUT)
	}

done_
	call fseti (STDIN, F_RAW, NO)
	call putci (STDOUT, '\n')
	call ttycdes (tty)
end


# TTY -- Test direct terminal i/o.

procedure t_tty()

int	in, out, ch
int	ttopen(), getci()
bool	clgetb()

begin
	if (clgetb ("dualstreams")) {
	    in  = ttopen ("dev$tty", READ_ONLY)
	    out = ttopen ("dev$tty", WRITE_ONLY)
	} else {
	    in  = ttopen ("dev$tty", READ_WRITE)	# NOT SUPPORTED
	    out = in
	}

	call fseti (in, F_RAW, YES)
	while (getci (in, ch) > 0) {
	    call fprintf (out, "%c\r\n")
		call pargi (ch)
	    call flush (out)
	    if (ch == EOFCHAR)
		break
	}

	if (in == out)
	    call close (in)
	else {
	    call close (out)
	    call close (in)
	}
end


# URL_GET -- Do an HTTP GET of a URL

procedure t_urlget ()

pointer	reply
char	url[SZ_LINE], fname[SZ_FNAME]
bool	hdr
int	nread

int	url_get()

begin
	call clgstr ("url", url, SZ_LINE) 		# get the parameters
	call clgstr ("fname", fname, SZ_FNAME)
	hdr = clgetb ("hdr")

	call calloc (reply, SZ_BUF, TY_CHAR)

	nread = url_get (url, fname, reply)

	call eprintf ("File '%s', downloaded %d bytes.\n")
	    call pargstr (fname)
	    call pargi (nread)

	if (hdr)
	    call eprintf (Memc[reply])
	call mfree (reply, TY_CHAR)
end
