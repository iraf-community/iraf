# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<ctype.h>
include	"environ.h"

define	MAXLEV	8			# max nesting of includes
define	SZ_LBUF	(SZ_COMMAND+SZ_LINE)	# max length SET on a single line


# ENVSCAN -- Parse one or more SET or RESET declarations and enter them into
# the environment list.
#
# Syntax:	(set|reset) name = value	enter a definition
#		set @filename			scan a file
#
# Comments, blank lines, and lines containing unrecognized statements are
# ignored without warning.

int procedure envscan (cmd)

char	cmd[ARB]		# command text to begin scan

char	ch
int	fd, in, nset, lev, sv_fd[MAXLEV]
pointer	sp, ip, op, op_top, lbuf, name, value
int	open(), stropen(), getlline(), strmatch(), nowhite()
errchk	open, stropen, getlline, syserrs
string	s_reset "^#reset#"
string	s_set "^#set#"
define	again_ 91

begin
	call smark (sp)
	call salloc (lbuf, SZ_LBUF, TY_CHAR)
	call salloc (name, MAX_SZKEY, TY_CHAR)
	call salloc (value, MAX_SZVALUE, TY_CHAR)

	# Position to after the set or reset.
	in = strmatch (cmd, s_set)
	if (in == 0) {
	    in = strmatch (cmd, s_reset)
	    if (in == 0) {
		call sfree (sp)
		return (0)
	    }
	}

	# Open the input to be scanned.
	if (cmd[in] == '@')
	    fd = open (cmd[in+1], READ_ONLY, TEXT_FILE)
	else
	    fd = stropen (cmd, ARB, READ_ONLY)

	# Process all SET or RESET statements in the file.  Ignore all other
	# statements.

	nset = 0
	lev  = 0

	repeat {
	    # Get the next SET statement into lbuf, leave IN at index of first
	    # char of the name field.

	    if (getlline (fd, Memc[lbuf], SZ_LBUF) == EOF) {
		if (lev > 0) {
		    call close (fd)
		    fd = sv_fd[lev]
		    lev = lev - 1
		    next
		} else
		    break
	    } else if (Memc[lbuf] == '\n' || Memc[lbuf] == '#') {
		next
	    } else {
		in = strmatch (Memc[lbuf], s_set)
		if (in == 0)
		    in = strmatch (Memc[lbuf], s_reset)

		if (in <= 0)
		    next
		else if (Memc[lbuf+in-1] == '@') {
		    ch = nowhite (Memc[lbuf+in], Memc[lbuf], SZ_LINE)
		    lev = lev + 1
		    if (lev > MAXLEV)
			call syserrs (SYS_FOPEN, Memc[lbuf])
		    sv_fd[lev] = fd
		    fd = open (Memc[lbuf], READ_ONLY, TEXT_FILE)
		    next
		}
	    }

	    # Parse the name and value strings and enter into the environment
	    # list.  Ignore optional quotes and whitespace.  Ignore rest of
	    # line following the value field.

	    op = name
	    op_top = name + MAX_SZKEY
	    ip = lbuf + in - 1
	    for (ch=Memc[ip];  ch != '=' && ch != EOS;  ch=Memc[ip]) {
		if (!IS_QUOTE(ch) && !IS_WHITE(ch)) {
		    Memc[op] = Memc[ip]
		    op = min (op_top, op + 1)
		}
		ip = ip + 1
	    }
	    Memc[op] = EOS

	    op = value
	    if (Memc[ip] == '=') {
		ip = ip + 1
		while (IS_WHITE(Memc[ip]) || IS_QUOTE(Memc[ip]))
		    ip = ip + 1
		op_top = value + MAX_SZVALUE

		for (ch=Memc[ip];  ch != EOS;  ch=Memc[ip]) {
		    if (IS_QUOTE(ch) || ch == '\n') {
			break

		    } else if (ch == '\\' && Memc[ip+1] == '\n') {
again_			if (getlline (fd, Memc[lbuf], SZ_LBUF) == EOF)
			    break

			# Skip leading whitespace on the continuation line.
			for (ip=lbuf;  IS_WHITE(Memc[ip]);  ip=ip+1)
			    ;
			# Check for a commented out continuation line.
			if (Memc[ip] == '#')
			    goto again_

		    } else {
			Memc[op] = ch
			op = min (op_top, op + 1)
			ip = ip + 1
		    }
		}
	    }
	    Memc[op] = EOS

	    # Enter the SET definition into the environment list.
	    call envreset (Memc[name], Memc[value])
	    nset = nset + 1
	}

	call close (fd)
	call sfree (sp)

	return (nset)
end
