# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	"stdgraph.h"

define	SZ_PROGRAM		256
define	MAXARGSTR		15


# SHOWCAP - Show the ascii control string sent to a device to implement a
# control function.  Useful for testing graphcap entries.

procedure t_showcap()

char	cap[2]
int	g_reg[NREGISTERS]
char	g_mem[SZ_MEMORY]
char	argstr[MAXARGSTR]
int	arg1, arg2, arg3, op, len_prog, status, nchars
pointer	tty, sp, prog, ip, cmd
pointer	ttygdes(), ttycaps()
int	stg_encode(), ctoi(), getline(), strncmp()
int	ttygets(), ctowrd(), strlen()
bool	ttygetb(), streq()
define	getargs_ 91

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (prog, SZ_PROGRAM, TY_CHAR)

	# Print instructions.
	call printf ("cmd :  `set' device\n")
	call printf ("    |  `*' (to dump full graphcap entry\n")
	call printf ("    |  cc [arg1 [arg2 [arg3]]]\n")
	call printf ("    ;\n")
	call printf ("\n")
	call printf ("cc  :   a two chararacter capcode (e.g., 'cm')\n")
	call printf ("    |   an encoder program (non alpha first char)\n")
	call printf ("    ;\n")
	call printf ("\n")

	# Interpret and translate control commands until EOF or "bye"
	# is typed.

	tty = NULL

	repeat {
	    # Prompt for input.
	    call printf ("* ")
	    call flush (STDOUT)

	    if (getline (STDIN, Memc[cmd]) == EOF) {
		call printf ("\n")
		break
	    }

	    for (ip=cmd;  IS_WHITE (Memc[ip]);  ip=ip+1)
		;

	    if (Memc[ip] == '\n') {
		next
	    } else if (strncmp (Memc[ip], "set", 3) == 0) {
		ip = ip + 3
		len_prog = ctowrd (Memc, ip, Memc[prog], SZ_PROGRAM)
		if (tty != NULL)
		    call ttycdes (tty)
		tty = ttygdes (Memc[prog])
		call sgc_dump (STDOUT, Memc[ttycaps(tty)],
		    strlen (Memc[ttycaps(tty)]))
		next
	    } else if (Memc[ip] == '*') {
		call sgc_dump (STDOUT, Memc[ttycaps(tty)],
		    strlen (Memc[ttycaps(tty)]))
		next
	    } else if (!IS_ALPHA (Memc[ip])) {
		len_prog = ctowrd (Memc, ip, Memc[prog], SZ_PROGRAM)
		cap[1] = EOS
		goto getargs_
	    } else if (strncmp (Memc[ip], "bye", 3) == 0)
		break

	    # Parse command with optional arguments, e.g., "RC 1".
	    # Extract 2 character capability name (required).

	    op = 1
	    while (IS_ALNUM(Memc[ip])) {
		cap[op] = Memc[ip]
		ip = ip + 1
		op = min (2, op + 1)
	    }
	    cap[3] = EOS
getargs_
	    # Argument type depends on whether encoding or decoding.
	    if (streq ("SC", cap)) {
		nchars = ctowrd (Memc, ip, argstr, MAXARGSTR)
		if (nchars == 0) {
		    call printf ("SC must have 1 contiguous string argument\n")
		    next
		}

	    } else {
		# Extract up to three arguments (optional).
		if (ctoi (Memc, ip, arg1) <= 0)
		    arg1 = 0
		if (ctoi (Memc, ip, arg2) <= 0)
		    arg2 = 0
		if (ctoi (Memc, ip, arg3) <= 0)
		    arg3 = 0
	    }

	    # Fetch the program from the graphcap file.  Zero is returned if
	    # the device does not have the named capability, in which case
	    # the function is inapplicable and should be ignored.

	    if (cap[1] != EOS)
		if (tty == NULL) {
		    call printf ("use `set' to specify device name\n")
		    next
		} else
		    len_prog = ttygets (tty, cap, Memc[prog], SZ_PROGRAM)

	    if (len_prog > 0) {
		if (Memc[prog] == '#')
		    call sgc_dump (STDOUT, Memc[prog+1], len_prog - 1)
		else {
		    # Dump the program on the standard output.
		    if (cap[1] != EOS) {
			call printf ("program:  ")
			call sgc_dump (STDOUT, Memc[prog], len_prog)
		    }

		    # Set memory or registers depending on whether encoding or
		    # decoding.
		    if (streq ("SC", cap))
			call strcpy (argstr, g_mem, nchars)

		    else {
			g_reg[1] = arg1
			g_reg[2] = arg2
			g_reg[3] = arg3
		    }
		    g_reg[E_IOP] = 1
		    g_reg[E_TOP] = SZ_MEMORY

		    # If scan_cursor, decode the input string and write the
		    # registers to the output file.  Else, encode the output
		    # string and write the encoded string to the output file.

		    status = stg_encode (Memc[prog], g_mem, g_reg) 
		    if (status == OK) {
			nchars = g_reg[E_IOP] - 1

			if (streq ("SC", cap)) {
			    call printf ("X(R1)=%d, Y(R2)=%d, key=%c\n")
				call pargi (g_reg[1])
				call pargi (g_reg[2])
				call pargi (g_reg[3])
			} else {
			    call printf ("encoding: ")
			    call sgc_dump (STDOUT, g_mem, nchars)
			}

		    } else
			call printf ("error encoding control string\n")
			call printf ("      status = %d\n")
			    call pargi (status)
		}

	    } else if (len_prog == 0)
		if (ttygetb (tty, cap))
		    call printf ("boolean capability is true\n")

	    else {
		call printf ("device capability `%s' not found\n")
		    call pargstr (cap)
	    }
	}

	if (tty != NULL)
	    call ttycdes (tty)
	call sfree (sp)
end


# SGC_DUMP -- Dump a sequence of ascii characters in printable form.

procedure sgc_dump (fd, data, nchars)

int	fd			# output file
char	data[ARB]		# chars to be dumped
int	nchars

int	ip
int	col

begin
	col = 1
	for (ip=1;  ip <= nchars;  ip=ip+1) {
	    call putcc (fd, data[ip])
	    if (data[ip] == ':' && col > 60) {
		call putci (fd, '\\')
		call putci (fd, '\n')
		col = 1
	    } else
		col = col + 1
	}

	call putci (fd, '\n')
end
