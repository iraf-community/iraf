# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<ctype.h>
include	<fset.h>
include	<pmset.h>
include	<plio.h>

.help pmtest
.nf -------------------------------------------------------------------------
PMTEST -- PMIO package debug and test facility.

    Due to the complexity of the PMIO package, testing is performed using an
interactive interpreter which reads commands from the standard input.
All commands operate internally upon a set of four mask registers A-D,
and a set of ten vector registers V[0-9].

	a,b,c,d		- mask registers
	v0,...,v9	- vector registers

The following commands are defined:

	help					# print command summary
	timer					# toggle timing of commands
	run fname				# read commands from a file
	clear					# clear the screen
	bye					# all done (also EOF)

	create [mask] naxes axlen depth		# create a new mask
	load [mask] fname			# load mask from file
	save [mask] fname [title]		# save mask in file
	loadim [mask] image			# load mask from image
	saveim [mask] image [title]		# save mask in image
	erase [mask] [vs ve]			# erase a mask or region
	draw [mask] [vs ve] [>ofile]		# draw mask or region of mask

	set [mask]				# set reference mask
	set [vector] i j k...			# load vector register
	show [vector]				# print vector register
	show [mask] [index] [ll] [rl] [>ofile]	# print debug info for a mask

	box [mask] P1 P2 rop			# draw a box
	circle [mask] P1 r rop			# draw a circle
	line [mask] P1 P2 width rop		# draw a line segment
	point [mask] P1 rop			# draw a point
	polygon [mask] P1 ... PN rop		# draw a polygon

	rop src [vs] dst [vs] [vn] rop			# rasterop
	stencil src [vs] dst [vs] stn [vs] [vn] rop	# stencil

	compare mask1 mask2			# compare two masks
	rtest mask1 mask2			# range list conversion test
	ptest mask1 mask2			# pixel array conversion test

Rasterops may be specified either as integer constants (any radix) or via
a simple symbolic notation, e.g.:  "opcode+[value]".

A mask may be examined in detail with SHOW, which calls pm_debug to decode
the contents of a mask.  A graphic image of a mask may be drawn with DRAW,
which renders each pixel in the mask as a printable character.
.endhelp --------------------------------------------------------------------

define	SZ_SBUF		512			# size limiting parameters
define	DEF_MASKSIZE_X	75
define	DEF_MASKSIZE_Y	40
define	MAXKWLEN	20
define	MAXARGS		50
define	MAXMREG		4
define	MAXVREG		10
define	MAXINCL		10
define	WIDTH		80

define	INT_ARG		1			# argument types
define	STRING_ARG	2
define	VECTOR_ARG	3
define	MASK_ARG	4

define	v_i	argval[$1]			# integer argument
define	v_s	sbuf[argval[$1]]		# string argument
define	v_si	sbuf[argval[$1]+$2-1]		# indexed string argument
define	v_v	v_reg[1,argval[$1]+1]		# vector argument
define	v_vi	v_reg[$2,argval[$1]+1]		# indexed vector argument
define	v_m	v_mask[argval[$1]]		# mask argument

define	KW_BOX		1			# interpreter commands
define	KW_BYE		2
define	KW_CIRCLE	3
define	KW_CLEAR	4
define	KW_COMPARE	5
define	KW_CREATE	6
define	KW_DRAW		7
define	KW_ERASE	8
define	KW_HELP		9
# eol			10
define	KW_LINE		11
define	KW_POINT	12
define	KW_POLYGON	13
define	KW_LOAD		14
define	KW_LOADIM	15
define	KW_PTEST	16
define	KW_ROP		17
define	KW_RTEST	18
define	KW_RUN		19
define	KW_SAVE		20
define	KW_SAVEIM	21
# eol			22
define	KW_SET		23
define	KW_SHOW		24
define	KW_STENCIL	25
define	KW_TIMER	26


# PMTEST -- Test the PMIO package.  Read and execute commands from the standard
# input until EOF or BYE is seen.

procedure t_pmtest()

bool	timer
long	time[2]
int	x, y, r
int	px[MAXARGS], py[MAXARGS]
int	what, rop, v_arg, x1, x2, y1, y2, ip, op, ch, i, j
pointer	pm, pm_1, pm_2, def_pm, pm_src, pm_dst, pm_stn, tty
char	cmd[SZ_LINE], kwname[SZ_FNAME], fname[SZ_FNAME], title[SZ_LINE]
int	opcode, save_fd[MAXINCL], in, fd, o_fd, maskno, depth, naxes, npts
int	v1[PL_MAXDIM], v2[PL_MAXDIM], v3[PL_MAXDIM], v4[PL_MAXDIM], v[PL_MAXDIM]
int	fstati(), strdic(), open(), getline(), strncmp()
pointer	pm_create(), ttyodes()

char	sbuf[SZ_SBUF]
int	v_mask[MAXMREG], v_reg[PL_MAXDIM,MAXVREG]
int	nargs, argno, argtype[MAXARGS], argval[MAXARGS], s_op, width
common	/pmzcom/ v_mask, v_reg, nargs, argno, argtype, argval, s_op, sbuf
define	argerr_ 91
define	eof_ 92

string	keywords "|box|bye|circle|clear|compare|create|draw|erase|help|\
	|line|point|polygon|load|loadim|ptest|rop|rtest|run|save|saveim|\
	|set|show|stencil|timer|"

begin
	in = 0
	ip = 1
	fd = STDIN
	cmd[ip] = EOS
	timer = false

	# Initialize the mask registers.
	v[1] = DEF_MASKSIZE_X
	v[2] = DEF_MASKSIZE_Y
	do i = 1, MAXMREG
	    v_mask[i] = pm_create (2, v, 7)
	def_pm = v_mask[1]

	# Initialize the vector registers.
	do i = 1, MAXVREG
	    call amovki (1, v_reg[1,i], PL_MAXDIM)

	# Main interpreter loop.
	# ---------------------------

	repeat {
	    # Get next command.
	    if (cmd[ip] == '\n' || cmd[ip] == '#' || cmd[ip] == EOS) {
		# Prompt if reading from the standard input.
		if (in == 0 && fstati (STDIN, F_REDIR) == NO) {
		    call putline (STDOUT, "* ")
		    call flush (STDOUT)
		}

		# Handle EOF on the current command stream.
		if (getline (fd, cmd) == EOF) {
eof_		    if (in > 0) {
			call close (fd)
			fd = save_fd[in]
			in = in - 1
		    } else
			break
		}

		ip = 1
	    }

	    # Skip blank lines and comment lines.
	    for (ip=1;  IS_WHITE(cmd[ip]) || cmd[ip] == ';';  ip=ip+1)
		;
	    if (cmd[ip] == '\n' || cmd[ip] == '#' || cmd[ip] == EOS)
		next

	    # Extract the keyword into the KWNAME buffer.  Leave the input
	    # pointer positioned to the first char following the keyword.

	    for (op=1;  cmd[ip] != EOS && cmd[ip] != '\n';  ip=ip+1) {
		ch = cmd[ip]
		if (IS_ALNUM(ch)) {
		    kwname[op] = ch
		    op = op + 1
		} else
		    break
	    }
	    kwname[op] = EOS

	    # Look up the keyword in the dictionary.  If not found ring the
	    # bell, but do not quit.

	    opcode = strdic (kwname, kwname, MAXKWLEN, keywords)
	    if (opcode <= 0) {
		call eprintf ("unknown command\007\n")
		call flush (STDERR)
		ip=1;  cmd[ip] = EOS
		next
	    }

	    # Parse the argument list.
	    call parse_args (cmd, ip)

	    # Process the command.
	    # -------------------------

	    switch (opcode) {
	    case KW_BYE:
		goto eof_

	    case KW_POINT:
		# Draw a point.

		# Get mask.
		pm = def_pm
		if (argtype[argno] == MASK_ARG) {
		    pm = v_m(argno)
		    argno = argno + 1
		}

		# Get coords of point.
		if (argtype[argno] == VECTOR_ARG) {
		    # Coords given as vectors.
		    x1 = v_vi(argno,1)
		    y1 = v_vi(argno,2)
		    argno = argno + 1
		} else {
		    # Coords given explicitly.
		    do i = 1, 2
			if (argtype[argno+i-1] != INT_ARG)
			    goto argerr_

		    x1 = v_i(argno);  argno = argno + 1
		    y1 = v_i(argno);  argno = argno + 1
		}

		# Get rop.
		if (argno <= nargs) {
		    if (argtype[argno] != INT_ARG)
			goto argerr_
		    rop = v_i(argno);  argno = argno + 1
		} else
		    rop = or (PIX_SRC, PIX_DST) + PIX_VALUE('1')

		# Perform the operation.
		if (timer)
		    call sys_mtime (time)
		call pm_point (pm, x1, y1, rop)
		if (timer)
		    call sys_ptime (STDOUT, "", time)

	    case KW_BOX:
		# Draw a box.

		# Get mask.
		pm = def_pm
		if (argtype[argno] == MASK_ARG) {
		    pm = v_m(argno)
		    argno = argno + 1
		}

		# Get corner coords of box.
		if (argtype[argno] == VECTOR_ARG) {
		    # Coords given as vectors.
		    x1 = v_vi(argno,1)
		    y1 = v_vi(argno,2)
		    argno = argno + 1
		    if (argtype[argno] != VECTOR_ARG)
			goto argerr_
		    x2 = v_vi(argno,1)
		    y2 = v_vi(argno,2)
		    argno = argno + 1

		} else {
		    # Coords given explicitly.
		    do i = 1, 4
			if (argtype[argno+i-1] != INT_ARG)
			    goto argerr_

		    x1 = v_i(argno);  argno = argno + 1
		    y1 = v_i(argno);  argno = argno + 1
		    x2 = v_i(argno);  argno = argno + 1
		    y2 = v_i(argno);  argno = argno + 1
		}

		# Get rop.
		if (argno <= nargs) {
		    if (argtype[argno] != INT_ARG)
			goto argerr_
		    rop = v_i(argno);  argno = argno + 1
		} else
		    rop = or (PIX_SRC, PIX_DST) + PIX_VALUE('2')

		# Perform the operation.
		if (timer)
		    call sys_mtime (time)
		call pm_box (pm, x1, y1, x2, y2, rop)
		if (timer)
		    call sys_ptime (STDOUT, "", time)

	    case KW_LINE:
		# Draw a line of arbitrary orientation and width.

		# Get mask.
		pm = def_pm
		if (argtype[argno] == MASK_ARG) {
		    pm = v_m(argno)
		    argno = argno + 1
		}

		# Get endpoints of line.
		if (argtype[argno] == VECTOR_ARG) {
		    # Coords given as vectors.
		    x1 = v_vi(argno,1)
		    y1 = v_vi(argno,2)
		    argno = argno + 1
		    if (argtype[argno] != VECTOR_ARG)
			goto argerr_
		    x2 = v_vi(argno,1)
		    y2 = v_vi(argno,2)
		    argno = argno + 1

		} else {
		    # Coords given explicitly.
		    do i = 1, 4
			if (argtype[argno+i-1] != INT_ARG)
			    goto argerr_

		    x1 = v_i(argno);  argno = argno + 1
		    y1 = v_i(argno);  argno = argno + 1
		    x2 = v_i(argno);  argno = argno + 1
		    y2 = v_i(argno);  argno = argno + 1
		}

		# Get line width.
		if (argno <= nargs) {
		    if (argtype[argno] != INT_ARG)
			goto argerr_
		    width = v_i(argno);  argno = argno + 1
		} else
		    width = 1

		# Get rop.
		if (argno <= nargs) {
		    if (argtype[argno] != INT_ARG)
			goto argerr_
		    rop = v_i(argno);  argno = argno + 1
		} else
		    rop = or (PIX_SRC, PIX_DST) + PIX_VALUE('4')

		# Perform the operation.
		if (timer)
		    call sys_mtime (time)
		call pm_line (pm, x1, y1, x2, y2, width, rop)
		if (timer)
		    call sys_ptime (STDOUT, "", time)

	    case KW_CIRCLE:
		# Draw a circle.

		# Get mask.
		pm = def_pm
		if (argtype[argno] == MASK_ARG) {
		    pm = v_m(argno)
		    argno = argno + 1
		}

		# Get center coords and radius of circle.
		if (argtype[argno] == VECTOR_ARG) {
		    # Center coords given as a vector.
		    x = v_vi(argno,1)
		    y = v_vi(argno,2)
		    argno = argno + 1

		} else {
		    # Center coords given explicitly.
		    do i = 1, 2
			if (argtype[argno+i-1] != INT_ARG)
			    goto argerr_

		    x = v_i(argno);  argno = argno + 1
		    y = v_i(argno);  argno = argno + 1
		}

		if (argtype[argno] != INT_ARG)
		    goto argerr_
		r = v_i(argno);  argno = argno + 1

		# Get rop.
		if (argno <= nargs) {
		    if (argtype[argno] != INT_ARG)
			goto argerr_
		    rop = v_i(argno);  argno = argno + 1
		} else
		    rop = or (PIX_SRC, PIX_DST) + PIX_VALUE('Q')

		# Perform the operation.
		if (timer)
		    call sys_mtime (time)
		call pm_circle (pm, x, y, r, rop)
		if (timer)
		    call sys_ptime (STDOUT, "", time)

	    case KW_POLYGON:
		# Draw a polygon.

		# Get mask.
		pm = def_pm
		if (argtype[argno] == MASK_ARG) {
		    pm = v_m(argno)
		    argno = argno + 1
		}

		# Get the coordinates of the polygon.
		for (npts=0;  argno <= nargs;  ) {
		    npts = npts + 1

		    if (argtype[argno] == VECTOR_ARG) {
			# Coords of point given as a vector.
			px[npts] = v_vi(argno,1)
			py[npts] = v_vi(argno,2)
			argno = argno + 1

		    } else if (argtype[argno] == INT_ARG &&
			argtype[argno+1] == INT_ARG) {

			# Center coords given explicitly.
			px[npts] = v_i(argno);  argno = argno + 1
			py[npts] = v_i(argno);  argno = argno + 1
		    }
		}

		# Get rop.
		if (argno <= nargs) {
		    if (argtype[argno] != INT_ARG)
			goto argerr_
		    rop = v_i(argno);  argno = argno + 1
		} else
		    rop = or (PIX_SRC, PIX_DST) + PIX_VALUE('R')

		# Perform the operation.
		if (timer)
		    call sys_mtime (time)
		call pm_polygon (pm, px, py, npts, rop)
		if (timer)
		    call sys_ptime (STDOUT, "", time)

	    case KW_CLEAR:
		# Clear the screen.
		tty = ttyodes ("terminal")
		call ttyclear (STDOUT, tty)
		call ttycdes (tty)

	    case KW_COMPARE:
		# Compare two masks.
		if (nargs < 2)
		    goto argerr_

		# Get mask 1.
		if (argtype[argno] == MASK_ARG) {
		    pm_1 = v_m(argno)
		    argno = argno + 1
		} else
		    goto argerr_

		# Get mask 2.
		if (argtype[argno] == MASK_ARG) {
		    pm_2 = v_m(argno)
		    argno = argno + 1
		} else
		    goto argerr_

		# Perform the operation.
		if (timer)
		    call sys_mtime (time)
		call pm_compare (pm_1, pm_2, STDOUT)
		if (timer)
		    call sys_ptime (STDOUT, "", time)

	    case KW_CREATE:
		# Create a new, emtpy mask of the given size and depth.

		# Get mask.
		if (argtype[argno] == MASK_ARG) {
		    maskno = v_i(argno)
		    argno = argno + 1
		}

		# Get naxes.
		if (argtype[argno] != INT_ARG)
		    goto argerr_
		naxes = v_i(argno);  argno = argno + 1

		# Get mask size.
		if (argtype[argno] == VECTOR_ARG) {
		    # Mask size given as vector.
		    call amovi (v_v(argno), v1, PL_MAXDIM)
		    argno = argno + 1
		} else {
		    # Mask size given explicitly.
		    do i = 1, naxes {
			if (argtype[argno+i-1] != INT_ARG)
			    goto argerr_
			v1[i] = v_i(argno)
			argno = argno + 1
		    }
		}

		# Get mask depth.
		if (argtype[argno] != INT_ARG)
		    depth = 1
		else {
		    depth = v_i(argno)
		    argno = argno + 1
		}

		# Perform the operation.
		if (timer)
		    call sys_mtime (time)
		v_mask[maskno] = pm_create (naxes, v1, depth)
		def_pm = v_mask[maskno]
		if (timer)
		    call sys_ptime (STDOUT, "", time)
		
	    case KW_DRAW:
		# Draw a mask or region of a mask on the screen.

		# Get mask.
		pm = def_pm
		if (nargs >= 1 && argtype[argno] == MASK_ARG) {
		    pm = v_m(argno)
		    argno = argno + 1
		}

		# Get vector coords of region to be drawn.
		if (argtype[argno] == VECTOR_ARG) {
		    call amovi (v_v(argno), v1, PL_MAXDIM)
		    argno = argno + 1
		} else
		    call amovki (1, v1, PL_MAXDIM)

		if (argtype[argno] == VECTOR_ARG) {
		    call amovi (v_v(argno), v2, PL_MAXDIM)
		    argno = argno + 1
		} else
		    call amovi (PL_AXLEN(pm,1), v2, PL_MAXDIM)

		# Perform the operation.
		call pm_asciidump (pm, v1, v2, STDOUT)

	    case KW_ERASE:
		# Erase a mask, or a region of a mask.

		# Get mask.
		pm = def_pm
		if (nargs >= 1 && argtype[argno] == MASK_ARG) {
		    pm = v_m(argno)
		    argno = argno + 1
		}

		# Get vector coords of region to be erased.
		if (argtype[argno] == VECTOR_ARG) {
		    call amovi (v_v(argno), v1, PL_MAXDIM)
		    argno = argno + 1
		} else
		    call amovki (1, v1, PL_MAXDIM)

		if (argtype[argno] == VECTOR_ARG) {
		    call amovi (v_v(argno), v2, PL_MAXDIM)
		    argno = argno + 1
		} else
		    call amovi (PL_AXLEN(pm,1), v2, PL_MAXDIM)

		# Perform the operation.
		if (timer)
		    call sys_mtime (time)
		if (nargs <= 1)
		    call pm_clear (pm)
		else
		    call pm_rop (NULL, 0, pm, v1, v2, PIX_CLR)
		if (timer)
		    call sys_ptime (STDOUT, "", time)

	    case KW_HELP:
		# Print a command summary.
		call print_help (STDOUT)

	    case KW_RUN:
		# Read commands from a file.
		if (nargs < 1 || argtype[argno] != STRING_ARG)
		    goto argerr_

		in = in + 1
		if (in > MAXINCL)
		    call error (1, "too many nested run files\n")
		save_fd[in] = fd
		iferr (fd = open (v_s(argno), READ_ONLY, TEXT_FILE)) {
		    call erract (EA_WARN)
		    fd = save_fd[in]
		    in = in - 1
		}

	    case KW_SET:
		# Set the value of a mask or vector register.
		if (nargs < 1) {
		    goto argerr_

		} else if (argtype[argno] == MASK_ARG) {
		    # Set the default mask.
		    def_pm = v_m(argno)
		    maskno = v_i(argno)

		} else if (argtype[argno] == VECTOR_ARG) {
		    # Set a vector register.
		    v_arg = argno
		    argno = argno + 1

		    do i = 1, PL_MAXDIM
			if (argno <= nargs && argtype[argno] == INT_ARG) {
			    v[i] = v_i(argno)
			    argno = argno + 1
			} else
			    v[i] = 1

		    call amovi (v, v_v(v_arg), PL_MAXDIM)
		}

	    case KW_SHOW:
		# Print information about a mask or vector register.

		if (nargs < 1 || argtype[argno] == MASK_ARG) {
		    # Print information about a mask.

		    if (nargs < 1)
			pm = def_pm
		    else {
			pm = v_m(argno)
			argno = argno + 1
		    }

		    o_fd = STDOUT

		    # Process option selects.
		    what = PD_SUMMARY
		    while (argno <= nargs && argtype[argno] == STRING_ARG) {
			if (strncmp (v_s(argno), "i", 1) == 0) {
			    what = or (what, PD_INDEX)
			} else if (strncmp (v_s(argno), "ll", 2) == 0) {
			    what = or (what, PD_LLOUT)
			} else if (strncmp (v_s(argno), "rl", 2) == 0) {
			    what = or (what, PD_RLOUT)
			} else if (strncmp (v_s(argno), "lh", 2) == 0) {
			    what = or (what, PD_LHDR)

			} else if (v_s(argno) == '>') {
			    # Write output to a file.
			    if (v_si(argno,2) == '>') {
				iferr (o_fd = open (v_si(argno,3),
				    APPEND, TEXT_FILE)) {
				    call erract (EA_WARN)
				    o_fd = STDOUT
				}
			    } else {
				iferr (o_fd = open (v_si(argno,2),
				    NEW_FILE, TEXT_FILE)) {
				    call erract (EA_WARN)
				    o_fd = STDOUT
				}
			    }
			} else {
			    call eprintf ("unknown option `%s'\n")
				call pargstr (v_s(argno))
			}
			argno = argno + 1
		    }

		    # Perform the operation.
		    call pm_debug (pm, o_fd, WIDTH, what)
		    if (o_fd != STDOUT)
			call close (o_fd)

		} else if (argtype[argno] == VECTOR_ARG) {
		    # Print the value of a vector register.
		    call printf ("v%d: ")
			call pargi (v_i(argno))
		    do i = 1, PL_MAXDIM {
			call printf (" %d")
			    call pargi (v_vi(argno,i))
		    }
		    call printf ("\n")

		} else {
		    # Print the value of all vector registers.
		    do j = 1, MAXVREG {
			call printf ("v%d: ")
			    call pargi (j-1)
			do i = 1, PL_MAXDIM {
			    call printf (" %d")
				call pargi (v_reg(i,j))
			}
			call printf ("\n")
		    }
		}

	    case KW_LOAD:
		# Load a mask from a file.

		# Get mask to be loaded.
		pm = def_pm
		if (nargs >= 1 && argtype[argno] == MASK_ARG) {
		    pm = v_m(argno)
		    argno = argno + 1
		}

		# Get mask filename.
		if (argno > nargs || argtype[argno] != STRING_ARG)
		    goto argerr_

		# Perform the operation.
		if (timer)
		    call sys_mtime (time)
		iferr (call pm_loadf (pm, v_s(argno), title, SZ_LINE))
		    call erract (EA_WARN)
		else if (title[1] != EOS) {
		    call printf ("mask: %s\n")
			call pargstr (title)
		    call flush (STDOUT)
		}
		if (timer)
		    call sys_ptime (STDOUT, "", time)

	    case KW_SAVE:
		# Save a mask in a file.

		# Get mask to be saved.
		pm = def_pm
		if (nargs >= 1 && argtype[argno] == MASK_ARG) {
		    pm = v_m(argno)
		    argno = argno + 1
		}

		# Get mask filename.
		if (argno > nargs || argtype[argno] != STRING_ARG)
		    goto argerr_
		else {
		    call strcpy (v_s(argno), fname, SZ_FNAME)
		    argno = argno + 1
		}

		# Get title string.
		if (argno <= nargs && argtype[argno] == STRING_ARG) {
		    call strcpy (v_s(argno), title, SZ_LINE)
		    argno = argno + 1
		}

		# Perform the operation.
		if (timer)
		    call sys_mtime (time)
		iferr (call pm_savef (pm, fname, title, 0))
		    call erract (EA_WARN)
		if (timer)
		    call sys_ptime (STDOUT, "", time)

	    case KW_LOADIM:
		# Load a mask from an image.

		# Get mask to be loaded.
		pm = def_pm
		if (nargs >= 1 && argtype[argno] == MASK_ARG) {
		    pm = v_m(argno)
		    argno = argno + 1
		}

		# Get image section.
		if (argno > nargs || argtype[argno] != STRING_ARG)
		    goto argerr_

		# Perform the operation.
		if (timer)
		    call sys_mtime (time)
		iferr (call pm_loadim (pm, v_s(argno), title, 0))
		    call erract (EA_WARN)
		if (timer)
		    call sys_ptime (STDOUT, "", time)

	    case KW_SAVEIM:
		# Save a mask in an image.

		# Get mask to be saved.
		pm = def_pm
		if (nargs >= 1 && argtype[argno] == MASK_ARG) {
		    pm = v_m(argno)
		    argno = argno + 1
		}

		# Get output image name.
		if (argno > nargs || argtype[argno] != STRING_ARG)
		    goto argerr_
		else {
		    call strcpy (v_s(argno), fname, SZ_FNAME)
		    argno = argno + 1
		}

		# Get title string.
		if (argno <= nargs && argtype[argno] == STRING_ARG) {
		    call strcpy (v_s(argno), title, SZ_LINE)
		    argno = argno + 1
		}

		# Perform the operation.
		if (timer)
		    call sys_mtime (time)
		iferr (call pm_saveim (pm, fname, title, 0))
		    call erract (EA_WARN)
		if (timer)
		    call sys_ptime (STDOUT, "", time)

	    case KW_ROP:
		# General rasterop operation.

		# Get source mask.
		pm_src = def_pm
		if (argtype[argno] == MASK_ARG) {
		    pm_src = v_m(argno)
		    argno = argno + 1
		}

		# Get start vector in source mask.
		if (argtype[argno] == VECTOR_ARG) {
		    call amovi (v_v(argno), v1, PL_MAXDIM)
		    argno = argno + 1
		} else
		    call amovki (1, v1, PL_MAXDIM)

		# Get destination mask.
		pm_dst = def_pm
		if (nargs >= 1 && argtype[argno] == MASK_ARG) {
		    pm_dst = v_m(argno)
		    argno = argno + 1
		}

		# Get start vector in destination mask.
		if (argtype[argno] == VECTOR_ARG) {
		    call amovi (v_v(argno), v2, PL_MAXDIM)
		    argno = argno + 1
		} else
		    call amovki (1, v2, PL_MAXDIM)

		# Get vector defining size of region to be modified.
		if (argtype[argno] == VECTOR_ARG) {
		    call amovi (v_v(argno), v3, PL_MAXDIM)
		    argno = argno + 1
		} else
		    call amovi (PL_AXLEN(pm_dst,1), v3, PL_MAXDIM)

		# Get rop.
		if (argno <= nargs) {
		    if (argtype[argno] != INT_ARG)
			goto argerr_
		    rop = v_i(argno);  argno = argno + 1
		} else
		    rop = PIX_SRC

		# Perform the operation.
		if (timer)
		    call sys_mtime (time)
		call pm_rop (pm_src, v1, pm_dst, v2, v3, rop)
		if (timer)
		    call sys_ptime (STDOUT, "", time)

	    case KW_STENCIL:
		# Rasterop operation though a stencil mask.

		# Get source mask.
		pm_src = def_pm
		if (nargs >= 1 && argtype[argno] == MASK_ARG) {
		    pm_src = v_m(argno)
		    argno = argno + 1
		}

		# Get start vector in source mask.
		if (argtype[argno] == VECTOR_ARG) {
		    call amovi (v_v(argno), v1, PL_MAXDIM)
		    argno = argno + 1
		} else
		    call amovki (1, v1, PL_MAXDIM)

		# Get destination mask.
		pm_dst = def_pm
		if (nargs >= 1 && argtype[argno] == MASK_ARG) {
		    pm_dst = v_m(argno)
		    argno = argno + 1
		}

		# Get start vector in destination mask.
		if (argtype[argno] == VECTOR_ARG) {
		    call amovi (v_v(argno), v2, PL_MAXDIM)
		    argno = argno + 1
		} else
		    call amovki (1, v2, PL_MAXDIM)

		# Get stencil mask.
		pm_stn = def_pm
		if (nargs >= 1 && argtype[argno] == MASK_ARG) {
		    pm_stn = v_m(argno)
		    argno = argno + 1
		}

		# Get start vector in stencil mask.
		if (argtype[argno] == VECTOR_ARG) {
		    call amovi (v_v(argno), v3, PL_MAXDIM)
		    argno = argno + 1
		} else
		    call amovki (1, v3, PL_MAXDIM)

		# Get vector defining size of region to be modified.
		if (argtype[argno] == VECTOR_ARG) {
		    call amovi (v_v(argno), v4, PL_MAXDIM)
		    argno = argno + 1
		} else
		    call amovi (PL_AXLEN(pm_dst,1), v4, PL_MAXDIM)

		# Get rop.
		if (argno <= nargs) {
		    if (argtype[argno] != INT_ARG)
			goto argerr_
		    rop = v_i(argno);  argno = argno + 1
		} else {
		    call eprintf ("no rop specified - copying src to dst\n")
		    rop = PIX_SRC
		}

		# Perform the operation.
		if (timer)
		    call sys_mtime (time)
		call pm_stencil (pm_src, v1, pm_dst, v2, pm_stn, v3, v4, rop)
		if (timer)
		    call sys_ptime (STDOUT, "", time)

	    case KW_PTEST, KW_RTEST:
		# Line list to pixel array or range list conversion test.
		if (nargs < 2)
		    goto argerr_

		# Get mask 1.
		if (argtype[argno] == MASK_ARG) {
		    pm_1 = v_m(argno)
		    argno = argno + 1
		} else
		    goto argerr_

		# Get mask 2.
		if (argtype[argno] == MASK_ARG) {
		    pm_2 = v_m(argno)
		    argno = argno + 1
		} else
		    goto argerr_

		# Perform the operation.
		if (timer)
		    call sys_mtime (time)
		call conv_test (pm_1, pm_2, STDOUT, opcode)
		if (timer)
		    call sys_ptime (STDOUT, "", time)

	    case KW_TIMER:
		if (timer) {
		    call printf ("timer off\n")
		    timer = false
		} else {
		    call printf ("timer on\n")
		    timer = true
		}

	    default:
		# Unrecognized command.
		call eprintf ("unknown switch\007\n")
		call flush (STDERR)
	    }

	    call flush (STDOUT)
	    next
argerr_
	    call eprintf ("invalid argument list\n")
	}

	do i = 1, MAXMREG
	    call pm_close (v_mask[i])
end


# PARSE_ARGS -- Parse the argument list to an interpreter command, leaving
# the decoded arguments in the interpreter common, and returning the number
# of arguments as the function value.

procedure parse_args (args, ip)

char	args[ARB]		# argument list
int	ip			# pointer into argument list

double	dval
int	nchars, junk, i
int	ctowrd(), stridx(), gctod(), strlen()

char	sbuf[SZ_SBUF]
int	v_mask[MAXMREG], v_reg[PL_MAXDIM,MAXVREG]
int	nargs, argno, argtype[MAXARGS], argval[MAXARGS], s_op
common	/pmzcom/ v_mask, v_reg, nargs, argno, argtype, argval, s_op, sbuf

begin
	s_op = 1
	argno = 1
	nargs = 0

	do i = 1, MAXARGS
	    argtype[i] = 0

	# Get next token.
	junk = ctowrd (args, ip, sbuf[s_op], SZ_SBUF-s_op)
	nchars = strlen (sbuf[s_op])

	while (nchars > 0) {
	    nargs = nargs + 1
	    if (nargs > MAXARGS)
		call error (1, "too many arguments")

	    if (nchars == 1 && sbuf[s_op] == '=') {
		# Discard assignment operator.
		nargs = nargs - 1

	    } else if (nchars == 1 && stridx (sbuf[s_op], "abcd") > 0) {
		# Mask register.
		argval[nargs] = stridx (sbuf[s_op], "abcd")
		argtype[nargs] = MASK_ARG

	    } else if (nchars == 2 && sbuf[s_op] == 'v' &&
		# Vector register.
		IS_DIGIT(sbuf[s_op+1])) {
		argval[nargs] = TO_INTEG(sbuf[s_op+1])
		argtype[nargs] = VECTOR_ARG

	    } else if (IS_DIGIT (sbuf[s_op])) {
		# Get an integer constant.
		i=1;  nchars = gctod (sbuf[s_op], i, dval)
		argval[nargs] = nint(dval)
		argtype[nargs] = INT_ARG

		# Handle the notation "opcode+value", for rasterops.
		if (sbuf[s_op+i-1] == '+') {
		    i=i+1;  nchars = gctod (sbuf[s_op], i, dval)
		    argval[nargs] = argval[nargs] + PIX_VALUE(nint(dval))
		}

	    } else {
		# String constant.
		argval[nargs] = s_op
		argtype[nargs] = STRING_ARG
		s_op = s_op + nchars + 1
	    }

	    while (IS_WHITE(args[ip]))
		ip = ip + 1
	    if (args[ip] == ';' || args[ip] == '\n') {
		ip = ip + 1
		break
	    }

	    # Get next token.
	    junk = ctowrd (args, ip, sbuf[s_op], SZ_SBUF-s_op)
	    nchars = strlen (sbuf[s_op])
	}
end


# CONV_TEST -- Test the line list to pixel array or range list conversion
# routines.

procedure conv_test (pm_1, pm_2, fd, opcode)

pointer	pm_1			#I input mask
pointer	pm_2			#I output mask
int	fd			#I output file, for reporting errors
int	opcode			#I KW_[PR]TEST

begin
	call fprintf (fd, "conv_test called\n")
end


# PRINT_HELP -- Print the PMIO test interpreter commands help summary.

procedure print_help (fd)

int	fd			#I output file

begin
	call fprintf (fd, "help%48t# print command summary\n")
	call fprintf (fd, "timer%48t# toggle timing of commands\n")
	call fprintf (fd, "run fname%48t# read commands from a file\n")
	call fprintf (fd, "clear%48t# clear the screen\n")
	call fprintf (fd, "bye%48t# all done (also EOF)\n\n")
	call fprintf (fd,
	"create [mask] naxes axlen [depth]%48t# create a new mask\n")
	call fprintf (fd, "load [mask] fname%48t# load mask from file\n")
	call fprintf (fd, "save [mask] fname%48t# save mask in file\n")
	call fprintf (fd, "loadim [mask] image%48t# load mask from image\n")
	call fprintf (fd, "saveim [mask] image%48t# save mask in image\n")
	call fprintf (fd, "erase [mask] [vs ve]%48t# erase a mask or region\n")
	call fprintf (fd,
	"draw [mask] [vs ve] [>ofile]%48t# draw mask or region of mask\n\n")
	call fprintf (fd, "set [mask]%48t# set reference mask\n")
	call fprintf (fd, "set [vector] i j k...%48t# load vector register\n")
	call fprintf (fd, "show [vector]%48t# print vector register\n")
	call fprintf (fd,
"show [mask] [index] [ll] [rl] [>ofile]%48t# print debug info for a mask\n\n")
	call fprintf (fd, "box P1 P2 rop%48t# draw a box\n")
	call fprintf (fd, "circle P1 r rop%48t# draw a circle\n \n")
	call fprintf (fd,
	"line [mask] P1 P2 width rop%48t# draw a line segment\n")
	call fprintf (fd, "point [mask] P1 rop%48t# draw a point\n")
	call fprintf (fd, "polygon [mask] P1 ... PN rop%48t# draw a polygon\n")
	call fprintf (fd, "rop src [vs] dst [vs] [vn] rop%48t# rasterop\n")
	call fprintf (fd,
	"stencil src [vs] dst [vs] stn [vs] [vn] rop%48t# stencil\n \n")
	call fprintf (fd, "compare mask1 mask2%48t# compare two masks\n")
	call fprintf (fd, "rtest mask1 mask2%48t# range list conversion test\n")
	call fprintf (fd,
	"ptest mask1 mask2%48t# pixel array conversion test\n")
end
