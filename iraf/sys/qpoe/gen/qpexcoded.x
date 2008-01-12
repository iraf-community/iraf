# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include "../qpex.h"

# QPEX_CODEGEN -- Generate interpreter metacode to evaluate the given
# expression.  The new code is appended to the current compiled program,
# adding additional constraints which a data event will have to meet to
# pass the filter.

int procedure qpex_codegend (ex, atname, assignop, expr, offset, dtype)

pointer ex			#I qpex descriptor
char	atname[ARB]		#I attribute name (for expr regeneration)
char	assignop[ARB]		#I "=" or "+=" (for expr regeneration)
char	expr[ARB]		#I expression to be compiled
int	offset			#I typed offset of referenced attribute
int	dtype			#I datatype of referenced attribute

int	nbins, bin, xp
pointer	lt, lut, lutx, pb
double	x1, x2, xmin, xmax
int	xlen, nranges, n_nranges, level, opcode, ip, i
pointer	pb_save, db_save, xs_buf, xe_buf, xs, xe, n_xs, n_xe, et, prev

double	sv_xs[MAX_LEVELS], sv_xe[MAX_LEVELS]
pointer	sv_lt[MAX_LEVELS], sv_lut[MAX_LEVELS], sv_lutx[MAX_LEVELS]
int	sv_xp[MAX_LEVELS], sv_nranges[MAX_LEVELS], sv_bin[MAX_LEVELS]
int	sv_nbins[MAX_LEVELS]

double	xoffset, xscale
double	sv_xoffset[MAX_LEVELS], sv_xscale[MAX_LEVELS]
int	d_x1, d_x2
int	qpex_refd()

bool	fp_equald()


int	qpex_parsed()
int	stridxs(), btoi(), qpex_sublistd()
pointer	qpex_dballoc(), qpex_dbpstr(), qpex_pbpos()
errchk	qpex_dballoc, qpex_pbpin, malloc, calloc, realloc, qpex_parsed

string	qpexwarn "QPEX Warning"
define	error_ 91
define	next_  92
define	null_  93
define	resume_ 94
define	bbmask_ 95
define	continue_ 96
define	XS Memd[xs+($1)-1]
define	XE Memd[xe+($1)-1]

begin
	pb = EX_PB(ex)

	# Save the program state in case we have to abort.
	call qpex_mark (ex, pb_save, db_save)

	# Allocate and initialize a new expression term descriptor, linking
	# it onto the tail of the ETTERMs list.

	et = qpex_dballoc (ex, LEN_ETDES, TY_STRUCT)

	ET_ATTTYPE(et)	= dtype
	ET_ATTOFF(et)	= offset
	ET_ATNAME(et)	= qpex_dbpstr (ex, atname)
	ET_ASSIGNOP(et)	= qpex_dbpstr (ex, assignop)
	ET_EXPRTEXT(et) = qpex_dbpstr (ex, expr)
	ET_PROGPTR(et)	= qpex_pbpos (ex)
	ET_DELETED(et)	= NO

	prev = EX_ETTAIL(ex)
	if (prev != NULL)
	    ET_NEXT(prev) = et
	ET_NEXT(et) = NULL
	EX_ETTAIL(ex) = et
	if (EX_ETHEAD(ex) == NULL)
	    EX_ETHEAD(ex) = et

	ip = stridxs ("%", expr)
	    # Bitmask tests are meaningless for floating point data.
	    if (ip > 0) {
		call eprintf ("%s: bitmasks not permitted for floating data\n")
		    call pargstr (qpexwarn)
		goto error_
	    }

	# Compile a general range list expression.  The basic procedure is
	# to parse the expression to produce an optimized binary range list,
	# then either compile the range list as an explicit series of
	# instructions or as a lookup table, depending upon the number of
	# ranges.

	xlen = DEF_XLEN
	call malloc (xs_buf, xlen, TY_DOUBLE)
	call malloc (xe_buf, xlen, TY_DOUBLE)

	# Convert expr to a binary range list and set up the initial context.
	# Ensure that the range list buffers are large enough to hold any
	# sublists extracted during compilation.

	nranges = qpex_parsed (expr, xs_buf, xe_buf, xlen)
	if (xlen < nranges * 2) {
	    xlen = nranges * 2
	    call realloc (xs_buf, xlen, TY_DOUBLE)
	    call realloc (xe_buf, xlen, TY_DOUBLE)
	}

	xs = xs_buf
	xe = xe_buf
	level = 0

	repeat {
next_
	    # Compile a new range list (or sublist).
	    if (nranges <= 0) {
		# This shouldn't happen.
null_		call eprintf ("%s: null range list\n")
		    call pargstr (qpexwarn)
		call qpex_pbpin (ex, PASS, 0, 0, 0)

	    } else if (nranges == 1) {
		# Output an instruction to load the data, perform the range
		# test, and conditionally exit all in a single instruction.

		x1 = XS(1);  x2 = XE(1)
		    d_x1 = qpex_refd (ex, x1)
		    d_x2 = qpex_refd (ex, x2)

		if (dtype == TY_SHORT) {
		    if (IS_LEFTD(x1) && IS_RIGHTD(x2))
			; # pass everything (no tests)
		    else if (IS_LEFTD(x1))
			call qpex_pbpin (ex, LEQXS, offset, d_x2, 0)
		    else if (IS_RIGHTD(x2))
			call qpex_pbpin (ex, GEQXS, offset, d_x1, 0)
		    else if (fp_equald (x1, x2))
			call qpex_pbpin (ex, EQLXS, offset, d_x1, d_x2)
		    else
			call qpex_pbpin (ex, RNGXS, offset, d_x1, d_x2)
		} else {
		    if (IS_LEFTD(x1) && IS_RIGHTD(x2))
			; # pass everything (no tests)
		    else if (IS_LEFTD(x1))
			call qpex_pbpin (ex, LEQXD, offset, d_x2, 0)
		    else if (IS_RIGHTD(x2))
			call qpex_pbpin (ex, GEQXD, offset, d_x1, 0)
		    else if (fp_equald (x1, x2))
			call qpex_pbpin (ex, EQLXD, offset, d_x1, d_x2)
		    else
			call qpex_pbpin (ex, RNGXD, offset, d_x1, d_x2)
		}

	    } else if (nranges < EX_LUTMINRANGES(ex)) {
		# If the number of ranges to be tested for the data is small,
		# compile explicit code to perform the range tests directly.
		# Otherwise skip forward and compile a lookup table instead.
		# In either case, the function of the instructions compiled
		# is to test the data loaded into the register above, setting
		# the value of PASS to true if the data lies in any of the
		# indicated ranges.

		# Check for !X, which is indicated in range list form by a
		# two element list bracketing the X on each side.

		if (nranges == 2)
		    if (IS_LEFTD(XS(1)) && IS_RIGHTD(XE(2)))
			if (fp_equald (XE(1), XS(2))) {
			    call qpex_pbpin (ex, NEQXD, offset,
				qpex_refd(ex,XE(1)), 0)
			    goto resume_
			}

		# If at level zero, output instruction to load data into
		# register and initialize PASS to false.  Don't bother if
		# compiling a subprogram, as these operations will already
		# have been performed by the caller.

		if (level == 0) {
			opcode = LDDD
		    call qpex_pbpin (ex, opcode, offset, 0, 0)
		}

		# Compile a series of equality or range tests.
		do i = 1, nranges {
		    x1 = XS(i);  x2 = XE(i)
			d_x1 = qpex_refd (ex, x1)
			d_x2 = qpex_refd (ex, x2)

		    if (IS_LEFTD(x1))
			call qpex_pbpin (ex, LEQD, d_x2, 0, 0)
		    else if (IS_RIGHTD(x2))
			call qpex_pbpin (ex, GEQD, d_x1, 0, 0)
		    else if (fp_equald (x1, x2))
			call qpex_pbpin (ex, EQLD, d_x1, d_x2, 0)
		    else
			call qpex_pbpin (ex, RNGD, d_x1, d_x2, 0)
		}

		# Compile a test and exit instruction.
		call qpex_pbpin (ex, XIFF, 0, 0, 0)

	    } else {
		# Compile a lookup table test. Lookup tables may be
		# either compressed or fully resolved.  If compressed
		# (the resolution of the table is less than that of the
		# range data, e.g., for floating point lookup tables) a
		# LUT bin may have as its value, in addition to the
		# usual 0 or 1, the address of an interpreter subprogram
		# to be executed to test data values mapping to that bin.
		# The subprogram pointed to may in turn be another lookup
		# table, hence in the general case a tree of lookup tables
		# and little code segments may be compiled to implement
		# a complex range list test.

		# Get the data range of the lookup table.
		xmin = XS(1)
		if (IS_LEFTD(xmin))
		    xmin = XE(1)
		xmax = XE(nranges)
		if (IS_RIGHTD(xmax))
		    xmax = XS(nranges)

		# Get the lookup table size.  Use a fully resolved table
		# if the data is integer and the number of bins required
		# is modest.

		    nbins = min (EX_MAXRRLUTLEN(ex), nranges * EX_LUTSCALE(ex))

		# Determine the mapping from data space to table space.
		xoffset = xmin
		    xscale = nbins / (xmax - xmin)

		# Allocate and initialize the lookup table descriptor.
		lt = qpex_dballoc (ex, LEN_LTDES, TY_STRUCT)
		call calloc (lut, nbins, TY_SHORT)

		LT_NEXT(lt)	= EX_LTHEAD(ex)
		EX_LTHEAD(ex)	= lt
		LT_TYPE(lt)	= TY_DOUBLE
		LT_LUTP(lt)	= lut
		LT_NBINS(lt)	= nbins
		LT_D0(lt)	= xoffset
		LT_DS(lt)	= xscale
		LT_LEFT(lt)	= btoi (IS_LEFTD(XS(1)))
		LT_RIGHT(lt)	= btoi (IS_RIGHTD(XE(nranges)))

		# Compile the LUTX test instruction.  Save a back pointer
		# to the instruction so that we can edit the jump field in
		# case a subprogram is compiled after the LUTXt.

		lutx = qpex_pbpos (ex)
		if (dtype == TY_SHORT)
		    call qpex_pbpin (ex, LUTXS, offset, lt, 0)
		else
		    call qpex_pbpin (ex, LUTXD, offset, lt, 0)

		xp = 1
		bin = 1
continue_
		n_xs = xs + nranges
		n_xe = xe + nranges

		# Initialize the lookup table.
		do i = bin, nbins {
		    x1 = (i-1) / xscale + xoffset
			x2 = i / xscale + xoffset

		    # Get sub-rangelist for range x1:x2.
		    n_nranges = qpex_sublistd (x1, x2,
			Memd[xs], Memd[xe], nranges, xp,
			Memd[n_xs], Memd[n_xe])

		    if (n_nranges <= 0) {
			Mems[lut+i-1] = 0

		    } else if (n_nranges == 1 && IS_LEFTD(Memd[n_xs]) &&
			IS_RIGHTD(Memd[n_xe])) {

			Mems[lut+i-1] = 1

		    } else {
			# Compile the sub-rangelist as a subprogram.

			# First set the LUT bin to point to the subprogram.
			# We cannot use the IP directly here since the LUT
			# bins are short integer, so store the offset into
			# the pb instead (guaranteed to be >= 4).

			Mems[lut+i-1] = qpex_pbpos(ex) - pb

			# Push a new context.
			level = level + 1
			if (level > MAX_LEVELS) {
			    call eprintf ("%s: ")
				call pargstr (qpexwarn)
			    call eprintf ("Excessive LUT nesting\n")
			    goto error_
			}

			# Save current LUT compilation context.
			sv_xs[level] = xs
			sv_xe[level] = xe
			sv_xp[level] = xp
			sv_xoffset[level] = xoffset
			sv_xscale[level] = xscale
			sv_nranges[level] = nranges
			sv_lt[level] = lt
			sv_bin[level] = i
			sv_nbins[level] = nbins
			sv_lut[level] = lut
			sv_lutx[level] = lutx

			# Set up context for the new rangelist.
			xs = n_xs
			xe = n_xe
			nranges = n_nranges

			goto next_
		    }
		}

		# Compile a test and exit instruction if the LUT calls any
		# subprograms.

		if (qpex_pbpos(ex) - lutx > LEN_INSTRUCTION)
		    call qpex_pbpin (ex, XIFF, 0, 0, 0)
	    }
resume_
	    # Resume lookup table compilation if exiting due to LUT-bin
	    # subprogram compilation.

	    if (level > 0) {
		# Pop saved context.
		xs = sv_xs[level]
		xe = sv_xe[level]
		xp = sv_xp[level]
		xoffset = sv_xoffset[level]
		xscale = sv_xscale[level]
		nranges = sv_nranges[level]
		lt = sv_lt[level]
		bin = sv_bin[level]
		nbins = sv_nbins[level]
		lut = sv_lut[level]
		lutx = sv_lutx[level]

		# Compile a return from subprogram.
		call qpex_pbpin (ex, RET, 0, 0, 0)

		# Patch up the original LUTX instruction to jump over the
		# subprogram we have just finished compiling.

		IARG3(lutx) = qpex_pbpos (ex)

		# Resume compilation at the next LUT bin.
		bin = bin + 1
		level = level - 1
		goto continue_
	    }
	} until (level <= 0)

	# Finish setting up the eterm descriptor.
	ET_NINSTR(et) = (qpex_pbpos(ex) - ET_PROGPTR(et)) / LEN_INSTRUCTION

	return (OK)
error_
	call qpex_free (ex, pb_save, db_save)
	return (ERR)
end
