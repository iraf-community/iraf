include	<ctype.h>
include "sgraph.h"

# GG_RDLIST2 -- Read a list of two dimensional data pairs into two type
# real arrays in memory.  Return pointers to the arrays and a count of the
# number of pixels.  If mark sizes are to be read from the input list,
# a third array of mark sizes is returned.  Mark sizes can only be given
# in two column (x,y) mode, with the mark size given as a third column.
#
#  Mod 7/20/90 to skip whitespace before comment character, ZGL
#
#  1/31/91 Changed input asymetrical error bar size to conform to the
#  interpretation of the value as the half width.  ZGL

int procedure gg_rdlist2 (fname, x, y, size, rdmarks, erraxis)

char	fname[SZ_FNAME]		# Name of list file
pointer	x, y, size		# Pointers to x, y and size vectors
bool	rdmarks			# Read markers from file?
int	erraxis			# X or Y errors?

int	buflen, n, fd, ncols, lineno
pointer	sp, lbuf, ip
real	xval, yval, szmark1, szmark2
int	getline(), nscan(), open()

#errchk	open, sscan, getline, malloc
errchk	open

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	fd = open (fname, READ_ONLY, TEXT_FILE)

	buflen = SZ_BUF

#	iferr {
	    call malloc (x, buflen, TY_REAL)
	    call malloc (y, buflen, TY_REAL)
	    call malloc (size, buflen, TY_REAL)
#	} then
#	    call erract (EA_FATAL)

	n = 0
	ncols = 0
	lineno = 0
	szmark1 = 1E-2
	szmark2 = 1E-2

	while (getline (fd, Memc[lbuf]) != EOF) {
	    lineno = lineno + 1
	    for (ip=lbuf;  IS_WHITE(Memc[ip]);  ip=ip+1)
		# Skip whitespace
		;

	    if (Memc[ip] == '#' || Memc[ip] == '\n' || Memc[ip] == EOS)
		# Skip comment lines and blank lines.
		next

	    # Decode the points to be plotted.
	    call sscan (Memc[ip])
		call gargr (xval)
		call gargr (yval)
		if (rdmarks) {
		    # Find the marker or error bar size(s)
		    call gargr (szmark1)
		    call gargr (szmark2)
		}

	    # The first line determines whether we have an x,y list or a
	    # y-list.  It is an error if only one value can be decoded when
	    # processing a two column list.

	    if (ncols == 0 && nscan() > 0)
		ncols = nscan()
	    
	    switch (nscan()) {
	    case 0:
		call eprintf ("no args; %s, line %d: %s\n")
		    call pargstr (fname)
		    call pargi (lineno)
		    call pargstr (Memc[lbuf])
		next
	    case 1:
		if (ncols == 2) {
		    call eprintf ("only 1 arg; %s, line %d: %s\n")
			call pargstr (fname)
			call pargi (lineno)
			call pargstr (Memc[lbuf])
		    next
		} else {
		    yval = xval
		    xval = n + 1.0
		}
	    case 2:
		if (rdmarks) {
		    call eprintf ("no mark size field; %s, line %d: %s\n")
			call pargstr (fname)
			call pargi (lineno)
			call pargstr (Memc[lbuf])
		    szmark1 = 1E-2
		}
	    case 3:
		if (ncols == 4 && rdmarks) {
		    call eprintf ("no upper error field; %s, line %d: %s\n")
			call pargstr (fname)
			call pargi (lineno)
			call pargstr (Memc[lbuf])
		    szmark2 = szmark1
		}
	    }

	    n = n + 1
	    if (n > buflen) {
		buflen = buflen + SZ_BUF
		call realloc (x, buflen, TY_REAL)
		call realloc (y, buflen, TY_REAL)
		call realloc (size, buflen, TY_REAL)
	    }

	    if (rdmarks) {
		if (IS_INDEF(xval) || IS_INDEF(yval) ||
		    IS_INDEF(szmark1) || IS_INDEF(szmark2))
		    Memr[size+n-1] = INDEF
		else {
		    if (erraxis == 1) {
			# Errors in X
			Memr[size+n-1] = (szmark1 + szmark2)
			xval = xval + (szmark2 - szmark1) / 2.0
		    } else if (erraxis == 2) {
			# Errors in Y
			Memr[size+n-1] = (szmark1 + szmark2)
			yval = yval + (szmark2 - szmark1) / 2.0
		    } else
			# Marker
			Memr[size+n-1] = szmark1
		}
	    }

	    Memr[x+n-1] = xval
	    Memr[y+n-1] = yval
	}

	call realloc (x, n, TY_REAL)
	call realloc (y, n, TY_REAL)
	call realloc (size, n, TY_REAL)

	call close (fd)
	call sfree (sp)
	return (n)
end
