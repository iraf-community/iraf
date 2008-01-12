include <tbset.h>	# TBtables

# GGRSER -- Find the location and size of error bars from one or two
# columns in the table 

procedure ggrser (tdp, errcol, numrows, x, y, size, erraxis)

pointer	tdp
char	errcol[SZ_FNAME]
int	numrows
real	x[numrows]
real	y[numrows]
real	size[numrows]
int	erraxis

char	lerrcol[SZ_COLNAME], herrcol[SZ_COLNAME]
pointer	secp, lecp, hecp
pointer	null			# Pointer to null column values array
pointer	lerr, herr, offs
char	errmsg[SZ_LINE]		# Error message
int	i

int	nscan()

begin
	# Read the errors column name(s)
	call malloc (null, numrows, TY_BOOL)
	call sscan (errcol)
	    call gargwrd (lerrcol, SZ_COLNAME)
	    call gargwrd (herrcol, SZ_COLNAME)

	switch (nscan ()) {
	case 1:
	    # One column ==> symmetrical error bars
	    call tbcfnd (tdp, lerrcol, secp, 1)
	    if (secp <= 0) {
		call sprintf (errmsg, SZ_LINE, 
		    "Cannot find errors column: %s\n")
		call pargstr (lerrcol)
		call error (0, errmsg)
	    }

	    # Read the error column
	    call tbcgtr (tdp, secp, size, Memb[null], 1, numrows)

	    do i = 1, numrows {
		if (IS_INDEF(size[i]) || IS_INDEF(x[i]) || IS_INDEF(y[i]))
		    # Preserve invalid points
		    size[i] = INDEF
		else
		    # The specified size is the half-amplitude
		    size[i] = 2.0 * size[i]
	    }

	case 2:
	    # Two columns ==> asymmetrical errors (low high)
	    call tbcfnd (tdp, lerrcol, lecp, 1)
	    if (lecp <= 0) {
		call sprintf (errmsg, SZ_LINE, 
		    "Cannot find low errors column: %s\n")
		call pargstr (lerrcol)
		call error (0, errmsg)
	    }

	    call tbcfnd (tdp, herrcol, hecp, 1)
	    if (hecp <= 0) {
		call sprintf (errmsg, SZ_LINE, 
		    "Cannot find high errors column: %s\n")
		call pargstr (herrcol)
		call error (0, errmsg)
	    }

	    # Total size of error bar
	    call malloc (lerr, numrows, TY_REAL)
	    call malloc (herr, numrows, TY_REAL)
	    call tbcgtr (tdp, lecp, Memr[lerr], Memb[null], 1, numrows)
	    call tbcgtr (tdp, hecp, Memr[herr], Memb[null], 1, numrows)
	    call aaddr  (Memr[lerr], Memr[herr], size, numrows]

	    # Position of centered marker
	    call malloc (offs, numrows, TY_REAL)
	    call asubr  (Memr[herr], Memr[lerr], Memr[offs], numrows)
	    call adivkr (Memr[offs], 2.0, Memr[offs], numrows)

	    do i = 1, numrows {
		if (IS_INDEF(x[i]) || IS_INDEF(y[i]))
		    # Preserve invalid points
		    size[i] = INDEF
	    }

	    if (erraxis == 1)
		# Errors in X;  adjust the horizontal position
		call aaddr (x, Memr[offs], x, numrows)

	    else if (erraxis == 2)
		# Errors in Y;  adjust the vertical position
		call aaddr (y, Memr[offs], y, numrows)

	    call mfree (lerr, TY_REAL)
	    call mfree (herr, TY_REAL)
	    call mfree (offs, TY_REAL)
	}

	call mfree (null, TY_BOOL)
end
