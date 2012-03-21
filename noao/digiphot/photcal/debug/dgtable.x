include	<time.h>
include	"debug.h"

# DG_DCATDAT - Dump catalog data symbol table

procedure dg_dcatdat (label, ctable, nvars)

char	label[ARB]		# string label
pointer	ctable			# catalog data symbol table
int	nvars			# number of catalog variables

int	n
int	fd
pointer	sym

bool	clgetb()
int	open()
pointer	sthead(), stnext(), stname()

begin
	# Debug ?
	if (!clgetb ("debug.cattable"))
	    return

	# Open the dump file.
	iferr (fd = open (DUMPFILE, APPEND, TEXT_FILE))
	    return

	# Log the time.
	call dg_ptime (fd, label)

	# Test the pointer.
	if (ctable == NULL) {
	    call fprintf (fd, "dg_dcatat: Null table pointer\n")
	    call close (fd)
	    return
	}

	# Print the title.
	call fprintf (fd, "dg_dcatdat: (ctable=%d) (nvars=%d)\n")
	    call pargi (ctable)
	    call pargi (nvars)

	# Print the values in reverse order.
	sym = sthead (ctable)
	while (sym != NULL) {

	    # Print the matching name.
	    call fprintf (fd, "%d: (%s) ")
		call pargi (sym)
		call pargstr (Memc[stname (ctable, sym)])

	    # Print the indices.
	    do n = 1, nvars {
		call fprintf (fd, "%g ")
		    call pargr (Memr[P2R(sym + n - 1)])
	    }

	    # Skip one line.
	    call fprintf (fd, "\n")

	    # Advance to the next symbol.
	    sym = stnext (ctable, sym)
	}

	# Close the file.
	call close (fd)
end


# DG_DCATOBS - Dump the catalog observation table.

procedure dg_dcatobs (label, otable)

char	label[ARB]		# string label
pointer	otable			# observation table

int	fd
int	row, col

bool	clgetb()

int	open()
int	mct_nrows(), mct_maxcol(), mct_ncols()
real	mct_getr()

begin
	# Debug ?
	if (!clgetb ("debug.obstable"))
	    return

	# Open the dump file.
	iferr (fd = open (DUMPFILE, APPEND, TEXT_FILE))
	    return

	# Log the time.
	call dg_ptime (fd, label)

	# Test the table pointer.
	if (otable == NULL) {
	    call fprintf (fd, "dg_dcatobs: Null observation table pointer\n")
	    call close (fd)
	    return
	}

	# Print the title.
	call fprintf (fd,
	    "dg_dcatobs: (otable=%d) (maxcols=%d) (nrows=%d) (ncols=%d\n")
	    call pargi (otable)
	    call pargi (mct_maxcol (otable))
	    call pargi (mct_nrows (otable))
	    call pargi (mct_ncols (otable))

	# Loop over all data in the table.
	do row = 1, mct_nrows (otable) {

	    # Print the running number.
	    call fprintf (fd, "%3d : ")
		call pargi (row)

	    # Print the values.
	    do col = 1, mct_maxcol (otable) {
		call fprintf (fd, "%g ")
		    call pargr (mct_getr (otable, row, col))
	    }

	    # Skip one line.
	    call fprintf (fd, "\n")
	}

	# Close the file.
	call close (fd)
end


# DG_DREF - Dump the reference table.

procedure dg_dref (label, rtable)

char	label[ARB]		# string label
pointer	rtable			# reference table

int	n
int	fd

bool	clgetb()
int	open()
int	mct_nrows(), mct_maxcol(), mct_ncols()
real	mct_getr()

begin
	# Debug ?
	if (!clgetb ("debug.reftable"))
	    return

	# Open the dump file.
	iferr (fd = open (DUMPFILE, APPEND, TEXT_FILE))
	    return

	# Log the time.
	call dg_ptime (fd, label)

	# Test the pointer.
	if (rtable == NULL) {
	    call fprintf (fd, "dg_dref: Null reference table pointer\n")
	    call close (fd)
	    return
	}

	# Print the title.
	call fprintf (fd,
	    "dg_dref: (rtable=%d) (maxcols=%d) (nrows=%d) (ncols=%d)\n")
	    call pargi (rtable)
	    call pargi (mct_maxcol (rtable))
	    call pargi (mct_nrows (rtable))
	    call pargi (mct_ncols (rtable))

	# Print the values.
	do n = 1, mct_nrows (rtable) {
	    call fprintf (fd, "%d: %g\n")
		call pargi (n)
		call pargr (mct_getr (rtable, n, 1))
	}

	# Close the file.
	call close (fd)
end


# DG_DWEIGHTS - Dump the weight table.

procedure dg_dweights (label, wtable)

char	label[ARB]		# string label
pointer	wtable			# weight table

int	n
int	fd

bool	clgetb()
int	open()
int	mct_nrows(), mct_maxcol(), mct_ncols()
real	mct_getr()

begin
	# Debug ?
	if (!clgetb ("debug.wtstable"))
	    return

	# Open the dump file.
	iferr (fd = open (DUMPFILE, APPEND, TEXT_FILE))
	    return

	# Log the time.
	call dg_ptime (fd, label)

	# Test the table pointer.
	if (wtable == NULL) {
	    call fprintf (fd, "dg_dweight: Null weight table pointer\n")
	    call close (fd)
	    return
	}

	# Print the title.
	call fprintf (fd,
	    "dg_dweight: (wtable=%d) (maxcols=%d) (nrows=%d) (ncols=%d)\n")
	    call pargi (wtable)
	    call pargi (mct_maxcol (wtable))
	    call pargi (mct_nrows (wtable))
	    call pargi (mct_ncols (wtable))

	# Print the values.
	do n = 1, mct_nrows (wtable) {
	    call fprintf (fd, "%d: %g\n")
		call pargi (n)
		call pargr (mct_getr (wtable, n, 1))
	}

	# Close the file.
	call close (fd)
end
