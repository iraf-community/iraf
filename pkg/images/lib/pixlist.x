# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

.help pixels xtools "Pixel List Handling Tools"
.ih
PURPOSE
These routines provide simple pixel list handling facilities and are
intended as a temporary facility pending full scale completion of
image masking. The list is stored in the form of ranges as a function
of line number. Each image line has a offset which may be NULL for
no entry or an offset into the list itself. The actual list is a set of
ranges with the ranges for each line delimited by a NULL. Routines
exist to fetch the ranges for a given line, add or append ranges to a
given line, fetch the next or previous line number with a non-NULL
range and specify whether two lines have the same ranges. At present
the list can grow indefinitely, with additional memory being added as
necessary. No attempt is made to clean up redundant entries though
such a faclity could easily be added. The ranges arguments conform
with the design of the ranges routinesr, with each range consisting
of and intitial and final entry and a step size. A list of ranges
is terminated with a NULL
.ih
PROCEDURE
.nf
plinit (pl, ncols, nlines)

	pointer	pl		# pointer to list descriptor
	int	ncols		# number of image columns
	int	nlines		# number of image lines

nranges = pl_get_ranges (pl, lineno, ranges, max_nranges)

	pointer	pl		# pointer to list descriptor
	int	lineno		# line number of ranges to be fetched
	int	ranges[ARB]	# ranges to be output
	int	max_nranges	# the maximum number of ranges to be output

pl_put_ranges (pl, linemin, linemax, ranges)

	pointer	pl		# pointer to list descriptor
	int	linemin		# minimum line number
	int	linemax		# maximum line number
	int	ranges[ARB]	# ranges to be added to list

pl_append_ranges (pl, linemin, linemax, ranges)

	pointer	pl		# pointer to list descriptor
	int	linemin		# minimum line number
	int	linemax		# maximum line number
	int	ranges[ARB]	# ranges to be added to list

next_lineno/EOF = pl_nextlineno (pl, current_lineno)

	pointer	pl		# pointer to list descriptor
	int	current_lineno	# current line number

prev_lineno/EOF = pl_prevlineno (pl, current_lineno)

	pointer	pl		# pointer to the list descriptor
	int	current_lineno	# current line number

YES/NO = pl_eqlines (pl, line1, line2)

	pointer	pl		# pointer to the list descriptor
	int	line1		# first line number
	int	line2		# second line number

plfree	(pl)

	pointer	pl		# pointer to list descriptor
.fi
.endhelp

include "pixlist.h"

# PL_ADD_RANGES --  Procedure to add the ranges for a given range of
# line numbers to the pixel list. The new ranges will be appended to any
# previously existing ranges for the specified line numbers.

procedure pl_add_ranges (pl, linemin, linemax, ranges)

pointer	pl		# pointer to the list descriptor
int	linemin		# minimum line number
int	linemax		# maximum line number
int	ranges[ARB]	# ranges

int	i, j, lc
int	olp, lp, lnull, lold
int	nr, nnewr, noldr

begin
	# check conditions
	if ((linemin < 1) || (linemax > PL_NLINES(pl)) || linemin > linemax)
	    return

	# calculate the length of the range to be appended minus the null
	nr = 0
	while (ranges[nr+1] != NULL)
	    nr = nr + 1


	lc = 1
	olp = -1
	do i = linemin, linemax {

	    # get offset for line i
	    lp = Memi[PL_LINES(pl)+i-1]

	    # if line pointer is undefined
	    if (lp == NULL) {

		if (lc == 1) {

		    # set line pointer and store
	    	    Memi[PL_LINES(pl)+i-1] = PL_LP(pl)
		    lnull = PL_LP(pl)

		    # check the size of the list
		    if (PL_SZLIST(pl) < (nr + PL_LP(pl))) {
	    		PL_SZLIST(pl) = PL_SZLIST(pl) + nr + 1 
	    		call realloc (PL_LIST(pl), PL_SZLIST(pl), TY_INT)
		    }

		    # move ranges and reset pointers
		    call amovi (ranges, Memi[PL_LIST(pl)+PL_LP(pl)-1], nr)
		    PL_LP(pl) = PL_LP(pl) + nr + 1
		    Memi[PL_LIST(pl)+PL_LP(pl)-2] = NULL
		    lc = lc + 1

		} else

		    # set line pointer
		    Memi[PL_LINES(pl)+i-1] = lnull

	    } else {

		if (lp != olp) {

		    # set line pointer and store
	    	    Memi[PL_LINES(pl)+i-1] = PL_LP(pl)
		    lold = PL_LP(pl)

		    # find length of previously defined range and calculate
		    # length of new ranges
		    for (j = lp; Memi[PL_LIST(pl)+j-1] != NULL; j = j + 1)
		        ;
		    noldr = j - lp
		    nnewr = noldr + nr

		    # check size of list
	            if (PL_SZLIST(pl) < (nnewr + PL_LP(pl))) {
		        PL_SZLIST(pl) = PL_SZLIST(pl) + nnewr + 1
	                call realloc (PL_LIST(pl), PL_SZLIST(pl), TY_INT)
		    }

		    # add ranges to list and update pointers
		    call amovi (Memi[PL_LIST(pl)+lp-1],
		        Memi[PL_LIST(pl)+PL_LP(pl)-1], noldr)
		    PL_LP(pl) = PL_LP(pl) + noldr
		    call amovi (ranges, Memi[PL_LIST(pl)+PL_LP(pl)-1], nr)
		    PL_LP(pl) = PL_LP(pl) + nr + 1
		    Memi[PL_LIST(pl)+PL_LP(pl)-2] = NULL

		} else

		    # set line pointers
		    Memi[PL_LINES(pl)+i-1] = lold

		olp = lp
	    }
	}

end

# PL_EQLINES -- Routine to test whether two lines have equal ranges.
# The routine returns YES or NO.

int procedure pl_eqlines (pl, line1, line2)

pointer	pl	# pointer to the list
int	line1	# line numbers
int	line2

begin
	if (Memi[PL_LINES(pl)+line1-1] == Memi[PL_LINES(pl)+line2-1])
	    return (YES)
	else
	    return (NO)
end

# PL_GET_RANGES -- Procedure to fetch the ranges for the specified lineno.
# Zero is returned if there are no ranges otherwise the number of ranges
# are returned. The ranges are stored in an integer array. Three positive
# numbers are used to define a range a minimum, maximum and a step size.
# The ranges are delimited by a NULL.

int procedure pl_get_ranges (pl, lineno, ranges, max_nranges)

pointer	pl		# pointer to the pixel list descriptor
int	lineno		# line number
int	ranges[ARB]	# array of ranges
int	max_nranges	# the maximum number of ranges

int	lp, ip
int	nranges

begin
	# check for existence of ranges
	if (Memi[PL_LINES(pl)+lineno-1] == NULL) {
	    ranges[1] = NULL
	    return (0)
	}

	# set pointer to the first element in list for line lineno
	lp = PL_LIST(pl) + Memi[PL_LINES(pl)+lineno-1] - 1

	# get ranges
	nranges = 0
	ip = 1
	while (Memi[lp+ip-1] != NULL && nranges <= 3 * max_nranges) {
	    ranges[ip] = Memi[lp+ip-1]
	    ip = ip + 1
	    nranges = nranges + 1
	}
	ranges[ip] = NULL

	# return nranges
	if (nranges == 0)
	    return (nranges)
	else
	    return (nranges / 3)
end

# PL_NEXTLINENO -- Procedure to fetch the next line number with a set of
# defined ranges given the current line number. Note that the current
# line number is updated.

int procedure pl_nextlineno (pl, current_lineno)

pointer	pl			# pointer to the pixel list descriptor
int	current_lineno		# current line number

int findex, lp

begin
	findex = max (1, current_lineno + 1)
	do lp = findex, PL_NLINES(pl) {
	    if (Memi[PL_LINES(pl)+lp-1] != NULL) {
		current_lineno = lp
		return (lp)
	    }
	}

	return (EOF)
end

# PL_PREVLINENO -- Procedure to fetch the first previous line number
# with a set of defined ranges given the current line number.
# Note that the current line number is updated.

int procedure pl_prevlineno (pl, current_lineno)

pointer	pl		# pointer to the pixel list descriptor
int	current_lineno	# current line number

int	findex, lp

begin
	findex = min (current_lineno - 1, PL_NLINES(pl))
	do lp = findex, 1, -1 {
	    if (Memi[PL_LINES(pl)+lp-1] != NULL) {
		current_lineno = lp
		return (lp)
	    }
	}

	return (EOF)
end

# PL_PUT_RANGES --  Procedure to add the ranges for a given range of
# lines to the pixel list. Note that any previously defined ranges are
# lost.

procedure pl_put_ranges (pl, linemin, linemax, ranges)

pointer	pl		# pointer to the list
int	linemin		# minimum line
int	linemax		# maximum line
int	ranges[ARB]	# list of ranges

int	i
int	len_range

begin
	# check boundary conditions
	if ((linemin < 1) || (linemax > PL_NLINES(pl)) || (linemin > linemax))
	    return

	# determine length of range string minus the NULL
	len_range = 0
	while (ranges[len_range+1] != NULL)
	    len_range = len_range + 1

	# check space allocation
	if (PL_SZLIST(pl) < (len_range + PL_LP(pl))) {
	    PL_SZLIST(pl) = PL_SZLIST(pl) + len_range + 1
	    call realloc (PL_LIST(pl), PL_SZLIST(pl), TY_INT)
	}

	# set the line pointers
	do i = linemin, linemax
	    Memi[PL_LINES(pl)+i-1] = PL_LP(pl)

	# add ranges
	call amovi (ranges, Memi[PL_LIST(pl)+PL_LP(pl)-1], len_range)
	PL_LP(pl) = PL_LP(pl) + len_range + 1
	Memi[PL_LIST(pl)+PL_LP(pl)-2] = NULL
end

# PLFREE -- Procedure to free the pixel list descriptor

procedure plfree (pl)

pointer	pl		# pointer to pixel list descriptor

begin
	if (pl == NULL)
	    return

	if (PL_LIST(pl) != NULL)
	    call mfree (PL_LIST(pl), TY_INT)
	if (PL_LINES(pl) != NULL)
	    call mfree (PL_LINES(pl), TY_INT)

	call mfree (pl, TY_STRUCT)
end

# PLINIT -- Procedure to initialize the pixel list. Ncols and nlines are
# the number of columns and lines respectively in the associated IRAF
# image.

procedure plinit (pl, ncols, nlines)

pointer	pl		# pixel list descriptor
int	ncols		# number of image columns 
int	nlines		# number of image lines

begin
	# allocate space for a pixel list descriptor
	call malloc (pl, LEN_PLSTRUCT, TY_STRUCT)

	# initialize
	PL_NCOLS(pl) = ncols
	PL_NLINES(pl) = nlines

	# allocate space for the line pointers
	call malloc (PL_LINES(pl), PL_NLINES(pl), TY_INT) 
	call amovki (NULL, Memi[PL_LINES(pl)], PL_NLINES(pl))

	# set pointer to next free element
	PL_LP(pl) = 1

	# allocate space for the actual list
	call malloc (PL_LIST(pl), PL_NLINES(pl), TY_INT)
	PL_SZLIST(pl) = PL_NLINES(pl)
end
