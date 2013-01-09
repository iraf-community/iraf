.help  -----------------------------------------------------------------
RST -- Functions used to manipulate row sets

A row set is a structure used to represent some boolean condition over
the rows of a table. Rows for which the condition is true are included
in the set. The structure stores row numbers as an array of
ranges. The structure also contains the cumulative number of rows up
to the end of the range for each range in order to assist in searching
for the i-th row in the set.

.nf
Create and destroy a row set

set = rst_create (loval, hival)
set2 = rst_copy (set1)
call rst_free (set)

Add or delete a row from the set

call rst_addval (set, value)
call rst_delval (set, value)

Update set to match insertion or deletions to table

call rst_addtab (set, loval, nval)
call rst_deltab (set, loval, nval)

Logical operations on a set

set3 = rst_and (set1, set2)
set3 = rst_or (set1, set2)
set2 = rst_not (nrow, set1)

Check to see if a row is in the set

found = rst_inset (set, value)

Get number of rows in the set

count = rst_nelem (set)

Retrieve the i-th row from the set

row = rst_rownum (set, index)

Make a string representation of a set

call rst_show (set, str, maxch)

.fi

See the comments in the source for more information on the use of
these functions. Or ask Bernie Simon (bsimon@stsci.edu).

.endhelp ---------------------------------------------------------------

define	LEN_RST		6		# length of row set structure
define	LEN_TAIL	5		# length of tail structure

define	RST_LAST	Memi[$1]	# last element in row set
define	RST_MAX		Memi[$1+1]	# max elements in row set
define	RST_CURRENT	Memi[$1+2]	# current element in row set	
define	RST_LOARY	Memi[$1+3]	# array of low range ends
define	RST_HIARY	Memi[$1+4]	# array of high range ends
define	RST_NUMARY	Memi[$1+5]	# array of cumulative number of rows

define	RST_LOVAL	Memi[RST_LOARY($1)+($2)-1]
define	RST_HIVAL	Memi[RST_HIARY($1)+($2)-1]
define	RST_NROW	Memi[RST_NUMARY($1)+($2)-1]

# RST_ADDTAB -- Update set to reflect inserted rows in underlying table
#
# The important point is rows are inserted *after* loval and loval is 
# not modified. All inserted rows are added to the set. Values after
# the range are increased by the number of values in the range.

procedure rst_addtab (set, loval, nval)

pointer	set		# i: row set
int	loval		# i: rows are inserted after this row
int	nval		# i: number of rows inserted
#--
int	idx, ndx, hival, range[2]
pointer	tail

int	rst_findloc()
pointer	rst_tail()

begin
	# Find range where new rows are inserted in the table

	idx = rst_findloc (set, loval + 1)

	# Handle the simple case where new rows are beyond rows already in set

	if (idx > RST_LAST(set)) {
	    call rst_addrange (set, loval + 1, loval + nval)
	    return
	}

	# Check for union with existing range

	hival = loval + nval

	if (loval + 1 <  RST_LOVAL(set,idx)) {
	    range[1] = loval + 1
	    range[2] = hival
	    ndx = 0

	} else {
	    range[1] = RST_LOVAL(set,idx)
	    range[2] = RST_HIVAL(set,idx) + nval
	    ndx = 1
	}

	# Save tail of set and truncate set

	tail = rst_tail (set, idx + ndx)
	RST_LAST(set) = idx - 1

	# Add range

	call rst_addrange (set, range[1], range[2])

	# Add tail of set, shifting rows by number of inserted rows

	call rst_concat (set, tail, nval)
	call rst_notail (tail)
end

# RST_ADDVAL -- Add a value to a set
#
# Modify the set by adding a single row. The set is modified in place.
# If this function is called more than once in succession, it will be 
# most efficient to order the values before adding them.

procedure rst_addval (set, value)

pointer	set		# i: row set
int	value		# i:value to add
#--
int	idx
pointer	tail

int	rst_findloc()
pointer	rst_tail()

begin
	# Find the location of the value in the set

	idx = rst_findloc (set, value)

	# Handle values past the end of the set as a special case

	if (idx > RST_LAST(set)) {
	    call rst_addrange (set, value, value)
	    return
	}

	# Return if the value is already in the set

	if (value >= RST_LOVAL(set,idx))
	    return

	# Save the tail of the current set and then truncate it

	tail = rst_tail (set, idx)
	RST_LAST(set) = idx - 1

	# Add value to the set

	call rst_addrange (set, value, value)

	# Restore the tail to the set

	call rst_concat (set, tail, 0)
	call rst_notail (tail)

end

# RST_AND -- Intersection of two row sets
#
# Do a logical AND, or intersection, of two sets producing a third set.

pointer procedure rst_and (set1, set2)

pointer	set1		# i: first row set
pointer	set2		# i: second row set
#--
int	idx1, idx2, loval3, loval4, hival3, hival4
pointer	set3

pointer	rst_create()

begin
	# Create output row set

	set3 = rst_create (0, 0)

	# Main loop: intersection of two sets

	idx1 = 1
	idx2 = 1
	loval3 = 0

	while (idx1 <= RST_LAST(set1) && idx2 <= RST_LAST(set2)) {

	    # If the output range is not set yet, set it
	    # Otherwise take the intesection of the input range
	    # with the input range that starts at the lower 
	    # value. Add the intersection to the output set.
	    # When the output range is disjoint with both
	    # input ranges, discard it.

	    if (loval3 == 0) {
		if (RST_LOVAL(set1,idx1) <= RST_LOVAL(set2,idx2)) {
		    loval3 = RST_LOVAL(set1,idx1)
		    hival3 = RST_HIVAL(set1,idx1)
		    idx1 = idx1 + 1

		} else {
		    loval3 = RST_LOVAL(set2,idx2)
		    hival3 = RST_HIVAL(set2,idx2)
		    idx2 = idx2 + 1
		}

	    } else if (RST_LOVAL(set1,idx1) <= RST_LOVAL(set2,idx2)) {
		if (RST_LOVAL(set1,idx1) <= hival3) {
		    loval4 = max (loval3, RST_LOVAL(set1,idx1))
		    hival4 = min (hival3, RST_HIVAL(set1,idx1))

		    call rst_addrange (set3, loval4, hival4)

		    if (RST_HIVAL(set1,idx1) <= hival3) {
			idx1 = idx1 + 1
		    } else {
			loval3 = RST_LOVAL(set2,idx2)
			hival3 = RST_HIVAL(set2,idx2)
			idx2 = idx2 + 1
		    }

		} else {
		    loval3 = 0
		}

	    } else {
		if (RST_LOVAL(set2,idx2) <= hival3) {
		    loval4 = max (loval3, RST_LOVAL(set2,idx2))
		    hival4 = min (hival3, RST_HIVAL(set2,idx2))

		    call rst_addrange (set3, loval4, hival4)

		    if (RST_HIVAL(set2,idx2) <= hival3) {
			idx2 = idx2 + 1
		    } else {
			loval3 = RST_LOVAL(set1,idx1)
			hival3 = RST_HIVAL(set1,idx1)
			idx1 = idx1 + 1
		    }

		} else {
		    loval3 = 0
		}
	    }
	}

	# Take the intersection of the output range
	# with the remaining input range

	while (idx1 <= RST_LAST(set1)) {
	    if (loval3 == 0 || RST_LOVAL(set1,idx1) > hival3) {
		loval3 = 0
		break
	    }

	    if (loval3 <= RST_HIVAL(set1,idx1)) {
		loval4 = max (loval3, RST_LOVAL(set1,idx1))
		hival4 = min (hival3, RST_HIVAL(set1,idx1))
		call rst_addrange (set3, loval4, hival4)
	    }

	    idx1 = idx1 + 1
	}

	while (idx2 <= RST_LAST(set2)) {
	    if (loval3 == 0 || RST_LOVAL(set2,idx2) > hival3) {
		loval3 = 0
		break
	    }

	    if (loval3 <= RST_HIVAL(set2,idx2)) {
		loval4 = max (loval3, RST_LOVAL(set2,idx2))
		hival4 = min (hival3, RST_HIVAL(set2,idx2))
		call rst_addrange (set3, loval4, hival4)
	    }

	    idx2 = idx2 + 1
	}

	return (set3)
end

# RST_COPY -- Create a copy of an existing row set

pointer procedure rst_copy (set1)

pointer	set1		# i: row set
#--
int	last, max
pointer	set2

begin
	call malloc (set2, LEN_RST, TY_INT)

	last = RST_LAST(set1)
	max = RST_MAX(set1)

	call malloc (RST_LOARY(set2), max, TY_INT)
	call malloc (RST_HIARY(set2), max, TY_INT)
	call malloc (RST_NUMARY(set2), max, TY_INT)

	RST_LAST(set2) = last
	RST_MAX(set2) = max
	RST_CURRENT(set2) = 0

	call amovi (RST_LOVAL(set1,1), RST_LOVAL(set2,1), last)
	call amovi (RST_HIVAL(set1,1), RST_HIVAL(set2,1), last)
	call amovi (RST_NROW(set1,1), RST_NROW(set2,1), last)

	return (set2)
end

# RST_CREATE -- Create and initialize a new row set
#
# Create a new set containg a single range. To create an empty set,
# make the range (0,0). If the range limits are out of order, the 
# procedure will swap them.

pointer procedure rst_create (loval, hival)

int	loval		# i: low end of range
int	hival		# i: high end of range
#--
int	temp	
pointer	set

begin
	call malloc (set, LEN_RST, TY_INT)

	call malloc (RST_LOARY(set), 1, TY_INT)
	call malloc (RST_HIARY(set), 1, TY_INT)
	call malloc (RST_NUMARY(set), 1, TY_INT)

	RST_MAX(set) = 1
	RST_CURRENT(set) = 0

	if (loval > hival) {
	    temp = loval
	    loval = hival
	    hival = temp
	}

	if (loval == 0) {
	    RST_LAST(set) = 0

	} else {
	    RST_LAST(set) = 1
	    RST_LOVAL(set,1) = loval
	    RST_HIVAL(set,1) = hival
	    RST_NROW(set,1) = hival - loval + 1
	}

	return (set)
end

# RST_DELTAB -- Update set to reflect deleted rows in underlying table
#
# Update a set after rows have been deleted from the underlying table.
# All values within the deleted range are removed and values above the 
# range are decreased by the number of rows in the range.

procedure rst_deltab (set, loval, nval)

pointer set		# u: row set
int	loval		# i: first row deleted in underlying table
int	nval		# i: number of rows deleted in underlying table
#--
int	idx, jdx, ndx, hival, range[2,2]
pointer	tail

int	rst_findloc()
pointer	rst_tail()

begin
	# Find lower end of intersection of deleted rows with row set

	idx = rst_findloc (set, loval)

	if (idx > RST_LAST(set))
	    return

	# If deleted rows intesect a range in the set, take the intersection

	ndx = 0
	if (loval > RST_LOVAL(set,idx)) {
	    ndx = 1
	    range[1,1] = RST_LOVAL(set,idx)
	    range[2,1] = loval - 1
	}

	# Find the upper end of intersection of deleted rows with the row set
	# hival is the first element past the deleted range

	hival = loval + nval
	jdx = rst_findloc (set, hival)

	# If deleted rows intesect a range in the set, take the intersection
	# Shift row numbers to account for deleted rows

	if (jdx <= RST_LAST(set)) {
	    if (hival > RST_LOVAL(set,jdx)) {
		ndx = ndx + 1
		range[1,ndx] = hival - nval
		range[2,ndx] = RST_HIVAL(set,jdx) - nval
		jdx = jdx + 1
	    }
	}

	# Save the tail of the row set and truncate the set

	tail = rst_tail (set, jdx)
	RST_LAST(set) = idx  - 1

	# Add the modified ranges to the table

	do jdx = 1, ndx
	    call rst_addrange (set, range[1,jdx], range[2,jdx])

	# Add the ranges past the deleted range to the table,
	# shifting row number to account for deleted rows

	call rst_concat (set, tail, - nval)
	call rst_notail (tail)

end

# RST_DELVAL -- Delete a value from a set
#
# Remove a single value from the set. The set is updated in place. If
# this procedure is called several times, it is most effcient to order
# the values before deleting them.

procedure rst_delval (set, value)

pointer	set		# u: row set
int	value		# i:value to add
#--
int	idx, jdx, ndx, range[2,2]
pointer	tail

int	rst_findloc()
pointer	rst_tail()

begin
	# Find the location of the value in the set

	idx = rst_findloc (set, value)

	# Return if the value is not in the set

	if (idx < 1 || idx > RST_LAST(set))
	    return

	if (value < RST_LOVAL(set,idx))
	    return

	# Modify the range containing the element, 
	# which may split the range in two

	if (RST_LOVAL(set,idx) == RST_HIVAL(set, idx)) {
	    ndx = 0

	} else if (value == RST_LOVAL(set,idx)) {
	    range[1,1] = value + 1
	    range[2,1] = RST_HIVAL(set,idx)
	    ndx = 1

	} else if (value == RST_HIVAL(set,idx)) {
	    range[1,1] = RST_LOVAL(set,idx)
	    range[2,1] = value - 1
	    ndx = 1

	} else {
	    range[1,1] = RST_LOVAL(set,idx)
	    range[2,1] = value - 1

	    range[1,2] = value + 1
	    range[2,2] = RST_HIVAL(set,idx)
	    ndx = 2
	}

	# Save the tail of the current set and then truncate it

	tail = rst_tail (set, idx + 1)
	RST_LAST(set) = idx - 1

	# Add the modified ranges to the set

	do jdx = 1, ndx
	    call rst_addrange (set, range[1,jdx], range[2,jdx])

	# Restore the tail to the set

	call rst_concat (set, tail, 0)
	call rst_notail (tail)

end

# RST_FREE -- Free row set structure
#
# Release memory used by the row set

procedure rst_free (set)

pointer	set		# i: row set
#--

begin
	call mfree (RST_NUMARY(set), TY_INT)
	call mfree (RST_HIARY(set), TY_INT)
	call mfree (RST_LOARY(set), TY_INT)

	call mfree (set, TY_INT)
end

# RST_INSET -- Return true if value is in set

bool procedure rst_inset (set, value)

pointer	set		# i: row set
int	value		# i: value to be checked
#--
bool	result
int	idx

int	rst_findloc()

begin
	idx = rst_findloc (set, value)

	if (idx > RST_LAST(set)) {
	    result = false
	} else {
	    result = value >= RST_LOVAL(set,idx)
	}

	return (result)
end

# RST_NELEM -- Number of elements in a set

int procedure rst_nelem (set)

pointer	set		# i: row set
#--
int	nelem

begin
	if (RST_LAST(set) == 0) {
	    nelem = 0
	} else {
	    nelem = RST_NROW(set,RST_LAST(set))
	}

	return (nelem)
end

# RST_NOT -- Complement of a row set
#
# Do a logical NOT, or complement of a set, producing a second set.
# the procedure requires the number of rows in the underlying table to
# know where to stop adding rows. This is the only procedure in this
# file where information about the underlying table is required.

pointer procedure rst_not (nrow, set1)

int	nrow 		# i: largest possible value in set
pointer	set1		# i: set to be negated
#--
int	idx1, loval2, hival2
pointer	set2

pointer	rst_create()

begin
	set2 = rst_create (0,0)

	loval2 = 1
	do idx1 = 1, RST_LAST(set1) {
	    if (loval2 < RST_LOVAL(set1,idx1)) {
		hival2 = RST_LOVAL(set1,idx1) - 1
		call rst_addrange (set2, loval2, hival2)
	    }

	    loval2 = RST_HIVAL(set1,idx1) + 1
	}

	if (loval2 <= nrow) {
	    hival2 = nrow
	    call rst_addrange (set2, loval2, hival2)
	}

	return (set2)
end

# RST_OR -- Union of two row sets
#
# Do the logical OR, or union,of two sets, producing a third set. 

pointer procedure rst_or (set1, set2)

pointer	set1		# i: first row set
pointer	set2		# i: second row set
#--
int	idx1, idx2, loval3, hival3
pointer	set3

pointer	rst_create()

begin
	# Create output row set

	set3 = rst_create (0, 0)

	# Main loop: union of two sets

	idx1 = 1
	idx2 = 1
	loval3 = 0

	while (idx1 <= RST_LAST(set1) && idx2 <= RST_LAST(set2)) {

	    # Set the output range if not yet set, otherwise
	    # take the union of it with the set range that starts
	    # at the lowest value. If the output range is disjoint
	    # with the lower input range, add the output range to
	    # the output set and push back the input range

	    if (loval3 == 0) {
		if (RST_LOVAL(set1,idx1) <= RST_LOVAL(set2,idx2)) {
		    loval3 = RST_LOVAL(set1,idx1)
		    hival3 = RST_HIVAL(set1,idx1)
		    idx1 = idx1 + 1

		} else {
		    loval3 = RST_LOVAL(set2,idx2)
		    hival3 = RST_HIVAL(set2,idx2)
		    idx2 = idx2 + 1
		}

	    } else if (RST_LOVAL(set1,idx1) <= RST_LOVAL(set2,idx2)) {
		if (RST_LOVAL(set1,idx1) <= hival3) {
		    loval3 = min (loval3, RST_LOVAL(set1,idx1))
		    hival3 = max (hival3, RST_HIVAL(set1,idx1))
		    idx1 = idx1 + 1

		} else {
		    call rst_addrange (set3, loval3, hival3)
		    loval3 = 0
		}

	    } else {
		if (RST_LOVAL(set2,idx2) <= hival3) {
		    loval3 = min (loval3, RST_LOVAL(set2,idx2))
		    hival3 = max (hival3, RST_HIVAL(set2,idx2))
		    idx2 = idx2 + 1

		} else {
		    call rst_addrange (set3, loval3, hival3)
		    loval3 = 0
		}
	    }
	}

	# After comparison of two sets is finished, take union
	# of output range with remaining input set. 

	while (loval3 != 0 && idx1 <= RST_LAST(set1)) {
	    if (RST_LOVAL(set1,idx1) <= hival3) {
		loval3 = min (loval3, RST_LOVAL(set1,idx1))
		hival3 = max (hival3, RST_HIVAL(set1,idx1))
		idx1 = idx1 + 1

	    } else {
		call rst_addrange (set3, loval3, hival3)
		loval3 = 0
	    }
	}

	while (loval3 != 0 && idx2 <= RST_LAST(set2)) {
	    if (RST_LOVAL(set2,idx2) <= hival3) {
		loval3 = min (loval3, RST_LOVAL(set2,idx2))
		hival3 = max (hival3, RST_HIVAL(set2,idx2))
		idx2 = idx2 + 1

	    } else {
		call rst_addrange (set3, loval3, hival3)
		loval3 = 0
	    }
	}

	if (loval3 != 0)
	    call rst_addrange (set3, loval3, hival3)

	# When the two are disjoint, copy the remainder of the input set
	#  to the output set.

	while (idx1 <= RST_LAST(set1)) {
	    call rst_addrange(set3, RST_LOVAL(set1,idx1), RST_HIVAL(set1,idx1))
	    idx1 = idx1 + 1
	}

	while (idx2 <= RST_LAST(set2)) {
	    call rst_addrange(set3, RST_LOVAL(set2,idx2), RST_HIVAL(set2,idx2))
	    idx2 = idx2 + 1
	}

	return (set3)
end

# RST_ROWNUM -- Convert an index into the set into a row number
#
# The row number is returned as the function value. If the index is not 
# in the set, the row number is set to zero. The search method used is 
# a compromise between sequential and binary search. The procedure uses 
# the current row pointer as hint on where to locate the new row.

int procedure rst_rownum (set, index)

pointer	set		# i: row set
int	index		# i: index into the set
#--
int	inc, hi, lo, mid, irow

begin
	# Search for a bracket containing the element 
	# we are looking for

	if (RST_CURRENT(set) < 1 || RST_CURRENT(set) > RST_LAST(set)) {
	    # If range is undefined, set the bracket to the entire array

	    lo = 0
	    hi = RST_LAST(set) + 1

	} else {
	    # Do we have the low end of the bracket or the high end?

	    inc = 1
	    if (index <= RST_NROW(set,RST_CURRENT(set))) {
		# Have high end, search for low end

		hi = RST_CURRENT(set)
		repeat {
		    lo = hi - inc
		    if (lo < 1) {
			lo = 0
			break
		    } 

		    if (index > RST_NROW(set,lo))
			break

		    hi = lo
		    inc = 2 * inc
		}

	    } else {
		# Have low, end, search for high end
		lo = RST_CURRENT(set)
		repeat {
		    hi = lo + inc
		    if (hi > RST_LAST(set)) {
			hi = RST_LAST(set) + 1
			break
		    }

		    if (index <= RST_NROW(set,hi))
			break

		    lo = hi
		    inc = 2 * inc
		}
	    }
	}

	# Now that we have a bracket, do a binary search 
	# to locate the range within the bracket

	while (hi > lo + 1) {
	    mid = (lo + hi) / 2
	    if (index > RST_NROW(set,mid)) {
		lo = mid
	    } else {
		hi = mid
	    }
	}

	# Find the row within the range

	if (hi < 1 || hi > RST_LAST(set))  {
	    irow = 0

	} else {
	    irow = RST_HIVAL(set,hi) - (RST_NROW(set,hi) - index)
	    if (irow < 1) {
		irow = 0
		hi = 0
	    }
	}

	RST_CURRENT(set) = hi
	return (irow)
end

# RST_SHOW -- Produce a string representation of the set
#
# Ranges are separated by commas and ranges with more than one value
# are represented by their endpoints separated by a colon. The notation
# is meant to match that used by trseval.

procedure rst_show (set, str, maxch)

pointer	set		# i: row set
char	str[ARB]	# o: string representation of set
int	maxch		# i: maximum length of string
#--
int	ic, idx
int	itoc()

begin
	ic = 1
	do idx = 1, RST_LAST(set) {
	    ic = ic + itoc (RST_LOVAL(set,idx), str[ic], maxch-ic)

	    if (RST_LOVAL(set,idx) != RST_HIVAL(set,idx)) {
		str[ic] = ':'
		ic = ic + 1

		ic = ic + itoc (RST_HIVAL(set,idx), str[ic], maxch-ic)
	    }

	    str[ic] = ','
	    ic = ic + 1
	}

	if (ic > 1)
	    ic = ic - 1

	str[ic] = EOS
end

# ----------------------------------------------------------------------
# Functions below this line are internal and not part of the public 
# interface
# ----------------------------------------------------------------------

# RST_ADDRANGE -- Add a range at the end of a row set (low level)

procedure rst_addrange (set, loval, hival) 

pointer	set		# u: row set
int	loval		# i: low end of range
int	hival		# i: high end of range
#--
int	last, nrow

begin

	last = RST_LAST(set)

	if (last == 0) {
	    nrow = 0

	} else {
	    nrow = RST_NROW(set,last)

	    # Check for union with previous range

	    if (RST_HIVAL(set,last) + 1 == loval) {

		RST_HIVAL(set,last) = hival
		RST_NROW(set,last) = nrow + hival - loval + 1
		return
	    }
	}

	# Increment number of values in arrays

	last = last + 1
	RST_LAST(set) = last

	# Allocate more space if arrays are full

	if (last > RST_MAX(set)) {
	    RST_MAX(set) = 2 * RST_MAX(set)

	    call realloc (RST_LOARY(set), RST_MAX(set), TY_INT)
	    call realloc (RST_HIARY(set), RST_MAX(set), TY_INT)
	    call realloc (RST_NUMARY(set), RST_MAX(set), TY_INT)
	}

	# Set array values

	RST_LOVAL(set,last) = loval
	RST_HIVAL(set,last) = hival
	RST_NROW(set,last) = nrow + hival - loval + 1
end

# RST_CONCAT -- Concatenate a tail structure onto a row set (low level)

procedure rst_concat (set, tail, shift)

pointer	set	# u: row set
pointer	tail	# i: tail structure
int	shift	# i: Amount to shift each value by
#--
int	idx

begin
	do idx = 1, RST_LAST(tail)
	    call rst_addrange (set, RST_LOVAL(tail,idx) + shift, 
			       RST_HIVAL(tail,idx) + shift)

end

# RST_FINDLOC -- Find the location of an element within the set (low level)

int procedure rst_findloc (set, value)

pointer	set		# i: row set
int	value		# i: value whose location is sought
#--
int	inc, hi, lo, mid

begin
	# Search for a bracket containing the element 
	# we are looking for

	if (RST_CURRENT(set) < 1 || RST_CURRENT(set) > RST_LAST(set)) {
	    # If range is undefined, set the bracket to the entire array

	    lo = 0
	    hi = RST_LAST(set) + 1

	} else {
	    # Do we have the low end of the bracket or the high end?

	    inc = 1
	    if (value <= RST_HIVAL(set,RST_CURRENT(set))) {
		# Have high end, search for low end

		hi = RST_CURRENT(set)
		repeat {
		    lo = hi - inc
		    if (lo < 1) {
			lo = 0
			break
		    } 

		    if (value > RST_HIVAL(set,lo))
			break

		    hi = lo
		    inc = 2 * inc
		}

	    } else {
		# Have low, end, search for high end
		lo = RST_CURRENT(set)
		repeat {
		    hi = lo + inc
		    if (hi > RST_LAST(set)) {
			hi = RST_LAST(set) + 1
			break
		    }

		    if (value <= RST_HIVAL(set,hi))
			break

		    lo = hi
		    inc = 2 * inc
		}
	    }
	}

	# Now that we have a bracket, do a binary search 
	# to locate the range within the bracket

	while (hi > lo + 1) {
	    mid = (lo + hi) / 2
	    if (value > RST_HIVAL(set,mid)) {
		lo = mid
	    } else {
		hi = mid
	    }
	}

	RST_CURRENT(set) = hi
	return (hi)
end

# RST_NOTAIL -- Free structure allocated to hold tail (low level)

procedure rst_notail (tail)

pointer	tail		# u: tail structure
#--

begin
	if (RST_HIARY(tail) != NULL)
	    call mfree (RST_HIARY(tail), TY_INT)

	if (RST_LOARY(tail) != NULL)
	    call mfree (RST_LOARY(tail), TY_INT)

	call mfree (tail, TY_INT)
end

# RST_TAIL -- Copy the tail of a row set into another structure (low level)

pointer procedure rst_tail (set, idx)

pointer	set		# i: row set
int	idx		# i: index of where copy starts
#--
pointer	tail

begin
	# Allocate and initialize structure

	call malloc (tail, LEN_TAIL, TY_INT)

	RST_LAST(tail) = max (RST_LAST(set) - idx + 1, 0)
	RST_MAX(tail) = RST_LAST(tail)
	RST_CURRENT(tail) = 0

	if (RST_LAST(tail) == 0)  {
	    # Tail is zero length, don't bother to allocate arrays

	    RST_LOARY(tail) = NULL
	    RST_HIARY(tail) = NULL

	} else {
	    # Allocate memory for data arrays

	    call malloc (RST_LOARY(tail), RST_LAST(tail), TY_INT)
	    call malloc (RST_HIARY(tail), RST_LAST(tail), TY_INT)

	    # Copy data from old structure to data arrays

	    call amovi (RST_LOVAL(set,idx), RST_LOVAL(tail,1), RST_LAST(tail))
	    call amovi (RST_HIVAL(set,idx), RST_HIVAL(tail,1), RST_LAST(tail))
	}

	# Return 
	return (tail)
end
