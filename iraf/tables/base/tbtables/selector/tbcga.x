include <tbset.h>

# tbcga[] -- get an array of elements
# This routine gets an array of values, all elements from all selected rows.
# The number of elements in one row may have been reduced by the use of an
# array section, however, in which case only elements in the section will
# be copied to output.
#
# The function value will be the actual number of elements returned
# in the output buffer.  It is an error if the output buffer is not
# large enough to contain all of the values.
#
# Phil Hodge,  5-Mar-1998  Function created.
# Phil Hodge, 18-Jun-1998  Error check the subroutines.

long procedure tbcgad (tp, cp, buffer, nelem)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
double	buffer[ARB]	# o: values read from table
long	nelem		# i: maximum number of elements to read
#--
pointer descrip		# column selector descriptor
long	nrows		# number of selected rows
long	row		# loop index for selected row number
long	nvals		# number of elements in one cell
long	nret		# number returned, should be the same as nvals
long	i
long	c_1
long	tbagtd()
errchk	tbagtd(), tbegtd(), tcs_rdaryd()

begin
	c_1 = 1

	# Get descrip, nvals, and nrows.
	call tbcnel1 (tp, cp, descrip, nvals, nrows)

	# Set nret because tbegtd doesn't return it.
	if (nvals == 1)
	    nret = 1

	if (nvals * nrows > nelem)
	    call error (1, "tbcgad:  output buffer is too small")

	i = 1
	do row = 1, nrows {

	    if (descrip == NULL) {
		if (nvals == 1)
		    call tbegtd (tp, cp, row, buffer[i])
		else
		    nret = tbagtd (tp, cp, row, buffer[i], c_1, nvals)
	    } else {
		call tcs_rdaryd (tp, descrip, row, nelem-i+1, nret, buffer[i])
	    }

	    if (nret != nvals)
		call error (1, "tbcgad:  not all elements read from column")

	    i = i + nvals
	}

	return (i - 1)
end

long procedure tbcgar (tp, cp, buffer, nelem)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
real	buffer[ARB]	# o: values read from table
long	nelem		# i: maximum number of elements to read
#--
pointer descrip		# column selector descriptor
long	nrows		# number of selected rows
long	row		# loop index for selected row number
long	nvals		# number of elements in one cell
long	nret		# number returned, should be the same as nvals
long	i
long	c_1
long	tbagtr()
errchk	tbagtr(), tbegtr(), tcs_rdaryr()

begin
	c_1 = 1

	# Get descrip, nvals, and nrows.
	call tbcnel1 (tp, cp, descrip, nvals, nrows)

	# Set nret because tbegtd doesn't return it.
	if (nvals == 1)
	    nret = 1

	if (nvals * nrows > nelem)
	    call error (1, "tbcgar:  output buffer is too small")

	i = 1
	do row = 1, nrows {

	    if (descrip == NULL) {
		if (nvals == 1)
		    call tbegtr (tp, cp, row, buffer[i])
		else
		    nret = tbagtr (tp, cp, row, buffer[i], c_1, nvals)
	    } else {
		call tcs_rdaryr (tp, descrip, row, nelem-i+1, nret, buffer[i])
	    }

	    if (nret != nvals)
		call error (1, "tbcgar:  not all elements read from column")

	    i = i + nvals
	}

	return (i - 1)
end
