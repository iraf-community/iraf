# This routine trims elements from the end of the xout array by
# decrementing nelem.  Values are trimmed if they are INDEF or
# equal to padvalue (and padvalue itself is not INDEF).  Trimming
# starts at the end and stops with the first value that is not
# INDEF and not equal to padvalue.
#
# Phil Hodge, 26-Apr-2000

procedure tu_trim (xout, nelem, padvalue)

double	xout[ARB]	# i: independent variable values
int	nelem		# io: size of xout array
double	padvalue	# i: trim these values at end of xout array
#--
int	curr_nelem	# current value of nelem
int	i

begin
	curr_nelem = nelem

	if (!IS_INDEFD(padvalue)) {

	    # Check for either INDEF or padvalue.
	    do i = curr_nelem, 1, -1 {
		if (IS_INDEFD(xout[i]))
		    nelem = nelem - 1
		else if (xout[i] == padvalue)
		    nelem = nelem - 1
		else			# neither INDEF nor a pad value
		    break
	    }

	} else {

	    # Just check for INDEF.
	    do i = curr_nelem, 1, -1 {
		if (IS_INDEFD(xout[i]))
		    nelem = nelem - 1
		else			# not INDEF
		    break
	    }
	}
end
