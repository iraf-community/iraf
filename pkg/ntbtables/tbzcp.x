include "tbtables.h"

# tbzcp[tbirds] -- put values into a column
# This procedure puts elements into an internal buffer corresponding
# to values in a text file.
#
# Phil Hodge,  3-Feb-1992  Subroutines created.
# Phil Hodge, 31-Mar-1993  Include short datatype.
# Phil Hodge,  4-Mar-1998  Remove calls to tbtwer.
# Phil Hodge,  5-Mar-1998  In tbzcpt, remove lenstr from call to tbzptt.

procedure tbzcpb (tp, cp, buffer, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
bool	buffer[ARB]		# i: buffer containing values to be put
int	firstrow		# i: number of first row to put
int	lastrow			# i: number of last row to put
#--
int	row			# loop index for row number
int	k			# index into buffer
errchk	tbzptb

begin
	k = 1
	do row = firstrow, lastrow {
	    call tbzptb (tp, cp, row, buffer[k])
	    k = k + 1
	}
end

procedure tbzcpd (tp, cp, buffer, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
double	buffer[ARB]		# i: buffer containing values to be put
int	firstrow		# i: number of first row to put
int	lastrow			# i: number of last row to put
#--
int	row			# loop index for row number
int	k			# index into buffer
errchk	tbzptd

begin
	if (COL_DTYPE(cp) == TBL_TY_DOUBLE) {
	    k = 1
	    do row = firstrow, lastrow {
		Memd[COL_OFFSET(cp) + row - 1] = buffer[k]
		k = k + 1
	    }

	} else {
	    k = 1
	    do row = firstrow, lastrow {
		call tbzptd (tp, cp, row, buffer[k])
		k = k + 1
	    }
	}
end

procedure tbzcpr (tp, cp, buffer, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
real	buffer[ARB]		# i: buffer containing values to be put
int	firstrow		# i: number of first row to put
int	lastrow			# i: number of last row to put
#--
int	row			# loop index for row number
int	k			# index into buffer
errchk	tbzptr

begin
	k = 1
	do row = firstrow, lastrow {
	    call tbzptr (tp, cp, row, buffer[k])
	    k = k + 1
	}
end

procedure tbzcpi (tp, cp, buffer, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
int	buffer[ARB]		# i: buffer containing values to be put
int	firstrow		# i: number of first row to put
int	lastrow			# i: number of last row to put
#--
int	row			# loop index for row number
int	k			# index into buffer
errchk	tbzpti

begin
	if (COL_DTYPE(cp) == TBL_TY_INT) {
	    k = 1
	    do row = firstrow, lastrow {
		Memi[COL_OFFSET(cp) + row - 1] = buffer[k]
		k = k + 1
	    }

	} else {
	    k = 1
	    do row = firstrow, lastrow {
		call tbzpti (tp, cp, row, buffer[k])
		k = k + 1
	    }
	}
end

procedure tbzcps (tp, cp, buffer, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
short	buffer[ARB]		# i: buffer containing values to be put
int	firstrow		# i: number of first row to put
int	lastrow			# i: number of last row to put
#--
int	row			# loop index for row number
int	k			# index into buffer
errchk	tbzpts

begin
	k = 1
	do row = firstrow, lastrow {
	    call tbzpts (tp, cp, row, buffer[k])
	    k = k + 1
	}
end

procedure tbzcpt (tp, cp, buffer, lenstr, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
char	buffer[lenstr,ARB]	# i: buffer containing values to be put
int	lenstr			# i: size of each element
int	firstrow		# i: number of first row to put
int	lastrow			# i: number of last row to put
#--
int	row			# loop index for row number
int	k			# index into buffer
errchk	tbzptt

begin
	k = 1
	do row = firstrow, lastrow {
	    call tbzptt (tp, cp, row, buffer[1,k])
	    k = k + 1
	}
end
