include "tbtables.h"

# tbzcg[tbirds] -- get values from a column
# This procedure gets elements from an internal buffer corresponding
# to a value in a text file.
#
# Phil Hodge,  3-Feb-1992  Subroutines created.
# Phil Hodge, 31-Mar-1993  Include short datatype.

procedure tbzcgb (tp, cp, buffer, nullflag, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
bool	buffer[ARB]		# o: buffer for values to be gotten
bool	nullflag[ARB]		# o: true if element is undefined in table
int	firstrow		# i: number of first row to get
int	lastrow			# i: number of last row to get
#--
int	row			# loop index for row number
int	k			# index into buffer & nullflag
errchk	tbzgtb

begin
	k = 1
	do row = firstrow, lastrow {
	    call tbzgtb (tp, cp, row, buffer[k])
	    nullflag[k] = false
	    k = k + 1
	}
end

procedure tbzcgd (tp, cp, buffer, nullflag, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
double	buffer[ARB]		# o: buffer for values to be gotten
bool	nullflag[ARB]		# o: true if element is undefined in table
int	firstrow		# i: number of first row to get
int	lastrow			# i: number of last row to get
#--
int	row			# loop index for row number
int	k			# index into buffer & nullflag
errchk	tbzgtd

begin
	if (COL_DTYPE(cp) == TBL_TY_DOUBLE) {
	    k = 1
	    do row = firstrow, lastrow {
		buffer[k] = Memd[COL_OFFSET(cp) + row - 1]
		nullflag[k] = (IS_INDEFD (buffer[k]))
		k = k + 1
	    }

	} else {
	    k = 1
	    do row = firstrow, lastrow {
		call tbzgtd (tp, cp, row, buffer[k])
		nullflag[k] = (IS_INDEFD (buffer[k]))
		k = k + 1
	    }
	}
end

procedure tbzcgr (tp, cp, buffer, nullflag, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
real	buffer[ARB]		# o: buffer for values to be gotten
bool	nullflag[ARB]		# o: true if element is undefined in table
int	firstrow		# i: number of first row to get
int	lastrow			# i: number of last row to get
#--
int	row			# loop index for row number
int	k			# index into buffer & nullflag
errchk	tbzgtr

begin
	k = 1
	do row = firstrow, lastrow {
	    call tbzgtr (tp, cp, row, buffer[k])
	    nullflag[k] = (IS_INDEFR (buffer[k]))
	    k = k + 1
	}
end

procedure tbzcgi (tp, cp, buffer, nullflag, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
int	buffer[ARB]		# o: buffer for values to be gotten
bool	nullflag[ARB]		# o: true if element is undefined in table
int	firstrow		# i: number of first row to get
int	lastrow			# i: number of last row to get
#--
int	row			# loop index for row number
int	k			# index into buffer & nullflag
errchk	tbzgti

begin
	if (COL_DTYPE(cp) == TBL_TY_INT) {
	    k = 1
	    do row = firstrow, lastrow {
		buffer[k] = Memi[COL_OFFSET(cp) + row - 1]
		nullflag[k] = (IS_INDEFI (buffer[k]))
		k = k + 1
	    }

	} else {
	    k = 1
	    do row = firstrow, lastrow {
		call tbzgti (tp, cp, row, buffer[k])
		nullflag[k] = (IS_INDEFI (buffer[k]))
		k = k + 1
	    }
	}
end

procedure tbzcgs (tp, cp, buffer, nullflag, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
short	buffer[ARB]		# o: buffer for values to be gotten
bool	nullflag[ARB]		# o: true if element is undefined in table
int	firstrow		# i: number of first row to get
int	lastrow			# i: number of last row to get
#--
int	row			# loop index for row number
int	k			# index into buffer & nullflag
errchk	tbzgts

begin
	k = 1
	do row = firstrow, lastrow {
	    call tbzgts (tp, cp, row, buffer[k])
	    nullflag[k] = (IS_INDEFS (buffer[k]))
	    k = k + 1
	}
end

procedure tbzcgt (tp, cp, buffer, nullflag, lenstr,
		firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
char	buffer[lenstr,ARB]	# o: buffer for values to be gotten
bool	nullflag[ARB]		# o: true if element is undefined in table
int	lenstr			# i: size of each element
int	firstrow		# i: number of first row to get
int	lastrow			# i: number of last row to get
#--
int	row			# loop index for row number
int	k			# index into buffer & nullflag
bool	streq()
errchk	tbzgtt

begin
	k = 1
	do row = firstrow, lastrow {
	    call tbzgtt (tp, cp, row, buffer[1,k], lenstr)
	    nullflag[k] = (buffer[1,k] == EOS || streq (buffer[1,k], "INDEF"))
	    k = k + 1
	}
end
