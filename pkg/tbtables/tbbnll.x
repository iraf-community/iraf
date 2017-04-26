include "tbtables.h"

# tbbnll -- indef record null
# Set a specific element in the INDEF record buffer to undefined.
#
# Phil Hodge, 10-Nov-1987  Pass Memi instead of Memr to tbbeqd.
# Phil Hodge,  2-Aug-1990  The data type for char was assumed to be TY_CHAR.
# Phil Hodge, 30-Mar-1993  Include short datatype.
# Phil Hodge, 29-Jul-1994  Include array option.
# Phil Hodge,  2-Jun-1997  Replace INDEFD with TBL_INDEFD.

procedure tbbnll (tp, colptr)

pointer tp		# i: pointer to table descriptor
pointer colptr		# i: pointer to column descriptor
#--
pointer locn		# Location in INDEF record (unit = SZ_CHAR)
int	nchar		# number of char in entry for current column
short	sbuf
bool	bbuf
int	k

begin
	locn = TB_INDEF(tp) + COL_OFFSET(colptr)
	nchar = COL_LEN(colptr)

	switch (COL_DTYPE(colptr)) {

	case TY_REAL:
	    do k = locn, locn+nchar-SZ_REAL, SZ_REAL
		call tbbeqr (INDEFR, Memc[k])

	case TY_DOUBLE:
	    do k = locn, locn+nchar-SZ_DOUBLE, SZ_DOUBLE
		call tbbeqd (TBL_INDEFD, Memc[k])

	case TY_INT:
	    do k = locn, locn+nchar-SZ_INT32, SZ_INT32
		call tbbeqi (INDEFI, Memc[k])

	case TY_SHORT:
	    sbuf = INDEFS
	    do k = locn, locn+nchar-SZ_SHORT, SZ_SHORT
		call tbbeqs (sbuf, Memc[k])

	case TY_BOOL:
	    bbuf = false
	    do k = locn, locn+nchar-SZ_BOOL, SZ_BOOL
		call tbbeqb (bbuf, Memc[k])

	case TY_CHAR:
	    Memc[locn] = EOS

	default:			# datatype < 0 implies character
	    do k = 0, nchar-1		# zero indexed
		Memc[locn+k] = EOS
	}
end

# tbbeqd -- assign a double-precision value
# The purpose of this is to assign the input value of type double to
# a character output buffer.

procedure tbbeqd (input, output)

double	input			# i: input double-precision value
char	output[ARB]		# o: same as input, bit for bit
#--
double	buf			# local copy of input
char	cbuf[SZ_DOUBLE]		# will be copied to output
int	i
#equivalence (buf, cbuf)

begin
	buf = input
	do i = 1, SZ_DOUBLE
	    output[i] = cbuf[i]
end

# tbbeqr -- assign a single-precision value
# The purpose of this is to assign the input value of type real to
# a character output buffer.

procedure tbbeqr (input, output)

real	input			# i: input single-precision value
char	output[ARB]		# o: same as input, bit for bit
#--
real	buf			# local copy of input
char	cbuf[SZ_REAL]		# will be copied to output
int	i
#equivalence (buf, cbuf)

begin
	buf = input
	do i = 1, SZ_REAL
	    output[i] = cbuf[i]
end

# tbbeqi -- assign an integer value
# The purpose of this is to assign the input value of type integer to
# a character output buffer.

procedure tbbeqi (input, output)

int	input			# i: input integer value
char	output[ARB]		# o: same as input, bit for bit
#--
int	buf			# local copy of input
char	cbuf[SZ_INT32]		# will be copied to output
int	i
#equivalence (buf, cbuf)

begin
	buf = input
	do i = 1, SZ_INT32
	    output[i] = cbuf[i]
end

# tbbeqs -- assign a short integer value
# The purpose of this is to assign the input value of type short integer to
# a character output buffer.

procedure tbbeqs (input, output)

short	input			# i: input integer value
char	output[ARB]		# o: same as input, bit for bit
#--
short	buf			# local copy of input
char	cbuf[SZ_SHORT]		# will be copied to output
int	i
#equivalence (buf, cbuf)

begin
	if (SZ_SHORT == SZ_CHAR) {
	    output[1] = input
	} else {
	    buf = input
	    do i = 1, SZ_SHORT
		output[i] = cbuf[i]
	}
end

# tbbeqb -- assign a boolean value
# The purpose of this is to assign the input value of type boolean to
# a character output buffer.

procedure tbbeqb (input, output)

bool	input			# i: input integer value
char	output[ARB]		# o: same as input, bit for bit
#--
bool	buf			# local copy of input
char	cbuf[SZ_BOOL]		# will be copied to output
int	i
#equivalence (buf, cbuf)

begin
	buf = input
	do i = 1, SZ_BOOL
	    output[i] = cbuf[i]
end
