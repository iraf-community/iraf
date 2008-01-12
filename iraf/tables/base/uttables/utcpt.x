# utcptb -- putcol logical
# Write values into a column.  This is for data type logical.
#
# Phil Hodge, 16-Sep-1987  Delete section to write error message.
# Phil Hodge, 28-Dec-1987  Different data types combined into one file.
# Phil Hodge,  5-May-1993  Include utcpts.

procedure utcptb (tp, colptr, firstrow, lastrow, buffer, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr			# i: Pointer to column descriptor
bool	buffer[ARB]		# i: Buffer for values
int	firstrow		# i: first row into which to put values
int	lastrow			# i: last row into which to put values
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbcptb (tp, colptr, buffer, firstrow, lastrow)) {
	    istat = errcode()
	}
end

# utcptd -- putcol double
# Write values into a column.  This is for data type double precision.

procedure utcptd (tp, colptr, firstrow, lastrow, buffer, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr			# i: Pointer to column descriptor
double	buffer[ARB]		# i: Buffer for values
int	firstrow		# i: first row into which to put values
int	lastrow			# i: last row into which to put values
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbcptd (tp, colptr, buffer, firstrow, lastrow)) {
	    istat = errcode()
	}
end

# utcpti -- putcol integer
# Write values into a column.  This is for data type integer.

procedure utcpti (tp, colptr, firstrow, lastrow, buffer, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr			# i: Pointer to column descriptor
int	buffer[ARB]		# i: Buffer for values
int	firstrow		# i: first row into which to put values
int	lastrow			# i: last row into which to put values
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbcpti (tp, colptr, buffer, firstrow, lastrow)) {
	    istat = errcode()
	}
end

# utcpts -- putcol short
# Write values into a column.  This is for data type short.

procedure utcpts (tp, colptr, firstrow, lastrow, buffer, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr			# i: Pointer to column descriptor
short	buffer[ARB]		# i: Buffer for values
int	firstrow		# i: first row into which to put values
int	lastrow			# i: last row into which to put values
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbcpts (tp, colptr, buffer, firstrow, lastrow)) {
	    istat = errcode()
	}
end

# utcptr -- putcol real
# Write values into a column.  This is for data type real.

procedure utcptr (tp, colptr, firstrow, lastrow, buffer, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr			# i: Pointer to column descriptor
real	buffer[ARB]		# i: Buffer for values
int	firstrow		# i: first row into which to put values
int	lastrow			# i: last row into which to put values
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbcptr (tp, colptr, buffer, firstrow, lastrow)) {
	    istat = errcode()
	}
end

# utcptt -- putcol text
# Write values into a column.  This is for character strings.

procedure utcptt (tp, colptr, firstrow, lastrow, f77buf, istat)

pointer tp			# i: pointer to table descriptor
pointer colptr			# i: pointer to column descriptor
				# i: values to be put into column
%      character*(*) f77buf(1)
int	firstrow		# i: first row into which to put values
int	lastrow			# i: last row into which to put values
int	istat			# o: return status
#--
pointer sp, cbuf, jbuf		# Stack pointer; ptr to scratch; index
int	k			# Loop index
int	slen			# Length of each string (each element of buffer)
int	errcode()

begin
	istat = OK

%      slen = len (f77buf(1))

	call smark (sp)
	# Add one extra character for EOS.
	call salloc (cbuf, (slen+1)*(lastrow-firstrow+1), TY_CHAR)

	jbuf = cbuf
	do k = 1, lastrow-firstrow+1 {
	    call f77upk (f77buf[k], Memc[jbuf], slen)
	    jbuf = jbuf + slen+1
	}
	iferr (call tbcptt (tp, colptr, Memc[cbuf], slen, firstrow, lastrow)) {
	    istat = errcode()
	}
	call sfree (sp)
end
