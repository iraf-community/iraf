# utcgtb -- getcol logical
# Read values from a column.  This is for data type logical.
#
# Phil Hodge, 16-Sep-1987  Delete section to write error message.
# Phil Hodge, 28-Dec-1987  Different data types combined into one file.
# Phil Hodge,  5-May-1993  Include utcgts.

procedure utcgtb (tp, colptr, firstrow, lastrow,
		buffer, nullflag, istat)

pointer tp			# i: pointer to table descriptor
pointer colptr			# i: pointer to column descriptor
bool	buffer[ARB]		# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	firstrow		# i: first row from which to get values
int	lastrow			# i: last row from which to get values
int	istat			# o: return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbcgtb (tp, colptr, buffer, nullflag, firstrow, lastrow)) {
	    istat = errcode()
	}
end

# utcgtd -- getcol double
# Read values from a column.  This is for data type double precision.

procedure utcgtd (tp, colptr, firstrow, lastrow,
		buffer, nullflag, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr			# i: Pointer to column descriptor
double	buffer[ARB]		# o: Buffer for values
bool	nullflag[ARB]		# o: True if element is undefined in table
int	firstrow		# i: first row from which to get values
int	lastrow			# i: last row from which to get values
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbcgtd (tp, colptr, buffer, nullflag, firstrow, lastrow)) {
	    istat = errcode()
	}
end

# utcgti -- getcol integer
# Read values from a column.  This is for data type integer.

procedure utcgti (tp, colptr, firstrow, lastrow,
		buffer, nullflag, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr			# i: Pointer to column descriptor
int	buffer[ARB]		# o: Buffer for values
bool	nullflag[ARB]		# o: True if element is undefined in table
int	firstrow		# i: first row from which to get values
int	lastrow			# i: last row from which to get values
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbcgti (tp, colptr, buffer, nullflag, firstrow, lastrow)) {
	    istat = errcode()
	}
end

# utcgts -- getcol short
# Read values from a column.  This is for data type short.

procedure utcgts (tp, colptr, firstrow, lastrow,
		buffer, nullflag, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr			# i: Pointer to column descriptor
short	buffer[ARB]		# o: Buffer for values
bool	nullflag[ARB]		# o: True if element is undefined in table
int	firstrow		# i: first row from which to get values
int	lastrow			# i: last row from which to get values
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbcgts (tp, colptr, buffer, nullflag, firstrow, lastrow)) {
	    istat = errcode()
	}
end

# utcgtr -- getcol real
# Read values from a column.  This is for data type real.

procedure utcgtr (tp, colptr, firstrow, lastrow,
		buffer, nullflag, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr			# i: Pointer to column descriptor
real	buffer[ARB]		# o: Buffer for values
bool	nullflag[ARB]		# o: True if element is undefined in table
int	firstrow		# i: first row from which to get values
int	lastrow			# i: last row from which to get values
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbcgtr (tp, colptr, buffer, nullflag, firstrow, lastrow)) {
	    istat = errcode()
	}
end

# utcgtt -- getcol text
# Read values from a column.  This is for character string columns.

procedure utcgtt (tp, colptr, firstrow, lastrow,
		f77buf, nullflag, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr			# i: Pointer to column descriptor
				# o: buffer to receive values from column
%      character*(*) f77buf(1)
bool	nullflag[ARB]		# o: True if element is undefined in table
int	firstrow		# i: first row from which to get values
int	lastrow			# i: last row from which to get values
int	istat			# o: Return status
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

	iferr (call tbcgtt (tp, colptr, Memc[cbuf], nullflag, slen,
			firstrow, lastrow)) {
	    istat = errcode()
	    call sfree (sp)
	    return
	}
	jbuf = cbuf
	do k = 1, lastrow-firstrow+1 {
	    call f77pak (Memc[jbuf], f77buf[k], slen)
	    jbuf = jbuf + slen+1
	}
	call sfree (sp)
end
