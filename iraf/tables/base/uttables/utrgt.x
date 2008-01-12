# utrgtb -- getrow logical
# Read column values from a row.  This is for data type logical.
#
# Phil Hodge, 16-Sep-1987  Delete section to write error message.
# Phil Hodge, 28-Dec-1987  Different data types combined into one file.
# Phil Hodge,  5-May-1993  Include utrgts.

procedure utrgtb (tp, colptr, numcols, rownum,
		buffer, nullflag, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr[ARB]		# i: Array of pointers to column descriptors
bool	buffer[ARB]		# o: Buffer for values
bool	nullflag[ARB]		# o: True if element is undefined in table
int	numcols			# i: Number of columns from which to get values
int	rownum			# i: Row number
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbrgtb (tp, colptr, buffer, nullflag, numcols, rownum)) {
	    istat = errcode()
	}
end

# utrgtd -- getrow double
# Read column values from a row.  This is for data type double precision.

procedure utrgtd (tp, colptr, numcols, rownum,
		buffer, nullflag, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr[ARB]		# i: Array of pointers to column descriptors
double	buffer[ARB]		# o: Buffer for values
bool	nullflag[ARB]		# o: True if element is undefined in table
int	numcols			# i: Number of columns from which to get values
int	rownum			# i: Row number
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbrgtd (tp, colptr, buffer, nullflag, numcols, rownum)) {
	    istat = errcode()
	}
end

# utrgti -- getrow integer
# Read column values from a row.  This is for data type integer.

procedure utrgti (tp, colptr, numcols, rownum,
		buffer, nullflag, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr[ARB]		# i: Array of pointers to column descriptors
int	buffer[ARB]		# o: Buffer for values
bool	nullflag[ARB]		# o: True if element is undefined in table
int	numcols			# i: Number of columns from which to get values
int	rownum			# i: Row number
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbrgti (tp, colptr, buffer, nullflag, numcols, rownum)) {
	    istat = errcode()
	}
end

# utrgts -- getrow short
# Read column values from a row.  This is for data type short.

procedure utrgts (tp, colptr, numcols, rownum,
		buffer, nullflag, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr[ARB]		# i: Array of pointers to column descriptors
short	buffer[ARB]		# o: Buffer for values
bool	nullflag[ARB]		# o: True if element is undefined in table
int	numcols			# i: Number of columns from which to get values
int	rownum			# i: Row number
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbrgts (tp, colptr, buffer, nullflag, numcols, rownum)) {
	    istat = errcode()
	}
end

# utrgtr -- getrow real
# Read column values from a row.  This is for data type real.

procedure utrgtr (tp, colptr, numcols, rownum,
		buffer, nullflag, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr[ARB]		# i: Array of pointers to column descriptors
real	buffer[ARB]		# o: Buffer for values
bool	nullflag[ARB]		# o: True if element is undefined in table
int	numcols			# i: Number of columns from which to get values
int	rownum			# i: Row number
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbrgtr (tp, colptr, buffer, nullflag, numcols, rownum)) {
	    istat = errcode()
	}
end

# utrgtt -- getrow text string
# Read column values from a row.  This is for data type character.

procedure utrgtt (tp, colptr, numcols, rownum,
		buffer, nullflag, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr[ARB]		# i: Array of pointers to column descriptors
				# o: buffer to receive values
%      character*(*) buffer(1)
bool	nullflag[ARB]		# o: True if element is undefined in table
int	numcols			# i: Number of columns from which to get values
int	rownum			# i: Row number
int	istat			# o: Return status
#--
pointer sp, cbuf, jbuf		# Stack pointer; ptr to scratch; index
int	k			# Loop index
int	slen			# Length of each string (each element of buffer)
int	errcode()

begin
	istat = OK

%      slen = len (buffer(1))

	call smark (sp)
	# Add one extra character for EOS.
	call salloc (cbuf, (slen+1)*numcols, TY_CHAR)

	iferr (call tbrgtt (tp, colptr, Memc[cbuf],
			nullflag, slen, numcols, rownum)) {
	    istat = errcode()
	    call sfree (sp)
	    return
	}
	jbuf = cbuf
	do k = 1, numcols {
	    call f77pak (Memc[jbuf], buffer[k], slen)
	    jbuf = jbuf + slen+1
	}
	call sfree (sp)
end
