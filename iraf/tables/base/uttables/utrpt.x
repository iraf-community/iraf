# utrptb -- putrow logical
# Write column values into a row.  This is for data type logical.
#
# Phil Hodge, 16-Sep-1987  Delete section to write error message.
# Phil Hodge, 28-Dec-1987  Different data types combined into one file.
# Phil Hodge,  5-May-1993  Include utrpts.

procedure utrptb (tp, colptr, numcols, rownum, buffer, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr[ARB]		# i: Array of pointers to column descriptors
bool	buffer[ARB]		# i: Buffer for values
int	numcols			# i: Number of columns from which to get values
int	rownum			# i: Row number
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbrptb (tp, colptr, buffer, numcols, rownum)) {
	    istat = errcode()
	}
end

# utrptd -- putrow double
# Write column values into a row.  This is for data type double precision.

procedure utrptd (tp, colptr, numcols, rownum, buffer, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr[ARB]		# i: Array of pointers to column descriptors
double	buffer[ARB]		# i: Buffer for values
int	numcols			# i: Number of columns from which to get values
int	rownum			# i: Row number
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbrptd (tp, colptr, buffer, numcols, rownum)) {
	    istat = errcode()
	}
end

# utrpti -- putrow integer
# Write column values into a row.  This is for data type integer.

procedure utrpti (tp, colptr, numcols, rownum, buffer, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr[ARB]		# i: Array of pointers to column descriptors
int	buffer[ARB]		# i: Buffer for values
int	numcols			# i: Number of columns from which to get values
int	rownum			# i: Row number
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbrpti (tp, colptr, buffer, numcols, rownum)) {
	    istat = errcode()
	}
end

# utrpts -- putrow short
# Write column values into a row.  This is for data type short.

procedure utrpts (tp, colptr, numcols, rownum, buffer, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr[ARB]		# i: Array of pointers to column descriptors
short	buffer[ARB]		# i: Buffer for values
int	numcols			# i: Number of columns from which to get values
int	rownum			# i: Row number
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbrpts (tp, colptr, buffer, numcols, rownum)) {
	    istat = errcode()
	}
end

# utrptr -- putrow real
# Write column values into a row.  This is for data type real.

procedure utrptr (tp, colptr, numcols, rownum, buffer, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr[ARB]		# i: Array of pointers to column descriptors
real	buffer[ARB]		# i: Buffer for values
int	numcols			# i: Number of columns from which to get values
int	rownum			# i: Row number
int	istat			# o: Return status
#--
int	errcode()

begin
	istat = OK

	iferr (call tbrptr (tp, colptr, buffer, numcols, rownum)) {
	    istat = errcode()
	}
end

# utrptt -- putrow text string
# Write column values into a row.  This is for data type character.

procedure utrptt (tp, colptr, numcols, rownum, buffer, istat)

pointer tp			# i: Pointer to table descriptor
pointer colptr[ARB]		# i: Array of pointers to column descriptors
				# i: buffer containing values to be put
%      character*(*) buffer(1)
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

	jbuf = cbuf
	do k = 1, numcols {
	    call f77upk (buffer[k], Memc[jbuf], slen)
	    jbuf = jbuf + slen+1
	}
	iferr (call tbrptt (tp, colptr, Memc[cbuf], slen, numcols, rownum)) {
	    istat = errcode()
	}
	call sfree (sp)
end
