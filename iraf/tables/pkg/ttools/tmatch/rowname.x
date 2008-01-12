#* HISTORY *
#* B.Simon	26-Aug-94

# ROWNAME -- Create name for table row by concatenating column values

procedure rowname (in, irow, ncol, col, name, namelen)

pointer	in		# i: table descriptor
int	irow		# i: table row number
int	ncol		# i: number of table columns
pointer	col[ARB]	# i: table column pointers
char	name[ARB]	# o: concatenated values of columns
int	namelen		# i: maximum name length
#--
int	ic, jc, icol
pointer	sp, value

begin
	# Allocate memory for column buffer

	call smark (sp)
	call salloc (value, SZ_LINE, TY_CHAR)

	# Concatenate column values into name string

	jc = 0
	icol = 0
	for (ic = 1; ic <= namelen; ic = ic + 1) {

	    # A value of zero is a flag to read the next coumn

	    if (jc == 0) {
		icol = icol + 1
		if (icol > ncol) {
		    if (ic > 1)
			ic = ic - 1	# remove trailing blank

		    break
		}

		call tbegtt (in, col[icol], irow, Memc[value], SZ_LINE)
	    }

	    # Copy a single character from the buffer to the output string
	    # until the buffer is exhausted. At this point we copy a blank
	    # as a spacer and set the value of jc as a flag to read the
	    # next column.

	    if (Memc[value+jc] == EOS) {
		name[ic] = ' '
		jc = 0
	    } else {
		name[ic] = Memc[value+jc]
		jc = jc + 1
	    }
	}

	name[ic] = EOS
	call sfree (sp)
end

