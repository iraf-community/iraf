include <ctype.h>



# RG_APACK_LINE -- Fields are packed into the output buffer. Transformed
# fields are converted to strings; other fields are copied from the input
# line to the output buffer.

procedure rg_apack_liner (inbuf, outbuf, maxch, field_pos, nfields,
    cinfields, ncin, coords, laxno, formats, nsdig, ncout, min_sigdigits)

char	inbuf[ARB]		#I the input string buffer
char	outbuf[maxch]		#O the output string buffer
int	maxch			#I the maximum size of the output buffer
int	field_pos[ARB]		#I starting positions for the fields
int	nfields			#I the number of fields
int	cinfields[ARB]		#I fields to be replaced
int	ncin			#I the number of input fields
real	coords[ARB]		#I the transformed coordinates
int	laxno[ARB]		#I the logical axis mapping
pointer	formats[ARB]		#I array of format pointers
int	nsdig[ARB]		#I array of numbers of significant digits
int	ncout			#I the number of coordinates	
int	min_sigdigits		#I the minimum number of signficant digits

int	op, num_field, width, cf, cfptr
pointer	sp, field
int	gstrcpy()

begin
	call smark (sp)
	call salloc (field, SZ_LINE, TY_CHAR)

	# Initialize output pointer.
	op = 1

	# Copy the file replacing fields as one goes.
	do num_field = 1, nfields {

	    # Find the width of the field.
	    width = field_pos[num_field + 1] - field_pos[num_field]

	    # Find the field to be replaced.
	    cfptr = 0
	    do cf = 1, ncin {
		if (cinfields[cf] != num_field)
		    next
		cfptr = cf
		    break
	    }

	    # Replace the field.
	    if (cfptr != 0) {
		if (laxno[cfptr] == 0)
	            call li_format_fieldr (INDEFR, Memc[field], maxch,
		        Memc[formats[cfptr]], nsdig[cfptr], width,
			min_sigdigits)
		else
	            call li_format_fieldr (coords[laxno[cfptr]], Memc[field],
		        maxch, Memc[formats[laxno[cfptr]]], nsdig[laxno[cfptr]],
		        width, min_sigdigits)
	    } else {
	        # Put "width" characters from inbuf into field
		call strcpy (inbuf[field_pos[num_field]], Memc[field], width)
	    }

	    # Fields must be delimited by at least one blank.
	    if (num_field > 1 && !IS_WHITE (Memc[field])) {
		outbuf[op] = ' '
		op = op + 1
	    }

	    # Copy "field" to output buffer.
	    op = op + gstrcpy (Memc[field], outbuf[op], maxch)
	}

	do cfptr = ncin + 1, ncout {

	    # Copy out the extra fields if any.
	    if (laxno[cfptr] == 0)
	        call li_format_fieldr (INDEFR, Memc[field], maxch, "%g",
		    min_sigdigits, width, min_sigdigits)
	    else
	        call li_format_fieldr (coords[laxno[cfptr]], Memc[field],
		    maxch, Memc[formats[laxno[cfptr]]], nsdig[laxno[cfptr]],
		    width, min_sigdigits)

	    # Fields must be delimited by at least one blank.
	    if (!IS_WHITE (Memc[field])) {
		outbuf[op] = ' '
		op = op + 1
	    }

	    # Copy "field" to output buffer.
	    op = op + gstrcpy (Memc[field], outbuf[op], maxch)
	}

	outbuf[op] = '\n'
	outbuf[op+1] = EOS

	call sfree (sp)
end




# RG_APACK_LINE -- Fields are packed into the output buffer. Transformed
# fields are converted to strings; other fields are copied from the input
# line to the output buffer.

procedure rg_apack_lined (inbuf, outbuf, maxch, field_pos, nfields,
    cinfields, ncin, coords, laxno, formats, nsdig, ncout, min_sigdigits)

char	inbuf[ARB]		#I the input string buffer
char	outbuf[maxch]		#O the output string buffer
int	maxch			#I the maximum size of the output buffer
int	field_pos[ARB]		#I starting positions for the fields
int	nfields			#I the number of fields
int	cinfields[ARB]		#I fields to be replaced
int	ncin			#I the number of input fields
double	coords[ARB]		#I the transformed coordinates
int	laxno[ARB]		#I the logical axis mapping
pointer	formats[ARB]		#I array of format pointers
int	nsdig[ARB]		#I array of numbers of significant digits
int	ncout			#I the number of coordinates	
int	min_sigdigits		#I the minimum number of signficant digits

int	op, num_field, width, cf, cfptr
pointer	sp, field
int	gstrcpy()

begin
	call smark (sp)
	call salloc (field, SZ_LINE, TY_CHAR)

	# Initialize output pointer.
	op = 1

	# Copy the file replacing fields as one goes.
	do num_field = 1, nfields {

	    # Find the width of the field.
	    width = field_pos[num_field + 1] - field_pos[num_field]

	    # Find the field to be replaced.
	    cfptr = 0
	    do cf = 1, ncin {
		if (cinfields[cf] != num_field)
		    next
		cfptr = cf
		    break
	    }

	    # Replace the field.
	    if (cfptr != 0) {
		if (laxno[cfptr] == 0)
	            call li_format_fieldd (INDEFD, Memc[field], maxch,
		        Memc[formats[cfptr]], nsdig[cfptr], width,
			min_sigdigits)
		else
	            call li_format_fieldd (coords[laxno[cfptr]], Memc[field],
		        maxch, Memc[formats[laxno[cfptr]]], nsdig[laxno[cfptr]],
		        width, min_sigdigits)
	    } else {
	        # Put "width" characters from inbuf into field
		call strcpy (inbuf[field_pos[num_field]], Memc[field], width)
	    }

	    # Fields must be delimited by at least one blank.
	    if (num_field > 1 && !IS_WHITE (Memc[field])) {
		outbuf[op] = ' '
		op = op + 1
	    }

	    # Copy "field" to output buffer.
	    op = op + gstrcpy (Memc[field], outbuf[op], maxch)
	}

	do cfptr = ncin + 1, ncout {

	    # Copy out the extra fields if any.
	    if (laxno[cfptr] == 0)
	        call li_format_fieldd (INDEFD, Memc[field], maxch, "%g",
		    min_sigdigits, width, min_sigdigits)
	    else
	        call li_format_fieldd (coords[laxno[cfptr]], Memc[field],
		    maxch, Memc[formats[laxno[cfptr]]], nsdig[laxno[cfptr]],
		    width, min_sigdigits)

	    # Fields must be delimited by at least one blank.
	    if (!IS_WHITE (Memc[field])) {
		outbuf[op] = ' '
		op = op + 1
	    }

	    # Copy "field" to output buffer.
	    op = op + gstrcpy (Memc[field], outbuf[op], maxch)
	}

	outbuf[op] = '\n'
	outbuf[op+1] = EOS

	call sfree (sp)
end



