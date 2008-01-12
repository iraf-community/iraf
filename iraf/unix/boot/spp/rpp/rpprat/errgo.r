include defs

# ERRGO -- Ouput error checking code.

subroutine errgo

include COMMON_BLOCKS
string	serrchk "if (xerflg) "

	# In the processing of the last line, was an indentifier encountered
	# for which error checking is required (named in errchk declaration)?

	if (ername == YES) {
	    call outtab
	    if (esp > 0) {				# in iferr ... stmt?
		# Omit goto if goto statement label number is zero.  This
		# happens in "iferr (...)" statements.
		if (errstk(esp) > 0) {
		    call outstr (serrchk)
		    call ogotos (errstk(esp)+2, NO)	# "goto lab"
		}
	    } else {
		call outstr (serrchk)
		call ogotos (retlab, NO)
		call outdon
	    }
	    ername = NO
	}
end
