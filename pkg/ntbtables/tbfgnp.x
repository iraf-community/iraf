include <chars.h>		# for SQUOTE, ESCAPE, etc
include <tbset.h>
include "tbtables.h"

define	LOCN_BEGIN	11	# location of beginning of keyword value
define	LOCN_END	30	# location of end of keyword value

# tbfgnp -- get Nth parameter from FITS table
# Get the keyword and value string of header parameter number parnum.
#
# Phil Hodge,  6-Jul-1995  Subroutine created
# Phil Hodge, 27-Nov-1995  Add comment to calling sequence.

procedure tbfgnp (tp, parnum, keyword, dtype, str, comment, maxch)

pointer tp			# i: pointer to table descriptor
int	parnum			# i: number of the parameter to be gotten
char	keyword[SZ_KEYWORD]	# o: keyword for the parameter
int	dtype			# o: data type (TY_CHAR, etc)
char	str[maxch]		# o: string to contain the value of the param.
char	comment[maxch]		# o: string to contain comment, if any
int	maxch			# i: max size of str
#--
pointer sp
pointer rec		# scratch for header record
pointer value		# scratch for value
pointer cmt		# scratch for comment
int	i, j		# loop indexes
int	status		# zero is OK
int	strlen()
bool	tbhisc()
errchk	tbferr

begin
	call smark (sp)
	call salloc (rec, SZ_LINE, TY_CHAR)
	call salloc (value, SZ_LINE, TY_CHAR)
	call salloc (cmt, SZ_LINE, TY_CHAR)

	status = 0

	# Get the Nth header record.
	call fsgrec (TB_FILE(tp), parnum, Memc[rec], status)

	if (status != 0)
	    call tbferr (status)

	# Copy the keyword to output and append EOS.
	do i = 1, SZ_KEYWORD {
	    if (Memc[rec+i-1] == BLANK) {	# stop at first blank
		keyword[i] = EOS
		break
	    }
	    keyword[i] = Memc[rec+i-1]
	}
	keyword[SZ_KEYWORD+1] = EOS

	# Parse the value and comment.
	call fspsvc (Memc[rec], Memc[value], Memc[cmt], status)

	# The FITSIO interface puts the contents of a HISTORY or COMMENT
	# record in the comment portion, but I prefer it to be the value.
	if (tbhisc (keyword)) {

	    call strcpy (Memc[cmt], Memc[value], maxch)
	    Memc[cmt] = EOS

	    # Remove equal sign, quotes, and /, if they are present.
	    j = strlen (Memc[value])
	    i = 0				# i is zero indexed
	    while (Memc[value+i] == BLANK)
		i = i + 1
	    if (Memc[value+i] == '=')
		Memc[value+i] = BLANK		# replace '=' with blank
	    while (Memc[value+i] == BLANK)
		i = i + 1
	    if (Memc[value+i] == SQUOTE) {
		Memc[value+i] = BLANK		# replace quote with blank
		while (i < j) {			# look for trailing quote
		    if (Memc[value+i] == SQUOTE) {
			if (Memc[value+i-1] != ESCAPE) {
			    Memc[value+i] = EOS
			    break
			}
		    }
		    i = i + 1
		}
	    }
	}

	# Check for (and remove) quotes enclosing the value.
	if (Memc[value] == SQUOTE) {
	    j = strlen (Memc[value])
	    Memc[value+j-1] = EOS		# clobber close quote
	    do i = 1, j-1			# shift left one character
		Memc[value+i-1] = Memc[value+i]
	}

	# Trim trailing blanks from keyword value.
	do i = strlen (Memc[value]), 1, -1 {
	    if (Memc[value+i-1] == BLANK)
		Memc[value+i-1] = EOS
	    else
		break
	}

	# Trim trailing blanks from comment.
	do i = strlen (Memc[cmt]), 1, -1 {
	    if (Memc[cmt+i-1] == BLANK)
		Memc[cmt+i-1] = EOS
	    else
		break
	}

	# Copy the value and comment to output.
	call strcpy (Memc[value], str, maxch)
	call strcpy (Memc[cmt], comment, maxch)

	# Determine the data type.

	call strupr (Memc[rec])
	do i = 1, SZ_LINE {
	    # Fill out the buffer from the comment on (or from EOS).
	    if (Memc[rec+i-1] == '/' || Memc[rec+i-1] == EOS) {
		do j = i, SZ_LINE
		    Memc[rec+j-1] = EOS
		break
	    }
	}

	if (tbhisc (keyword)) {

	    dtype = TY_CHAR

	} else if (Memc[rec+LOCN_BEGIN-1] == SQUOTE) {

	    dtype = TY_CHAR

	} else if (Memc[rec+LOCN_END-1] == 'T' ||
		   Memc[rec+LOCN_END-1] == 'F') {

	    dtype = TY_BOOL

	} else {

	    dtype = TY_INT				# may be reset below
	    do i = LOCN_BEGIN, LOCN_END {
		if (Memc[rec+i-1] == EOS)
		    break
		if (Memc[rec+i-1] == '.' ||
		    Memc[rec+i-1] == 'E' || Memc[rec+i-1] == 'D') {
		    dtype = TY_DOUBLE
		    break
		}
	    }
	    # We should also check whether there's an imaginary part.
	}

	call sfree (sp)
end
