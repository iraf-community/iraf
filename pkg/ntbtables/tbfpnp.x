include <ctype.h>		# for IS_WHITE
include <tbset.h>
include "tbtables.h"

# tbfpnp -- put Nth parameter to a FITS table header
# Put the keyword and value string of header parameter number parnum,
# which must already exist.  The data type may be changed as well.
# If the keyword name of the current parnum in the table is the same as
# the replacement keyword, any existing comment will be preserved.
#
# Phil Hodge, 27-Nov-1995  Subroutine created

procedure tbfpnp (tp, parnum, keyword, dtype, str)

pointer tp			# i: pointer to table descriptor
int	parnum			# i: number of the parameter to be put
char	keyword[SZ_KEYWORD]	# i: keyword for the parameter
int	dtype			# i: data type (TY_CHAR, etc)
char	str[ARB]		# i: string containing the value of the param.
#--
pointer sp
pointer rec			# scratch for header record to be written
pointer strval			# copy of str, without leading & trailing blanks
pointer oldrec			# buffer for current value
pointer cmt			# buffer for current comment
char	ukkey[SZ_KEYWORD]	# keyword name in upper case
char	oldkey[SZ_KEYWORD]	# current name of keyword number parnum
int	odtype			# data type of current keyword number parnum
double	dval			# for reformatting str, if too long
int	i			# loop index
int	lenval			# number of char in value string
int	status			# zero is OK
bool	iscomm			# true if keyword is history or comment
int	ip, ctod()
int	strlen()
bool	streq()
bool	tbhisc()
errchk	tbferr

begin
	call smark (sp)
	call salloc (rec, SZ_LINE, TY_CHAR)
	call salloc (strval, SZ_LINE, TY_CHAR)
	call salloc (oldrec, SZ_LINE, TY_CHAR)

	# Copy the keyword to scratch and convert to upper case.
	call strcpy (keyword, ukkey, SZ_KEYWORD)
	call strupr (ukkey)

	# Copy str to scratch, deleting leading and trailing whitespace.

	# Skip leading blanks in scr.
	i = 1
	while (IS_WHITE(str[i]))
	    i = i + 1

	call strcpy (str[i], Memc[strval], SZ_LINE)

	# Delete trailing blanks in strval.
	i = strlen (Memc[strval])
	while (IS_WHITE(Memc[strval+i-1]) && i > 0) {
	    Memc[strval+i-1] = EOS
	    i = i - 1
	}
	lenval = i			# number of char in value string

	iscomm = tbhisc (keyword)	# is the keyword history or comment?

	# The format depends on the data type.
	if (dtype == TY_CHAR) {

	    if (iscomm) {
		# No quotes for history or comment.
		call sprintf (Memc[rec], SZ_LINE, "%-8s %s")
		    call pargstr (ukkey)
		    call pargstr (Memc[strval])
	    } else if (lenval < 8) {
		call sprintf (Memc[rec], SZ_LINE, "%-8s= '%-8s'           / ")
		    call pargstr (ukkey)
		    call pargstr (Memc[strval])
	    } else if (lenval < 18) {
		call sprintf (Memc[rec], SZ_LINE, "%-8s= '%-s'%31t / ")
		    call pargstr (ukkey)
		    call pargstr (Memc[strval])
	    } else {
		call sprintf (Memc[rec], SZ_LINE, "%-8s= '%s' / ")
		    call pargstr (ukkey)
		    call pargstr (Memc[strval])
	    }

	} else if (dtype == TY_BOOL) {

	    call strlwr (Memc[strval])
	    if (streq (Memc[strval], "yes") || streq (Memc[strval], "y") ||
		streq (Memc[strval], "true") || streq (Memc[strval], "t") ||
		streq (Memc[strval], "1")) {
		call sprintf (Memc[rec], SZ_LINE,
			"%-8s=                    T / ")
		    call pargstr (ukkey)
	    } else {
		call sprintf (Memc[rec], SZ_LINE,
			"%-8s=                    F / ")
		    call pargstr (ukkey)
	    }

	} else {

	    if (lenval <= 20) {
		call sprintf (Memc[rec], SZ_LINE, "%-8s= %20s / ")
		    call pargstr (ukkey)
		    call pargstr (Memc[strval])
	    } else {
		# Value is too long.  Reformat it.
		ip = 1
		if (ctod (Memc[strval], ip, dval) < 1)
		    dval = 0.d0
		call sprintf (Memc[rec], SZ_LINE, "%-8s= %20g / ")
		    call pargstr (ukkey)
		    call pargd (dval)
	    }
	}

	# If the old record contains a comment, concatenate it to the
	# parameter record.  Ignore if keyword is history or comment.
	if (!iscomm) {
	    # Read the current value to see if the keywords are the same,
	    # and if so, to get the comment.
	    call salloc (cmt, SZ_LINE, TY_CHAR)
	    call tbfgnp (tp, parnum, oldkey, odtype,
			Memc[oldrec], Memc[cmt], SZ_LINE)
	    if (streq (ukkey, oldkey)) {
		if (Memc[cmt] != EOS)
		    call strcat (Memc[cmt], Memc[rec], SZ_LINE)
	    }
	}

	status = 0

	# Clobber the Nth header record.
	call fsmrec (TB_FILE(tp), parnum, Memc[rec], status)

	if (status != 0)
	    call tbferr (status)

	call sfree (sp)
end
