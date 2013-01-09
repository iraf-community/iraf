include <ctype.h>		# for IS_WHITE
include <tbset.h>
include "tbtables.h"

# tbhpnp -- put Nth parameter
# Write the keyword and value string of parameter number parnum.
# A keyword may not contain embedded blanks.
#
# Trailing whitespace will be ignored when writing the
# value (str) to the table.  If the keyword is HISTORY, COMMENT, or
# blank, the value will be written without enclosing quotes, and no
# comment may be appended.  If the parameter is of type text, and the
# string is not too long, it will be enclosed in double quotes before
# being written to the table.  (If it is too long, no quotes will be
# used, and therefore no comment may be appended.)  If the parameter is
# already present in the table, and if there is an associated comment,
# the comment will be preserved.  If the buffer is not long enough to
# contain both the string value and comment, the comment will be silently
# truncated.
#
# Phil Hodge,  9-Mar-1989  Change dtype from char to int.
# Phil Hodge,  6-Mar-1995  Preserve comment when writing existing parameter;
#			enclose string in quotes if parameter is of type text;
#			ignore leading and trailing whitespace.
# Phil Hodge, 12-May-1995  Change string delimiter from " to '.
# Phil Hodge, 27-Nov-1995  Modify for FITS tables.
# Phil Hodge, 30-Jan-1996  Set TB_MODIFIED to true.
# Phil Hodge,  2-Jul-1998  Set value to 1 or 0 for boolean parameter.
# Frank Valdes, 29-Nov-2003  Don't eliminate leading whitespace in text.

procedure tbhpnp (tp, parnum, keyword, dtype, str)

pointer tp			# i: pointer to table descriptor
int	parnum			# i: number of the parameter to be put
char	keyword[SZ_KEYWORD]	# i: keyword for the parameter
int	dtype			# i: data type (TY_CHAR, etc)
char	str[ARB]		# i: string containing the value of the param.
#--
pointer sp
pointer str2			# copy of str, without leading & trailing blanks
pointer par			# buffer for parameter record
pointer oldpar			# buffer for existing parameter record, if any
char	uckey[SZ_KEYWORD]	# keyword converted to upper case
int	k			# loop index for copying keyword
int	char_type		# data type as a letter (t, b, i, r, d)
int	index			# location of comment in string
int	strlen(), strncmp()
bool	streq()
bool	tbhisc()
errchk	tbfpnp, tbhwpr, tbhrpr

begin
	if (parnum < 1 || parnum > TB_MAXPAR(tp))
	    call error (1, "tbhpnp:  parnum out of range")

	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfpnp (tp, parnum, keyword, dtype, str)
	    TB_MODIFIED(tp) = true
	    return
	}

	call smark (sp)
	call salloc (str2, SZ_PARREC, TY_CHAR)
	call salloc (par, SZ_PARREC, TY_CHAR)
	call salloc (oldpar, SZ_PARREC, TY_CHAR)

	# We must have an upper case keyword.
	call strcpy (keyword, uckey, SZ_KEYWORD)
	call strupr (uckey)

	# Copy str to scratch, deleting trailing whitespace.

	call strcpy (str, Memc[str2], SZ_PARREC)

	# Delete trailing blanks in scr2.
	k = strlen (Memc[str2])
	while (IS_WHITE(Memc[str2+k-1]) && k > 0) {
	    Memc[str2+k-1] = EOS
	    k = k - 1
	}

	# Fill the beginning portion of the output buffer with blanks.
	# This includes the keyword portion (which must be padded with
	# blanks) but also includes the datatype.  Put the EOS at the
	# point where the string value would start.
	do k = 1, START_OF_VALUE-1 {
	    Memc[par+k-1] = ' '
	    Memc[oldpar+k-1] = ' '
	}
	Memc[par+START_OF_VALUE-1] = EOS
	Memc[oldpar+START_OF_VALUE-1] = EOS

	# Read the current value, if the parameter already exists,
	# and make sure we really do have the parameter.
	if (parnum <= TB_NPAR(tp))
	    call tbhrpr (tp, parnum, Memc[oldpar])
	if (strncmp (uckey, Memc[oldpar], strlen (uckey)) != 0)
	    Memc[oldpar] = EOS			# nope; it's a new parameter

	# Copy the upper-case keyword (but not the EOS) to the output buffer.
	do k = 1, SZ_KEYWORD {
	    if (uckey[k] == EOS) {
		break
	    }
	    Memc[par+k-1] = uckey[k]
	}

	switch (dtype) {
	case TY_REAL:
	    char_type = 'r'
	case TY_INT:
	    char_type = 'i'
	case TY_DOUBLE:
	    char_type = 'd'
	case TY_BOOL:
	    char_type = 'b'
	default:
	    char_type = 't'
	}
	Memc[par+LOCN_DTYPE-1] = char_type	# data type (char const)

	Memc[par+LOCN_DTYPE] = EOS		# so we can use strcat

	# Copy the string containing the value to the output buffer.
	if (char_type == 't') {

	    # Check whether we have enough space to add quotes.
	    # The total space available for the parameter value is
	    # SZ_PARREC - START_OF_VALUE + 1.
	    if (strlen (Memc[str2]) > SZ_PARREC - START_OF_VALUE - 1 ||
		tbhisc (keyword)) {

		# Just append the value.  Set index to zero, implying that
		# there's no existing comment, because we can't write one
		# if there are no enclosing quotes.
		call strcat (Memc[str2], Memc[par], SZ_PARREC)
		index = 0			# no comment allowed

	    } else {

		# Enclose in quotes.
		call strcat ("'", Memc[par], SZ_PARREC)
		call strcat (Memc[str2], Memc[par], SZ_PARREC)
		call strcat ("'", Memc[par], SZ_PARREC)

		# Find the comment, if there is one, in the existing parameter.
		call tbhfcm (Memc[oldpar], index)
	    }

	} else if (dtype == TY_BOOL) {

	    call strlwr (Memc[str2])
	    if (streq (Memc[str2], "1") ||
		streq (Memc[str2], "yes") || streq (Memc[str2], "y") ||
		streq (Memc[str2], "true") || streq (Memc[str2], "t"))
		call strcat ("1", Memc[par], SZ_PARREC)
	    else
		call strcat ("0", Memc[par], SZ_PARREC)
	    call tbhfcm (Memc[oldpar], index)		# find comment

	} else {

	    call strcat (Memc[str2], Memc[par], SZ_PARREC)
	    call tbhfcm (Memc[oldpar], index)		# find comment
	}

	if (index > 0) {
	    # A comment was found; concatenate it to the parameter record.
	    call strcat (" ", Memc[par], SZ_PARREC)
	    call strcat (Memc[oldpar+index-1], Memc[par], SZ_PARREC)
	}

	# Write the parameter record to the table.
	call tbhwpr (tp, parnum, Memc[par])

	TB_MODIFIED(tp) = true

	call sfree (sp)
end
