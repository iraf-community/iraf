include <ctype.h>		# defines IS_WHITE
include <tbset.h>
include "tbtables.h"

# tbhanp -- add new parameter
# This procedure writes a new user parameter to the table; the number
# of the new parameter is returned.  The data type may be TY_CHAR (for a
# string), TY_REAL, TY_DOUBLE, TY_INT, or TY_BOOL.
# The table will be rewritten if necessary in order to increase the amount
# of space allocated for user parameters.
#
# TB_MODIFIED is set by this routine.
#
# Phil Hodge,  9-Mar-1989  Change dtype from char to int.
# Phil Hodge, 14-Feb-1992  Add option for text table type.
# Phil Hodge, 22-Apr-1994  For text table, append to comment buffer.
# Phil Hodge,  6-Mar-1995  Erase space in table before writing new parameter.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge, 24-Jul-1995  Modify for FITS tables.
# Phil Hodge,  7-Jun-1999  Call tbzkey instead of tbbcmt for text table.
# Frank Valdes, 20-Nov-2003  Quote strings in text, strip trailing spaces, and
#                            capitalize keywords to be consistent with tbhpnp.

procedure tbhanp (tp, keyword, dtype, str, parnum)

pointer tp			# i: pointer to table descriptor
char	keyword[SZ_KEYWORD]	# i: keyword for the parameter
int	dtype			# i: data type
char	str[ARB]		# i: string containing the value of the param.
int	parnum			# o: number of the parameter in the table
#--
pointer sp
pointer text_str		# scratch
pointer word			# value (extracted from str) of parameter
pointer comment			# comment string, if any, from str
int	maxpar			# increased value of TB_MAXPAR
int	i
int	ip, nchar, ctowrd(), strlen()
errchk	tbtchs, tbfanp, tbhpnp, tbhwpr, tbhpcm, tbzkey

begin
	call smark (sp)

	if (TB_TYPE(tp) == TBL_TYPE_TEXT) {

	    call salloc (text_str, SZ_LINE, TY_CHAR)

	    # Construct "#k keyword = value", and add to keyword list.
	    call sprintf (Memc[text_str], SZ_LINE, "#k %-8s = ")
	        call pargstr (keyword)
	    call strupr (Memc[text_str+3])
	    if (dtype == TY_CHAR) {
		call strcat ("'", Memc[text_str], SZ_LINE)
		call strcat (str, Memc[text_str], SZ_LINE)
		i = text_str + strlen (Memc[text_str]) - 1
		for (; IS_WHITE(Memc[i]); i=i-1)
		    Memc[i] = EOS
		call strcat ("'", Memc[text_str], SZ_LINE)
	    } else
		call strcat (str, Memc[text_str], SZ_LINE)
	    call tbzkey (tp, Memc[text_str], 0)
	    parnum = TB_NPAR(tp)

	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {

	    call tbfanp (tp, keyword, dtype, str, parnum)

	} else {

	    call salloc (text_str, SZ_PARREC, TY_CHAR)
	    call salloc (word, SZ_PARREC, TY_CHAR)
	    call salloc (comment, SZ_PARREC, TY_CHAR)
	    do i = 0, SZ_PARREC-1
		Memc[text_str+i] = ' '
	    Memc[text_str+SZ_PARREC] = EOS

	    # Extract value and comment from str.
	    if (dtype == TY_CHAR) {
		i = 1
		while (IS_WHITE(str[i]))
		    i = i + 1
		if (str[i] == '"' || str[i] == '\'') {
		    ip = 1
		    nchar = ctowrd (str, ip, Memc[word], SZ_PARREC)
		    i = ip
		    while (IS_WHITE(str[i]))
			i = i + 1
		    call strcpy (str[i], Memc[comment], SZ_PARREC)
		} else {
		    call strcpy (str[i], Memc[word], SZ_PARREC)
		    Memc[comment] = EOS			# no comment
		}
	    } else {
		ip = 1
		nchar = ctowrd (str, ip, Memc[word], SZ_PARREC)
		i = ip
		while (IS_WHITE(str[i]))
		    i = i + 1
		call strcpy (str[i], Memc[comment], SZ_PARREC)
	    }

	    if (TB_NPAR(tp) >= TB_MAXPAR(tp)) {
		maxpar = TB_MAXPAR(tp) + DEFMAXPAR
		call tbtchs (tp, maxpar, -1, -1, -1)	# change size
	    }
	    parnum = TB_NPAR(tp) + 1
	    TB_NPAR(tp) = parnum

	    # Blank out the space and then put the Nth parameter.
	    call tbhwpr (tp, parnum, Memc[text_str])
	    call tbhpnp (tp, parnum, keyword, dtype, Memc[word])
	    call tbhpcm (tp, keyword, Memc[comment])	# append comment
	}

	TB_MODIFIED(tp) = true

	call sfree (sp)
end
