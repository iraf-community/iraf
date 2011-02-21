include	<tbset.h>
define	USRERR		1

# TABHDRTYP -- Return the type of a table header keyword
#
# B. Simon	12-Aug-87	First Code
# Phil Hodge	 9-Mar-89	Change to itype in calling sequence of tbhfkr.

int procedure tabhdrtyp (tp, keyword)

pointer	tp		# i: Table descriptor
char	keyword[ARB]	# i: Header keyword
#--
int	parnum, itype
pointer	sp, keyval, errtxt

string	nokeyfnd	"Keyword not found (%s)"

begin
	call smark (sp)
	call salloc (keyval, SZ_PARREC, TY_CHAR)
	call salloc (errtxt, SZ_LINE, TY_CHAR)

	call tbhfkr (tp, keyword, itype, Memc[keyval], parnum)

	if (parnum == 0) {
	    call sprintf (Memc[errtxt], SZ_LINE, nokeyfnd)
	    call pargstr (keyword)
	    call error (USRERR, Memc[errtxt])
	}

	call sfree (sp)
	return (itype)   
end
