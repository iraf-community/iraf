include <tbset.h>

# tbfres -- is the keyword a FITS reserved keyword?
# If the input keyword is "NAXIS", "TTYPEn", etc, this routine returns YES;
# otherwise NO is returned.
#
# Phil Hodge,  6-Jul-1995  Subroutine created
# Phil Hodge, 13-Nov-1995  Change type from bool to int.

int procedure tbfres (keyword)

char	keyword[ARB]		# i: name of parameter
#--
char	uckey[SZ_KEYWORD]	# keyword converted to upper case
int	strncmp()
bool	streq()

begin
	call strcpy (keyword, uckey, SZ_KEYWORD)
	call strupr (uckey)

	if (streq (uckey, "XTENSION"))
	    return (YES)
	else if (streq (uckey, "BITPIX"))
	    return (YES)
	else if (strncmp (uckey, "NAXIS", 5) == 0)
	    return (YES)
	else if (streq (uckey, "PCOUNT"))
	    return (YES)
	else if (streq (uckey, "GCOUNT"))
	    return (YES)
	else if (streq (uckey, "TFIELDS"))
	    return (YES)
	else if (streq (uckey, "END"))
	    return (YES)
	else if (strncmp (uckey, "TBCOL", 5) == 0)
	    return (YES)
	else if (strncmp (uckey, "TFORM", 5) == 0)
	    return (YES)
	else if (strncmp (uckey, "TTYPE", 5) == 0)
	    return (YES)
	else if (strncmp (uckey, "TUNIT", 5) == 0)
	    return (YES)
	else if (strncmp (uckey, "TSCAL", 5) == 0)
	    return (YES)
	else if (strncmp (uckey, "TZERO", 5) == 0)
	    return (YES)
	else if (strncmp (uckey, "TNULL", 5) == 0)
	    return (YES)
	else if (strncmp (uckey, "TDISP", 5) == 0)
	    return (YES)
	else if (strncmp (uckey, "TDIM", 4) == 0)
	    return (YES)
	else if (streq (uckey, "THEAP"))
	    return (YES)

	return (NO)
end
