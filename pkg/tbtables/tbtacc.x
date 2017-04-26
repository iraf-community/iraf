include <tbset.h>

# tbtacc -- test for existence of table
# This function returns YES if the table exists, NO if not.
# We attempt to open the specified file read-only as a table using tbtopn.
# If that fails, either the file does not exist or it is not a table (or
# we don't have read access to it), and the value NO is returned as the
# function value.  If the open is successful, it exists but still might not
# be a table (it could be a FITS primary header or IMAGE extension).  We
# therefore get the table subtype to check for this.  If the subtype is
# image, we return NO; otherwise, we return YES.
# (Until 4-Dec-90 we called tbtext and access.  The problem with that was
# that it would report YES for any file that existed regardless of whether
# it really was a table.)
#
# Phil Hodge, 25-Aug-1987  Function created.
# Phil Hodge,  4-Dec-1990  Use tbtopn instead of access.
# Phil Hodge, 22-Feb-2002  If tbtopn succeeds, test if subtype is image.

int procedure tbtacc (tablename)

char	tablename[ARB]		# i: the table name
#--
pointer tp
pointer tbtopn()
int	subtype, tbpsta()

begin
	iferr {
	    tp = tbtopn (tablename, READ_ONLY, NULL)
	} then {
	    return (NO)
	}
	subtype = tbpsta (tp, TBL_SUBTYPE)
	call tbtclo (tp)
	if (subtype == TBL_SUBTYPE_IMAGE)
	    return (NO)
	else
	    return (YES)
end
