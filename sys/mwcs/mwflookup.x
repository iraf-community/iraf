# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"mwcs.h"

# MW_FLOOKUP -- Look up the named WCS function in the driver table, and
# return the index of the associated driver.  ERR is returned if the named
# function is not found, 0 if the function is "linear", otherwise the
# index of the function driver is returned.

int procedure mw_flookup (mw, fnname)

pointer	mw			#I pointer to MWCS descriptor
char	fnname[ARB]		#I function to be lookup up

int	fn, i
bool	streq()
include	"mwcs.com"

begin
	if (streq (fnname, "linear"))
	    return (F_LINEAR)

	fn = ERR
	do i = 1, fn_nfn
	    if (streq (fnname, FN_NAME(i))) {
		fn = i
		break
	    }
	
	return (fn)
end
