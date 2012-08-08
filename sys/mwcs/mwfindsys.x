# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"mwcs.h"

# MW_FINDSYS -- Lookup the named world coordinate system and return a pointer
# to the WCS descriptor, or NULL if the system is not defined.

pointer procedure mw_findsys (mw, system)

pointer	mw			#I pointer to MWCS descriptor
char	system[ARB]		#I system to be looked up

int	i
pointer	wp
bool	streq()

begin
	# Search the list of defined systems.
	do i = 1, MI_NWCS(mw) {
	    wp = MI_WCSP(mw,i)
	    if (WCS_SYSTEM(wp) != NULL)
		if (streq (S(mw,WCS_SYSTEM(wp)), system))
		    return (wp)
	}

	# Not found.
	return (NULL)
end
