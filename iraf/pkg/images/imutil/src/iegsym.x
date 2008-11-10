include	<ctotok.h>
include	<imhdr.h>
include	<ctype.h>
include	<mach.h>
include	<imset.h>
include	<fset.h>
include	<lexnum.h>
include	<evvexpr.h>
include	"gettok.h"


# Expression database symbol.
define	LEN_SYM		2
define	SYM_TEXT	Memi[$1]
define	SYM_NARGS	Memi[$1+1]



# IE_GSYM -- Get symbol routine for the gettok package.

pointer procedure ie_gsym (st, symname, nargs)

pointer	st			#I symbol table
char	symname[ARB]		#I symbol to be looked up
int	nargs			#O number of macro arguments

pointer	sym
pointer	strefsbuf(), stfind()

begin
	sym = stfind (st, symname)
	if (sym == NULL)
	    return (NULL)

	nargs = SYM_NARGS(sym)
	return (strefsbuf (st, SYM_TEXT(sym)))
end
