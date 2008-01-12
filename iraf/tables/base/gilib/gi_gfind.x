include <imio.h>
include "gi.h"

# GI_GFIND -- Find the index of a group parameter keyword
#
# This procedure returns the index of a keyword in the group parameter block.
# If the keyword is not a group parameter, the procedure returns zero.
#
# B.Simon	24-Apr-90	Original
# B.Simon	12-Jul-93	gi_geis extracted

int procedure gi_gfind (im, keyword)

pointer im		# i: Image descriptor
char	keyword[ARB] 	# i: Group parameter keyword name
#--
int	pn
pointer	sp, key, stf, pp

bool	streq(), gi_geis()

begin
	# If this is not a GEIS format image, it can't be
	# a group parameter

	if (! gi_geis (im))
	    return (0)

	# Convert keyword to upper case

	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call strcpy (keyword, Memc[key], SZ_FNAME)
	call strupr (Memc[key])

	# Check each group parameter. If the name matches the 
	# keyword name, return the number of the keyword. If
	# no match was found, return zero.

	stf = IM_KDES(im)
	for (pn = 1; pn <= STF_PCOUNT(stf); pn = pn + 1) {
	    pp = STF_PDES(stf,pn)
	    if (streq (Memc[key], P_PTYPE(pp)))
		break
	}

	call sfree (sp)
	if (pn > STF_PCOUNT(stf))
	    return (0)
	else
	    return (pn)

end
