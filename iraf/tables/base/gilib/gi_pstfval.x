include <imio.h>
include "gi.h"

# GI_PSTFVAL -- Change the value of an stf descriptor

procedure gi_pstfval (im, what, value)

pointer	im	# image descriptor
char	what[SZ_DATATYPE]
int	value

int	stf, strcmp()

begin
	stf = IM_KDES(im)
	if (strcmp (what, "GCOUNT") == 0)
	   STF_GCOUNT(stf) = value
	else if (strcmp (what, "PSIZE") == 0)
	   STF_PSIZE(stf) = value
	else if (strcmp (what, "PCOUNT") == 0)
	   STF_PCOUNT(stf) = value
	else if (strcmp (what, "GROUPS") == 0)
	   STF_GROUPS(stf) = value
	else if (strcmp (what, "GROUP") == 0)
	   STF_GROUP(stf) = value
	else if (strcmp (what, "SZGROUP") == 0)
	   STF_SZGROUP(stf) = value
	else if (strcmp (what, "NEWIMAGE") == 0)
	   STF_NEWIMAGE(stf) = value
	else if (strcmp (what, "NAXIS") == 0)
	   STF_NAXIS(stf) = value
	else if (strcmp (what, "NAXIS1") == 0)
	   STF_LENAXIS(stf,1) = value
	else if (strcmp (what, "NAXIS2") == 0)
	   STF_LENAXIS(stf,2) = value
	else if (strcmp (what, "NAXIS3") == 0)
	   STF_LENAXIS(stf,3) = value
	else
	   call error (13,"gi_pstfval: Item not found")

end
