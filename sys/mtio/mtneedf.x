# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "mtio.h"

# MTNEEDFILENO -- Returns YES if no file number is specified in mtname, or
# NO if a file is specified.

int procedure mtneedfileno (mtname)

char	mtname[ARB]			#I magtape device specification

int	fileno, recno
pointer	sp, device, devcap
int	btoi()

begin
	call smark (sp)
	call salloc (device, SZ_DEVICE, TY_CHAR)
	call salloc (devcap, SZ_DEVCAP, TY_CHAR)

	call mtparse (mtname, Memc[device], SZ_DEVICE, fileno, recno,
	    Memc[devcap], SZ_DEVCAP)

	call sfree (sp)
	return (btoi(fileno == ERR))
end
