# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pmset.h>
include	"mio.h"

# MIO_CLOSE -- Close an MIO descriptor.

procedure mio_close (mp)

pointer	mp			#I MIO descriptor

begin
	if (M_PMCLOSE(mp) == YES)
	    call pm_close (M_PM(mp))
	if (M_RLP(mp) != NULL)
	    call mfree (M_RLP(mp), TY_INT)
	call mfree (mp, TY_STRUCT)
end
