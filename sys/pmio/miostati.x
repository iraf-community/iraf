# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pmset.h>
include	"mio.h"

# MIO_STATI -- Stat an MIO parameter.

int procedure mio_stati (mp, param)

pointer	mp			#I MIO descriptor
int	param			#I parameter to be set

begin
	switch (param) {
	case P_PMDES:
	    return (M_PM(mp))
	case P_IMDES:
	    return (M_IM(mp))
	case P_REGCOORDS:
	    return (M_REGCOORDS(mp))
	}
end
