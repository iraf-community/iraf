# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<pmset.h>
include	"mio.h"

# MIO_OPENO -- Open an MIO descriptor for the given mask and data image.

pointer procedure mio_openo (pm, im)

pointer	pm			#I mask descriptor
pointer	im			#I image descriptor

pointer	mp

begin
	call calloc (mp, LEN_MIODES, TY_STRUCT)
	call malloc (M_RLP(mp), RL_MAXLEN(pm), TY_INT)
	RLI_LEN(M_RLP(mp)) = 0

	call amovkl (1, M_VS(mp,1), IM_MAXDIM)
	call amovl (IM_LEN(im,1), M_VN(mp,1), IM_MAXDIM)

	M_IM(mp) = im
	M_PM(mp) = pm
	call pm_seti (pm, P_REFIM, im)
	call mio_setrange (mp, M_VS(mp,1), M_VN(mp,1), IM_NDIM(im))

	return (mp)
end
