# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<pmset.h>
include	"mio.h"

# MIO_OPENO -- Open an MIO descriptor for the given mask and data image.

pointer procedure mio_openo (pm, im)

pointer	pm			#I mask descriptor
pointer	im			#I image descriptor

size_t	sz_val
long	lval
pointer	mp

begin
	sz_val = LEN_MIODES
	call calloc (mp, sz_val, TY_STRUCT)
	call malloc (M_RLP(mp), RL_LENMAX(pm), TY_INT)
	RLI_LEN(M_RLP(mp)) = 0

	lval = 1
	sz_val = IM_MAXDIM
	call amovkl (lval, M_VS(mp,1), sz_val)
	call amovl (IM_LEN(im,1), M_VN(mp,1), sz_val)

	M_IM(mp) = im
	M_PM(mp) = pm
	call pm_setp (pm, P_REFIM, im)
	call mio_setrange (mp, M_VS(mp,1), M_VN(mp,1), IM_NDIM(im))

	return (mp)
end
