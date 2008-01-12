include <mach.h>
include <imio.h>
include "gi.h"

# Procedure to change INTEGER*2 gpb value to INTEGER*4.
procedure gi_fixpsize(im)

pointer	im			# i: image descriptor (stf format)

pointer	pp
int	i, pcount, stf

begin

	stf = IM_KDES(im)
	pcount = STF_PCOUNT(stf)
	do i = 1, pcount {
	   pp = STF_PDES(stf,i)
	   switch (P_SPPTYPE(pp)) {
	   case TY_SHORT:
		call strcpy ("INTEGER*4", P_PDTYPE(pp), SZ_PDTYPE)
		P_SPPTYPE(pp) = TY_LONG
		STF_PSIZE(stf) = STF_PSIZE(stf) + SZB_CHAR * NBITS_BYTE 
		P_PSIZE(pp) = P_PSIZE(pp) + SZB_CHAR * NBITS_BYTE
		STF_SZGROUP(stf) = STF_SZGROUP(stf) + SZB_CHAR
	   default:
	         ;
	   }
	}
end
