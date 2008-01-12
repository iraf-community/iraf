include <imio.h>
include <imhdr.h>
include "gi.h"

define   SZ_OBJ  70
# GI_CPGPB -- Procedure to copy the IM_USERAREA from the input image 
#             given its descriptor 'im', to the output area in 'om'.
#	      It copies all the keywords except the gpb ones;
#	      the output image is open mode NEW_IMAGE, so it has a 
#	      default set of keyword for the gpb.

procedure gi_cpdgpb (im, om)

pointer im	# input image descriptor
pointer om	# output image descriptor

int	pn, pp, sg, out, in, stf, stfo
int	stropen(), strlen(), len_hdrmem
char    buf[SZ_OBJ]

begin

	stf = IM_KDES(im)
	stfo = IM_KDES(om)

	sg = SZ_EXTRASPACE + strlen (Memc[IM_USERAREA(im)])

	IM_HDRLEN(om) = LEN_IMHDR +
	    (sg - SZ_EXTRASPACE + SZ_STRUCT-1) / SZ_STRUCT
	len_hdrmem = LEN_IMHDR +
	    (sg+1 + SZ_STRUCT-1) / SZ_STRUCT

	if (IM_LENHDRMEM(om) < len_hdrmem) {
	    IM_LENHDRMEM(om) = len_hdrmem
	    call realloc (om, IM_LENHDRMEM(om) + LEN_IMDES, TY_STRUCT)
	}

	in = stropen (Memc[IM_USERAREA(im)], ARB, READ_ONLY)
	out = stropen (Memc[IM_USERAREA(om)], sg, APPEND)
	call stf_copyfits (stf, in, NULL, out)

	call close (in)
	call close (out)

	# Now copy the GPB values from the input UA to the
	# output user area.
	for (pn=1;  pn <= STF_PCOUNT(stfo);  pn=pn+1) {
	    pp = STF_PDES(stfo,pn)

	    call imgstr (im, P_PTYPE(pp), buf, SZ_OBJ)
	    call impstr (om, P_PTYPE(pp), buf)
	}
end
