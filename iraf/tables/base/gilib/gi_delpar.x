include	<imio.h>
include <mach.h>
include "gi.h"

# GI_DELPAR -- delete a group parameter from the STF structure. Also delete
#              the entry from the IMIO user area.
#	       WARNING: Deleting a TY_SHORT parameter might cause a 
#		        memory alignment abort. (bus error more likely)

procedure gi_delpar (im, pname)

pointer	im			#I image descriptor
char	pname[ARB]		#I parameter name

pointer	pp, stf, po, pi

int	i, j, pnum, strncmp(), nbits, imaccf()

begin

	stf = IM_KDES(im)
	
	pnum = STF_PCOUNT(stf)
	do i = 1, pnum {
	   pp = STF_PDES(stf,i)
	   if (strncmp(pname, P_PTYPE(pp), SZ_PTYPE) == 0) {
	      nbits = P_PSIZE(pp)
	      for (j = i; j < pnum; j=j+1) {
		  pi = STF_PDES(stf,j+1)
		  po = STF_PDES(stf,j)
		  call amovi (P_OFFSET(pi), P_OFFSET(po), LEN_PDES)
	      }
	      break
	   }
	}

	if (i <= pnum) {
	   STF_PSIZE(stf) =  STF_PSIZE(stf) - nbits
	   STF_SZGROUP(stf) = STF_SZGROUP(stf) - 
			   nbits / (SZB_CHAR * NBITS_BYTE)
	   STF_PCOUNT(stf) = STF_PCOUNT(stf) - 1
	}
	if (imaccf (im, pname) == YES)
	   call imdelf (im, pname)
end
