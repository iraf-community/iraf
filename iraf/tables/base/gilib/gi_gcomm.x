include <imio.h>
include "gi.h"

define  MAXP	32000
# GI_GCOMM -- Procedure to get comment from table user parameters
# store in the array 'buf', which has keyword and a text after the
# 9 character position.

procedure gi_gcomm (im, npar, buf)

pointer	im		# Image descriptor
int	npar		# Number of parameters in buffer
char    buf[FITS_RECLEN,MAXP]	# Has the table user parameters.

int	i, j
int	strncmp(), strlen(), plen
pointer stf, pp

begin
	
	stf = IM_KDES(im)

	do i = 1, STF_PCOUNT(stf) {
	   pp = STF_PDES(stf,i)
	   plen = strlen(P_PTYPE(pp))
	   do j = 1, npar {
	      if (strncmp(P_PTYPE(pp), buf[1,j], plen) == 0 ) {
		 call stf_gets (buf[1,j], P_COMMENT(pp), FITS_RECLEN)
		 break
	      }
	   }
	}

end
