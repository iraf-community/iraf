include <imio.h>
include "gi.h"

# GI_GGPV -- Procedure to get a group parameter value on a
# string buffer.

procedure gi_ggpv (im, pn, buff)

pointer im		# image descriptor
int	pn		# group parameter number 
char	buff[SZ_LINE] 	# Buffer with the gpv in string form

char	blank
int	pp, stf, k, strlen()

data	blank  / ' ' /

begin
	      stf = IM_KDES(im)
	      pp = STF_PDES(stf,pn)

	      if (P_PDTYPE(pp) == 'C') {
	         call imgstr (im, P_PTYPE(pp), buff, P_LEN(pp))
		 # Clear noisy characters
		 k = strlen(buff)
		 call amovkc (blank, buff(k+1), P_LEN(pp)-k)
	      } else {
	         call imgstr (im, P_PTYPE(pp), buff, SZ_LINE)
		 # If datatype is BOOLEAN (LOGICAL) then change
		 # the values T or F to Y and N.
		 if (P_PDTYPE(pp) == 'L') {
		    if (buff[1] == 'T')
		       buff[1] = 'Y'
	  	    else
		       buff[1] = 'N'
		 }
	      }
end
