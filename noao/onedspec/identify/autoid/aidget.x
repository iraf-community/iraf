include	"autoid.h"

define	AIDGET "|best|"


# AID_GETR -- Get AID parameters by name.

real procedure aid_getr (aid, param)

pointer	aid		#I AID object
char	param[ARB]	#I Parameter name

char	temp[10]
int	strdic()

begin
	switch (strdic (param, temp, 10, AIDGET)) {
	case 1:
	    return (AID_BEST(aid))
	}
end
