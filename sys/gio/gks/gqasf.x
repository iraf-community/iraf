# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gks.h"

# GQASF -- Inquire aspect source flags. 

procedure gqasf (ierror, lasf)

int	lasf[13]		# Array of source aspect flags
int	ierror			# Error indicator, where ierror = 0 for no error
int	i
include	"gks.com"

begin
	ierror = 0
	do i = 1, NASF
	    lasf[i] = gk_asf[i]
end
