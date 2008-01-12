# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# include "ccp.h"

# CCP_CLOSEWS -- Close the named workstation.  
# If the plot were terminated (plot (0, 0, 999)) APPEND mode would not work.

procedure ccp_closews (devname, n)

short	devname[ARB]		# device name (not used)
int	n			# length of device name
# include "ccp.com"

begin
	# noop 
	return 
end
