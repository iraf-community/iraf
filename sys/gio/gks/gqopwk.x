# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gks.h"

# GQOPWK -- Inquire number of open work stations.  From looking at how this
# procedure is called, it seems to have two functions, depending on the value
# of "n".  It returns either the number of active workstations (n=0), or the
# wkid for nth open workstation.

procedure gqopwk (n, errind, ol, wkid)

int	n			# Number of workstation to query
int	errind			# Error indicator; errind = 0 means no error
int	ol			# Returned value (number of open workstations)
int	wkid			# WKID of nth open workstation - returned

int	i, this_wkstation
include	"gks.com"

begin
	if (gk_std == NULL) {
	    # GKS not in proper state; no active workstations
	    errind = 7
	    wkid = -1
	    return
	} else
	    errind = 0

	if (n < 0 || n > NDEV) {
	    # Invalid workstation identifier
	    wkid = -1
	    errind = 502
	    return
	} else {
	    ol = 0
	    if (n == 0) {
	        # return the number of active workstations
	        do i = 1, NDEV {
		    if (gk_status[i] == ACTIVE)
		        ol = ol + 1
	        }
	    } else {
	        # Find the nth open workstation and return its wkid
	        this_wkstation = 0
	        do i = 1, NDEV {
		    if (gk_status[i] == ACTIVE) {
		        this_wkstation = this_wkstation + 1
		        if (this_wkstation == n) {
			    wkid = i
			    break
		        }
		    }
	        }
	    }
	}
end
