include "../lib/daophot.h"

# DP_PPPARS -- Procedure to store the daophot package parameters.

procedure dp_pppars (dao)

pointer	dao		# pointer to daophot structure

int	dap
bool	itob()
int	dp_stati()
pointer	clopset()
real	dp_statr()

begin
	# Set the package parameter text.
	call clputb ("text", itob (dp_stati (dao, TEXT)))

	# Get and set the datapars parameter sets.
	dap = clopset ("datapars")
	call clppsetr (dap, "datamin", dp_statr (dao, MINGDATA))
	call clppsetr (dap, "datamax", dp_statr (dao, MAXGDATA))
	call clcpset (dap)

	# Get and set the daopars parameters.
	dap = clopset ("daopars")
	call clppsetb (dap, "varpsf", itob (dp_stati (dao, VARPSF)))
	call clppsetr (dap, "psfrad", dp_statr (dao, PSFRAD))
	call clppsetr (dap, "fitrad", dp_statr (dao, FITRAD))
	call clppsetr (dap, "matchrad", dp_statr (dao, MATCHRAD))
	call clppsetr (dap, "critoverlap", dp_statr (dao, CRITOVLAP))
	call clppseti (dap, "maxiter", dp_stati (dao, MAXITER))
	call clppseti (dap, "maxgroup", dp_stati (dao, MAXGROUP))
	call clppseti (dap, "maxnstar", dp_stati (dao, MAXSTAR))
	call clppsetb (dap, "recenter", itob (dp_stati (dao, RECENTER)))
	call clppsetr (dap, "cliprange", dp_statr (dao, CLIPRANGE))
	call clppseti (dap, "clipexp", dp_stati (dao, CLIPEXP))
	call clcpset (dap)
end
