#{
# DTOI package -- Density to Intensity Transformation Package.  (Feb87)

set	dtoi		= "iraf$noao/imred/dtoi/"

package	dtoi

task	dematch,
	hdfit,
	hdtoi,
	hdshift,
	selftest,
	spotlist	= "dtoi$x_dtoi.e"


clbye()
