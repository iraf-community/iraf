#{ ECHELLE -- KPNO Echelle Spectral Reduction Package

# Load necessary packages

bias
generic
lists
utilities

# Define necessary paths

set	onedstds	= "noao$lib/onedstds/"
set	apextract	= "twodspec$apextract/"

package echelle

	
task	calibrate,
	eccontinuum,
	ecdispcor,
	ecidentify,
	ecreidentify,
	ecselect,
	refspectra,
	setdisp,
	shedit,
	slist,
	sensfunc,
	specplot,
	splot,
	standard	= echelle$x_onedspec.e

task	apfind,
	apnormalize,
	apscatter,
	apstrip,
	apsum,
	aptrace		= apextract$x_apextract.e

task	apedit		= echelle$x_apextract.e

# Scripts

task	ecbplot		= echelle$ecbplot.cl

task	apio		= echelle$apio.par
task	apscat1		= echelle$apscat1.par
task	apscat2		= echelle$apscat2.par
task	dispcor1	= onedspec$dispcor1.par
task	shparams	= onedspec$shparams.cl
task	apdefault	= apextract$apdefault.cl

hidetask apstrip,apscat1,apscat2,shparams,dispcor1

clbye()
