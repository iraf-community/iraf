#{ COUDE -- KPNO Coude Spectral Reduction Package

# Load necessary packages

bias
generic
lists

# Define necessary paths

set	onedstds	= "noao$lib/onedstds/"
set	apextract	= "twodspec$apextract/"

package coude

task	apedit,
	dispcor,
	identify,
	names,
	reidentify,
	refspectra,
	setdisp,
	shedit,
	slist,
	specplot,
	splot		= coude$x_onedspec.e

task	apfind,
	apstrip,
	apsum,
	aptrace		= apextract$x_apextract.e

task	combine,
	rebin,
	sflip		= onedspec$x_onedspec.e

# Scripts

task	continuum	= coude$continuum.cl
task	shparams	= onedspec$shparams.par
task	dispcor1	= onedspec$dispcor1.par
task	apdefault	= apextract$apdefault.cl
task	apio		= apextract$apio.cl
task	bplot		= onedspec$bplot.cl
task	sextract	= onedspec$sextract.cl

hidetask apstrip,shparams,dispcor1

clbye()
