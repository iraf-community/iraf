#{ SPECPHOT -- NOAO CCD Spectrophotometric Reduction Package

# Load necessary packages

bias
generic
lists
proto
twodspec
longslit

# Define necessary paths

set	onedstds	= "noao$lib/onedstds/"
set	ctiocal		= "onedstds$ctiocal/"
set	apextract	= "twodspec$apextract/"

package specphot

task	calibrate,
	dispcor,
	identify,
	reidentify,
	refspectra,
	sensfunc,
	setdisp,
	shedit,
	slist,
	specplot,
	splot,
	standard	= specphot$x_onedspec.e

task	apedit,
	apfind,
	aptrace,
	apsum		= specphot$x_apextract.e

task	apstrip		= apextract$x_apextract.e

task	combine,
	lcalib,
	names,
	rebin,
	sflip,
	sinterp,
	sums		= onedspec$x_onedspec.e

# Scripts

task	continuum	= specphot$continuum.cl
task	shparams	= onedspec$shparams.par
task	dispcor1	= onedspec$dispcor1.par
task	apio		= apextract$apio.cl
task	apdefault	= specphot$apdefault.cl
task	bplot		= onedspec$bplot.cl
task	sextract	= onedspec$sextract.cl

# Hide tasks

hidetask apstrip,shparams,dispcor1,names

clbye()
