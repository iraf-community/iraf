#{ Package script task for the ONEDSPEC package.

# Define necessary paths

set	onedstds	= "noao$lib/onedstds/"

package onedspec

task	addsets,
	bswitch,
	calibrate,
	coincor,
	combine,
	dispcor,
	flatfit,
	flatdiv,
	identify,
	lcalib,
	mkspec,
	names,
	rebin,
	refspectra,
	reidentify,
	sensfunc,
	setdisp,
	sflip,
	shedit,
	sinterp,
	slist,
	specplot,
	splot,
	standard,
	subsets,
	sums		= onedspec$x_onedspec.e

# Scripts

task	batchred	= onedspec$batchred.cl
task	continuum	= onedspec$continuum.cl
task	powercor	= onedspec$powercor.cl
task	observatory	= imred$observatory.cl
task	bplot		= onedspec$bplot.cl
task	sextract	= onedspec$sextract.cl

task	$process	= process.cl		# Used by BATCHRED
task	dispcor1	= onedspec$dispcor1.par	# Used by DISPCOR
task	shparams	= onedspec$shparams.par # Used by SHEDIT
hidetask shparams,dispcor1,process

clbye()
