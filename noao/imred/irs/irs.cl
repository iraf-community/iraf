#{ IRS -- KPNO IRS Spectral Reduction Package

# Load necessary packages

lists		# List package for table

# Define necessary paths

set	onedstds	= "noao$lib/onedstds/"
set	irscal		= "onedstds$irscal/"

package irs

task	addsets,
	bswitch,
	calibrate,
	dispcor,
	flatfit,
	flatdiv,
	identify,
	names,
	reidentify,
	refspectra,
	sensfunc,
	splot,
	shedit,
	slist,
	specplot,
	standard,
	subsets		= irs$x_onedspec.e

task	coefs,
	combine,
	lcalib,
	mkspec,
	rebin,
	sflip,
	sinterp,
	sums		= onedspec$x_onedspec.e

# Scripts

task	batchred	= irs$batchred.cl
task	continuum	= irs$continuum.cl
task	shparams	= onedspec$shparams.par
task	dispcor1	= onedspec$dispcor1.par
task	bplot		= onedspec$bplot.cl
task	sextract	= onedspec$sextract.cl

# Define a task living in the users directory - it is created by BATCHRED

task	$process	= process.cl
hidetask shparams,dispcor1

clbye()
