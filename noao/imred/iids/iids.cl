#{ IIDS -- KPNO IIDS Spectral Reduction Package

# Load necessary packages

lists		# List package for table

# Define necessary paths

set	onedstds	= "noao$lib/onedstds/"
set	iidscal		= "onedstds$iidscal/"

package iids

task	addsets,
	bswitch,
	calibrate,
	coincor,
	dispcor,
	flatfit,
	flatdiv,
	identify,
	reidentify,
	refspectra,
	sensfunc,
	splot,
	shedit,
	slist,
	specplot,
	standard,
	subsets		= iids$x_onedspec.e

task	coefs,
	combine,
	lcalib,
	mkspec,
	rebin,
	sflip,
	sinterp,
	sums		= onedspec$x_onedspec.e

task	names		= iids$x_onedspec.e

# Scripts

task	batchred	= iids$batchred.cl
task	continuum	= iids$continuum.cl
task	powercor	= iids$powercor.cl
task	shparams	= onedspec$shparams.par
task	dispcor1	= onedspec$dispcor1.par
task	bplot		= onedspec$bplot.cl
task	sextract	= onedspec$sextract.cl

# Define a task living in the users directory - it is created by BATCHRED

task	$process	= process.cl
hidetask shparams,dispcor1

clbye()
