#{ IRS -- KPNO IRS Spectral Reduction Package

# Load necessary packages

lists		# List package for table

# Define necessary paths

set	irscal		= "onedstds$irscal/"
set	irsiids		= "onedspec$irsiids/"

package irs

# Standard ONEDSPEC tasks
task	autoidentify,
	continuum,
	deredden,
	dopcor,
	mkspec,
	names,
	sarith,
	sflip,
	sinterp,
	splot,
	specplot,
	specshift	= onedspec$x_onedspec.e
task	scombine	= "onedspec$scombine/x_scombine.e"
task	aidpars		= "onedspec$aidpars.par"
task	dispcor1	= onedspec$dispcor1.par
task	scopy		= onedspec$scopy.cl
hidetask dispcor1

# Special  IRS/IIDS tasks
task	addsets,
	bswitch,
	coefs,
	flatdiv,
	slist1d,
	subsets,
	sums		= irsiids$x_onedspec.e
task	batchred	= irsiids$batchred.cl
task	bplot		= irsiids$bplot.cl
task	extinct		= irsiids$extinct.cl

# Different default parameters
task	calibrate,
	dispcor,
	flatfit,
	identify,
	lcalib,
	reidentify,
	refspectra,
	sensfunc,
	standard	= irs$x_onedspec.e

# Astutil tasks
task	setairmass,
	setjd		= "astutil$x_astutil.e"

# Define a task living in the users directory - it is created by BATCHRED

task	$process	= process.cl

clbye()
