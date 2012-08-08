#{ OBSUTIL.CL -- Observing utilities package.

package	obsutil

# Logical directories

set	obssrc = "obsutil$src/"
set	ccdtimesrc = "obssrc$ccdtime/"
set	pairmass = "obssrc$pairmass/"
set	sptime = "obssrc$sptime/"
set	sptimelib = "sptime$lib/"
set	specfocus = "obssrc$specfocus/"
set	starfocus = "obssrc$starfocus/"


# Executable Tasks

task	bitcount	= "obssrc$x_obsutil.e"
task	ccdtime		= "ccdtimesrc$x_obsutil.e"
task	pairmass	= "pairmass$x_obsutil.e"
task	specfocus	= "specfocus$x_obsutil.e"
task	sptime,
	$cgiparse	= "sptime$x_obsutil.e"
task	psfmeasure,
	starfocus	= "starfocus$x_obsutil.e"

# Script Tasks

task	findgain = "obssrc$findgain.cl"
task	shutcor = "obssrc$shutcor.cl"

# Pset Tasks

task	specpars = "sptime$specpars.par"

# User Tasks.
if (access (obsutil.custom//"scripts.cl"))
    cl (< obsutil.custom//"scripts.cl")
else
    ;

clbye
