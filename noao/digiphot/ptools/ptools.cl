# Package script task for the PTOOLS package.
#{ PTOOLS -- Photometry tools package.

if (deftask ("tables")) {
    tables
} else {
    type "ptools$lib/warning.dat"
}

package ptools

# Define the ptools executable tasks.

task	pconvert,
	istable,
	pexamine,
	tbcrename,
	tbkeycol,
	txappend,
	txdump,
	txrenumber,
	txselect,
	txsort		= "ptools$x_ptools.e"

# Define the tasks which are CL scripts.

task	tbappend	= "ptools$tbappend.cl"
task	tbdump		= "ptools$tbdump.cl"
task	tbrenumber	= "ptools$tbrenumber.cl"
task	tbselect	= "ptools$tbselect.cl"
task	tbsort		= "ptools$tbsort.cl"

task	pappend		= "ptools$pappend.cl"
task	pdump		= "ptools$pdump.cl"
task	prenumber	= "ptools$prenumber.cl"
task	pselect		= "ptools$pselect.cl"
task	psort		= "ptools$psort.cl"

task	pttest		= "ptools$pttest.cl"

# Pset tasks.

task	xyplot		= "ptools$xyplot.par"
task	histplot	= "ptools$histplot.par"
task	radplot		= "ptools$radplot.par"
task	surfplot	= "ptools$surfplot.par"
task	cntrplot	= "ptools$cntrplot.par"

hidetask	tbkeycol, tbcrename
hidetask	xyplot, histplot, radplot, surfplot, cntrplot

clbye()
