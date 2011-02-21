# Package script task for the PTOOLSX package.
#{ PTOOLSX -- Photometry tools package.


package ptools

# Define the ptools executable tasks.

task	pconvert,
	istable,
	pexamine,
	tbcrename,
	tbkeycol,
	txconcat,
	txcalc,
	txdump,
	txrenumber,
	txselect,
	txsort		= "ptools$x_ptools.e"

# Define the tasks which are CL scripts.

task	tbconcat	= "ptools$tbconcat.cl"
task	tbdump		= "ptools$tbdump.cl"
task	tbcalc		= "ptools$tbcalc.cl"
task	tbrenumber	= "ptools$tbrenumber.cl"
task	tbselect	= "ptools$tbselect.cl"
task	tbsort		= "ptools$tbsort.cl"

task	pconcat		= "ptools$pconcat.cl"
task	pcalc		= "ptools$pcalc.cl"
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
