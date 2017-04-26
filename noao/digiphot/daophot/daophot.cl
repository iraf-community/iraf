# Package script task for the DAOPHOTX package.
#{ DAOPHOTX -- Point Spread Function photometry package.



# Load other packages

dataio			# rfits task is required for the daotest script
;

package daophot

task	setimpars	= "daophot$setimpars.cl"
task	daotest		= "daophot$daotest.cl"

task	daofind,
	phot		= "daophot$x_apphot.e"

task	addstar,
	allstar,
	group,
	grpselect,
	nstar,
	peak,
	pfmerge,
	psf,
	pstselect,
	seepsf,
	daoedit,
	substar		= "daophot$x_daophot.e"

task	datapars	= "daophot$datapars.par"
task	findpars	= "daophot$findpars.par"
task	centerpars	= "daophot$centerpars.par"
task	fitskypars	= "daophot$fitskypars.par"
task	photpars	= "daophot$photpars.par"
task	daopars		= "daophot$daopars.par"

# PTOOLS tasks

task	pconvert,
	istable,
	txcalc,
	txconcat,
	txdump,
	txrenumber,
	txselect,
	txsort		= "ptools$x_ptools.e"

task	pexamine	= "daophot$x_ptools.e"

task	xyplot		= "ptools$xyplot.par"
task	histplot	= "ptools$histplot.par"
task	radplot		= "ptools$radplot.par"
task	surfplot	= "ptools$surfplot.par"
task	cntrplot	= "ptools$cntrplot.par"

# PTOOLS scripts which depend on PTOOLS and TTOOLS tasks

task	pcalc		= "ptools$pcalc.cl"
task	pconcat		= "ptools$pconcat.cl"
task	pdump	 	= "ptools$pdump.cl"
task	prenumber	= "ptools$prenumber.cl"
task	pselect		= "ptools$pselect.cl"
task	psort		= "ptools$psort.cl"

# PTOOLS Scripts which depend only on TTOOLS tasks

task	tbconcat	= "ptools$tbconcat.cl"
task	tbcalc		= "ptools$tbcalc.cl"
task	tbdump		= "ptools$tbdump.cl"

hidetask    istable, txcalc, txconcat, txdump, txrenumber, txselect, txsort
hidetask    xyplot, histplot, radplot, surfplot, cntrplot
hidetask    tbcalc, tbconcat, tbdump

clbye()
