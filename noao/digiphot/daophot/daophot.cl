# Package script task for the DAOPHOT package.
#{ DAOPHOT -- Point Spread Function photometry package.

if (deftask ("tables")) {
    tables
} else {
    type "daophot$lib/warning.dat"
}

dataio			# rfits task is required for the daotest script

package daophot

task	daofind,
	phot		= "daophot$x_apphot.e"

task	addstar,
	allstar,
	group,
	grpselect,
	nstar,
	peak,
	psf,
	seepsf,
	substar		= "daophot$x_daophot.e"

task	datapars	= "daophot$datapars.par"
task	centerpars	= "daophot$centerpars.par"
task	fitskypars	= "daophot$fitskypars.par"
task	photpars	= "daophot$photpars.par"
task	daopars		= "daophot$daopars.par"
task	daotest		= "daophot$daotest.cl"

# PTOOLS tasks

task	pconvert,
	istable,
	txappend,
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

task	pappend		= "ptools$pappend.cl"
task	pdump	 	= "ptools$pdump.cl"
task	prenumber	= "ptools$prenumber.cl"
task	pselect		= "ptools$pselect.cl"
task	psort		= "ptools$psort.cl"

# PTOOLS Scripts which depend only on TTOOLS tasks

task	tbappend	= "ptools$tbappend.cl"
task	tbdump		= "ptools$tbdump.cl"

hidetask    istable, txappend, txdump, txrenumber, txselect, txsort
hidetask    xyplot, histplot, radplot, surfplot, cntrplot
hidetask    tbappend, tbdump

clbye()
