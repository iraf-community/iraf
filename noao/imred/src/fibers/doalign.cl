# DOALIGN -- Align sky lines in objects.
# If there is no database of features for alignment have user identify
# them interactively.

procedure doalign (spec, specms, align, table, logfile, batch)

file	spec
file	specms
file	align
file	table
file	logfile
bool	batch

begin
	file	temp
	bool	log, verbose1

	if (batch)
	    verbose1 = no
	else
	    verbose1 = verbose

	if (!access (align)) {
	    print ("Identify alignment features")
	    dispcor (specms, align, linearize=no,
		database=database, table=table, w1=INDEF,
		w2=INDEF, dw=INDEF, nw=INDEF, log=params.log,
		flux=params.flux, samedisp=no, global=no,
		ignoreaps=no, confirm=no, listonly=no,
		verbose=no, logfile="")
	    identify (align, section="middle line", database=database,
		coordlist="", nsum=1, match=params.match, maxfeatures=50,
		zwidth=100., ftype="emission", fwidth=params.fwidth,
		cradius=params.cradius, threshold=params.threshold,
		minsep=2., function=params.i_function,
		order=params.i_order, sample="*",
		niterate=params.i_niterate, low_reject=params.i_low,
		high_reject=params.i_high, grow=0., autowrite=yes)
	    print ("g") |
	    identify (align, section="middle line", database=database,
		coordlist="", nsum=1, match=params.match, maxfeatures=50,
		zwidth=100., ftype="emission", fwidth=params.fwidth,
		cradius=params.cradius, threshold=params.threshold,
		minsep=2., function=params.i_function,
		order=params.i_order, sample="*",
		niterate=params.i_niterate, low_reject=params.i_low,
		high_reject=params.i_high, grow=0., autowrite=yes,
		cursor="STDIN", >G "dev$null", >& "dev$null")
	    reidentify (align, "",
		interactive=no, section="middle line", shift=0.,
		step=1, nsum=1, cradius=params.cradius,
		threshold=params.threshold, nlost=100, newaps=no,
		refit=no, trace=no, override=yes, addfeatures=no,
		database=database, plotfile=plotfile,
		logfiles=logfile, verbose=verbose1)
	}

	# Set arc dispersion function in image header.
	if (!batch)
	    print ("Identify alignment features in ", spec)
	print ("Identify alignment features in ", spec, >> logfile)
	dispcor (specms, "", linearize=no,
	    database=database, table=table, w1=INDEF,
	    w2=INDEF, dw=INDEF, nw=INDEF, log=params.log,
	    flux=params.flux, samedisp=no, global=no,
	    ignoreaps=no, confirm=no, listonly=no,
	    verbose=no, logfile="")
	hedit (specms, "refspec1", align, add=yes,
	    verify=no, show=no, update=yes)
	delete (database//"/id"//spec//".ms", verify=no, >& "dev$null")
	reidentify (align, specms,
	    interactive=no, section="middle line", shift=0.,
	    step=1, nsum=1, cradius=params.cradius,
	    threshold=params.threshold, nlost=100, newaps=no,
	    refit=no, trace=no, override=no, addfeatures=no,
	    database=database, plotfile=plotfile,
	    logfiles=logfile, verbose=verbose1)
end
