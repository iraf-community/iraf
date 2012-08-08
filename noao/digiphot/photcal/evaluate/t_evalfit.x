include	<error.h>
include <math/nlfit.h>
include	"../lib/io.h"
include	"../lib/parser.h"

# Define the pointer Mem
define	MEMP	Memi

# T_EVALFIT - Main photometry processing task. This task will convert 
# intrumental photometric indices into standard indices, by using the 
# same configuration table and coefficients found by FITCOEFFS.

procedure t_evalfit()

pointer	observations		# pointer to the observations files list
pointer	catalogs		# pointer to the catalogs files list
pointer	config			# pointer to configuration file name
pointer paramfile		# pointer to fitted parameters file name
pointer calibfile		# pointer to the output file name
int	type			# type of output to be processed
int	etype			# algorithm for computing errors
pointer	print			# pointer to the output variables list
pointer	formatstr		# pointer to the output format string
pointer	catdir			# pointer to the standard catalogs directory

int	i, j, getid, matchid, vcol, ecol, pindex, dummy, stat
int	obslist, stdlist, plist, ofd, ifd, sym, symvar, ncols
int	ntrneqs, nparams, nstd, nobsvars, nvars, len_plist
pointer	sp, input, starname, dummyname, stdtable, cmap, omap
pointer	vars, uservars, usererrs, fsym, rsym, esym, params, errors, psym, pcols
real	ref, fit, errval, resid, chisqr, rms, pval

bool	clgetb()
int	clgwrd(), open(), io_gcoeffs(), io_gobs(), pr_parse(), pr_geti()
int	pr_gsym(), pr_gsymi(), pr_findmap1(), pr_gvari(), fntopnb()
int	fntgfnb(), fntlenb(), ph_mkplist(), ph_header(), ph_ofields()
pointer	pr_gsymp(), pr_xgetname()
real	pr_eval(), ph_erval()
errchk	io_gcoeffs()

begin
	# Allocate space for file names and character strings.
	call smark (sp)
	call salloc (observations, SZ_LINE, TY_CHAR)
	call salloc (catalogs, SZ_LINE, TY_CHAR)
	call salloc (config, SZ_FNAME, TY_CHAR)
	call salloc (paramfile, SZ_FNAME, TY_CHAR)
	call salloc (calibfile, SZ_FNAME, TY_CHAR)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (starname, SZ_LINE, TY_CHAR)
	call salloc (dummyname, SZ_LINE, TY_CHAR)
	call salloc (print, SZ_LINE, TY_CHAR)
	call salloc (formatstr, SZ_LINE, TY_CHAR)
	call salloc (catdir, SZ_FNAME, TY_CHAR)

	# Get the input and output file lists and  names.
	call clgstr ("observations", Memc[observations], SZ_LINE)
	call clgstr ("config", Memc[config], SZ_FNAME)
	call clgstr ("parameters", Memc[paramfile], SZ_FNAME)
	call clgstr ("calib", Memc[calibfile], SZ_FNAME)
	call clgstr ("catalogs", Memc[catalogs], SZ_LINE)
	call clgstr ("catdir", Memc[catdir], SZ_FNAME)

	# Get output type flags.
	etype = clgwrd ("errors", Memc[dummyname], SZ_LINE, ERR_OPTIONS)
	type = clgwrd ("objects", Memc[dummyname], SZ_LINE, TYPE_STRING)
	call clgstr ("print", Memc[print], SZ_LINE)
	call clgstr ("format", Memc[formatstr], SZ_LINE)

	# Open the output file.
	iferr {
	    if (clgetb ("append"))
	        ofd = open (Memc[calibfile], APPEND, TEXT_FILE)
	    else
	        ofd = open (Memc[calibfile], NEW_FILE, TEXT_FILE)
	} then {
	    call erract (EA_WARN)
	    call sfree (sp)
	    return
	}

	# Parse the configuration table.
	if (pr_parse (Memc[config]) == ERR) {
	    call eprintf ("Error: Cannot parse the configuration file\n")
	    call close (ofd)
	    call sfree (sp)
	    return
	}

	# Get number of variables and allocate memory for their values.
	# Program stars do not have standard indices (last values in
	# the array), and they will remain undefined, and this will give
	# undefined equation values for equations that use them.

	nobsvars = pr_geti (NOBSVARS)
	nvars = nobsvars + pr_geti (NCATVARS)
	call salloc (vars, nvars, TY_REAL)
	call salloc (uservars, nvars, TY_INT)
	call aclri (Memi[uservars], nvars)
	call salloc (usererrs, nvars, TY_INT)
	call aclri (Memi[usererrs], nvars)

	# Map observational and catalog columns. This has to be done here in
	# order to avoid mapping columns every time a new program
	# star is read.

	call pr_catmap (cmap, dummy)
	call pr_obsmap (omap, dummy)

	# Get all the parameter values for all the equations. This has
	# to be done outside the main loop to minimize disk i/o.
	# Otherwise it would be necessary to fetch them for each
	# star observation.

	ntrneqs = pr_geti (NTRNEQS)
	call salloc (fsym, ntrneqs, TY_POINTER)
	call salloc (rsym, ntrneqs, TY_POINTER)
	call salloc (esym, ntrneqs, TY_POINTER)
	call salloc (params, ntrneqs, TY_POINTER)
	call salloc (errors, ntrneqs, TY_POINTER)
	do i = 1, ntrneqs {

	    # Get equation symbol and number of parameters for it.
	    sym = pr_gsym (i, PTY_TRNEQ)
	    nparams = pr_gsymi (sym, PTEQNPAR)

	    # Get the fit, reference and error equation pointers.
	    Memi[fsym+i-1] = pr_gsymp (sym, PTEQRPNFIT)
	    Memi[rsym+i-1] = pr_gsymp (sym, PTEQRPNREF)
	    Memi[esym+i-1] = pr_gsymp (sym, PTEQRPNERROR)

	    # Determine which catalog and observational variables were
	    # actually used in the reference equations and which have
	    # defined error columns.

	    do j = 1, pr_gsymi (sym, PTEQNRCAT) + pr_gsymi(sym, PTEQNROBS) {
		symvar = pr_gvari (sym, j, PTEQREFVAR)
		vcol = pr_gsymi (symvar, PINPCOL)
		ecol = pr_gsymi (symvar, PINPERRCOL)
		if (pr_gsymi (symvar, PSYMTYPE) == PTY_CATVAR) {
		    vcol = pr_findmap1 (cmap, vcol) + nobsvars
		    if (! IS_INDEFI(ecol))
		        ecol = pr_findmap1 (cmap, ecol) + nobsvars
		} else {
		    vcol = pr_findmap1 (omap, vcol)
		    if (! IS_INDEFI(ecol))
		        ecol = pr_findmap1 (omap, ecol)
		}
		Memi[uservars+vcol-1] = vcol
		if (! IS_INDEFI(ecol))
		    Memi[usererrs+vcol-1] = ecol
	    }

	    # Determine which catalog and observational variables were
	    # actually used in the fit equations and which have
	    # defined error columns.

	    do j = 1, pr_gsymi (sym, PTEQNFCAT) + pr_gsymi(sym, PTEQNFOBS) {
		symvar = pr_gvari (sym, j, PTEQFITVAR)
		vcol = pr_gsymi (symvar, PINPCOL)
		ecol = pr_gsymi (symvar, PINPERRCOL)
		if (pr_gsymi (symvar, PSYMTYPE) == PTY_CATVAR) {
		    vcol = pr_findmap1 (cmap, vcol) + nobsvars
		    if (! IS_INDEFI(ecol))
		        ecol = pr_findmap1 (cmap, ecol) + nobsvars
		} else {
		    vcol = pr_findmap1 (omap, vcol)
		    if (! IS_INDEFI(ecol))
		        ecol = pr_findmap1 (omap, ecol)
		}
		Memi[uservars+vcol-1] = vcol
		if (! IS_INDEFI(ecol))
		    Memi[usererrs+vcol-1] = ecol
	    }

	    # Allocate space for parameter values and errors,
	    # for the current equation, and read them from the
	    # parameters file.

	    call salloc (MEMP[params+i-1], nparams, TY_REAL)
	    call salloc (MEMP[errors+i-1], nparams, TY_REAL)
	    iferr {
	        if (io_gcoeffs (Memc[paramfile], sym, stat, chisqr, rms,
		    Memr[MEMP[params+i-1]], Memr[MEMP[errors+i-1]],
		    nparams) != nparams) {
		    call eprintf ("Warning: Error reading parameters for ")
		    call eprintf ("equation %s from %s\n")
			call pargstr (Memc[pr_xgetname (sym)])
			call pargstr (Memc[paramfile])
		    call amovkr (INDEFR, Memr[MEMP[params+i-1]], nparams)
		    call amovkr (INDEFR, Memr[MEMP[errors+i-1]], nparams)
		}
	    } then {
		call erract (EA_WARN)
		call close (ofd)
		call pr_unmap (cmap)
		call pr_unmap (omap)
		call pr_free()
		call sfree (sp)
		return
	    }

	    # Issue a warning if some of the equations in the system did
	    # not converge but proceed with the evaluation.
	    # didn't converge.

	    if (stat != DONE) {
		call eprintf (
		    "Warning: The solution for equation %s did not converge")
		    call pargstr (Memc[pr_xgetname (sym)])
	    }
	}

	# Decide whether to use id and/or catalog matching or not.
	if (pr_geti (MINCOL) <= 1) {
	    getid = NO
	    matchid = NO
	    stdtable = NULL
	}  else if (Memc[catalogs] == EOS) {
	    getid = YES
	    matchid = NO
	    stdtable = NULL
	} else {
	    getid = YES
	    matchid = YES
	}

	# Get the list of optional variables to be printed. These variables
	# may include any of the catalog or observations variables or the
	# set equations. Variables which match any of these catagories
	# will be ommited from the list to be printed.

	plist = fntopnb (Memc[print], NO)
	len_plist = fntlenb (plist)
	if (len_plist > 0) {
	    call salloc (psym, len_plist, TY_INT)
	    call salloc (pcols, len_plist, TY_INT)
	    len_plist = ph_mkplist (plist, cmap, omap, nobsvars, Memi[psym],
	        Memi[pcols], len_plist)
	}
	call fntclsb (plist)

	# Print the output header.
	ncols = ph_header (ofd, Memc[observations], Memc[catalogs],
	    Memc[config], Memc[paramfile], type, getid, Memi[psym],
	    Memi[pcols], len_plist, etype, matchid)

	# Set the formatstr string.
	if (Memc[formatstr] == EOS) {
	    call ph_oformatstr (getid, ncols, Memc[formatstr], SZ_LINE)
	} else if (ph_ofields (Memc[formatstr]) != ncols) {
	    call eprintf ("Warning: The number of formatstr string fields ")
	    call eprintf ("does not match the number of output columns\n")
	    call ph_oformatstr (getid, ncols, Memc[formatstr], SZ_LINE)
	}

	# If catalog matching is enabled, read in the catalog data.
	if (matchid == YES) {
	    stdlist = fntopnb (Memc[catalogs], NO)
	    call io_gcatdat (Memc[catdir], stdlist, stdtable, nstd, dummy)
	    call fntclsb (stdlist)
	} 

	# Loop over the observation files.
	obslist = fntopnb (Memc[observations], NO)
	while (fntgfnb (obslist, Memc[input], SZ_FNAME) != EOF) {

	    # Open the input file.
	    iferr (ifd = open (Memc[input], READ_ONLY, TEXT_FILE)) {
		call erract (EA_WARN)
		next
	    }

	    # Get observations and names for program stars. The call
	    # to io_getline_init() is necessary since it's not called
	    # inside io_gobs(), and it should be called at least once
	    # per input file.

	    call io_getline_init()
	    while (io_gobs (ifd, stdtable, omap, type, Memr[vars], nvars,
		   getid, Memc[starname], Memc[dummyname], SZ_LINE) != EOF) {

		# Print the star name.
		call fprintf (ofd, Memc[formatstr])
		if (getid == YES)
		    call pargstr (Memc[starname])

		# Print the extra variables.
		do i = 1, len_plist {
		    pindex = Memi[pcols+i-1]
		    if (pindex > 0)
			pval = Memr[vars+pindex-1]
		    else
			pval = pr_eval (Memi[psym+i-1], Memr[vars],
			    Memr[MEMP[params]])
		    call pargr (pval)
		}

		# Loop over all the transformatstrion equations.
		do i = 1, pr_geti (NTRNEQS) {

		    # Get the equation symbol.
		    sym = pr_gsym (i, PTY_TRNEQ)

		    # Compute the fit. 
		    fit = pr_eval (Memi[fsym+i-1], Memr[vars],
		        Memr[MEMP[params+i-1]])
		    call pargr (fit)

		    # Compute the error.
		    if (etype != ERR_UNDEFINED) {
		        if (IS_INDEFR(fit))
			    errval = INDEFR
			switch (etype) {
		        case ERR_OBSERRORS:
		            if (IS_INDEFR(fit))
			        errval = INDEFR
			    else
			        errval = ph_erval (Memi[fsym+i-1], fit,
			            Memr[MEMP[params+i-1]], Memr[vars],
				    Memi[uservars], Memi[usererrs], nobsvars)
		        case ERR_EQUATIONS:
			    if (Memi[esym+i-1] == NULL)
			        errval = INDEFR
			    else
			        errval = pr_eval (Memi[esym+i-1], Memr[vars],
			            Memr[MEMP[params+i-1]])
		        default:
			    errval = INDEFR
			}
			call pargr (errval)
		    }

		    # Compute the residual.
		    if (getid == NO || (matchid == YES &&
		        type != TYPE_PROGRAM)) {
		        if (IS_INDEFR (fit))
			    resid = INDEFR
		        else {
		            ref = pr_eval (Memi[rsym+i-1], Memr[vars],
		                Memr[MEMP[params+i-1]])
			    if (IS_INDEFR(ref))
			        resid = INDEFR
			    else
		                resid = ref - fit
		        }
		        call pargr (resid)
		    }
		}
	    }

	    # Close the input file.
	    call close (ifd)
	}

	# Free memory.
	call sfree (sp)
	call pr_free()
	call pr_unmap (cmap)
	call pr_unmap (omap)

	# Close all files.
	if (stdtable != NULL)
	    call stclose (stdtable)
	call close (ofd)
	call fntclsb (obslist)
end


# PH_HEADER - Print the output file header.

int procedure ph_header (fd, observations, catalogs, config, paramfile, type,
	getid, psym, pcols, len_plist, etype, matchid)

int	fd			# output file descriptor
char	observations[ARB]	# observation file list
char	catalogs[ARB]		# catalog file list
char	config[ARB]		# configuration file name
char	paramfile[ARB]		# fitted parameters file
int	type			# type of object to output
int	getid			# output the object id
int	psym[ARB]		# list of additional variables to be output
int	pcols[ARB]		# columns numbers of additional variables
int	len_plist		# length of symbol list
int	etype			# type of error output
int	matchid			# is it possible to match ids

int	i, olist, slist, ncols
pointer sp, time, name
int	fntopnb(), fntgfnb(), pr_geti(), pr_gsym()
long	clktime()
pointer	pr_gsymc(), pr_gsymp(), pr_xgetname()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (time, SZ_LINE, TY_CHAR)
	call salloc (name, SZ_FNAME, TY_CHAR)

	# Add the time stamp.
	call cnvtime (clktime (0), Memc[time], SZ_LINE)
	call fprintf (fd, "\n# %s\n")
	    call pargstr (Memc[time])

	# Add the observation file names.
	olist = fntopnb (observations, NO)
	call fprintf (fd, "# List of observations files:\n") 
	while (fntgfnb (olist, Memc[name], SZ_FNAME) != EOF) {
	    call fprintf (fd, "#\t\t%s\n")
		call pargstr (Memc[name])
	}
	call fntclsb (olist)

	# Add the catalog file names.
	if (matchid == YES) {
	    slist = fntopnb (catalogs, NO)
	    call fprintf (fd, "# List of catalog files:\n") 
	    while (fntgfnb (slist, Memc[name], SZ_FNAME) != EOF) {
		call fprintf (fd, "#\t\t%s\n")
		    call pargstr (Memc[name])
	    }
	    call fntclsb (slist)
	}

	# Add the configuration file name.
	call fprintf (fd, "# Config:\t%s\n")
	    call pargstr (config)

	# Add the parameters file name.
	call fprintf (fd, "# Parameters:\t%s\n")
	    call pargstr (paramfile)

	# Add the output options.
	call fprintf (fd, "#\n")
	if (matchid == YES) {
	    if (type == TYPE_ALL)
		call fprintf (fd,
		    "# Computed indices for program and standard objects\n")
	    else if (type == TYPE_PROGRAM)
		call fprintf (fd,
		    "# Computed indices for program objects only\n")
	    else if (type == TYPE_STANDARDS)
		call fprintf (fd,
		    "# Computed indices for standard objects only\n")
	} else
	    call fprintf (fd,
	        "# Computed indices for program and standard objects\n")

	# Print the optional id header.
	call fprintf (fd, "#\n")
	call fprintf (fd, "# Columns:\n")
	if (getid == YES) {
	    call fprintf (fd, "#\t1\tobject id\n")
	    ncols = 1
	} else
	    ncols = 0

	# Print headers for the variables in the print list and set any set
	# equation offsets to pointers.
	do i = 1, len_plist {
	    ncols = ncols + 1
	    call fprintf (fd, "#\t%d\t%s\n")
		call pargi (ncols)
		call pargstr (Memc[pr_xgetname(psym[i])])
	    if (pcols[i] <= 0)
		psym[i] = pr_gsymp (psym[i], PSEQRPNEQ)
	}


	# Print the equation headers.
	do i = 1, pr_geti (NTRNEQS) {
	    ncols = ncols + 1
	    call fprintf (fd, "#\t%d\t%s\n")
		call pargi (ncols)
	        call pargstr (Memc[pr_gsymc(pr_gsym (i,PTY_TRNEQ),PTEQREF)])
	    if (etype != ERR_UNDEFINED) {
	        ncols = ncols + 1
	        call fprintf (fd, "#\t%d\terror(%s)\n")
		    call pargi (ncols)
	            call pargstr (Memc[pr_gsymc(pr_gsym (i,PTY_TRNEQ),PTEQREF)])
	    }
	    if (getid == NO || (matchid == YES && type != TYPE_PROGRAM)) {
	        ncols = ncols + 1
	        call fprintf (fd, "#\t%d\tresid(%s)\n")
		    call pargi (ncols)
	            call pargstr (Memc[pr_gsymc (pr_gsym(i,PTY_TRNEQ),PTEQREF)])
	    }
	}

	call fprintf (fd,"\n\n")

	call sfree (sp)

	return (ncols)
end
