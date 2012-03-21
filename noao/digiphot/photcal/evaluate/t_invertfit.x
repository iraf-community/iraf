include	<error.h>
include <math/nlfit.h>
include	"../lib/io.h"
include	"../lib/parser.h"
include "../lib/preval.h"

# Define the pointer Mem
define	MEMP	Memi

# T_INVERTFIT - INVERTFIT converts intrumental photometric indices into
# standard indices by using the configuration file, the coefficients
# determined by FITPARAMS and inverting the transformations.

procedure t_invertfit ()

pointer	observations		# pointer to the observations file list
pointer	catalogs		# pointer to the catalogs file list
pointer	config			# pointer to configuration file name
pointer paramfile		# pointer to fitted parameters file name
pointer calibfile		# pointer to the output file name
int	type			# type of output to be processed
int	etype			# algorithm for computing the errors
pointer	print			# pointer to output variables list
pointer	formatstr		# pointer to the output format string
pointer	catdir			# pointer to the standard star directory

int	i, j, vcol, ecol, pindex, dummy, stat, getid, matchid
int	obslist, stdlist, plist, ofd, ifd, sym, symvar, ncols, nstd, nset
int	ntrneqs, nparams, nstdvars, nustdvars, nobsvars, nvars, nueq, maxnset
int	len_plist, refcode
pointer	sp, input, starname, dummyname, stdtable, omap, cmap
pointer	vars, eqvartable, uservars, usererrs, userset, eqset, tvars
pointer	dtvars, avtvars, ervars, svars, servars, params, errors, psym, pcols
pointer	varindex, eqindex	
real	resid, chisqr, rms, pval

bool	clgetb()
int	fntopnb(), fntgfnb(), clgwrd(), open(), io_gcoeffs(), io_gobs()
int	pr_parse(), pr_geti(), pr_gsym(), pr_gsymi(), pr_gvari(), ph_objcheck()
int	ph_invert(), pr_findmap1(), ph_iereqn(), ph_ierval(), ph_seteqn()
int	ph_mkplist(), fntlenb(), ph_ofields(), ph_iheader(), ph_setvar()
pointer	pr_xgetname(), pr_gsymp()
real	pr_eval()
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

	# Get the observations list, the catalog list, the configuration
	# file, the parameters file, and the output file names.

	call clgstr ("observations", Memc[observations], SZ_LINE)
	call clgstr ("config", Memc[config], SZ_FNAME)
	call clgstr ("parameters", Memc[paramfile], SZ_FNAME)
	call clgstr ("calib", Memc[calibfile], SZ_FNAME)
	call clgstr ("catalogs", Memc[catalogs], SZ_LINE)
	call clgstr ("catdir", Memc[catdir], SZ_LINE)

	# Get the output type flags.

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

	# Parse the configuration file.

	if (pr_parse (Memc[config]) == ERR) {
	    call eprintf ("Error: Cannot parse the configuration file\n")
	    call close (ofd)
	    call sfree (sp)
	    return
	}

	# Fetch the total number of observed and catalog variables and
	# the total number of equations in the configuration file.

	nobsvars = pr_geti (NOBSVARS)
	nstdvars = pr_geti (NCATVARS)
	nvars = nobsvars + nstdvars
	ntrneqs = pr_geti (NTRNEQS)

	# Map observations file and catalog file columns.

	call pr_catmap (cmap, dummy)
	call pr_obsmap (omap, dummy)

	# Check which catalog and observations variables are actually
	# used in the equations to be inverted and determine whether the
	# system of equations can actually be inverted. This step is the
	# first pass through the system of equations. A second pass is
	# necessary to deal with the set equations. Use this first pass
	# to fetch the fitted parameters for the equations.

	call salloc (eqvartable, nstdvars * ntrneqs, TY_INT)
	call aclri (Memi[eqvartable], nstdvars * ntrneqs)
	call salloc (uservars, nstdvars, TY_INT)
	call aclri (Memi[uservars], nstdvars)
	call salloc (usererrs, nobsvars, TY_INT)
	call aclri (Memi[usererrs], nobsvars)
	call salloc (params, ntrneqs, TY_POINTER)
	call salloc (errors, ntrneqs, TY_POINTER)

	do i = 1, ntrneqs {

	    # Get the equation symbol.
	    sym = pr_gsym (i, PTY_TRNEQ)

	    # Get the reference equation symbol.
	    refcode = pr_gsymp (sym, PTEQRPNREF)

	    # Quit if there are catalog variables in the function expression
	    # or if there are set equations containing references to the
	    # catalog variables in the reference equation.

	    if ((pr_gsymi (sym, PTEQNRCAT) > 0) || (ph_seteqn (refcode) ==
	        YES)) {
		call eprintf ("Error: Cannot invert equations with catalog ")
		call eprintf ("variables in the function expression\n")
		call pr_unmap (cmap)
		call pr_unmap (omap)
	        call close (ofd)
	        call sfree (sp)
	        return
	    }

	    # Determine which catalog and observational variables were
	    # actually used in the reference equations and which have
	    # defined error columns.

	    do j = 1, pr_gsymi (sym, PTEQNRCAT) + pr_gsymi(sym, PTEQNROBS) {
		symvar = pr_gvari (sym, j, PTEQREFVAR)
		vcol = pr_gsymi (symvar, PINPCOL)
		ecol = pr_gsymi (symvar, PINPERRCOL)
		if (pr_gsymi (symvar, PSYMTYPE) == PTY_CATVAR) {
		    vcol = pr_findmap1 (cmap, vcol)
		    Memi[eqvartable+(i-1)*nstdvars+vcol-1] = vcol + nobsvars
		    Memi[uservars+vcol-1] = vcol + nobsvars
		} else {
		    vcol = pr_findmap1 (omap, vcol)
		    if (! IS_INDEFI(ecol))
		        ecol = pr_findmap1 (omap, ecol)
		    if (! IS_INDEFI(ecol))
		        Memi[usererrs+vcol-1] = ecol
		}
	    }

	    # Determine which catalog and observational variables were
	    # actually used in the fit equations and which have
	    # defined error columns.

	    do j = 1, pr_gsymi (sym, PTEQNFCAT) + pr_gsymi(sym, PTEQNFOBS) {
		symvar = pr_gvari (sym, j, PTEQFITVAR)
		vcol = pr_gsymi (symvar, PINPCOL)
		ecol = pr_gsymi (symvar, PINPERRCOL)
		if (pr_gsymi (symvar, PSYMTYPE) == PTY_CATVAR) {
		    vcol = pr_findmap1 (cmap, vcol)
		    Memi[eqvartable+(i-1)*nstdvars+vcol-1] = vcol + nobsvars
		    Memi[uservars+vcol-1] = vcol + nobsvars
		} else {
		    vcol = pr_findmap1 (omap, vcol)
		    if (! IS_INDEFI(ecol))
		        ecol = pr_findmap1 (omap, ecol)
		    if (! IS_INDEFI(ecol))
		        Memi[usererrs+vcol-1] = ecol
		}
	    }

	    # Get the number of parameters for the equation.
	    # Allocate space for parameter values and errors, for the
	    # current equation, and read them from the coefficient file.

	    nparams = pr_gsymi (sym, PTEQNPAR)
	    call salloc (MEMP[params+i-1], nparams, TY_REAL)
	    call salloc (MEMP[errors+i-1], nparams, TY_REAL)
	    iferr {
	        if (io_gcoeffs (Memc[paramfile], sym, stat, chisqr, rms,
	            Memr[MEMP[params+i-1]], Memr[MEMP[errors+i-1]], nparams) !=
		    nparams) {
		    call eprintf ("Warning: Error reading parameters for ")
		    call eprintf ("equation %s from %s\n")
			call pargstr (Memc[pr_xgetname(sym)])
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

	    # Issue a warning if any of the equations in the system to be
	    # inverted did not converge but proceed with the fit.

	    if (stat != DONE) {
		call eprintf (
		    "Warning: The solution for equation %s did not converge")
		    call pargstr (Memc[pr_xgetname(sym)])
	    }

	}

	# Count the number of catalog variables used in the transformation
	# equations.

	nustdvars = 0 
	do i = 1, nstdvars {
	    if (Memi[uservars+i-1] != 0)
		nustdvars = nustdvars + 1
	}

	# If the number of equations is less than the number of referenced
	# catalog variables it is not possible to invert the transformation
	# equations.

	if (nustdvars > ntrneqs) {
	    call eprintf ("Error: The number of equations is less than ")
	    call eprintf ("the number of unknowns\n")
	    call close (ofd)
	    call pr_unmap (cmap)
	    call pr_unmap (omap)
	    call pr_free()
	    call sfree (sp)
	    return
	}

	# Loop over the transformation equations a second time searching for
	# references to the set equations in the fit expressions. If a set
	# equation reference is found, check to see whether it contains any
	# reference to a catalog variable which is not referenced elsewhere
	# in the transformation equations.  Recompute the number of catalog
	# variables used in the fit equations.

	maxnset = max (1, pr_geti (NSETEQS))
	call salloc (eqset, maxnset * ntrneqs, TY_INT)
	call aclri (Memi[eqset], maxnset * ntrneqs)
	nset = 0
	if (pr_geti (NSETEQS) > 0) {

	    # Find the number of independent set equations.
	    call salloc (userset, pr_geti (NSETEQS), TY_INT)
	    call amovki (NULL, Memi[userset], pr_geti (NSETEQS))
	    do i = 1, ntrneqs {
	        sym = pr_gsym (i, PTY_TRNEQ)
	        nset = nset + ph_setvar (i, sym, cmap, omap, Memi[eqvartable],
		    Memi[uservars], Memi[usererrs], nstdvars, nobsvars,
		    Memi[eqset], pr_geti (NSETEQS), Memi[userset], nset)
	    }

	    # Is the system invertable?
	    if ((nustdvars + nset) > ntrneqs) {
	        call eprintf ("Error: The number of equations is less than ")
	        call eprintf ("the number of unknowns\n")
	        call close (ofd)
	        call pr_unmap (cmap)
	        call pr_unmap (omap)
	        call pr_free()
	        call sfree (sp)
	        return
	    }

	    # Recompute the number of independent catalog variables.
	    nustdvars = 0 
	    do i = 1, nstdvars {
	        if (Memi[uservars+i-1] != 0)
		    nustdvars = nustdvars + 1
	    }

	}


	# Decide whether to use id/catalog matching or not. If id matching
	# is enabled fetch the catalog data.

	if (pr_geti (MINCOL) <= 1) {
	    getid = NO
	    matchid = NO
	    stdtable = NULL
	} else if (Memc[catalogs] == EOS) {
	    getid = YES
	    matchid = NO
	    stdtable = NULL
	} else {
	    getid = YES
	    matchid = YES
	    stdtable = NULL
	}

	# Get the list of optional variables to be printed. These variables
	# may include any of the catalog or observations variables or the
	# set equations. Variables which do not match any of these categories
	# will be ommitted from the list to be printed.

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
	ncols = ph_iheader (ofd, Memc[observations], Memc[catalogs],
	    Memc[config], Memc[paramfile], type, getid, Memi[psym],
	    Memi[pcols], len_plist, Memi[uservars], nstdvars,
	    Memi[userset], nset, etype, matchid)

	# Set the format string.
	if (Memc[formatstr] == EOS) {
	    call ph_oformatstr (getid, ncols, Memc[formatstr], SZ_LINE)
	} else if (ph_ofields (Memc[formatstr]) != ncols) {
	    call eprintf ("Warning: The number of format string fields ")
	    call eprintf ("does not match the number of output columns\n")
	    call ph_oformatstr (getid, ncols, Memc[formatstr], SZ_LINE)
	}

	# Read in the catalog data.
	if (matchid == YES) {
	    stdlist = fntopnb (Memc[catalogs], NO)
	    call io_gcatdat (Memc[catdir], stdlist, stdtable, nstd, dummy)
	    call fntclsb (stdlist)
	}

	# Compute the initial values for the catalog variables and initial
	# values for the delta-variable estimates. If a catalog is present
	# the initial value of each catalog variable is the average value of
	# that variable in the catalog. The standard deviation of the catalog
	# variables is used as the initial value for the catalog variable
	# increments.

	call salloc (avtvars, nstdvars, TY_REAL)
	call salloc (dtvars, nstdvars, TY_REAL)
	call ph_avstdvars (stdtable, Memi[uservars], Memr[avtvars],
	    Memr[dtvars], nstdvars)

	# Allocated space for the catalog variables and the fitted variables
	# and their errors.
	call salloc (vars, nvars, TY_REAL)
	call salloc (tvars, nvars, TY_REAL)
	if (nset > 0)
	    call salloc (svars, nset, TY_REAL)
	call salloc (ervars, nstdvars, TY_REAL)
	if (nset > 0)
	    call salloc (servars, nset, TY_REAL)
	call salloc (varindex, nstdvars, TY_INT)
	call salloc (eqindex, ntrneqs, TY_INT)

	# Initialize the fit inversion code.
	call ph_ivinit (nstdvars, nustdvars, ntrneqs)

	# Loop over the observations files.
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

		# Print star name.
		call fprintf (ofd, Memc[formatstr])
		if (getid == YES)
		    call pargstr (Memc[starname])
		#call eprintf ("name=%s\n")
		    #call pargstr (Memc[starname])

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

		# Initialize the fit.
		call amovr (Memr[vars], Memr[tvars], nobsvars)
		call amovr (Memr[avtvars],  Memr[tvars+nobsvars], nstdvars)

		# Invert the transformations and compute the errors.
		if (ph_objcheck (MEMP[params], Memr[tvars], Memi[eqvartable],
		    nstdvars, ntrneqs, Memi[eqset], maxnset,
		    Memi[varindex], nustdvars, Memi[eqindex],
		    nueq) == ERR) {
		    call amovkr (INDEFR, Memr[tvars+nobsvars], nstdvars)
		    if (nset > 0)
			call amovkr (INDEFR, Memr[svars], nset)
		    call amovkr (INDEFR, Memr[ervars], nstdvars)
		    if (nset > 0)
			call amovkr (INDEFR, Memr[servars], nset)

		} else if (ph_invert (MEMP[params], Memr[tvars], nobsvars,
		    Memr[dtvars], Memi[varindex], nstdvars,
		    nustdvars, Memi[eqindex], nueq) == ERR) {

		    call amovkr (INDEFR, Memr[tvars+nobsvars], nstdvars)
		    if (nset > 0)
			call amovkr (INDEFR, Memr[svars], nset)
		    call amovkr (INDEFR, Memr[ervars], nstdvars)
		    if (nset > 0)
			call amovkr (INDEFR, Memr[servars], nset)

		} else {

		    # Set any unfitted variables to INDEF.
		    do i = nobsvars + 1, nvars {
			if (Memi[varindex+i-nobsvars-1] == 0)
			    Memr[tvars+i-1] = INDEFR
		    }

		    # Evaluate the set equations.
		    if (nset > 0) {
		        do i = 1, nset
		            Memr[svars+i-1] = pr_eval (Memi[userset+i-1],
			        Memr[tvars], Memr[MEMP[params+i-1]])
		    }

		    # Evaluate the errors.
		    switch (etype) {
		    case ERR_UNDEFINED:
			call amovkr (INDEFR, Memr[ervars], nstdvars)
			if (nset > 0)
			    call amovkr (INDEFR, Memr[servars], nset)
		    case ERR_EQUATIONS:
		        if (ph_iereqn (MEMP[params], Memr[tvars], nobsvars,
		            Memr[dtvars], Memi[varindex], Memr[ervars],
			    nstdvars, Memi[userset], Memr[svars],
			    Memr[servars], nset, nustdvars, Memi[eqindex],
			    nueq) <= 0) {
			    call amovkr (INDEFR, Memr[ervars], nstdvars)
			    if (nset > 0)
			        call amovkr (INDEFR, Memr[servars], nset)
			}
		    case ERR_OBSERRORS:
		        if (ph_ierval (MEMP[params], Memr[tvars],
			    Memi[usererrs], nobsvars, Memr[dtvars],
			    Memi[varindex], Memr[ervars], nstdvars,
			    Memi[userset], Memr[svars], Memr[servars], nset,
			    nustdvars, Memi[eqindex], nueq) <= 0) {
			    call amovkr (INDEFR, Memr[ervars], nstdvars)
			    if (nset > 0)
			        call amovkr (INDEFR, Memr[servars], nset)
			}
		    default:
			call amovkr (INDEFR, Memr[ervars], nstdvars)
		    }
		}

		# Write out the standard indices, errors and residuals.
		do i = nobsvars + 1, nvars {

		    # Skip catalog variables not used in the equation.
		    if (Memi[uservars+i-nobsvars-1] <= 0)
			next

		    call pargr (Memr[tvars+i-1])
		    if (etype != ERR_UNDEFINED) {
			if (IS_INDEFR(Memr[tvars+i-1]))
			    call pargr (INDEFR)
			else
			    call pargr (Memr[ervars+i-nobsvars-1])
		    }

		    # Compute the residual of the fit.
		    if (getid == NO || (matchid == YES &&
		        type != TYPE_PROGRAM)) {
		        if (IS_INDEFR (Memr[vars+i-1]) ||
		            IS_INDEFR (Memr[tvars+i-1]))
			    resid = INDEFR
		        else
		            resid = Memr[vars+i-1] - Memr[tvars+i-1]
		        call pargr (resid)
		    }
		}

		# Write out the set equations.
		do i = 1, nset {

		    # Write out the set equation.
		    call pargr (Memr[svars+i-1])

		    # Evaluate the error. This is tricky at the moment.
		    if (etype != ERR_UNDEFINED) {
			if (IS_INDEFR(Memr[svars+i-1]))
			    call pargr (INDEFR)
			else
			    call pargr (Memr[servars+i-1])
		    }

		    # Compute the residual of the fit.
		    if (getid == NO || (matchid == YES &&
		        type != TYPE_PROGRAM)) {
		        resid = pr_eval (Memi[userset+i-1], Memr[vars],
		            Memr[MEMP[params+i-1]])
		        if (IS_INDEFR (Memr[svars+i-1]) || IS_INDEFR (resid))
			    resid = INDEFR
		        else
		            resid = resid - Memr[svars+i-1]
		        call pargr (resid)
		    }
		}
	    }

	    # Close the input file.
	    call close (ifd)
	}

	# Free memory.
	call sfree (sp)
	call ph_ivfree()
	call pr_free()
	call pr_unmap (cmap)
	call pr_unmap (omap)

	# Close all the files.
	if (stdtable != NULL)
	    call stclose (stdtable)
	call close (ofd)
	call fntclsb (obslist)
end


# PH_IHEADER - Print the output file header.

int procedure ph_iheader (fd, observations, catalogs, config, paramfile, type,
	getid, psym, pcols, len_plist, catvars, ncatvars, userset, nset,
	etype, matchid)

int	fd			# output file descriptor
char	observations[ARB]	# the list of observations files
char	catalogs[ARB]		# the list of catalog files
char	config[ARB]		# the configuration file name
char	paramfile[ARB]		# the fitted parameters file
int	type			# the type of object to output
int	getid			# output the object id ?
int	psym[ARB]		# list of additional symbols to be printed
int	pcols[ARB]		# column numbers of additional output variables
int	len_plist		# length of symbol list
int	catvars[ARB]		# the list of active catalog variables
int	ncatvars		# number of catalog variables
int	userset[ARB]		# list of set equations included in fit
int	nset			# number of set equations
int	etype			# type of error output
int	matchid			# is it possible to match ids ?

int	i, olist, slist, ncols
pointer sp, time, name, sym
int	fntopnb(), fntgfnb(), pr_gsym()
long	clktime()
pointer	pr_xgetname(), pr_gsymp()

begin
	# Allocate some working space.
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
	    call fprintf (fd, "# Number of catalog files:\n") 
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

	# Write the output options.
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

	# Print the optional id header
	call fprintf (fd, "#\n")
	call fprintf (fd, "# Columns: \n")
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

	# Print the catalog variables headers.
	do i = 1, ncatvars {
	    if (catvars[i] <= 0)
		next
	    sym = pr_gsym (i, PTY_CATVAR)
	    ncols = ncols + 1
	    call fprintf (fd, "#\t%d\t%s\n")
		call pargi (ncols)
	        call pargstr (Memc[pr_xgetname(sym)])
	    if (etype != ERR_UNDEFINED) {
		ncols = ncols + 1
		call fprintf (fd, "#\t%d\terror(%s)\n")
		    call pargi (ncols)
	            call pargstr (Memc[pr_xgetname(sym)])
	    }
	    if (getid == NO || (matchid == YES && type != TYPE_PROGRAM)) {
		ncols = ncols + 1
		call fprintf (fd, "#\t%d\tresid(%s)\n")
		    call pargi (ncols)
	            call pargstr (Memc[pr_xgetname(sym)])
	    }
	}

	# Print the set equation variables headers.
	do i = 1, nset {
	    ncols = ncols + 1
	    call fprintf (fd, "#\t%d\t%s\n")
		call pargi (ncols)
		call pargstr (Memc[pr_xgetname(userset[i])])
	    if (etype != ERR_UNDEFINED) {
		ncols = ncols + 1
		call fprintf (fd, "#\t%d\terror(%s)\n")
		    call pargi (ncols)
	            call pargstr (Memc[pr_xgetname(userset[i])])
	    }
	    if (getid == NO || (matchid == YES && type != TYPE_PROGRAM)) {
		ncols = ncols + 1
		call fprintf (fd, "#\t%d\tresid(%s)\n")
		    call pargi (ncols)
	            call pargstr (Memc[pr_xgetname(userset[i])])
	    }
	    userset[i] = pr_gsymp (userset[i], PSEQRPNEQ)
	}

	call fprintf (fd, "\n\n")

	call sfree (sp)

	return (ncols)
end


# PH_AVSTDVARS -- Compute the mean and sigma of the variables in the
# standard table to use as initial guesses for the math routines.
# If the standard catalog is undefined or empty the mean values of the
# variables are set to 0.0 and the sigmas are set to 1.0. If there is only
# one catalog value the sigma is also set to 1.0.

procedure ph_avstdvars (stable, index, avg, sigma, npts)

pointer	stable			# pointer to the symbol table
int	index[ARB]		# index of active catalog variables
real	avg[ARB]		# the output array of averages
real	sigma[ARB]		# the output array of standard deviations
int	npts			# number of points

int	i, ndata
pointer	sym, sp, n
real	data
pointer	sthead(), stnext()

begin
	# Initialize and return if the standard table is undefined.
	call amovkr (0.0, avg, npts)
	if (stable == NULL) {
	    call amovkr (0.1, sigma, npts)
	    return
	}

	# Allocate some temporary space and intialize the accumulators.
	call smark (sp)
	call salloc (n, npts, TY_INT)
	call aclri (Memi[n], npts)
	call aclrr (sigma, npts)

	# Read the symbol table and accumlate the sums.
	sym = sthead (stable)
	while (sym != NULL) {
	    do i = 1, npts {
		if (index[i] == 0)
		    next
		data = Memr[P2R(sym+i-1)]
		if (IS_INDEFR(data))
		    next
		avg[i] = avg[i] + data
		sigma[i] = sigma[i] + data ** 2
	        Memi[n+i-1] = Memi[n+i-1] + 1
	    }
	    sym = stnext (stable, sym)
	}

	# Compute the averages.
	do i = 1, npts {
	    ndata = Memi[n+i-1]
	    if (ndata == 0)
		sigma[i] = 0.1
	    else if (ndata == 1)
		sigma[i] = 0.1
	    else {
		avg[i] = avg[i] / ndata
		sigma[i] = (sigma[i] - avg[i] * avg[i] * ndata) / (ndata - 1)
		if (sigma[i] <= 0.0)
		    sigma[i] = 0.1
		else
		    sigma[i] = min (0.1, sqrt (sigma[i]))
	    }
	}

	call sfree (sp)
end
