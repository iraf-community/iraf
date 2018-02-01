include	<error.h>
include	<pkg/xtanswer.h>
include	<pkg/gtools.h>
include	<math/nlfit.h>
include	<pkg/inlfit.h>
include	"../lib/parser.h"
include	"../lib/fitparams.h"

# Interactive commands

define	CMD_OPTIONS	"|first|last|prev|next|again|go|quit|"
define	CMD_FIRST	1
define	CMD_LAST	2
define	CMD_PREV	3
define	CMD_NEXT	4
define	CMD_AGAIN	5
define	CMD_GO		6
define	CMD_QUIT	7
define	CMD_MIN		CMD_FIRST
define	CMD_MAX		CMD_QUIT

# Prompt to save fit results into output file and equation structure

define	SAVE_PROMPT	"Do you want to save fit results ?"
define	QUIT_PROMPT	"Program will quit. Do you really want to do so ?"

# Program labels

define	retry		9999


# FT_TRNEQS - Fit all the transformation equations.

procedure ft_trneqs (output, logfile, graphics, otable, ntable, wtflag,
	addscatter, tol, itmax, interactive, high, low, niterate, grow,
	log_fit, log_results)

char	output[ARB]		# output file name
char	logfile[ARB]		# the log file name
char	graphics[ARB]		# graphics output device
pointer	otable			# standard observation table
pointer	ntable			# standard name table
int	wtflag			# the type of weighting
int	addscatter		# add a scatter term to the weights
real	tol			# fit tolerance
int	itmax			# max number of iterations
bool	interactive		# interactive fit ?
real	high, low		# rejection factors
int	niterate		# number of rejection iterations
real	grow			# rejection growing radius
bool	log_fit			# log the fit statistics
bool	log_results		# log the fit results

int	cmd, neq, maxeq, nobs, sym
pointer	rtable, wtable, totable, gp, gt

#bool	clgetb()
int	mct_nrows(), pr_gsym(), pr_geti(), ft_trnanswer()
pointer	gopen(), gt_init(), pr_gsymp()

begin
	# Debug ?
	#if (clgetb ("debug.fitcode")) {
	    #call eprintf ("ft_trneqs (output=%s) (graphics=%s) (ot=%d) "
		#call pargstr (output)
		#call pargstr (graphics)
		#call pargi (otable)
	    #call eprintf ("(tol=%g) (maxiter=%d) (int=%b)\n")
		#call pargr (tol)
		#call pargi (itmax)
		#call pargb (interactive)
	    #call eprintf ("ft_trneqs (gp=%d) (gt=%d) (high=%g) (lo=%g) ")
		#call pargi (gp)
		#call pargi (gt)
		#call pargr (high)
		#call pargr (low)
	    #call eprintf ("(niter=%d) (grow=%g)\n")
		#call pargi (niterate)
		#call pargr (grow)
	#}

	# Get number of observations. 
	nobs = mct_nrows (otable)

	# Allocate space for reference and weight tables. These tables
	# store the values of the reference equation for all observations,
	# and the weight for each of them, respectively.

	totable = NULL
	call mct_alloc (rtable, nobs, 1, TY_REAL)
	call mct_alloc (wtable, nobs, 1, TY_REAL)

	# Open graphics.

	if (interactive) {
	    gp = gopen (graphics, NEW_FILE, STDGRAPH)
	    gt = gt_init()
	} else {
	    gp = NULL
	    gt = NULL
	}

	# Loop over transformation equations, until the user
	# sends a quit command.

	neq = 1
	cmd = CMD_NEXT
	maxeq = pr_geti (NTRNEQS)
	repeat {

	    # Get the equation symbol.

	    sym = pr_gsym (neq, PTY_TRNEQ)

	    # Evaluate the reference equation for catalog
	    # observations and store them into a table.

	    call ft_rfeval (pr_gsymp (sym, PTEQRPNREF), otable, rtable)

	    # Evaluate weights for the current equation. If an error
	    # condition is raised (due to a negative weight value) the
	    # program continues with the next equation in the list.

	    switch (wtflag) {
	    case FWT_UNIFORM:
		call ft_wtuniform (otable, wtable)

	    case FWT_PHOTOMETRIC:
		iferr (call ft_wtphoterrs (sym, otable, wtable)) {
		    call erract (EA_WARN)
		    next
		}
	    case FWT_EQUATIONS:
	        iferr (call ft_wteqns (sym, otable, wtable)) {
		    call erract (EA_WARN)
		    next
	        }
	    default:
		call ft_wtuniform (otable, wtable)
	    }

	    # Save original observations, and check for undefined values.

	    call mct_copy (otable, totable)
	    call ft_indef (sym, totable, rtable, wtable)

	    # Fit transformation equation. 

	    call ft_trneq (output, logfile, sym, totable, wtable, rtable,
	        ntable, wtflag, addscatter, tol, itmax, interactive, gp, gt,
		high, low, niterate, grow, log_fit, log_results)

	    # Reset the reference table.

	    call mct_reset (rtable)

retry
	    # Prompt the user for command.
	    if (interactive)
		call ft_trncmd (cmd)
	    else
		cmd = CMD_NEXT

	    # Branch on command.
	    switch (cmd) {
	    case CMD_FIRST:
		neq = 1
	    case CMD_LAST:
		neq = maxeq
	    case CMD_PREV:
		if (neq > 1)
		    neq = neq - 1
		else
		    goto retry
	    case CMD_NEXT:
		neq = neq + 1
	    case CMD_AGAIN:
		;
	    case CMD_GO:
		interactive = false
		neq = neq + 1
	    case CMD_QUIT:
		if (ft_trnanswer (QUIT_PROMPT, YES) == YES)
		    break
	    default:
		call error (0, "fttrneqs: Unknown command")
	    }

	    # Prompt to quit.
	    if (neq > maxeq) {
		if (interactive && cmd != CMD_GO) {
		    if (ft_trnanswer (QUIT_PROMPT, YES) == NO) {
			neq = maxeq
			goto retry
		    } else 
			break
		} else
		    break
	    }
	}

	# Close graphics.
	if (gp != NULL)
	    call gclose (gp)
	if (gt != NULL)
	    call gt_free (gt)

	# Free tables.
	call mct_free (rtable)
	call mct_free (wtable)
	call mct_free (totable)
end


# FT_TRNEQ - Fit single transformation equation.

procedure ft_trneq (output, logfile, sym, otable, wtable, rtable, ntable,
	wtflag, addscatter, tol, itmax, interactive, gp, gt high, low,
	niterate, grow, log_fit, log_results)

char	output[ARB]		# output file name
char	logfile[ARB]		# log file name
int	sym			# transformation equation symbol
pointer	otable			# standard observation table
pointer	wtable			# weight table
pointer	rtable			# reference equation table
pointer	ntable			# names table
int	wtflag			# type of weighting
int	addscatter		# add a scatter term to the weight equation
real	tol			# fit tolerance
int	itmax			# max number of iterations
bool	interactive		# interactive fit ?
pointer	gp, gt			# GIO and GTOOLS descriptors
real	high, low		# rejection factors
int	niterate		# number of rejection iterations
real	grow			# rejection growing radius
bool	log_fit			# log the fit statistics
bool	log_results		# log the fit results

int	nobs, nvars, nparams, nfparams, len_name
int	i, stat, answer, psym, nlwtflag
pointer	sp, params, dparams, name, aux, plist, nl, in
real	chisqr, variance, scatter, rms

#bool	clgetb()
bool	streq()
int	locpr(), mct_nrows(), mct_maxcol(), pr_geti(), pr_gsym(), pr_gsymi()
int	pr_gpari(), ft_trnanswer()
pointer	pr_xgetname(), pr_gsymp(), pr_gsymc(), mct_getbuf(), in_getp()
real	pr_gsymr()
extern	ft_func(), ft_dfunc(), ft_plot()

begin
	# Debug ?
	#if (clgetb ("debug.fitcode")) {
	    #call eprintf ("ft_trneq (output=%s) (sym=%d) (ot=%d) (wt=%d) "
		#call pargstr (output)
		#call pargi (sym)
		#call pargi (otable)
		#call pargi (wtable)
	    #call eprintf ("(rt=%d) (tol=%g) (maxiter=%d)\n")
		#call pargi (rtable)
		#call pargr (tol)
		#call pargi (itmax)
	    #call eprintf ("ft_trneq (int=%b) (gp=%d) (gt=%d) (high=%g) "
		#call pargb (interactive)
		#call pargi (gp)
		#call pargi (gt)
		#call pargr (high)
	    #call eprintf ("(low=%g) (niter=%d) (grow=%g)\n")
		#call pargr (low)
		#call pargi (niterate)
		#call pargr (grow)
	#}

	# Get number of observations, and variables for the observational data.

	nobs = mct_nrows (otable)
	nvars = mct_maxcol (otable)
	len_name = mct_maxcol (ntable)

	# Get number of parameters for the current equation.

	nparams = pr_gsymi (sym, PTEQNPAR)

	# Allocate stack space.
	call smark (sp)
	call salloc (aux, SZ_LINE, TY_CHAR)
	call salloc (name, SZ_LINE, TY_CHAR)
	call salloc (params, nparams, TY_REAL)
	call salloc (dparams, nparams, TY_REAL)
	call salloc (plist, nparams, TY_INT)

	# Initialize parameters, errors, and parameter list.

	call aclrr (Memr[params], nparams)
	call aclrr (Memr[dparams], nparams)
	do i = 1, nparams {
	    psym = pr_gpari (sym, i, PTEQPAR)
	    Memr[dparams+i-1] = pr_gsymr (psym, PFITDELTA)
	}
	call amovi (Memi[P2I(pr_gsymp (sym, PTEQSPLIST))], Memi[plist], nparams)

	# Initialize the fit evaluation. This is necessary in order to
	# set the equations called by the INLFIT procedures.

	call ft_evinit (sym, nparams)

	# Get number of fitting parameters for the current equation.
	nfparams = pr_gsymi (sym, PTEQNFPAR)

	# Initialize INLFIT.
	call in_initr (in, locpr (ft_func), locpr (ft_dfunc),
	    Memr[P2R(pr_gsymp (sym, PTEQSPARVAL))], Memr[dparams],
	    nparams, Memi[plist], nfparams)

	# Set INLFIT fitting parameters.
	call in_putr (in, INLTOLERANCE, tol)
	call in_puti (in, INLMAXITER, itmax)
	call in_putr (in, INLHIGH, high)
	call in_putr (in, INLLOW, low)
	call in_puti (in, INLNREJECT, niterate)
	call in_putr (in, INLGROW, grow)

	# Put in the reference and fit equation names.
	call sprintf (Memc[aux], SZ_LINE, "|%s|%s|")
	    call pargstr (Memc[pr_gsymc (sym, PTEQREF)])
	    call pargstr (Memc[pr_gsymc (sym, PTEQFIT)])
	call in_pstr (in, INLFLABELS, Memc[aux])

	# Put in the parameter names.
	call strcpy ("|", Memc[aux], SZ_LINE)
	do i = 1, nparams {
	    call strcat (Memc[pr_xgetname (pr_gpari (sym, i, PTEQPAR))],
			 Memc[aux], SZ_LINE)
	    call strcat ("|", Memc[aux], SZ_LINE)
	}
	call in_pstr (in, INLPLABELS, Memc[aux])

	# Put in the variable names.
	call strcpy ("|", Memc[aux], SZ_LINE)
	do i = 1, pr_geti (NOBSVARS) {
	    call pr_vtran (Memc[pr_xgetname (pr_gsym (i, PTY_OBSVAR))],
		Memc[name], SZ_LINE)
	    call strcat (Memc[name], Memc[aux], SZ_LINE)
	    call strcat ("|", Memc[aux], SZ_LINE)
	}
	do i = 1, pr_geti (NCATVARS) {
	    call pr_vtran (Memc[pr_xgetname (pr_gsym (i, PTY_CATVAR))],
		Memc[name], SZ_LINE)
	    call strcat (Memc[name], Memc[aux], SZ_LINE)
	    call strcat ("|", Memc[aux], SZ_LINE)
	}
	call in_pstr (in, INLVLABELS, Memc[aux])

	# Put plot equations and redefine graph keys, but only if
	# both plot equations are defined. Otherwise leave the defaults.

	if (pr_gsymp (sym, PTEQRPNXPLOT) != NULL &&
	    pr_gsymp (sym, PTEQRPNYPLOT) != NULL) {

	    # Put in the plot equation names.
	    call sprintf (Memc[aux], SZ_LINE, "|%s|%s|")
		call pargstr (Memc[pr_gsymc (sym, PTEQXPLOT)])
		call pargstr (Memc[pr_gsymc (sym, PTEQYPLOT)])
	    call in_pstr (in, INLUSERLABELS, Memc[aux])

	    # Put in the plot equation.
	    call in_puti (in, INLUAXES, locpr (ft_plot))

	    # Redefine the last graph key.
	    call in_puti (in, INLGKEY, INLNGKEYS)
	    call in_pkey (in, INLNGKEYS, 1, KEY_UAXIS, 1)
	    call in_pkey (in, INLNGKEYS, 2, KEY_UAXIS, 2)
	}

	# Call the appropiate version of fitting routine according to the
	# value of the interactive flag.

	if (wtflag == FWT_UNIFORM)
	    nlwtflag = WTS_USER
	else if (addscatter == YES)
	    nlwtflag = WTS_SCATTER
	else
	    nlwtflag = WTS_USER

	if (interactive) {
	    call gt_setr (gt, GTXMIN, INDEFR)
	    call gt_setr (gt, GTXMAX, INDEFR)
	    call gt_setr (gt, GTYMIN, INDEFR)
	    call gt_setr (gt, GTYMAX, INDEFR)
	    call ing_fitr (in, gp, "cursor", gt, nl, Memr[mct_getbuf (otable)],
	        Memr[mct_getbuf (rtable)], Memr[mct_getbuf (wtable)],
		Memc[mct_getbuf(ntable)], nobs, nvars, len_name, nlwtflag,
		stat)
	} else {
	    call in_fitr (in, nl, Memr[mct_getbuf (otable)],
	        Memr[mct_getbuf (rtable)], Memr[mct_getbuf (wtable)],
		nobs, nvars, nlwtflag, stat)
	}

	# Decide whether to prompt the user for saving answer.
	if (interactive)
	    answer = ft_trnanswer (SAVE_PROMPT, YES)
	else
	    answer = YES

	# Write the parameter values into output file.
	if (answer == YES || answer == ALWAYSYES) {

	    # Get parameter values, and parameter list.
	    call nlpgetr (nl, Memr[params], nparams)
	    call amovi (Memi[in_getp (in, INLPLIST)], Memi[plist], nparams)

	    # Get the parameter errors.
	    call in_errorsr (in, nl, Memr[mct_getbuf (otable)],
	        Memr[mct_getbuf (rtable)], Memr[mct_getbuf (wtable)], nobs,
		nvars, variance, chisqr, scatter, rms, Memr[dparams])

	    # Write parameters and errors into output file.
	    iferr (call io_pcoeffs (output, sym, stat, wtflag, variance, chisqr,
	        scatter, rms, Memr[params], Memr[dparams], Memi[plist],
		nparams))
	        call erract (EA_WARN)

	    # Log the fit and results.
	    if (log_fit || log_results) {
		if (interactive && streq (logfile, "STDOUT"))
		    call gdeactivate (gp, 0)
		if (log_fit) {
		    iferr (call io_title (logfile, "#EQUATION:", sym))
		        ;
		    iferr (call ing_showr (in, logfile))
			;
		    iferr (call ing_errorsr (in, logfile, nl,
		        Memr[mct_getbuf(otable)], Memr[mct_getbuf(rtable)],
		        Memr[mct_getbuf(wtable)], nobs, nvars))
			;
		}
		if (log_results) {
		    iferr (call io_title (logfile, "#RESULTS:", sym))
		        ;
		    iferr (call ing_resultsr (in, logfile, nl,
		        Memr[mct_getbuf(otable)], Memr[mct_getbuf(rtable)],
		        Memr[mct_getbuf(wtable)], Memc[mct_getbuf(ntable)],
		        nobs, nvars, len_name))
			    ;
		}
		if (interactive && streq (logfile, "STDOUT"))
		    call greactivate (gp, 0)
	    }


	    # Update fitted parameter values into the equation symbol
	    # substructure if the fit was succesfull.
	    if (stat == DONE)
		call amovr (Memr[params], 
		    Memr[P2R(pr_gsymp (sym, PTEQSPARVAL))], nparams)
	}

	# Debug ?
	#if (clgetb ("debug.nlfit"))
	    #call dg_inldump (in, nl)


	# Free inlfit and nlfit descriptors.
	call in_freer (in)
	call nlfreer (nl)

	# Free fit evaluation.
	call ft_evfree ()

	# Free stack space.
	call sfree (sp)
end


# FT_TRNCMD -- Prompt the user for a command.

procedure ft_trncmd (cmd)

int	cmd			# command code

char	command[SZ_LINE]
int	scan(), strlen()
int	strdic(), io_strwrd()

begin
	# Get current command string.
	if (io_strwrd (cmd, command, SZ_LINE, CMD_OPTIONS) == 0)
	    call error (0, "ft_trneqs: Unknown command")

	# Keep prompting the user until there is a valid command.
	repeat {

	    # Print current setting.
	    call printf ("Command (%s) (%s) : ")
		call pargstr (CMD_OPTIONS)
		call pargstr (command)
	    call flush (STDOUT)

	    # Get new command.
	    if (scan () == EOF)
		command[1] = EOS
	    else
	        call gargstr (command, SZ_LINE)
	    if (strlen (command) == 0) {
		if (io_strwrd (cmd, command, SZ_LINE, CMD_OPTIONS) <= 0)
		    ;
	    }

	    # Search for command in the dictionary.
	    cmd = strdic (command, command, SZ_LINE, CMD_OPTIONS)

	    # Test command.
	    if (cmd >= CMD_MIN && cmd <= CMD_MAX)
		break
	    else
		call printf ("\007")
	}
end


# FT_TRNANSWER -- Get a YES/NO answer from the user, and return it
# as the procedure value.

int procedure ft_trnanswer (prompt, default)

char	prompt			# prompt to the user
int	default			# default answer

int	answer

begin
	answer = default
	call xt_answer (prompt, answer)
	if (answer == YES || answer == ALWAYSYES)
	    return (YES)
	else
	    return (NO)
end
