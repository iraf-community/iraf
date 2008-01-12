include	"../lib/parser.h"
include	"../lib/fitparams.h"


# T_FITPARAMS - Main fitting task. This task will determine the value of the
# fitting parameters, either interactively or non-interactively, by using
# the INLFIT package.

procedure t_fitparams()

pointer	observations		# list of observations files
pointer	catalogs		# list of standard catalogs
int	stdlist			# file list of standards
pointer	config			# pointer to configuration file name
pointer	output			# pointer to output file name
pointer	logfile			# pointer to the output log file
int	wtflag			# weighting type
int	addscatter		# compute an additional scatter term
real	tol			# fit tolerance
int	itmax			# max number of iterations
int	niterate		# number of rejection iterations
real	high, low		# rejection thresholds
real	grow			# rejection growing radius
bool	interactive		# interactive fit ?
pointer	catdir			# the standard catalogs directory
pointer graphics		# pointer to the graphics device name

int	obslist, nstd, nobs, nstdvars, getid
pointer	sp, dir, str, otable, ntable, stable

bool	clgetb()
int	fntopnb(), fntlenb(), clgeti(), clgwrd(), btoi(), pr_parse(), pr_geti()
int	fnldir(), access()
real	clgetr()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (observations, SZ_FNAME, TY_CHAR)
	call salloc (catalogs, SZ_FNAME, TY_CHAR)
	call salloc (config, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (logfile, SZ_FNAME, TY_CHAR)
	call salloc (catdir, SZ_FNAME, TY_CHAR)
	call salloc (graphics, SZ_FNAME, TY_CHAR)
	call salloc (dir, SZ_PATHNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Open the observations files list.
	call clgstr ("observations", Memc[observations], SZ_FNAME)
	obslist = fntopnb (Memc[observations], NO)
	if (Memc[observations] == EOS || fntlenb (obslist) <= 0) {
	    call eprintf ("ERROR: The observations files list is empty\n")
	    call fntclsb (obslist)
	    call sfree (sp)
	    return
	}

	# Get the list of catalog files.
	call clgstr ("catdir", Memc[catdir], SZ_FNAME)
	call clgstr ("catalogs", Memc[catalogs], SZ_FNAME)

	# Get the configuration file name. 
	call clgstr ("config", Memc[config], SZ_FNAME)
	if (access (Memc[config], READ_ONLY, TEXT_FILE) == NO) {
	    call eprintf (
	        "ERROR: Cannot open the configuration file for reading\n")
	    call fntclsb (obslist)
	    call sfree (sp)
	    return
	}

	# Get the output parameters database nam and check that the user
	# can create or append to the database.

	call clgstr ("parameters", Memc[output], SZ_FNAME)
	if (Memc[output] == EOS || access (Memc[output], 0,
	    DIRECTORY_FILE) == YES) {
	    call eprintf ("ERROR: The parameters file is undefined\n")
	    call fntclsb (obslist)
	    call sfree (sp)
	    return
	} else if (access (Memc[output], 0, 0) == NO) {
	    if (fnldir (Memc[output], Memc[dir], SZ_LINE) == 0)
		call strcpy (".", Memc[dir], SZ_LINE)
	    if (access (Memc[dir], APPEND, DIRECTORY_FILE) == NO) {
	    	call eprintf ("ERROR: Cannot open directory %s for writing\n")
		    call pargstr (Memc[dir])
	        call fntclsb (obslist)
	        call sfree (sp)
		return
	    }
	} else if (access (Memc[output], APPEND, TEXT_FILE) == NO) {
	    call eprintf (
	        "ERROR: Cannot open existing parameters file %s for writing\n")
		call pargstr (Memc[output])
	    call fntclsb (obslist)
	    call sfree (sp)
	    return
	}

	# Get the log file.
	call clgstr ("logfile", Memc[logfile], SZ_FNAME)
	if (Memc[logfile] != EOS)
	    call io_logtime (Memc[logfile])

	# Get weighting parameters.
	wtflag = clgwrd ("weighting", Memc[str], SZ_LINE, FWT_OPTIONS)
	addscatter = btoi (clgetb ("addscatter"))

	# Get the fitting parameters.
	tol = clgetr ("tolerance")
	itmax = clgeti ("maxiter")

	# Get the rejection parameters.
	low = clgetr ("low_reject")
	high = clgetr ("high_reject")
	niterate = clgeti ("nreject")
	grow = clgetr ("grow")

	# Get the graphics parameters.
	interactive = clgetb ("interactive")
	call clgstr ("graphics", Memc[graphics], SZ_LINE)

	# Parse the configuration table.
	if (pr_parse (Memc[config]) == ERR) {
	    call eprintf ("ERROR: Cannot parse the configuration file\n")
	    call fntclsb (obslist)
	    call sfree (sp)
	    return
	}

	# Read standard data catalog if the catalog section was in the
	# configuration file, the catalog section is not empty and the
	# catalogs files list. This can be tested by checking the value
	# of the minimum input column, the number of catalog variables
	# and the contents of the standard catalogs list respectively.

	stable = NULL
	ntable = NULL
	nstd = 0

	nstdvars = pr_geti (NCATVARS)
	if (pr_geti (MINCOL) == 1) {
	    getid = NO
	} else if (nstdvars == 0) {
	    getid = YES
	} else if (Memc[catalogs] == EOS) {
	    getid = YES
	    call eprintf ("WARNING: Cannot load catalog variables from ")
	    call eprintf ("the empty catalog files list\n")
	} else { 
	    getid = YES
	    stdlist = fntopnb (Memc[catalogs], NO)
	    call io_gcatdat (Memc[catdir], stdlist, stable, nstd, nstdvars)
	    call fntclsb (stdlist)
	} 

	# Quit if there is no data.
	if (stable != NULL && nstd <= 0) {
	    call eprintf ("ERROR: No data was read from the catalog files")
	    call stclose (stable)
	    call fntclsb (obslist)
	    call pr_free ()
	    call sfree (sp)
	    return
	} 

	# Read in the observations.
	if (clgetb ("log_unmatched"))
	    call io_gcatobs (obslist, stable, nstdvars, getid, Memc[logfile],
	        otable, ntable, nobs)
	else
	    call io_gcatobs (obslist, stable, nstdvars, getid, "", otable,
	        ntable, nobs)

	# Free standard data table since it's no longer needed for the
	# parameter fitting.

	if (stable != NULL)
	    call stclose (stable)

	# Process all transformation equations if there are enough observations.
	if (nobs > 0) {
	    call ft_trneqs (Memc[output], Memc[logfile], Memc[graphics],
	        otable, ntable, wtflag, addscatter, tol, itmax, interactive,
		high, low, niterate, grow, clgetb ("log_fit"),
		clgetb ("log_results"))
	} else if (nstd > 0) {
	    call eprintf ("ERROR: No observations could be matched with ")
	    call eprintf ("the catalog entries\n")
	} else {
	    call eprintf ("ERROR: No observations could be read from ")
	    call eprintf ("the observations files\n")
	}

	# Free all space.
	call pr_free()
	call mct_free (otable)
	if (ntable != NULL)
	    call mct_free (ntable)
	call fntclsb (obslist)

	call sfree (sp)
end
