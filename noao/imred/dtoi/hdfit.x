include	<math/curfit.h>
include	<imhdr.h>
include	<fset.h>
include	<mach.h>
include	<ctype.h>
include	<error.h>
include	<pkg/gtools.h>
include	<pkg/xtanswer.h>
include	"hdicfit/hdicfit.h"

# T_HDFIT -- Fit a curve.  This task fits a characteristic
# curve to density and log exposure data read from an input
# database.  The interactive curve fitting package is used.
# The database is updated to contain the values of the fit 
# necessary to reinitialize the curfit package for performing 
# the transformation in hdtoi. 

procedure t_hdfit ()

pointer	sp, fcn, device, db, gt, exp, wts, save, trans, weight
pointer	dbfile, ic, den, errs
int	db_list, order, interactive, nsave, nvals, wt_type, update
real	ref_fog, real_fog
double	fog, maxden

pointer	ddb_map(), gt_init()
bool	clgetb(), fp_equalr(), fp_equald()
int	clpopni(), clgeti(), strncmp(), clgfil(), hd_fit()
real	ic_getr()

begin
	call smark  (sp)
	call salloc (fcn, SZ_FNAME, TY_CHAR)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (trans, SZ_FNAME, TY_CHAR)
	call salloc (weight,  SZ_FNAME, TY_CHAR)
	call salloc (dbfile, SZ_FNAME, TY_CHAR)

	# Get cl parameters
	db_list = clpopni ("database")
	call clgstr ("function", Memc[fcn], SZ_FNAME)
	call clgstr ("transform", Memc[trans], SZ_FNAME)
	order = clgeti ("order")

	# Decide which type of weighting the user wants
	call clgstr ("weighting", Memc[weight], SZ_FNAME)
	if (strncmp (Memc[weight], "none", 1) == 0)
	    wt_type = WT_NONE
	else if (strncmp (Memc[weight], "user", 1) == 0)
	    wt_type = WT_USER
	else if (strncmp (Memc[weight], "calc", 1) == 0)
	    wt_type = WT_CALC
	else
	    call error (0, "Unrecognized weighting type")

	# Initialize interactive curve fitting package.
	gt = gt_init ()
	call ic_open (ic)
	call ic_pstr (ic, "function", Memc[fcn])
	call ic_pstr (ic, "transform", Memc[trans])
	call ic_puti (ic, "order", order)
	call ic_pstr (ic, "ylabel", "Log Exposure")
	call ic_pkey (ic, 5, 'y', 'u')

	if (clgetb ("interactive")) {
	    interactive = YES
	    call clgstr ("device", Memc[device], SZ_FNAME)
	} else {
	    interactive = NO
	    call strcpy ("", Memc[device], SZ_FNAME)
	}

	# Read information from each dlog file; accumulate number of values.
	# The density (not fog subtracted) is returned.  The density values
	# are also sorted in increasing order.

	call hd_rdloge (db_list, exp, den, wts, errs, nvals, fog, maxden,
	    wt_type)
	if (nvals == 0)
	    call error (1, "T_HDFIT: No data values in sample")

	call hd_sdloge (Memd[den], Memd[exp], Memd[wts], Memd[errs], nvals)
	call ic_putr (ic, "fog", real(fog))
	ref_fog = real (fog)
	call ic_putr (ic, "rfog", ref_fog)

	# Initialize the dtoi/icgfit interface.
	if (fp_equald (maxden, 0.0D0))
	    call error (1, "Saturated pixel density not initialized")
	call hdic_init (Memd[den], nvals, maxden)

	update = hd_fit (ic, gt, Memd[den], Memd[exp], Memd[wts], Memd[errs],
	    nvals, save, nsave, Memc[device], interactive)
	     
	if (update == YES) {
	    # Record fit information in (each) database
	    call ic_gstr  (ic, "function", Memc[fcn], SZ_FNAME)
	    call ic_gstr  (ic, "transform", Memc[trans], SZ_FNAME)
	    real_fog = ic_getr (ic, "fog")

	    while (clgfil (db_list, Memc[dbfile], SZ_FNAME) != EOF) {
	        db = ddb_map (Memc[dbfile], APPEND)
	        call ddb_ptime (db)
	        # Add new fog record if it was changed interactively.
	        if (!fp_equalr (real_fog, ref_fog)) {
		    call ddb_prec (db, "fog")
		    call ddb_putr (db, "density", real_fog)
	        }

	        call ddb_prec (db, "cv")
	        call ddb_pad  (db, "save", Memd[save], nsave)
	        call ddb_pstr (db, "function", Memc[fcn])
	        call ddb_pstr (db, "transformation", Memc[trans])

	        call ddb_unmap (db)
	    }
	}

	call ic_closed (ic)
	call mfree (save, TY_DOUBLE)
	call mfree (den,  TY_DOUBLE)
	call mfree (exp,  TY_DOUBLE)
	call mfree (wts , TY_DOUBLE)
	call mfree (errs, TY_DOUBLE)

	call gt_free (gt)
	call clpcls (db_list)
	call sfree (sp)
end


# HD_RLOGE -- Read log exposure, density and weights from a single dloge
# database file.  Pointers to the three arrays are returned as arguments.
# The number of values accumulated is returned also; note that the value
# of nvals is changed upon return; it should not be given as a constant.
# If more than one database is being read (as in HDSHIFT applications),
# the density ABOVE fog is returned, and the reference fog values set = 0.0
# The maximum density, the density of a saturated pixel, is read from the
# first databas and returned.

procedure hd_rdloge (db_list, exp, den, wts, errs, nvals, fog, maxden, wt_type)

int	db_list			# File descriptor for data base file
pointer	exp			# Pointer to exposure array - returned
pointer	den			# Pointer to density array - returned
pointer	wts			# Pointer to weights array - returned
pointer	errs			# Pointer to std deviation array - returned
int	nvals			# Number of data pairs read - returned
double	fog			# Value of fog read from database - returned
double  maxden			# Maximum density, read from db - returned
int	wt_type			# Type of weighting

pointer	db
bool	sdevrec
int	buflen, off, rec
int	nden, nexp, nwts, nspots, nerrs, nfiles
char	dloge[SZ_FNAME]

pointer	ddb_map()
bool	fp_equald()
int	ddb_locate(), ddb_geti(), clgfil(), imtlen()
real	ddb_getr()
errchk	ddb_locate, ddb_gad, malloc, ddb_map, ddb_unmap

begin
	nvals = 0
	off = 0
	buflen = NSPOTS
	nfiles = imtlen (db_list)
	maxden = 0.0D0

	# Dynamically allocate memory for arrays; it can be increased later.
	call malloc (exp, buflen, TY_DOUBLE)
	call malloc (den, buflen, TY_DOUBLE)
	call malloc (wts, buflen, TY_DOUBLE)
	call malloc (errs, buflen, TY_DOUBLE)

	while (clgfil (db_list, dloge, SZ_FNAME) != EOF) {
	    iferr (db = ddb_map (dloge, READ_ONLY)) {
		call erract (EA_WARN)
		next
	    }

	    # Get fog value to be subtracted from density
	    rec = ddb_locate (db, "fog")
	    fog = double (ddb_getr (db, rec, "density"))

	    # Get density array
	    rec = ddb_locate (db, "density")
	    nden = ddb_geti (db, rec, "den_val")

	    call ddb_gad (db, rec, "den_val", Memd[den+off], nden, nden)
	    if (nfiles > 1)
		call asubkd (Memd[den+off], fog, Memd[den+off], nden)

	    # Get log exposure array
	    rec = ddb_locate (db, "exposure")
	    nexp = ddb_geti (db, rec, "log_exp")
	    call ddb_gad (db, rec, "log_exp", Memd[exp+off], nexp, nexp)

	    # Get saturated pixel density if not already set
	    if (fp_equald (maxden, 0.0D0)) {
	        iferr (rec = ddb_locate (db, "common")){
		    ;
		} else
	            maxden = double (ddb_getr (db, rec, "maxden"))
	    }

	    # Get std deviation array
	    sdevrec = true
	    iferr {
	        rec = ddb_locate (db, "standard deviation")
	        nerrs = ddb_geti (db, rec, "sdev_val")
	        call ddb_gad (db, rec, "sdev_val", Memd[errs+off], nerrs, nerrs)
	    } then {
		call erract (EA_WARN)
		call eprintf ("Marker type '[ve]bar' can only show weights\n")
		call amovkd (0.0D0, Memd[errs+off], nden)
		sdevrec = FALSE
	    }

	    if (wt_type == WT_CALC) {
	        if (sdevrec) {
		    iferr {
                        nspots = min (nden, nexp, nerrs)
                        call adivd (Memd[den+off], Memd[errs+off], 
			    Memd[wts+off], nspots)
	            } then {
		        call erract (EA_WARN)
		        call eprintf ("All weights set to 1.0\n")
	                call amovkd (double (1.0), Memd[wts+off], nspots)
	            }
	        } else {
		    nspots = min (nden, nexp)	
		    call eprintf ("No sdev record; All weights set to 1.0\n")
	            call amovkd (double (1.0), Memd[wts+off], nspots)
	        }
	    }

	    if (wt_type == WT_USER) {
	        # WT_USER: fill "user" weights array
	        iferr {
	            rec = ddb_locate (db, "weight")
	            nwts = ddb_geti (db, rec, "wts_val")
		    nspots = min (nden, nexp, nwts)
	            call ddb_gad (db, rec, "wts_val", Memd[wts+off], nwts, nwts)
	        } then {
	            # Users weights can't be found. Set weights array to 1.0's.
		    call erract (EA_WARN)
		    call eprintf ("All weights set to 1.0\n")
	            nspots = min (nden, nexp)
	            call amovkd (double (1.0), Memd[wts+off], nspots)
		}
	    }

	    if (wt_type == WT_NONE) {
	        # WT_NONE: fill "none" weights array
	        nspots = min (nden, nexp)
		call amovkd (double (1.0), Memd[wts+off], nspots)
	    }
    
	    # Increment number of counts; reallocate memory if necessary.
	    nvals = nvals + nspots
	    off = off + nspots

	    if (nvals > buflen) {
		buflen = buflen + NSPOTS
		call realloc (exp, buflen, TY_DOUBLE)
		call realloc (den, buflen, TY_DOUBLE)
		call realloc (wts, buflen, TY_DOUBLE)
		call realloc (errs, buflen, TY_DOUBLE)
	    }

	    call ddb_unmap (db)
	}

	if (nfiles > 1)
	    fog = 0.0D0
	call clprew (db_list)
end


# HD_SLOGE -- Sort the log exposure, density and weight information in order
# of increasing exposure value.  The sorting is done is place.  The three
# data values are assummed matched on input, that is, exposure[i] matches
# density[i] with weight[i] for all array entries.

procedure hd_sdloge (density, exposure, weights, errors,  nvals)

double	density[nvals]		# Density array
double	exposure[nvals]		# Exposure array
double	weights[nvals]		# Weights array
double	errors[nvals]		# Standard deviation array
int	nvals			# Number of values in data arrays

int	i, j
double	temp
define	swap	{temp=$1;$1=$2;$2=temp}

begin
	# Bubble sort - inefficient, but sorting is done only once on
	# an expected small sample size (16 pts typically). 

	for (i = nvals; i > 1; i = i - 1)
	    for (j = 1; j < i; j = j + 1) 
		if (density [j] > density [j+1]) {

		    # Out of order; exchange values
		    swap (exposure[j], exposure[j+1])
		    swap ( density[j],  density[j+1])
		    swap ( weights[j],  weights[j+1])
		    swap (  errors[j],   errors[j+1])
		}
end


# HD_FIT -- Fit the curve to input density, exposure and weight values.
# The fit can be performed interactively or not. 

int procedure hd_fit (ic,
	gt, den, exp, wts, errs, nvals, save, nsave, dev, interact)

pointer	ic
pointer	gt			# Graphics tools pointer
double	den[ARB]		# Density values
double	exp[ARB]		# Exposure values
double	wts[ARB]		# Weight array
double	errs[ARB]		# Standard deviation array
int	nvals			# Number of data pairs to fit
pointer	save			# ??
int	nsave			# ??
char	dev[SZ_FNAME]		# Interactive graphics device
int	interact		# Flag for interactive graphics

pointer	gp, cv, sp, x, dum
pointer	gopen()
int	update, dcvstati()
errchk	malloc, gopen

begin
	if (interact == YES) {
	    gp = gopen (dev, NEW_FILE, STDGRAPH)
	    call icg_fitd (ic, gp, "cursor", gt, cv, den, exp, wts, errs, nvals)
	    call gclose (gp)
	    update = IC_UPDATE(ic)

	} else {
	    # Do fit non-interactively
	    call smark (sp)
	    call salloc (x, nvals, TY_DOUBLE)
	    call salloc (dum, nvals, TY_INT)
	    call hdic_transform (ic, den, wts, Memd[x], wts, Memi[dum], nvals)
	    call ic_fitd (ic, cv, Memd[x], exp, wts, nvals, YES, YES, YES, YES)
	    call sfree (sp)
	    update = YES
	}

	nsave = (dcvstati (cv, CVORDER)) + 7
	call malloc (save, nsave, TY_DOUBLE)
	call dcvsave (cv, Memd[save])
	call dcvfree (cv)

	return (update)
end
